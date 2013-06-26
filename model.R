#--------------------------------------------------------------------------------
# * 8 zero-inflated variables... 
      # for count data (response variable) - zero-inflated poisson and zero-inflated negative binomaial models (or just regular negative binomaial usually works)
#*# took covariates out that had zero variance
mod <- formula( log(price) ~ #transform(andemand_copy)##   
                               transformed.demand##
                             + log(depth)##
                             + log(height)##
                             + log(width)##
                             + log(weight)##
                             + log(boxvol)##
                             + moldcomplexity## log transformation?     #surfacea/partvol
                             + log(corevol+1)## 
                             #+ cores##
                             + ports## * 0.89 *
                             + portvol## * 0.89 *
                             #+ nrisers##
                             + log(surfacea)##
                             #+ surfaceareamachining##
                             + balance## * 0.96 *
                             + log(flatness+1) # * 0.48 * log transformation?
                             + partinglineperim## log transformation?
                             + drillholes## * 0.35 *
                             + drillholevol## * 0.35 *
                             + is_assembly##
                             #*#+ exnumsplineteeth # zero variance
                             #*#+ exsplinetoothsize # zero variance
                             #*#+ exsplinecount # zero variance
                             #*#+ innumsplineteeth # zero variance
                             #*#+ insplinetoothsize # zero variance
                             #*#+ insplinecount # zero variance
                             #*#+ exnumgearteeth # zero variance
                             ###+ exgeartoothsize  # too few, zero variance
                             + factor(exgearcount) # zero variance... ok since factor
                             #+ innumgearteeth # zero variance
                             #+ ingeartoothsize # zero variance
                             #+ ingearcount # zero variance
                             + pressuretestair## * 0.90 *
                             + pressuretestwater## * 0.93 *
                             #*#+ pressuretestoil## zero variance
                             #*#+ pressuretestfuel## zero variance
                             + factor(heattreatspecbins)## zero variance... ok
                             + factor(cleaningspec)## zero variance... ok
                             #+ factor(identificationspec) #
                             + factor(brazedweldedspecbins)## zero variance... ok
                             #+ factor(paintingspec) 
                             #+ factor(coatedspec)
                             + factor(matspecbins)##
                             + factor(rsf)
                             #####+ factor(complex)  #all false 101012

                             ###+ factor(heattreatspecbins) : log(boxvol)
                             #+ factor(cleaningspec) : log(surfacea)
                             #+ factor(paintingspec) : log(surfacea)  
                             ###+ factor(matspecbins) : log(surfacea)
                             ###+ factor(matspecbins) : log(flatness+1)
                             ###+ factor(matspecbins) : log(weight)
                             ###+ factor(matspecbins) : partinglineperim
                             ###+ factor(matspecbins) : drillholes
                             ###+ factor(matspecbins) : exgeartoothsize##
                             ###+ factor(matspecbins) : ingeartoothsize##
                             #+ factor(matspecbins) : exsplinetoothsize##
                             #+ factor(matspecbins) : insplinetoothsize##
               )
fmlp <- lm(mod, data=subset(parts.df, parts.df$inModel))
fmlpstep <- step(fmlp, trace=F)
# final model has 17 predictors (and an intercept) of the possible 25

#------------------------------------------------------------------------------

parts.df <- read.csv("~/Documents/Intern_Model/052113_intern_model/castings_data.csv", header = TRUE, stringsAsFactors=FALSE)

library(caret)
library(ggplot2)

# find the covariates that have zero varaince and remove them from the model
names_zero_var <- names(parts_df[, nearZeroVar(parts_df)])

# list of factors, including y (price), to include in the full model
factors <- c("price", "rsf", "is_assembly", "boxvol", "depth", "height", "width", "balance", "weight", "surfacea", "flatness", "partinglineperim", "drillholevol", "drillholes", "ports", "portvol", "corevol", "cleaningspec", "pressuretestair", "pressuretestwater", "exgearcount", "moldcomplexity","heattreatspecbins","matspecbins", "brazedweldedspecbins", "transformed.demand")

train1 <- subset(parts.df, parts.df$inModel, select=factors) # train1 <- parts_df[, factors]

train1$logprice <- log(train1$price+1) # train1$price <- log(train1$price +1) instead of having to take out price later
train1$price <- NULL # otherwise the model uses price as a variable when using the train function

sample1 <- sample(1:nrow(train1), .7*nrow(train1))
# use 70% of the data to select a model and predict on the remaining 30%

# train2: the six factor variables are factors instead of numeric as in train1
# train3: log of the 8 variables as listed in above model, mod
fmlp_lars2 <- train(logprice~., data=train2[sample1,], method="lars", trControl=trainControl(predictionBounds = c(0, NA))) # higher Rsquared
# lars is similar to lasso (I get errors with lasso)
# with trainControl we can specify that price cannot be predicted below 0
predictors(fmlp_lars2) # all 25 variables, R squared=0.61
fmlp_predict_lars2 <- predict(fmlp_lars2, newdata = train2[-sample1,]) 

fmlp_lars3 <- train(logprice~., data=train2[sample1,], method="lars", trControl = trainControl(selectionFunction="tolerance", predictionBounds = c(0, NA)))
# models with less variables are preferred
# if there is an insignificant change in the RMSE, the model with less variables is chosen
predictors(fmlp_lars3) # 14 predictors, Rsqaured=0.596
fmlp_predict_lars3 <- predict(fmlp_lars3, newdata = train2[-sample1,], testY = train2[-sample1, 26])

var <- varImp(lars2, scale=FALSE)
plot(var) 
# note that all of the six predictors that are factors in the full model are of minimal importance
# for linear models the absolute value of the t-statistic for each model parameter is used

# list of the 13 most important variables
importance <-  c("price", "boxvol", "depth", "height", "width", "weight", "surfacea", "flatness", "partinglineperim", "drillholevol", "drillholes", "corevol", "moldcomplexity","transformed.demand")

train.importance <- subset(parts.df, parts.df$inModel, select = importance)
train.importance$price <- log(train.importance$price + 1)

# see if a model with the most important variables (based on t-statistic for each parameter in the model) is better
fmlp_larsimp <- train(price~., data=train.importance, method = "lars")
varimp <- varImp(fmlp_larsimp, top = 13) # top 13 are in the plot, which are also the 13 in the model
# lars3 (which has 14 predictors) is better than larsimp (the 13 most important variables)
# could just be becasue there is one more variables in the model

# to count the number of predictors in the final model excluding repeated factors
aa <- predictors(pp_LGOCV)
aaa <- gsub("[0-9]", "", aa)
length(unique(aaa))

#-------------------------------------------------------------

# try LOOCV in trainControl options
lars_LOOCV <- train(logprice~., data=train2, method="lars", trControl = trainControl(method = "LOOCV", selectionFunction="tolerance", predictionBounds = c(0, NA), savePredictions=TRUE))
# all 25 predictors
# takes way too long, LGOCV is much faster

lars_LGOCV <- train(logprice~., data=train2, method="lars", trControl = trainControl(method = "LGOCV", selectionFunction="tolerance", predictionBounds = c(0, NA), savePredictions=TRUE))
# best choice: final model has 14 predictors, Rsqaured = 0.599 (highest)
# "selectionFunction=tolerance" ranks simpler models higher if the change in RMSE is not significant
# "method=LGOCV" (Leave Group Out Cross Validation) leaves a group out, trains the model 
    # with the others, and uses the excluded group to predict and cycles through 
    # until each group has been used to predict
# the "predictionBounds" restricts price predictions to positive values
residuals <- resid(lars_LGOCV) # sum= -3.052199, mean = -0.000201
# residuals from train procedure

predictions_LGOCV <- train2[,26]-residuals 
# use the residuals to get the price predictions made during the train procedure

# some diagnostic plots

plot(lars_LGOCV) # RMSE - as more variables are added, RMSE goes down
plot(lars_LGOCV, plotType="line", metric="Rsquared") # Rsquared, get larger with more variables
# the final value used for the  model was fraction = 0.525 (14 of the 25 predictors)

LGOCV_whole <- as.data.frame(cbind(predictions_LGOCV, residuals))

ggplot(LGOCV_whole) + geom_point(aes(x=1:length(residuals), y=residuals), alpha=.3) + geom_abline(intercept=0, slope=0, colour="red") + ggtitle("Observations vs Residuals \n Final Model") + xlab("Observation Number") + ylab("Residuals")
ggsave("~/Documents/Intern_Model/ObsVSResidual.jpeg")
# should not see a pattern: no observable pattern

# predicted (fitted) vs residuals
ggplot(LGOCV_whole)+geom_point(aes(x=predictions_LGOCV, y=residuals), alpha=.3)+geom_abline(intercept=0, slope=0, colour="red") + ggtitle("Predicted vs Residuals \n Final Model") + xlab("Predicted") + ylab("Residuals")
ggsave("~/Documents/Intern_Model/PredictedVSResiduals.jpeg")
# should not see a pattern: no clear pattern

# qqplot of residuals
ggplot(LGOCV_whole, aes(sample=residuals)) + stat_qq() + geom_abline(intercept=0, slope=1, colour="red") + ggtitle("QQ Plot of Residuals \n Final Model") + xlab("Normal Quantiles") + ylab("Residuals")
ggsave(file = "~/Documents/Intern_Model/qqplot.jpeg")
# should be linear: appears somewhat linear so the residuals are approximately normally dist.

# list of the 14 variables in the final model
vars_LGOCV = c("rsf", "boxvol", "depth", "height", "width", "balance", "surfacea", "partinglineperim", "drillholes", "pressuretestair", "moldcomplexity", "heattreatspecbins", "matspecbins", "transformed.demand")

# variable vs residuals plots
for(var in vars_LGOCV){
  ggplot(train2) + geom_point(aes(x=train2[[var]], y=residuals), alpha=0.3)+ggtitle(paste0(var, " vs Residuals \n Final Model")) + xlab(var) +ylab("Residuals")
  ggsave(file=paste0("~/Documents/Intern_Model/VariablePlots/", var, ".jpeg"))
}
# should not see a pattern: many funnel out
# the model may not fully explain the effect of the variables (that show a pattern) on price

# -----
# list of factors in the model, that will not be preProcessed
factor <- list("rsf", "is_assembly", "cleaningspec", "exgearcount","heattreatspecbins","matspecbins", "brazedweldedspecbins", "price")
numeric <- c("boxvol", "depth", "height", "width", "balance", "weight", "surfacea", "flatness", "partinglineperim", "drillholevol", "drillholes", "ports", "portvol", "corevol", "pressuretestair", "pressuretestwater", "moldcomplexity","transformed.demand")

PreProcess <- preProcess(x = subset(train2, select = numeric), method = "BoxCox")
# transformed 8 of the 17 numeric varibles
# cannot estimate lambda for the variables I thought needed it
# PreProcess$bc to get the box cox estimates

pp_LGOCV <- train(logprice~., data=train2, method="lars", preProcess="BoxCox", trControl = trainControl(method = "LGOCV", selectionFunction="tolerance", predictionBounds = c(0, NA), savePredictions=TRUE))
# added preProcessesing using box cox transformations
# higher Rsquared, should be 14 variables as well (final value for the model was fraction=0.525)
# predictors(pp_LGOCV) says it uses all variables...
trans <- pp_LGOCV$preProcess
trans$bc # displays box cox transformation values
# transformed transformed.demand (1.7), moldcomlexity (0.3), boxvol (0), depth (0), height (0), width (0), etc
# 0 is equivalent to a log transformation

residuals_pp <- resid(pp_LGOCV) # sum= -3.052199, mean = -0.000201
# residuals from train procedure

predictions_pp <- train2[,26]-residuals 

LGOCV_pp <- as.data.frame(cbind(predictions_pp, residuals_pp))

ggplot(LGOCV_pp) + geom_point(aes(x=1:length(residuals_pp), y=residuals_pp), alpha=.3) + geom_abline(intercept=0, slope=0, colour="red") + ggtitle("Observations vs Residuals \n Final Model") + xlab("Observation Number") + ylab("Residuals")
ggsave("~/Documents/Intern_Model/ObsVSResidual.jpeg")
# should not see a pattern: no observable pattern

# predicted (fitted) vs residuals
ggplot(LGOCV_pp)+geom_point(aes(x=predictions_pp, y=residuals_pp), alpha=.3)+geom_abline(intercept=0, slope=0, colour="red") + ggtitle("Predicted vs Residuals \n Final Model") + xlab("Predicted") + ylab("Residuals")
ggsave("~/Documents/Intern_Model/PredictedVSResiduals.jpeg")
# should not see a pattern: no clear pattern

# qqplot of residuals
ggplot(LGOCV_pp, aes(sample=residuals_pp)) + stat_qq() + geom_abline(intercept=0, slope=1, colour="red") + ggtitle("QQ Plot of Residuals \n Final Model") + xlab("Normal Quantiles") + ylab("Residuals")
ggsave(file = "~/Documents/Intern_Model/qqplot.jpeg")
# should be linear: appears somewhat linear so the residuals are approximately normally dist.

# variable vs residuals plots
for(var in factors){
  ggplot(train2) + geom_point(aes(x=train2[[var]], y=residuals_pp), alpha=0.3)+ggtitle(paste0(var, " vs Residuals \n Final Model")) + xlab(var) +ylab("Residuals")
  ggsave(file=paste0("~/Documents/Intern_Model/VariablePlots/pp_", var, ".jpeg"))
}
# no significant change in any of the plots when preprocessing is done...
