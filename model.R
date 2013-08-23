library(caret)
library(ggplot2)

dir <- "~/Documents/Intern_Model/"

# original linear model (using the stepwise procedure) had an R2 of 0.6087 and RMSE of 0.9344 but these are not from testing the model, they are from training, so they may be optimistic

df <- read.csv(paste0(dir, "052113_intern_model/castings_data.csv"), header = TRUE, stringsAsFactors=FALSE)
factors <- c("price", "rsf", "financerwt", "familyname", "segmentname", "partname", "classname", "coarse", "medium", "is_assembly", "partvol", "boxvol", "depth", "height", "width", "nrisers", "balance", "weight", "surfacea", "flatness", "maxflatness", "partinglineperim", "drillholevol", "drillholes", "ports", "cores", "corevol", "brazedweldedspecbins", "cleaningspec", "pressuretestair", "pressuretestoil", "pressuretestwater", "pressuretestfuel", "pressuretesthydraulic", "exnumsplineteeth", "exsplinetoothsize", "exsplinecount", "innumsplineteeth", "insplinetoothsize", "insplinecount", "exnumgearteeth", "exgeartoothsize", "exgearcount", "innumgearteeth", "ingeartoothsize", "ingearcount", "moldcomplexity", "matspecbins", "transformed.demand", "complex")
# list of to evaluate in the model (removed 28 meaningless predictors such as partnum etc)
df <- subset(df, inModel==TRUE, select = factors)

# remove any predictors with zero variance
zero_var <- nearZeroVar(df)
df <- df[,-zero_var] # 20 predictors were removed

num <- c("boxvol", "price", "partvol", "depth", "height", "width", "weight", "nrisers","surfacea", "flatness", "maxflatness", "partinglineperim", "drillholevol", "drillholes", "ports", "cores", "corevol", "pressuretestair", "pressuretestwater","moldcomplexity", "transformed.demand")
# lambda:   0        0.1         0       0.1       0        0.1       0          NA        0          NA            NA              0.1                NA             NA          NA        NA       NA              NA                  NA              0.3                 1.7      

trans <- preProcess(df[, num], method = c("BoxCox", "center", "scale"))
# only calculates the transforamtion, does not apply it to the data
# x has to be a data frame... all numeric
# a transformation of 0 is a log transformation

# apply the transformation, 12 predictors will be transformed
df[, num] <- predict(trans, df[, num])

corr <- cor(subset(df, select=num))
# plot of correlations, reordered to reveal clusters of highly correlated variables
library(corrplot)
corrplot(corr, order="hclust")

# returns column numbers recommended for deletion if the correlation is over the cutoff
highCorr <- findCorrelation(corr, cutoff = .8)
names(df[, highCorr]) # make sure price or another important predictor is not going to be removed
df <- df[, -highCorr] # 6 predictors were removed (is_assembly, financerwt, depth, partname, partvol...)

predictors <- c("rsf", "familyname", "classname", "coarse", "medium", "is_assembly", "partvol", "boxvol", "depth", "height", "width", "nrisers", "weight", "surfacea", "flatness", "maxflatness", "partinglineperim", "drillholevol", "drillholes", "ports", "cores", "corevol", "pressuretestair", "pressuretestwater", "moldcomplexity", "matspecbins", "transformed.demand")

for(pred in predictors){
  ggplot(test) + geom_point(aes(x=test[[pred]], y=test$price), alpha=0.3)+ ggtitle(paste0(pred, " vs Price")) + xlab(pred) +ylab("Price")
  ggsave(file=paste0(dir, "Diagnostic_Plots/VarVSPrice/", pred, ".jpeg"))
}
# after transforming the data, the predictors should show a linear relationship with price
# if they do not, quadratic terms may need to be considered 
# corevol, drillholes, drillholevol, flatness, maxflatness

# 75 - 25 train-test split
samp <- sample(nrow(df), .75*nrow(df))

train <- df[samp,]
test <- df[-samp,]

factor <- list("rsf", "familyname", "classname", "coarse", "medium", "matspecbins", "is_assembly")
# these seven predictors need to be converted to factors in the dataset for prediction

ctrl <- trainControl(method = "cv", savePredictions = TRUE, selectionFunction = "tolerance", verboseIter = TRUE)

lars <- train(price~., data=train, method = "lars", trControl = ctrl, tuneLength = 10)
test$lars <- predict(lars, newdata = test[, 1:28]) # predictors(lars) 15 predictors
# RMSE .593 (.842) , R2 .648 (.449)

svm <- train(price~., data=train, method = "svmRadial", trControl = ctrl, tuneLength = 10)
test$svm <- predict(svm, newdata = test[, 1:28]) # all 27 predictors
# took 3 hours...
# RMSE .606, R2 .639... got an error when predicting

mars <- train(price~., data=train, method = "mars", trControl = ctrl, tuneLength = 10)
test$mars <- predict(mars, newdata = test[, 1:28]) # 8 predictors
# took almost 2 hours...
# RMSE .57, R2 .674... got an error when predicting

glm <- train(price~., data=train, method = "glm", trControl = ctrl, tuneLength = 10)
test$glm <- predict(glm, newdata = test[, 1:28]) # all 27 predictors
# RMSE .589 (.6075), R2 .652 (.635)

# campare the four models 
resamp <- resamples(list(lars = lars, svm = svm , mars = mars, glm = glm))
diff <- diff(resamp, list("lars", "svm", "mars", "glm"))

# visualization that no model is significantly better than another
bwplot(diff, what = "differences") 
dotplot(diff, what = "differences")
# lars is simpler (faster)

plot(lars, metric = "Rsquared")

residuals <- test$price - test$lars

R2(test$lars, test$price) # R2(observed, predicted)
RMSE (test$lars, test$price)

# ----
# Diagnostic Plots

# residual plot
ggplot(test) + geom_point(aes(x=1:length(residuals), y=residuals), alpha=.3) + geom_abline(intercept=0, slope=0, colour="red") + geom_abline(intercept = 3, slope = 0, colour = "orange") +  geom_abline(intercept = -3, slope = 0, colour = "orange") + ggtitle("Observation vs Residuals") + xlab("Observation Number") + ylab("Residuals")
ggsave(paste0(dir, "Diagnostic_Plots/ObsVSResidual.jpeg"))
# should not see a pattern: no observable pattern

# Observed Price vs Residuals
ggplot(test)+geom_point(aes(x=test$price, y=residuals), alpha=.3)+ geom_abline(intercept = 0, slope = 1, colour = "red") +  ggtitle("Observed (Actual) Price vs Residuals") + xlab("Observed Price") + ylab("Residuals")
ggsave(paste0(dir, "Diagnostic_Plots/ObservedPriceVSResiduals.jpeg"))

# predicted (fitted) vs residuals
ggplot(test)+geom_point(aes(x=test$lars, y=residuals), alpha=.3)+geom_abline(intercept=0, slope=0, colour="red") + geom_abline(intercept = 3, slope = 0, colour = "orange") +  geom_abline(intercept = -3, slope = 0, colour = "orange")+ ggtitle("Predicted Price vs Residuals") + xlab("Predicted Price") + ylab("Residuals")
ggsave(paste0(dir, "Diagnostic_Plots/PredictedVSResiduals.jpeg"))
# should not see a pattern: no clear pattern

# qqplot of residuals
ggplot(test, aes(sample=residuals)) + stat_qq() + geom_abline(intercept=0, slope=1, colour="red") + ggtitle("QQ Plot of Residuals") + xlab("Normal Quantiles") + ylab("Residuals")
ggsave(file = paste0(dir, "Diagnostic_Plots/qqplot.jpeg"))
# should be linear: appears somewhat linear so the residuals are approximately normally dist.

vars <- predictors(lars)
vars <- c("rsf", "familyname", "medium", "partvol", "boxvol", "depth", "weight", "surfacea", "drillholevol", "drillholes", "cores", "corevol", "pressuretestair", "matspecbins", "transformed.demand")
# variable vs residuals plots
for(var in vars){
  ggplot(test) + geom_point(aes(x=test[[var]], y=residuals), alpha=0.3)+ggtitle(paste0(var, " vs Residuals")) + xlab(var) +ylab("Residuals")
  ggsave(file=paste0(dir, "Diagnostic_Plots/VarVSResiduals/", var, ".jpeg"))
}
# should not see a pattern