

#Load in Libraries
library("caret")
library("magrittr")
library("dplyr")
library("tidyr")
library("lmtest")
library("popbio")
library("e1071")

#data already binary
# Run the Base Logistic Model
mylogit <- glm(Gold ~ Antimony, data=minerals, family="binomial")

#Predict Wins and Losses
probabilities <- predict(mylogit, type = "response")

#
minerals$Predicted <- ifelse(probabilities > .5, "pos", "neg")

#recode
minerals$PredictedR <- NA
minerals$PredictedR[minerals$Predicted=='pos'] <- 1
minerals$PredictedR[minerals$Predicted=='neg'] <- 0

#factor time
minerals$PredictedR  <- as.factor(minerals$PredictedR )
minerals$GoldR  <- as.factor(minerals$Gold)

#confusion matrix
conf_mat <- caret::confusionMatrix(minerals$PredictedR, minerals$GoldR)
conf_mat
#failed at (0,1)

#Linearity

minerals1 <- minerals %>%
dplyr::select_if(is.numeric)
predictors <- colnames(minerals1)
minerals1 <- minerals1 %>%
mutate(logit=log(probabilities/(1-probabilities))) %>%
gather(key="predictors", value="predictor.value", -logit)

#graph to assess for linearity
ggplot(minerals1, aes(logit, predictor.value))+
  geom_point(size=.5, alpha=.5)+
  geom_smooth(method= "loess")+
  theme_bw()+
  facet_wrap(~predictors, scales="free_y")
#Antimony is so linear!
#calling regression model

summary(mylogit)
#Interp : seems significant. Did I possibly do the confusion chart wrong? possible.

#graph model
logi.hist.plot(minerals$Antimony,minerals$Gold, boxp=FALSE, type="hist", col="gray")
