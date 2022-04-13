

#Part 1
#IQ is the DV the 5 test results are the 5 IVS.
#Load data
#Backward Selection, start a model with everything in it.
FitAll1 <- lm(IQ ~ ., data = IQ)

summary(FitAll1)

step(FitAll1, direction = 'backward')
#Test 1,2, and 4 make the model. AIC= 71.69 (smaller AIC the better)
#Create another model for only selected variables and compare their results
fitsome <- lm(IQ ~ Test1 + Test2 + Test4, data = IQ)
summary(fitsome)

##Which model is the best? Why?
#The model using tests 1, 2 and 4 because that had the smallest AIC. the three tests kept are the best predictors of IQ.
##From the best model, what is the adjusted R2 value and what does it mean?
#The adjusted R2 value is 0.2158 and it means that thi model explains 21.6% of the variation of IQ variable, taken with the # of terms in the model.
##From the best model, how does each variable influence IQ?
# Test 1 coefficient is -1.965 so if the other two test results are the same this test will estimate the IQ as almost 2 points lower and so on with the following coefficents.
#Test 2 coefficient is -1.649. Test 4 coefficients 3.789.

#Part2 compare the three with the X Y data set. 

#Backward Selection
FitAll = lm(Y ~ ., data = stepwiseRegression)
summary(FitAll)

step(FitAll, direction = 'backward', scope = formula(FitAll))
#Best model here is with X2 + X4 + X6 + X10 + X11 + X12. AIC is 213.8. 
#coefficents are X2=-0.06975 X4 =2.80806 X6 = 5.98660 X10 = -11.97829 X11 =  -0.13102 X12=-25.98121  


#Forward Selection
fitstart = lm(Y ~ 1, data = stepwiseRegression)
summary(fitstart)

step(fitstart, direction = 'forward', scope = (formula(FitAll)))
#Here the best fit model is X6 + X4 + X12 + X10 + X2 + X11 (so the same probably not going to change)
# AIC is 213.38

#StepWise 
step(fitstart, direction = 'both', scope = formula(FitAll))

fitsome <- lm(formula = Y ~ X2 + X4 + X6 + X10 + X11 + X12, data = stepwiseRegression)
summary(fitsome)
#output is the same. 
#Compare the output from the different types of stepwise selections above and note the order of the elimination of features
#They all come to the same model. I like the Hybrid output. the elimination happens differently backward eliminates by first in, first out. forward starts with the lowest AIC variable and keeps adding the lower AIC variables to make the model.Stepwise adds them as they minimize that AIC and removes as they raise it.
