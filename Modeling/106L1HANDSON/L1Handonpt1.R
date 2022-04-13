


#DSO106 Modeling L1 Practice Hands-on Solution

#Load in Libraries
library("car")
library("caret")
library("gvlma")
library("predictmeans")

#Test Assumptions

# Linearity

scatter.smooth(x=heights$AM_Height, y=heights$PM_Height, main="Morning by Evening Height")

# linear, the assumption of linearity  met!

## Homoscedasticity

# Run Basic Model

lmModHeights = lm(PM_Height~AM_Height, data=heights)

# Graph it

par(mfrow=c(2,2))
plot(lmModHeights)

# should be a ~ flat line, top left curves and the bottom left graph has dip. Assumption of homoscedasticity may not be met.

### Breusch-Pagan Test

lmtest::bptest(lmModHeights)

# test not significant (p= 0.6484), there is homoscedasticity, assumption is met!

### Non-Constant Variance Test

car::ncvTest(lmModHeights)

# Same not significant, assumption met!

## Homogeneity of Variance

# Looking at the graphs from the last assumption, may not have passed. But moving on.

### GVLMA test

gvlma(lmModHeights)

# Acceptable! 

## Screening for outliers in x space

###Cook's D

CookD(lmModHeights, group=NULL, plot=TRUE, idn=3, newwd=TRUE)

####Observations 3, 4, and 12 may be a problem

### Leverage values

lev = hat(model.matrix(lmModHeights))
plot(lev)

heights[lev>.2,]

# Going by leverage values, only 3 really an issue

## Screening for outliers in y space

car::outlierTest(lmModHeights)

# This test was significant, so it's likely there is at least one outlier

## Screening for outliers in x and y space (influential points)

summary(influence.measures(lmModHeights))

# Looks like the values on the list are 3, 11, and 37.  Should probably try a model in which outliers are removed from the data

## Creating a new model without outliers to test against the model with outliers

heightsNoO <- heights[c(1,2,5,6,7,8,9,10,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40,41),]
lmModHeightsNoO = lm(PM_Height~AM_Height, data=heightsNoO)

## Look at the model summaries for each

summary(lmModHeights)

### Looks like morning height is a significant predictor of evening height and explains 99% of the variance in evening height.

summary(lmModHeightsNoO)

### Very similar results with the model with no outliers, so it's fine to keep and use the original model with all the data!