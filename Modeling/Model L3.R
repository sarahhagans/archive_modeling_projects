

#Load Libraries

library("ggplot2")

#quadratic relationships for X1, Y1
quadPlot <- ggplot(nonlinear, aes(x = X1, y=Y1)) + 
  geom_point() + stat_smooth(method = "lm", formula = y ~x + I(x^2), size =1)
quadPlot
#Looks like a quadratic line is a pretty good fit. Very parabolic.

X1sq <- nonlinear$X1^2
quadModel <- lm(nonlinear$Y1~nonlinear$X1 + X1sq)
summary(quadModel)
#significant 


#exponential function 
exMod <- lm(log(nonlinear$X1)~nonlinear$Y1)
summary(exMod)
#significant 



#quadratic relationships for X2, Y2
quadPlot <- ggplot(nonlinear, aes(x = X2, y=Y2)) + 
  geom_point() + stat_smooth(method = "lm", formula = y ~x + I(x^2), size =1)
quadPlot
#The quadratic line is a good fit nice parabola.

X2sq <- nonlinear$X2^2
quadModel <- lm(nonlinear$Y2~nonlinear$X2 + X2sq)
summary(quadModel)
#this quadratic model is significant.


#exponential function 
exMod <- lm(log(nonlinear$X2)~nonlinear$Y2)
summary(exMod)
#significant again