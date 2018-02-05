###colinearity solvtion, delete BMI

co1 <- lm(BODYFAT~., data = body[,c(-1,-3,-7)])
par(mfrow=c(2,2))
plot(co1)
layout(1)
plot(co1,which = 4)
 
co2 <- lm(BODYFAT~., data = body[-39,c(-1,-3,-7)])
par(mfrow=c(2,2))
plot(co2)
layout(1)
plot(co2,which = 4)
influencePlot(co2)

co3 <- lm(BODYFAT~., data = body[c(-39,-42),c(-1,-3,-7)])
influencePlot(co3)

co4 <- lm(BODYFAT~., data = body[c(-39,-42,-86),c(-1,-3,-7)])
influencePlot(co4)
plot(co4,which = 4)

co4_back <- step(co4,direction = "backward")
sum(co4_back$residuals^2)/length(co4_back$residuals)
vif(co4_back)
influencePlot(co4_back)
qqPlot(co4_back)
crPlots(co4_back)
ncvTest(co4_back)
durbinWatsonTest(co4_back)##independence may be a issue

co4_null <- lm(BODYFAT~1,body[c(-39,-42,-86),c(-1,-3,-7)])
co4_for <- step(co4_null,
                scope = list(lower = co4_null,upper = co4),
                direction = "forward")
sum(co4_for$residuals^2)/length(co4_for$residuals)
durbinWatsonTest(co4_for)
### Forward has good vif, not good in terms of mse
### Compare these two with original model(not delete BMI)
#################################################################

###under 45 clean
co45_down1 <- lm(BODYFAT~., body_45[,c(-1,-3,-7)])
plot(co45_down1,which=4)
influencePlot(co45_down1)

co45_down2 <- lm(BODYFAT~., 
                 body_45[(body_45$IDNO!=31&body_45$IDNO!=42),c(-1,-3,-7)])
plot(co45_down2,which = 4)
influencePlot(co45_down2)
ncvTest(co45_down2)
durbinWatsonTest(co45_down2)


###under 45 stepwise
co45_down_back <- step(co45_down2,direction = "backward")
vif(co45_down_back)
sum(co45_down_back$residuals^2)/length(co45_down_back$residuals)

###check
qqPlot(co45_down_back)##normality good
ncvTest(co45_down_back)##homoskedasticity
durbinWatsonTest(co45_down_back)##independence
crPlots(co45_down_back)##linearity


####vif may not be very good, but we can handle
#### Final choose co45_down_back for under 45
##############################################################

co45_down_null <- lm(BODYFAT~1,
                     body_45[(body_45$IDNO!=31&body_45$IDNO!=42),c(-1,-3,-7)])
co45_down_for <- step(co45_down_null,
                      scope = list(lower = co45_down_null,upper = co45_down2),
                      direction = "forward")
vif(co45_down_for)
sum(co45_down_for$residuals^2)/length(co45_down_for$residuals)

forward_model <- lm(formula = BODYFAT ~ ABDOMEN + WRIST + AGE + HEIGHT + THIGH + 
                    NECK + FOREARM, data = body_45[(body_45$IDNO != 31 & 
                                                              body_45$IDNO != 42), c(-1, -3, -7)])



####

