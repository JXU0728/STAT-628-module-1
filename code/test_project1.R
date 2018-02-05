library(ggplot2)
library(caret)
library(car)
library(rattle)
library(LEAP)
library(dplyr)
body_fit <- read.csv("BodyFat.csv",header = T)
qplot(DENSITY,BODYFAT,data = body)
scatterplotMatrix(body[,c(-1,-2,-3)])
plot(body$BODYFAT)

### DEPENDENT VARIABLE: body fat
summary(body_fit)
plot(body_fit$BODYFAT)
model <- lm(BODYFAT~.,data = body_fit)
par(mfrow=c(2,2))
plot(model)
layout(1)
plot(model,which = 4)

model_try <- lm(BODYFAT~.,data = body_fit[c(-182),])##bodyfat = 0
par(mfrow=c(2,2))
plot(model_try)

model2 <- lm(BODYFAT~., data = body_fit[c(-42),]) ##wrist so slim
par(mfrow=c(2,2))
plot(model2)
layout(1)
plot(model2,which = 4)
influencePlot(model2)

model3 <- lm(BODYFAT~., data = body_fit[c(-42,-39),]) ## wrist, weight
par(mfrow=c(2,2))
plot(model3)
layout(1)
plot(model3,which = 4)
influencePlot(model3)

model4 <- lm(BODYFAT~., data = body_fit[c(-42,-39,-221),])## wrist,weight,seems normal
par(mfrow=c(2,2))
plot(model4)
layout(1)
plot(model4,which = 4)
influencePlot(model4)

model5 <- lm(BODYFAT~., data = body_fit[c(-42,-39,-221,-86),]) ## wrist,weight,seems normal, ankle
par(mfrow=c(2,2))
plot(model5)
layout(1)
plot(model5,which = 4)
influencePlot(model5)##point 41, no reason to out


####stepwise choose model for full data
model5_back <- step(model5,direction = "back")
model5_null <- lm(BODYFAT~1, data = body_fit[c(-42,-39,-221,-86),])
model5_for <- step(model5_null,
                   scope = list(lower = model5_null,upper = model5),
                   direction = "forward")
qqPlot(model5_back,labels = body$IDNO, simulate = T)#normality
durbinWatsonTest(model5_back) ##independence of residual
crPlots(model5_back) ##linearity test
ncvTest(model5_back) ##homoscedasticity test
spreadLevelPlot(model5_back) ##suggeted transform, no need for transform
influencePlot(model5_back)##no point seems influent regression

qqPlot(model5_for,labels = body$IDNO,simulate = T)##normality good
durbinWatsonTest(model5_for)##resiual may have some aotucorrelation
crPlots(model5_for)##linearity seems good
ncvTest(model5_for)##homoscedasticity is good
spreadLevelPlot(model5_for)##no need for transform
influencePlot(model5_for)



####mse for forward and backward
sum(model5_back$residuals^2)/length(model5_back$residuals)
sum(model5_for$residuals^2)/length(model5_for$residuals)

### backward selection has better mse but forward has a good vif

##Final decision model5_for
###############################################################




###divided with age 45
body_45 <- body[body$AGE<=45,]
model45_1 <- lm(BODYFAT~., data = body_45[,c(-1,-3)])
par(mfrow = c(2,2))
plot(model45_1)
layout(1)
plot(model45_1,which = 4)

model45_2 <- lm(BODYFAT~.,data = body_45[body_45$IDNO!=42,c(-1,-3)])
par(mfrow = c(2,2))
plot(model45_2)
layout(1)
plot(model45_2,which = 4)

model45_3 <- lm(BODYFAT~., 
                data = body_45[(body_45$IDNO!=42&body_45$IDNO!=31),c(-1,-3)])#point 163
par(mfrow = c(2,2))
plot(model45_3)
layout(1)
plot(model45_3,which = 4)
influencePlot(model45_3)

###163 short strong man
model45_4 <- lm(BODYFAT~., 
                data = body_45[(body_45$IDNO!=42&body_45$IDNO!=31&body_45$IDNO!=163),c(-1,-3)])
outlierTest(model45_4)
influencePlot(model45_4)


model45_back <- step(model45_4,direction = "backward",k = 2)
model45_null <- lm(BODYFAT~1, 
                   data = body_45[(body_45$IDNO!=42&body_45$IDNO!=31&body_45$IDNO!=163),])
model45_for <- step(model45_null,
                    scope = list(lower = model45_null,upper = model45_4),
                    direction = "forward")
model45_both <- step(model45_null,
                     scope = list(lower = model45_null,upper = model45_4),
                     direction = "both")

sum(model45_back$residuals^2)/length(model45_back$residuals)
sum(model45_for$residuals^2)/length(model45_for$residuals)

qqPlot(model45_back)##normality good
ncvTest(model45_back)##homoskedasticity good
durbinWatsonTest(model45_back)##independence good
crPlots(model45_back)##linearity good

####age>45
body45_up <- body[body$AGE>45,]

model45_up1 <- lm(BODYFAT~.,data = body45_up[,c(-1,-3)])
par(mfrow = c(2,2))
plot(model45_up1)
layout(1)
plot(model45_up1,which = 4)

model45_up2 <- lm(BODYFAT~.,data = body45_up[body45_up$IDNO!=39,c(-1,-3)])
par(mfrow = c(2,2))
plot(model45_up2)
layout(1)
plot(model45_up2,which = 4)

model45_up3 <- lm(BODYFAT~.,
                  data = body45_up[(body45_up$IDNO!=39&body45_up$IDNO!=86),c(-1,-3)])
par(mfrow = c(2,2))
plot(model45_up3)
layout(1)
plot(model45_up3,which = 4)
influencePlot(model45_up3)

model45_up4 <- lm(BODYFAT~.,
                  data = body45_up[(body45_up$IDNO!=39&body45_up$IDNO!=86&body45_up$IDNO!=221),c(-1,-3)])

qqPlot(model45_up4, labels = body$IDNO,simulate = T)##normality good
crPlots(model45_up4)##linearity
ncvTest(model45_up4)##homoscedasticity
durbinWatsonTest(model45_up4)##independence

#model45_up3_221 <- lm(BODYFAT~.,
                      #data = body45_up[(body45_up$IDNO!=39&body45_up$IDNO!=86&body45_up$IDNO!=221),c(-1,-3,-4)])



####stepwise selection
model45_up_back <- step(model45_up4,direction = "backward")
model45_up3_null <- lm(BODYFAT~1,
                       data = body45_up[(body45_up$IDNO!=39&body45_up$IDNO!=86&body45_up!=221),c(-1,-3)])
model45_up_for <- step(model45_up3_null,
                       scope = list(lower = model45_up3_null,upper = model45_up4),
                       direction = "forward")
sum(model45_up_back$residuals^2)/length(model45_up_back$residuals)
sum(model45_up_for$residuals^2)/length(model45_up_for$residuals)



####MSE
mse_age45_back <- sum(model45_back$residuals^2)/length(model45_back$residuals^2)
mse_age45_for <- sum(model45_for$residuals^2)/length(model45_for$residuals)
mse_full45_for <- sum(model5_for$residuals[body_fit$AGE <= 45]^2)/137
mse_full45_back <- sum(model5_back$residuals[body_fit$AGE <= 45]^2)/137

mse_age45up_back <- sum(model45_up_back$residuals^2)/length(model45_up_back$residuals)
mse_full45up_back <- sum(model5_back$residuals[body_fit$AGE>45]^2,na.rm = T)/111
