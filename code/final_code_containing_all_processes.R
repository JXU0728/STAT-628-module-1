library(ggplot2)
library(caret)
library(car)
library(dplyr)

### Read data
body <- read.csv("code/BodyFat.csv",header = T)
body_fit <- body[,c(-1,-3)]

### Clean data DEPENDENT VARIABLE: body fat
summary(body_fit)

model <- lm(BODYFAT~.,data = body_fit)
par(mfrow=c(2,2))
plot(model)
layout(1)
plot(model,which = 4)

model2 <- lm(BODYFAT~., data = body_fit[c(-42),]) ##shortest one
par(mfrow=c(2,2))
plot(model2)
layout(1)
plot(model2,which = 4)
influencePlot(model2)

model3 <- lm(BODYFAT~., data = body_fit[c(-42,-39),]) ## shortest, weight
par(mfrow=c(2,2))
plot(model3)
layout(1)
plot(model3,which = 4)
influencePlot(model3)

model4 <- lm(BODYFAT~., data = body_fit[c(-42,-39,-221),])## shortest,weight,seems normal
par(mfrow=c(2,2))
plot(model4)
layout(1)
plot(model4,which = 4)
influencePlot(model4)

model5 <- lm(BODYFAT~., data = body_fit[c(-42,-39,-221,-86),]) ## shortest,weight,seems normal, ankle
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


### Based on forward selection, further delete 3 variables for adjust multicollinearity
vif(model5_for)
summary(model5_for)
full_final <- lm(BODYFAT~ABDOMEN+WEIGHT+WRIST,
                 body_fit[c(-42, -39, -221, -86),])
qqPlot(full_final)
durbinWatsonTest(full_final)
crPlots(full_final)
ncvTest(full_final)
##satisfy Gaussian assumption


### Divide data into two part
###### Under 45 Clean
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

model45_4 <- lm(BODYFAT~., 
                data = body_45[(body_45$IDNO!=42&body_45$IDNO!=31&body_45$IDNO!=163),c(-1,-3)])
######163 short strong man
outlierTest(model45_4)
influencePlot(model45_4)

###### Selection
model45_back <- step(model45_4,direction = "backward",k = 2)
model45_null <- lm(BODYFAT~1, 
                   data = body_45[(body_45$IDNO!=42&body_45$IDNO!=31&body_45$IDNO!=163),])
model45_for <- step(model45_null,
                    scope = list(lower = model45_null,upper = model45_4),
                    direction = "forward")
model45_both <- step(model45_null,
                     scope = list(lower = model45_null,upper = model45_4),
                     direction = "both")
vif(model45_back)## higher multicollinearity 
vif(model45_for) ## not good in selecting variables

###### remove adiposity based on analysis that abodomen has a high
###### correlation with adiposity

###### clean again
co45_down1 <- lm(BODYFAT~., body_45[,c(-1,-3,-7)])
plot(co45_down1,which=4)
influencePlot(co45_down1)

co45_down2 <- lm(BODYFAT~., 
                 body_45[(body_45$IDNO!=31&body_45$IDNO!=42),c(-1,-3,-7)])
plot(co45_down2,which = 4)
influencePlot(co45_down2)

###under 45 stepwise
co45_down_back <- step(co45_down2,direction = "backward")
vif(co45_down_back)
co45_down_null <- lm(BODYFAT~1,
                     body_45[(body_45$IDNO!=31&body_45$IDNO!=42),c(-1,-3,-7)])
co45_down_for <- step(co45_down_null,
                      scope = list(lower = co45_down_null,upper = co45_down2),
                      direction = "forward")
sum(co45_down_back$residuals^2)/length(co45_down_back$residuals)
sum(co45_down_for$residuals^2)/length(co45_down_for$residuals)

### although forward selection is good in terms of mse, model is too
### complex, choose back selection

# further delete age
varImp(co45_down_back)
under_45_final <- lm(BODYFAT~WEIGHT+ABDOMEN+WRIST+BICEPS+ANKLE,
                     body_45[(body_45$IDNO!=31&body_45$IDNO!=42),c(-1,-3,-7)])
#check
qqPlot(under_45_final)##normality good
ncvTest(under_45_final)##homoskedasticity
durbinWatsonTest(under_45_final)##independence
crPlots(under_45_final)##linearity


###### Beyond 45
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

###### select
model45_up_back <- step(model45_up4,direction = "backward")
model45_up3_null <- lm(BODYFAT~1,
                       data = body45_up[(body45_up$IDNO!=39&body45_up$IDNO!=86&body45_up!=221),c(-1,-3)])
model45_up_for <- step(model45_up3_null,
                       scope = list(lower = model45_up3_null,upper = model45_up4),
                       direction = "forward")
#### the two method gives the same model
#### model is simple, predictive and multicollinearity issue is solved
up_45_final <- lm(BODYFAT~ABDOMEN+WEIGHT,
                  body45_up[(body45_up$IDNO!=39&body45_up$IDNO!=86&body45_up$IDNO!=221),c(-1,-3)])
qqPlot(up_45_final)##normality good
ncvTest(up_45_final)##homoskedasticity
durbinWatsonTest(up_45_final)##independence
crPlots(up_45_final)##linearity
