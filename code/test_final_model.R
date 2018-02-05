####           Final model        ####
### Full model---- model5_for 
summary(model5_for)
###lm(formula = BODYFAT ~ ABDOMEN + WEIGHT + WRIST + FOREARM + THIGH + 
###             AGE, data = body_fit[c(-42, -39, -221, -86), ])


###check summary, delete forearm, thigh, age, final one is good.

#####################################

### under 45 --- co45_down_back
summary(co45_down_back)
###lm(formula = BODYFAT ~ AGE + WEIGHT + ABDOMEN + ANKLE + BICEPS + 
###             WRIST, body_45[(body_45$IDNO != 31 & body_45$IDNO != 
###                        42), c(-1, -3, -7)])

### beyond 45 --- model45_up_for
summary(model45_up_for)
###  lm(formula = BODYFAT ~ ABDOMEN + WEIGHT, 
###     data = body45_up[(body45_up$IDNO != 39 & body45_up$IDNO != 
###                       86 & body45_up != 221), c(-1, -3)])

### This model is the best one, with good prediction and most simple