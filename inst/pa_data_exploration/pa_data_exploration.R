library(arrow)
library(data.table)
library(ggplot2)
library(MASS)
library(glmnet)
library(robust)
library(robustbase)
#library()

mpe = function(pred, true){
  return(100* mean((true - pred)/true))
}


data = read_krafthack_data("/Users/peraugust/Documents/krafthack/Krafthack2022/data")
input1 = data[[1]]
input2 = data[[2]]
test = data[[3]]

# Linear model:
# Splitting training data input2 into consecutive bulks:
subset1 = 1:dim(input2)[1] %%400 <200
covariates = names(input2)[c(1:2, 4:8)]
formulastring = paste("Bolt_1_Tensile  ~ ", paste(covariates, collapse="*"))
formula1 = formula(formulastring)
#training linear model on training data using all possible interactions
lm1 = lm(formula=formula1, data = input2, subset = subset1)
#predict on holdout set:
predlm1 = predict(lm1, input2[!subset1,])
mpe_lm = mpe(predlm1, input2$Bolt_1_Tensile[!subset1] )
mpe_lm
# plot predicted on holdout vs true values of holdout set:
plot(predlm1,type="l",col=2)
lines(input2$Bolt_1_Tensile[!subset1], col=1)
#fit full model:
lm_full = lm(formula=formula1, data= input2)
predlm_full = lm_full$fitted.values
mpe_lm_full = mpe(input2$Bolt_1_Tensile, lm_full$fitted.values)
mpe_lm_full
# fitted values vs true values on whole input2:
plot(lm_full$fitted.values,type="l",col=2)
lines(input2$Bolt_1_Tensile, col=1)
#
# elastic net:
mmatrix1 = model.matrix(formula1, data= input2, subset= subset1)[subset1,]
glmnet1 = cv.glmnet(mmatrix1,input2$Bolt_1_Tensile[subset1], alpha=0, type.measure="mae")
mmatrix1_pred =model.matrix(formula1, data= input2, subset= subset1)[!subset1,]
pred_glmnet1 = predict(glmnet1,mmatrix1_pred)
plot(pred_glmnet1,type="l",col=2)
lines(input2$Bolt_1_Tensile[!subset1], col=1)
mpe(pred_glmnet1,input2$Bolt_1_Tensile[!subset1] )


#
# formula6 = formula(paste("Bolt_6_Tensile  ~ ", paste(names(input2)[c(1:8)], collapse="+")))
# subset6 = 1:dim(input2)[1] %%400 <200
# lm6 = rlm(formula=formula6, data = input2, subset = subset1)
# pred6 = predict(lm6, input2)[!subset1]
# plot(pred6,type="l",col=2)
# lines(input2$Bolt_6_Tensile[!subset1], col=1)
#
# mean((pred6 -input2$Bolt_6_Tensile[!subset1])^2)
#
#
