library(arrow)
library(data.table)
library(ggplot2)
library(MASS)
library(glmnet)
library(robust)
library(robustbase)
library(Krafthack2022)
#library()



# data = read_krafthack_data("/Users/peraugust/Documents/krafthack/Krafthack2022/data")
data = read_krafthack_data("../Data")
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
# ridge regression
mmatrix1 = model.matrix(formula1, data= input2, subset= subset1)[subset1,]
glmnet1 = cv.glmnet(mmatrix1,input2$Bolt_1_Tensile[subset1], alpha=0, type.measure="mae")
mmatrix1_pred =model.matrix(formula1, data= input2, subset= subset1)[!subset1,]
pred_glmnet1 = predict(glmnet1,mmatrix1_pred)
plot(pred_glmnet1,type="l",col=2)
lines(input2$Bolt_1_Tensile[!subset1], col=1)
mpe(pred_glmnet1,input2$Bolt_1_Tensile[!subset1] )


## adding seperate shutdown and burn-in periods
input2 = find_burnin_and_shutdown(input2)
input2$one_div_last_start = 1/sqrt(input2$within_segment_index)
input2$one_div_last_start[is.infinite(input2$one_div_last_start)] = 0
input2$one_div_last_start[is.na(input2$one_div_last_start)] = 0
input2$mode = as.factor(input2$mode)
covariates = c(names(input2)[c(1:2, 4:8)])
formulastring2 = paste("Bolt_1_Tensile  ~ ", paste(covariates, collapse="*"), "+one_div_last_start * (", paste(covariates[1:6], collapse="+"), ")")
#formulastring2 = paste("Bolt_1_Tensile  ~ ", paste(covariates, collapse="*"), "+one_div_last_start * timepoints" )
#covariates = c(names(input2)[c(1:2, 4:8,22)])
#formulastring2 = paste("Bolt_1_Tensile  ~ ", paste(covariates, collapse="*"))
formula2 = formula(formulastring2)
#training linear model on training data using all possible interactions
lm2 = lm(formula=formula2, data = input2, subset = subset1)
#predict on holdout set:
predlm2 = predict(lm2, input2[!subset1,])
mpe_lm2 = mpe(predlm2, input2$Bolt_1_Tensile[!subset1] )
mpe_lm2
ll = length(input2$Bolt_1_Tensile[!subset1])
mpe(predlm2[round(ll/2):ll], (input2$Bolt_1_Tensile[!subset1] )[round(ll/2):ll])

plot(predlm2,type="l",col=2)
lines(input2$Bolt_1_Tensile[!subset1], col=1)

# ridge regression
mmatrix2 = model.matrix(formula2, data= input2, subset= subset1)[subset1,]
glmnet2 = cv.glmnet(mmatrix2,input2$Bolt_1_Tensile[subset1], alpha=0, type.measure="mae")
mmatrix2_pred =model.matrix(formula2, data= input2, subset= subset1)[!subset1,]
pred_glmnet2 = predict(glmnet2,mmatrix2_pred)
plot(pred_glmnet2,type="l",col=2)
lines(input2$Bolt_1_Tensile[!subset1], col=1)
mpe(pred_glmnet2,input2$Bolt_1_Tensile[!subset1] )
ll = length(input2$Bolt_1_Tensile[!subset1])
mpe(pred_glmnet2[round(ll/2):ll], (input2$Bolt_1_Tensile[!subset1] )[round(ll/2):ll])









## massive elastic net:
covariates2 = c()
for (i in 1:length(covariates)) {
  covar = covariates[i]
  if(i==1 | i==7){
    covariates2 = c(covariates2, covar)
  }
  else{
    covariates2 = c(covariates2, paste("( ", covar, "+ I(", covar ,")^2)"))
  }
}
formula2string = paste("Bolt_1_Tensile  ~ ", paste(covariates2, collapse="*"))
formula2 = formula(formula2string)
mmatrix1 = model.matrix(formula2, data= input2, subset= subset1)[subset1,]
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
