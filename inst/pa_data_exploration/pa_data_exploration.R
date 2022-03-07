library(arrow)
library(data.table)
library(ggplot2)
library(MASS)
#library()

mpe = function(pred, true){
  return(100* mean((true - pred)/true))
}

input2 = as.data.table(arrow::read_parquet("/Users/peraugust/Documents/krafthack/Krafthack2022/data/input_dataset-2.parquet"))
input2 = input2[,1:21]
input2 = na.omit(input2)
test = as.data.table(arrow::read_parquet("/Users/peraugust/Documents/krafthack/Krafthack2022/data/prediction_input.parquet"))
names(input2) <- gsub("\\s+", "_", names(input2))
input2$mode = as.factor(input2$mode)
#input2$seconds = as.double(input2$timepoints) - as.double(input2$timepoints[1])
dim(input2)
#summary(input2$Unit_4_Power)


formula1 = formula(paste("Bolt_1_Torsion  ~ ", paste(names(input2)[c(1:8)], collapse="*")))
subset1 = 1:dim(input2)[1] %%400 <200
lm1 = lm.ridge(formula=formula1, data = input2, subset = subset1, lambda = 1)
pred1 = predict(lm1, input2)[!subset1]
plot(pred1,type="l",col=2)
lines(input2$Bolt_1_Torsion[!subset1], col=1)

mean((pred1 -input2$Bolt_1_Torsion[!subset1])^2)
mpe(pred1,input2$Bolt_1_Torsion[!subset1] )





formula6 = formula(paste("Bolt_6_Torsion  ~ ", paste(names(input2)[c(1:8)], collapse="+")))
subset6 = 1:dim(input2)[1] %%400 <200
lm6 = rlm(formula=formula6, data = input2, subset = subset1)
pred6 = predict(lm6, input2)[!subset1]
plot(pred6,type="l",col=2)
lines(input2$Bolt_6_Torsion[!subset1], col=1)

mean((pred6 -input2$Bolt_6_Torsion[!subset1])^2)


