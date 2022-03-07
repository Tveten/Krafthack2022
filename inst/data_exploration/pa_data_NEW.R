# This script generates the predicted values componentwise

# installation
install.packages("devtools")
devtools::install_github("Tveten/Krafthack2022")

#necessary libraries:
install.packages("arrow")
library(arrow)
install.packages("data.table")
library(data.table)
install.packages("ggplot2")
library(ggplot2)
install.packages("MASS")
library(MASS)
install.packages("glmnet")
library(glmnet)
library(Krafthack2022)

# SET DATA DIRECTORY (where you find the data sets)
datadirectory ="/Users/peraugust/Documents/krafthack/Krafthack2022/data"
# SET OUTPUT DIRECTORY
outputdirectory = "/Users/peraugust/Documents/krafthack/Krafthack2022/data"

# Data import
data = read_krafthack_data(datadirectory)
input1 = data[[1]]
input2 = data[[2]]
test = data[[3]]
originalnames = data[[4]]
# Pre-processing training data

# Add two additional modes:
input2 = find_burnin_and_shutdown(input2)
# Add additional feature, 1/sqrt ( time since last shutdown)
input2$one_div_last_start = 1/(input2$within_segment_index - input2$burnin_stop+1)
input2$one_div_last_start[is.infinite(input2$one_div_last_start)] = 0
input2$one_div_last_start[is.na(input2$one_div_last_start)] = 0
input2$one_div_last_start[input2$one_div_last_start<0] = 0
input2$one_div_last_start = (input2$one_div_last_start)^(1/4)
input2$mode = as.factor(input2$mode)

# Pre-processing test data
# Add two additional modes:
test = find_burnin_and_shutdown(test)
test$one_div_last_start = 1/(test$within_segment_index - test$burnin_stop+1)
test$one_div_last_start[is.infinite(test$one_div_last_start)] = 0
test$one_div_last_start[is.na(test$one_div_last_start)] = 0
test$one_div_last_start[test$one_div_last_start<0] = 0
test$one_div_last_start = (test$one_div_last_start)^(1/4)
test$mode = as.factor(test$mode)

# R technicalities:
test$Bolt_1_Tensile = rep(0, dim(test)[1])
test$Bolt_2_Tensile = rep(0, dim(test)[1])
test$Bolt_3_Tensile = rep(0, dim(test)[1])
test$Bolt_4_Tensile = rep(0, dim(test)[1])
test$Bolt_5_Tensile = rep(0, dim(test)[1])
test$Bolt_6_Tensile = rep(0, dim(test)[1])


models = c()
for (i in 1:6) {
  print(sprintf("at model %d",i))

  covariates = c(names(input2)[c(1:2, 4:8)])

  #create regression formula with all interactions of covariates
  #and 1/sqrt(time since last burnin period)
  formulastring2 = paste(sprintf("Bolt_%d_Tensile", i)," ~ ", paste(covariates, collapse="*"), "+one_div_last_start * (", paste(covariates[1:6], collapse="+"), ")")
  formula2 = formula(formulastring2)

  #creating model matrix:
  mmatrix2 = model.matrix(formula2, data= input2)
  name = sprintf("Bolt_%d_Tensile", i) # name of current bolt

  #fit ridge regression:
  glmnet2 = cv.glmnet(mmatrix2,(input2[, ..name])[[1]], alpha=0, type.measure="mae")

  #predicting:
  mmatrix2_pred =model.matrix(formula2, data= test)
  pred_glmnet2 = predict(glmnet2,mmatrix2_pred)
  test[, name] = pred_glmnet2
}

plot(test$Bolt_5_Tensile)
# Revert back names (had to remove whitespaces as default R function do not allow it)

# Dump predictions into CSV file
write_csv_arrow(test[,c(1,17:22)], file=sprintf("%s/predicted.csv", outputdirectory),
                col_names=TRUE)

plot(test$Bolt_6_Tensile,type="l")

