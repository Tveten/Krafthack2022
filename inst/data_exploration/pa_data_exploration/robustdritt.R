#robust dritt
remove.ind = remove.ind[2:length(remove.ind)]
pred1 = predict(lm1, input2)[!subset1]
plot(pred1,type="l",col=2)
lines(input2$Bolt_1_Tensile[!subset1], col=1)
mpe(pred1,input2$Bolt_1_Tensile[!subset1] )

# robust linear:
#mmatrix1 = model.matrix(formula1, data= input2, subset= subset1)[subset1,!remove.ind]
# lmtemp = lm(formula=formula(paste("Bolt_1_Tensile  ~ ", paste(names(input2)[c(1:2, 4:7)], collapse="*")))
#               , data = input2, subset = subset1)
# remove.ind = (is.na(lmtemp$coefficients))
# remove.ind = remove.ind[2:length(remove.ind)]
# namez = names(remove.ind[remove.ind==FALSE])
# #namez = namez[1:(length(namez))]
# formularob = formula(paste("Bolt_1_Tensile  ~ ","mode * (", paste(gsub("modestart", "mode",namez), collapse="+"), ")"))
#namez = names(remove.ind[remove.ind==FALSE])
#namez = names(remove.ind[remove.ind==FALSE])
#formularob = formula(paste("Bolt_1_Tensile  ~ ", paste(namez, collapse="+")))
#formularob = formula(paste("Bolt_1_Tensile  ~ ", paste(names(input2)[c(1:2, 4:7)], collapse="*")))
#formularob = formula(paste("Bolt_1_Tensile  ~ ", "(",paste(names(input2)[c(1:2, 4:7)],
#                                                           collapse="+"),")^2 + mode * (",paste(names(input2)[c(2, 4:7)],
#                                  collapse="+"), ")" ))
#rlm1 = lmrob(  formula = formularob, data = input2, subset = subset1, fast.s.large.n=Inf)
rlm1 = rlm(  formula = formularob, data = input2, subset = subset1,method="M")

#rlm1 = lmrob(  formula = formularob, data = input2, subset = subset1)
predr1 = predict(rlm1, input2)[!subset1]
plot(predr1,type="l",col=2)
lines(input2$Bolt_1_Tensile[!subset1], col=1)
mpe(predr1,input2$Bolt_1_Tensile[!subset1] )
