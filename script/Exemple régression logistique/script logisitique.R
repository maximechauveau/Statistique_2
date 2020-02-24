# Exemple regression logistique
maladie_cardiage<-read.delim('./script/Exemple régression logistique/test.txt', sep=" ")

maladie_cardiage$angine<-as.factor(maladie_cardiage$angine)

# Construction du mod?le
model_quali<-glm(coeur~taux_max + angine ,data=maladie_cardiage,family=binomial)

# Interpr?tation
summary(model_quali)
model_quali$deviance
model_quali$aic

 #calcul de la vraisemblance
 prev <- model_quali$fitted.values #on obtient les pi
 vrais <- rep(0,nrow(maladie_cardiage))
 vrais[maladie_cardiage$coeur=="presence"] <- prev[maladie_cardiage$coeur=="presence"]
 vrais[maladie_cardiage$coeur=="absence"] <- 1-prev[maladie_cardiage$coeur=="absence"]
 vrais <- prod(vrais) #vrais est la vraisemblance du modele
 dev <- -2*log(vrais) 
 dev 
 
 # MAtrice de confusion
 appren.p <- cbind(maladie_cardiage, predict(model_quali, newdata = maladie_cardiage, type = "link", 
                                   se = TRUE))
 appren.p <- within(appren.p, {
         PredictedProb <- plogis(fit)
         LL <- plogis(fit - (1.96 * se.fit))
         UL <- plogis(fit + (1.96 * se.fit))
 })
 appren.p <- cbind(appren.p, pred.chd = factor(ifelse(appren.p$PredictedProb > 0.5, 1, 0)))
 (m.confusion <- as.matrix(table(appren.p$pred.chd, appren.p$coeur)))
 
 
 # Courbe ROC
 install.packages("ROCR")
 library(ROCR)
 pred <- prediction(appren.p$PredictedProb, appren.p$coeur)
 perf <- performance(pred, "tpr", "fpr")
 plot(perf)
 
# Analyse des r?sidus
res_dev <- residuals(model_quali) #residus de deviance
res_pear <- residuals(model_quali,type="pearson") #residus de Pearson
res_dev_stand <- rstandard(model_quali) #residu de deviance standardises
H <- influence(model_quali)$hat #diagonale de la hat matrix
res_pear_stand <- res_pear/sqrt(1-H) #residu de Pearson standardises
plot(rstudent(model_quali),type="p",cex=0.5,ylab="R?sidus studentis?s par VC")
abline(h=c(-2,2))

# Pr?vision
plot(predict(model_quali),rstudent(model_quali),type="p",cex=0.5,xlab="pr?vision lin?aire",
     ylab="R?sidus studentis?s par VC")

# Distance de cook
plot(cooks.distance(model_quali),type="h",ylab="Distance de Cook") 
