##################################################################################
# 1 - Import des données 
##################################################################################
library(data.table)
library(lubridate)
# Importer la base de données 
# data.csv
data <- read.csv("C:/Users/ejosse/OneDrive - Business & Decision/Enseignement/Master MEDAS/2ieme année/data/data.csv",sep = ",")

# Pensez à bien vérifier le format de vos champs !!!
# Pensez à regarder si des erreurs de saisie / valeurs aberrantes sont présentes

#Résumé du jeux de données
summary(data)
str(data)


#suppréssion des combats < 2000 car trop de valeur manquante & règle différentes
data$date <- as.Date(data$date)
hist(year(data$date))
data<-data[which(year(data$date)>2000),]

##################################################################################
# 2 - Création d'une table de combattant unique
##################################################################################

# Etape 1 : faire un dataframe avec toutes les variables concernant le joueur bleu + Date + Gagnant + Catégorie de poids + Nombre de round
nom<-colnames(data)
nom_rouge<-nom[nom %like% "^R_"]
rouge<-(data[,nom_rouge])
rouge<-cbind(data$date,data$Winner,data$title_bout,data$weight_class,data$no_of_rounds,rouge)

# Etape 2 : faire un dataframe avec toutes les variables concernant le joueur rouge + Date + Gagnant + Catégorie de poids + Nombre de round
nom_bleu<-nom[nom %like% "^B_"]
bleu<-(data[,nom_bleu])
bleu<-cbind(data$date,data$Winner,data$title_bout,data$weight_class,data$no_of_rounds,bleu)


# Etape 3 : Renommer les variables pour que les noms de colonnes soit identiques entre les deux dataframe créés ci-dessus 
colnames(rouge)<-c( "date", "gagnant","titre","categ_poids","nb_rounds",substr(colnames(rouge[,colnames(rouge) %like% "^R_"]),3,1000) )
colnames(bleu)<-c( "date", "gagnant","titre","categ_poids","nb_rounds",substr(colnames(bleu[,colnames(bleu) %like% "^B_"]),3,1000) )

# Etape 4 : Concatener les deux dataframes en un seul
joueur<-rbind(rouge,bleu)
# Etape 5 : Sélectionner seulement la ligne correspondant au dernier combat par combattant
max_date<-aggregate(date ~ fighter, data = joueur, max)
joueur<-merge(max_date,joueur,all = FALSE)


##################################################################################
# 3 - Calculer la régression linéaire simple entre le poids et la taille 
##################################################################################

# A - Analyse graphique
taille_poid<-na.omit(joueur[joueur$Weight_lbs<500,c("Height_cms","Weight_lbs")])
plot(taille_poid$Height_cms,taille_poid$Weight_lbs, pch=20, cex=0.8, col=1)

# B - Construction du modèle 
# B1 - Estimation des paramètres (méthode des moindres carrés)
# B2 - Test global du modèle (test F) / tests de nullité descoefficients
# B3 - Qualité du modèle (coefficient R²)
res.lm<-lm(taille_poid$Height_cms~taille_poid$Weight_lbs)
summary(res.lm)
# C - Vérification des hypothèses
# C1 - Valeurs ajustées / résidus studentisés (indépendance, structure de variance, points aberrants)
rstud = rstudent(res.lm)
plot(rstud,pch=20,ylab="Résidus studentisés",ylim=c(-3,3))
abline(h=c(0), col="grey",lty=1,lwd=2)
abline(h=c(-2,2), col="grey",lty=2,lwd=2)

# C2 - Distance de Cook (points influents)
res.cook=cooks.distance(model=res.lm)
plot(res.cook, type="h",ylab="Distances de Cook", ylim=c(0,0.6))
abline(h=0.5,col="gray",lty=2)

# C3 - Droite de Henry (normalité)
res.qq=qqnorm(rstud, pch=20, ylim=c(-3,7),xlim=c(-3,3))
qqline(rstud, lty=2, lwd=2, col=2)

# D - Prédiction 
# Quel serait la taille d'une personne pesant 135 lbs

x0=150
x0 = data.frame(x0)
colnames(x0) <- "Weight_lbs"
x0$Weight_lbs<-as.numeric(x0$Weight_lbs)
predict(res.lm,x0, interval = "confidence" )


##################################################################################
# 4- Calculer la régression linéaire multiple entre le ratio de victoire et 
# Le nombre de coup à la tête / le nombre de coup au corp / le nombre de coup au sol 
##################################################################################

joueur$ratio_victoire<-joueur$wins/(joueur$wins + joueur$losses)
joueur$avg_GROUND_att

############################################################################.
#############Régression entre ratio victoire / nb coup tête & corp & sol 
############################################################################
# On sélectionne seulement les joueurs avec + de 5 match pour obtenir des statistiques par joueur robuste
data_reg<-joueur[which((joueur$wins + joueur$losses)>5),c("fighter","ratio_victoire","wins","losses","avg_BODY_att","avg_HEAD_att","avg_GROUND_att")]

# A - Analyse graphique

hist(data_reg$ratio_victoire)
plot(data_reg[,c("ratio_victoire","avg_BODY_att","avg_HEAD_att","avg_GROUND_att")])

# B - Construction du modèle 
res.lm<-lm(ratio_victoire~ avg_BODY_att +avg_HEAD_att+ avg_GROUND_att, data = data_reg)
summary(res.lm)

# C - Vérification des hypothèses
# C1 - Valeurs ajustées / résidus studentisés (indépendance, structure de variance, points aberrants)
rstud = rstudent(res.lm)
plot(rstud,pch=20,ylab="Résidus studentisés",ylim=c(-3,3))
abline(h=c(0), col="grey",lty=1,lwd=2)
abline(h=c(-2,2), col="grey",lty=2,lwd=2)
length(rstud[rstud >1.95 | rstud < -1.95])/length(rstud)


# C2 - Distance de Cook (points influents)
res.cook=cooks.distance(model=res.lm)
plot(res.cook, type="h",ylab="Distances de Cook", ylim=c(0,0.6))
abline(h=0.5,col="gray",lty=2)

# C3 - Droite de Henry (normalité)
res.qq=qqnorm(rstud, pch=20, ylim=c(-3,7),xlim=c(-3,3))
qqline(rstud, lty=2, lwd=2, col=2)

############################################################################
#############Régression entre ratio victoire / nb coup tête & corp & sol mais filtre sur les catégorie
############################################################################
# On filtre sur une catégorie de poids pour observer s'il faut mieux faire un modèle généraliste ou un modèle spécifique par catégorie de poids
# Welterweight / Lightweight / Catch Weight 
data_reg<-joueur[which((joueur$wins + joueur$losses)>5 & joueur$categ_poids =="Welterweight"),c("fighter","ratio_victoire","wins","losses","avg_BODY_landed","avg_HEAD_landed","avg_GROUND_landed")]

#############Régression entre ratio victoire / nb coup tête & corp & sol 
# A - Analyse graphique
hist(data_reg$ratio_victoire)
plot(data_reg[,c("ratio_victoire","avg_BODY_landed","avg_HEAD_landed","avg_GROUND_landed")])

# B - Construction du modèle 
res.lm<-lm(ratio_victoire~ avg_HEAD_landed+ avg_GROUND_landed , data = data_reg)
summary(res.lm)

res.lm<-lm(ratio_victoire~ avg_HEAD_landed+ avg_GROUND_landed -1, data = data_reg)
summary(res.lm)

# C - Vérification des hypothèses
# C1 - Valeurs ajustées / résidus studentisés (indépendance, structure de variance, points aberrants)
rstud = rstudent(res.lm)
plot(rstud,pch=20,ylab="Résidus studentisés",ylim=c(-3,3))
abline(h=c(0), col="grey",lty=1,lwd=2)
abline(h=c(-2,2), col="grey",lty=2,lwd=2)
length(rstud[rstud >1.95 | rstud < -1.95])/length(rstud)

# C2 - Distance de Cook (points influents)
res.cook=cooks.distance(model=res.lm)
plot(res.cook, type="h",ylab="Distances de Cook", ylim=c(0,0.6))
abline(h=0.5,col="gray",lty=2)

# C3 - Droite de Henry (normalité)
res.qq=qqnorm(rstud, pch=20, ylim=c(-3,7),xlim=c(-3,3))
qqline(rstud, lty=2, lwd=2, col=2)



############################################################################
#############ANOVA entre nombre de coup tenté et catégorie de poids retravailler
############################################################################
colnames(joueur)
data.frame(table(joueur$categ_poids))


data_anova<-joueur[!is.na(joueur$avg_TOTAL_STR_att),c("fighter","avg_TOTAL_STR_att","categ_poids")]
data_anova[which(data_anova$categ_poids %in% c("Flyweight","Bantamweight")),"categorie_poids2"]<-"poid_plume_homme"
data_anova[which(data_anova$categ_poids %in% c("Featherweight","Lightweight")),"categorie_poids2"]<-"poid_leger_homme"
data_anova[which(data_anova$categ_poids %in% c("Welterweight","Middleweight")),"categorie_poids2"]<-"poid_moyen_homme"
data_anova[which(data_anova$categ_poids %in% c("Light Heavyweight","Heavyweight")),"categorie_poids2"]<-"poid_lourd_homme"
data_anova[which(data_anova$categ_poids %in% c("Women's Strawweight","Women's Flyweight")),"categorie_poids2"]<-"poid_plumme_femme"
data_anova[which(data_anova$categ_poids %in% c("Women's Bantamweight","Women's Featherweight")),"categorie_poids2"]<-"poid_moyen_femme"

data_anova<-data_anova[which(data_anova$categ_poids!="Catch Weight"),]
table(data_anova$categorie_poids2)
#Analyse graphique
boxplot(formula=avg_TOTAL_STR_att~categorie_poids2, data=data_anova, boxwex=0.3, col="lightblue", pch=10, xlab="Catégorie de poids")
aggregate(avg_TOTAL_STR_att ~ categorie_poids2, data = data_anova, mean)

#Création du modèle
# Via lm
mod.lm=lm(formula=avg_TOTAL_STR_att~categorie_poids2,data=data_anova)
anova(mod.lm)
summary(mod.lm)


# C - Vérification des hypothèses
# C1 - Valeurs ajustées / résidus studentisés (indépendance, structure de variance, points aberrants)
rstud = rstudent(mod.lm)
plot(rstud,pch=20,ylab="Résidus studentisés",ylim=c(-3,3))
abline(h=c(0), col="grey",lty=1,lwd=2)
abline(h=c(-2,2), col="grey",lty=2,lwd=2)
length(rstud[rstud >1.95 | rstud < -1.95])/length(rstud)

# C2 - Distance de Cook (points influents)
res.cook=cooks.distance(model=mod.lm)
plot(res.cook, type="h",ylab="Distances de Cook", ylim=c(0,0.6))
abline(h=0.5,col="gray",lty=2)

# C3 - Droite de Henry (normalité)
res.qq=qqnorm(rstud, pch=20, ylim=c(-3,7),xlim=c(-3,3))
qqline(rstud, lty=2, lwd=2, col=2)



#Via factominer
library(FactoMineR)
?AovSum
mod.aovsum=AovSum(formula=avg_TOTAL_STR_att~categ_poids,data=data_anova)
mod.aovsum
summary(mod.aovsum)

mod.aov=aov(formula=avg_TOTAL_STR_att~categ_poids,data=data_anova)
mod.aov
summary(mod.aov)
mod.aov$coefficients


# Comparaison des modalités 2 à 2
res.Tukey = TukeyHSD( x = mod.aov, conf.level=0.95)
res.Tukey



############################################################################
#############ANOVA à 2 facteurs entre nombre de coup tenté et catégorie de poids retravailler et le Stance
############################################################################
colnames(joueur)
data.frame(table(joueur$categ_poids))
joueur$Stance<- factor(joueur$Stance,exclude=NULL)
table(joueur$Stance)
library(data.table)

data_anova<-joueur[!is.na(joueur$avg_TOTAL_STR_att),c("fighter","avg_TOTAL_STR_att","categ_poids","Stance")]
data_anova[which(data_anova$categ_poids %in% c("Flyweight","Bantamweight")),"categorie_poids2"]<-"poid_plume_homme"
data_anova[which(data_anova$categ_poids %in% c("Featherweight","Lightweight")),"categorie_poids2"]<-"poid_leger_homme"
data_anova[which(data_anova$categ_poids %in% c("Welterweight","Middleweight")),"categorie_poids2"]<-"poid_moyen_homme"
data_anova[which(data_anova$categ_poids %in% c("Light Heavyweight","Heavyweight")),"categorie_poids2"]<-"poid_lourd_homme"
data_anova[which(data_anova$categ_poids %in% c("Women's Strawweight","Women's Flyweight","Women's Bantamweight","Women's Featherweight")),"categorie_poids2"]<-"poid_femme"
data_anova<-data_anova[which(data_anova$categ_poids!="Catch Weight" & data_anova$Stance %in% c("Open Stance","Orthodox","Sideways","Southpaw","Switch")),]
data_anova$Stance<- factor(data_anova$Stance,exclude=NULL)
table(data_anova$Stance)

#Analyse graphique
boxplot(formula=avg_TOTAL_STR_att~categorie_poids2, data=data_anova, boxwex=0.3, col="lightblue", pch=10, xlab="Catégorie de poids")
boxplot(formula=avg_TOTAL_STR_att~Stance, data=data_anova, boxwex=0.3, col="lightblue", pch=10, xlab="Catégorie de poids")

#Création du modèle
# Via lm
#Modèle sans intéraction
mod.lm=lm(formula=avg_TOTAL_STR_att~categorie_poids2+Stance,data=data_anova)

#Modèle avec intéraction
mod.lm=lm(formula=avg_TOTAL_STR_att~categorie_poids2*Stance,data=data_anova)

mod.lm=lm(formula=avg_TOTAL_STR_att~ categorie_poids2/Stance,data=data_anova)

# Modèle hierarchique
mod.lm=lm(formula=avg_TOTAL_STR_att~categorie_poids2+categorie_poids2:Stance,data=data_anova)

anova(mod.lm)
summary(mod.lm)

data_anova$categorie_poids2

#Via factominer
library(FactoMineR)
?AovSum
mod.aovsum=AovSum(formula=avg_TOTAL_STR_att~Stance,data=data_anova)
mod.aovsum
summary(mod.aovsum)

# C - Vérification des hypothèses
# C1 - Valeurs ajustées / résidus studentisés (indépendance, structure de variance, points aberrants)
rstud = rstudent(mod.aovsum)
plot(rstud,pch=20,ylab="Résidus studentisés",ylim=c(-3,3))
abline(h=c(0), col="grey",lty=1,lwd=2)
abline(h=c(-2,2), col="grey",lty=2,lwd=2)
length(rstud[rstud >1.95 | rstud < -1.95])/length(rstud)

# C2 - Distance de Cook (points influents)
res.cook=cooks.distance(model=mod.aovsum)
plot(res.cook, type="h",ylab="Distances de Cook", ylim=c(0,0.6))
abline(h=0.5,col="gray",lty=2)

# C3 - Droite de Henry (normalité)
res.qq=qqnorm(rstud, pch=20, ylim=c(-3,7),xlim=c(-3,3))
qqline(rstud, lty=2, lwd=2, col=2)


?regsu

############################################################################
#############Comparaison ancova VS reg linéaire en fonction des catégorie de poids 
############################################################################

joueur$ratio_victoire<-joueur$wins/(joueur$wins + joueur$losses)

data_ancova<-joueur[which((joueur$wins + joueur$losses)>5),c("fighter","avg_TOTAL_STR_att","categ_poids","ratio_victoire")]
data_ancova[which(data_ancova$categ_poids %in% c("Flyweight","Bantamweight")),"categorie_poids2"]<-"poid_plume_homme"
data_ancova[which(data_ancova$categ_poids %in% c("Featherweight","Lightweight")),"categorie_poids2"]<-"poid_leger_homme"
data_ancova[which(data_ancova$categ_poids %in% c("Welterweight","Middleweight")),"categorie_poids2"]<-"poid_moyen_homme"
data_ancova[which(data_ancova$categ_poids %in% c("Light Heavyweight","Heavyweight")),"categorie_poids2"]<-"poid_lourd_homme"
data_ancova[which(data_ancova$categ_poids %in% c("Women's Strawweight","Women's Flyweight","Women's Bantamweight","Women's Featherweight")),"categorie_poids2"]<-"poid_femme"
data_ancova<-data_ancova[which(data_ancova$categ_poids!="Catch Weight"),]
table(data_ancova$Stance)

#Analyse graphique
boxplot(formula=ratio_victoire~categorie_poids2, data=data_ancova, boxwex=0.3, col="lightblue", pch=10, xlab="Catégorie de poids")
plot(data_ancova$ratio_victoire,data_ancova$avg_TOTAL_STR_att)
#Création du modèle

#Modèle avec coefficient B par modalité 
mod.ancova1=lm(formula=ratio_victoire~categorie_poids2+ categorie_poids2/avg_TOTAL_STR_att,data=data_ancova)
summary(mod.ancova1)

# Modèle avec seulement un coefficient B
mod.ancova2=lm(formula=ratio_victoire~categorie_poids2+avg_TOTAL_STR_att,data=data_ancova)
summary(mod.ancova2)

# Modèle avec seulement l'effet du facteur catégorie de poids
mod.ancova3=lm(formula=ratio_victoire~categorie_poids2,data=data_ancova)
summary(mod.ancova3)

#Modèle avec seulement l'effet de la variable avg_TOTAL_STR_att
mod.ancova4=lm(formula=ratio_victoire~avg_TOTAL_STR_att,data=data_ancova)
summary(mod.ancova4)

# Comparaison de modèle à partir du SCR de chacun
anova(mod.ancova4,mod.ancova2)
summary(mod.ancova)


# Calcule d'une régression par catégorie de poids
boucle<-unique(data_ancova$categorie_poids2)
pred<-data.frame(matrix(data=0, nrow=0,ncol=3))
colnames(pred)<-c("reel","pred","cat_poid")
str(pred)

for(i in 1:length(boucle)){

  jeux<-data_ancova[which(data_ancova$categorie_poids2==boucle[i]),]
  mod.ancova.boucle=lm(formula=ratio_victoire~avg_TOTAL_STR_att,data=jeux)
  toto<-cbind(jeux$ratio_victoire, mod.ancova.boucle$fitted.values,boucle[i])
  colnames(toto)<-c("reel","pred","cat_poid")
  pred<-rbind(pred,toto)
}

#Calcul du SCR liée au modèle par catégorie de poids
str(pred)
pred$reel<- as.numeric(as.character(pred$reel))
pred$pred<- as.numeric(as.character(pred$pred))
sum((pred$reel-pred$pred)^2)

#Calcul du SCR lée au modèle global
sum((mod.ancova4$fitted.values-data_ancova$ratio_victoire)^2)

#Les modèles par catégorie de poids sont meilleurs
