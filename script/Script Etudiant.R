##################################################################################
# 1 - Import des donn�es 
##################################################################################
library(data.table)
library(lubridate)
# Importer la base de donn�es 
# data.csv
data <- read.csv("C:/Users/ejosse/OneDrive - Business & Decision/Enseignement/Master MEDAS/2ieme ann�e/data/data.csv",sep = ",")

# Pensez � bien v�rifier le format de vos champs !!!
# Pensez � regarder si des erreurs de saisie / valeurs aberrantes sont pr�sentes

#R�sum� du jeux de donn�es
summary(data)
str(data)


#suppr�ssion des combats < 2000 car trop de valeur manquante & r�gle diff�rentes
data$date <- as.Date(data$date)
hist(year(data$date))
data<-data[which(year(data$date)>2000),]

##################################################################################
# 2 - Cr�ation d'une table de combattant unique
##################################################################################

# Etape 1 : faire un dataframe avec toutes les variables concernant le joueur bleu + Date + Gagnant + Cat�gorie de poids + Nombre de round
nom<-colnames(data)
nom_rouge<-nom[nom %like% "^R_"]
rouge<-(data[,nom_rouge])
rouge<-cbind(data$date,data$Winner,data$title_bout,data$weight_class,data$no_of_rounds,rouge)

# Etape 2 : faire un dataframe avec toutes les variables concernant le joueur rouge + Date + Gagnant + Cat�gorie de poids + Nombre de round
nom_bleu<-nom[nom %like% "^B_"]
bleu<-(data[,nom_bleu])
bleu<-cbind(data$date,data$Winner,data$title_bout,data$weight_class,data$no_of_rounds,bleu)


# Etape 3 : Renommer les variables pour que les noms de colonnes soit identiques entre les deux dataframe cr��s ci-dessus 
colnames(rouge)<-c( "date", "gagnant","titre","categ_poids","nb_rounds",substr(colnames(rouge[,colnames(rouge) %like% "^R_"]),3,1000) )
colnames(bleu)<-c( "date", "gagnant","titre","categ_poids","nb_rounds",substr(colnames(bleu[,colnames(bleu) %like% "^B_"]),3,1000) )

# Etape 4 : Concatener les deux dataframes en un seul
joueur<-rbind(rouge,bleu)
# Etape 5 : S�lectionner seulement la ligne correspondant au dernier combat par combattant
max_date<-aggregate(date ~ fighter, data = joueur, max)
joueur<-merge(max_date,joueur,all = FALSE)


##################################################################################
# 3 - Calculer la r�gression lin�aire simple entre le poids et la taille 
##################################################################################

# A - Analyse graphique
taille_poid<-na.omit(joueur[joueur$Weight_lbs<500,c("Height_cms","Weight_lbs")])
plot(taille_poid$Height_cms,taille_poid$Weight_lbs, pch=20, cex=0.8, col=1)

# B - Construction du mod�le 
# B1 - Estimation des param�tres (m�thode des moindres carr�s)
# B2 - Test global du mod�le (test F) / tests de nullit� descoefficients
# B3 - Qualit� du mod�le (coefficient R�)
res.lm<-lm(taille_poid$Height_cms~taille_poid$Weight_lbs)
summary(res.lm)
# C - V�rification des hypoth�ses
# C1 - Valeurs ajust�es / r�sidus studentis�s (ind�pendance, structure de variance, points aberrants)
rstud = rstudent(res.lm)
plot(rstud,pch=20,ylab="R�sidus studentis�s",ylim=c(-3,3))
abline(h=c(0), col="grey",lty=1,lwd=2)
abline(h=c(-2,2), col="grey",lty=2,lwd=2)

# C2 - Distance de Cook (points influents)
res.cook=cooks.distance(model=res.lm)
plot(res.cook, type="h",ylab="Distances de Cook", ylim=c(0,0.6))
abline(h=0.5,col="gray",lty=2)

# C3 - Droite de Henry (normalit�)
res.qq=qqnorm(rstud, pch=20, ylim=c(-3,7),xlim=c(-3,3))
qqline(rstud, lty=2, lwd=2, col=2)

# D - Pr�diction 
# Quel serait la taille d'une personne pesant 135 lbs

x0=150
x0 = data.frame(x0)
colnames(x0) <- "Weight_lbs"
x0$Weight_lbs<-as.numeric(x0$Weight_lbs)
predict(res.lm,x0, interval = "confidence" )


##################################################################################
# 4- Calculer la r�gression lin�aire multiple entre le ratio de victoire et 
# Le nombre de coup � la t�te / le nombre de coup au corp / le nombre de coup au sol 
##################################################################################
# A - Analyse graphique

# B - Construction du mod�le 
# B1 - Estimation des param�tres (m�thode des moindres carr�s)
# B2 - Test global du mod�le (test F) / tests de nullit� descoefficients
# B3 - Qualit� du mod�le (coefficient R�)


# C - V�rification des hypoth�ses
# C1 - Valeurs ajust�es / r�sidus studentis�s (ind�pendance, structure de variance, points aberrants)
# C2 - Distance de Cook (points influents)
# C3 - Droite de Henry (normalit�)
