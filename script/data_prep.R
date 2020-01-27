##################################################################################
# 1 - Import des donn�es 
##################################################################################

# Importer la base de données
# data.csv

data <- read.csv2("./data/data.csv", header = TRUE, sep = ",")

# Pensez à bien vérifier le format de vos champs !!!


data$date <- as.Date(data$date, format = "%Y-%m-%d")
data$R_fighter <- as.character(data$R_fighter)
data$B_fighter <- as.character(data$B_fighter)
data$Referee <- as.character(data$Referee)
data$location <- as.character(data$location)
data$R_fighter <- as.character(data$R_fighter)
data$R_fighter <- as.character(data$R_fighter)
data$R_fighter <- as.character(data$R_fighter)
data$R_fighter <- as.character(data$R_fighter)
data$R_fighter <- as.character(data$R_fighter)
data$R_fighter <- as.character(data$R_fighter)

data$R_fighter <- as.character(data$R_fighter)

# Pensez à regarder si des erreurs de saisie / valeurs aberrantes sont présentes

##################################################################################
# 2 - Création d'une table de combattant unique
##################################################################################

# Etape 1 : faire un dataframe avec toutes les variables concernant le joueur bleu + Date + Gagnant + Catégorie de poids + Nombre de round

library("dplyr")
library("tidyverse")

df_red <-  select(data,4,6,8,9, starts_with("R_"))


# Etape 2 : faire un dataframe avec toutes les variables concernant le joueur rouge + Date + Gagnant + Catégorie de poids + Nombre de round

df_blue <-  select(data,4,6,8,9, starts_with("B_"))


# Etape 3 : Renommer les variables pour que les noms de colonnes soit identiques entre les deux dataframe créés ci-dessus

nom_col <- sub("R_","",names(df_red))

names(df_blue) <- nom_col

names(df_red) <- nom_col


# Etape 4 : Concatener les deux dataframes en un seul

data_all <- rbind(df_red,df_blue)

# Etape 5 : Sélectionner seulement la ligne correspondant au dernier combat par combattant


data_all$wins <- as.numeric(data_all$wins)
data_all$losses <- as.numeric(data_all$losses)
data_all$draw <- as.numeric(data_all$draw)

#data_all <- as.data.frame(data_all)

data_all$Nb_total_combat <- data_all$wins + data_all$losses + data_all$draw

data_all_unique <- data_all %>%
  group_by(fighter) %>%
  filter(Nb_total_combat == max(Nb_total_combat))



##################################################################################
# 3 - Calculer la régression linéaire simple entre le poids et la taille 
##################################################################################

# A - Analyse graphique

data_all_unique$Weight_lbs <- as.numeric(data_all_unique$Weight_lbs)
data_all_unique$Height_cms <- as.numeric(data_all_unique$Height_cms)

library("car")

scatterplot(data_all_unique$Height_cms, data_all_unique$Weight_lbs)

plot(x = data_all_unique$Height_cms, y = data_all_unique$Weight_lbs)

with(data_all_unique, qplot(Height_cms, Weight_lbs))




# B - Construction du modèle 

summary(data_all_unique)

cor(data_all_unique$Height_cms,data_all_unique$Weight_lbs)

res.lm <- lm(formula=Weight_lbs~Height_cms,data=data_all_unique)
summary(res.lm)

res.lm$coefficients


plot(x = data_all_unique$Height_cms, y = data_all_unique$Weight_lbs)
abline(a=res.lm$coefficients[1], b= res.lm$coefficients[2],col=2, lty=2, lwd=1.5)

rstud = rstudent(res.lm)

rstud = rstudent(res.lm)
# graphique des résidus studentisés
plot(rstud,pch=20,ylab="Résidus studentisés",ylim=c(-3,3))
abline(h=c(0), col="grey",lty=1,lwd=2)
abline(h=c(-2,2), col="grey",lty=2,lwd=2)

#Distance de cook
res.cook=cooks.distance(model=res.lm)
plot(res.cook, type="h",ylab="Distances de Cook", ylim=c(0,0.6))
abline(h=0.5,col="gray",lty=2)


res.qq=qqnorm(rstud, pch=20, ylim=c(-3,7),xlim=c(-3,3))
qqline(rstud, lty=2, lwd=2, col=2)

# B1 - Estimation des paramètres (méthode des moindres carrés)
# B2 - Test global du modèle (test F) / tests de nullité des coefficients
# B3 - Qualité du modèle (coefficient R�)


# C - Vérification des hypothèses
# C1 - Valeurs ajustées / résidus studentisés (indépendance, structure de variance, points aberrants)
# C2 - Distance de Cook (points influents)
# C3 - Droite de Henry (normalité)

# D - Prédiction 
# Quel serait la taille d'une personne pesant 135 lbs

##################################################################################
# 4- Calculer la régression linéaire multiple entre le ratio de victoire et 
# Le nombre de coup à la tête / le nombre de coup au corp / le nombre de coup au sol 
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