##################################################################################
# 1 - Import des donn�es 
##################################################################################

# Importer la base de données
# data.csv

data <- read.csv2("./data/data.csv", header = TRUE, sep = ",")

load("./data/combat_MMA.RData")

data_all_unique <- df

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
# Le nombre de coups à la tête / le nombre de coups au corps / le nombre de coups au sol 
##################################################################################

# A - Analyse graphique

# B - Construction du modèle

# B1 - Estimation des paramètres (méthode des moindres carrés)

# B2 - Test global du modèle (test F) / tests de nullité descoefficients
# B3 - Qualité du modèle (coefficient R²)

#Calcul du ratio de victoire
data_all_unique$ratio_win <- (100 * data_all_unique$wins)/data_all_unique$totalCombat

#Foncition cor permet de calculer la corrélation entre 2 variables
#Ratio_win vs avg_HEAD_landed
round(cor(data_all_unique$ratio_win,data_all_unique$avg_HEAD_landed, use = "complete.obs"),2)

#Ratio_win vs avg_BODY_landed
round(cor(data_all_unique$ratio_win,data_all_unique$avg_BODY_landed, use = "complete.obs"),2)

#Ratio_win vs avg_GROUND_landed
round(cor(data_all_unique$ratio_win,data_all_unique$avg_GROUND_landed, use = "complete.obs"),2)

#Régression linéaire multiple
res.lm_ratio_win <- lm(formula=ratio_win ~ avg_HEAD_landed + avg_BODY_landed + avg_GROUND_landed,data=data_all_unique)
summary(res.lm_ratio_win)

#Régression linéaire multiple sans avg_BODY_landed
res.lm_ratio_win <- lm(formula=ratio_win ~ avg_HEAD_landed + avg_GROUND_landed,data=data_all_unique)
summary(res.lm_ratio_win)


## Graphique

# C - Vérification des hypothèses
# C1 - Valeurs ajustées / résidus studentisés (indépendance, structure de variance, points aberrants)
rstud_ratio_win <-  rstudent(res.lm_ratio_win)
plot(rstud_ratio_win, pch=20, ylab="Résidus studentisés", ylim = c(-3,3))
abline(h=c(0), col="grey",lty=1,lwd=2)
abline(h=c(-2,2), col="grey",lty=2,lwd=2)

#Calculs des points entre (2;3) et (-2;-3) 

install.packages("gvlma")

library("gvlma")

gvlma(res.lm_ratio_win)

#Skewness + data tassées à gauche
#Skewness - data tassées à droite
#Kurtosis + data centrées à la médiane ou moyenne
#Kurtosis + data + distribuées/élargies


# C - Vérification des hypothèses
# C1 - Valeurs ajustées / résidus studentis�s (ind�pendance, structure de variance, points aberrants)
# C2 - Distance de Cook (points influents)
# C3 - Droite de Henry (normalit�)



## Anova

## Prédire le nombre de coup tenté en fonction de la catégorie de poids. Que concluez-vous ?

#library(FactoMinerR)

res.anova <- aov(avg_TOTAL_STR_att ~ weight_class, data = data_all_unique)
summary(res.anova)

install.packages("ggplot2")
library("ggplot2")

#Graphique
ggplot(data_all_unique,aes(x=weight_class,y=avg_TOTAL_STR_att))+
  geom_boxplot()+
  ggtitle("Boites à moustaches")+
  xlab("Classe de combat")+
  ylab("Coup tenté")

#Calcul ANOVA
anova_class <- lm(avg_TOTAL_STR_att~weight_class,data=data_all_unique)
summary(anova_class)

# tableau de l'analyse de la variance
anova(anova_class)


## Prédire le nombre de coup tenté à la tête en fonction de la catégorie de poids. Que concluez-vous ?

res.anova_head <- aov(avg_HEAD_att ~ weight_class, data = data_all_unique)
summary(res.anova_head)

#Graphique
ggplot(data_all_unique,aes(x=weight_class,y=avg_HEAD_att))+
  geom_boxplot()+
  ggtitle("Boites à moustaches")+
  xlab("Classe de combat")+
  ylab("Coup tenté")

#
anova_class_head <- lm(avg_HEAD_att~weight_class,data=data_all_unique)
summary(anova_class_head)

# tableau de l'analyse de la variance
anova(anova_class_head)

## Anova à deux facteurs
#Prédire le nombre de coup tenté en fonction de la catégorie de poids et le style de combat

#Regroupement par sexe


#Regroupement par catégorie de poids

table(data_all_unique$weight_class)

data_all_unique$categorie_poids[data_all_unique$weight_class %in% c("Heavyweight", "Light Heavyweight")] <-  "Poids Lourds Homme"
data_all_unique$categorie_poids[data_all_unique$weight_class %in% c("Middleweight", "Welterweight")] <-  "Poids Moyens Homme"
data_all_unique$categorie_poids[data_all_unique$weight_class %in% c("Lightweight", "Featherweight")] <-  "Poids Legers Homme"
data_all_unique$categorie_poids[data_all_unique$weight_class %in% c("Bantamweight", "Flyweight")] <-  "SuperLegers Homme"

#data_all_unique$categorie_poids[data_all_unique$weight_class %in% c("Women's Bantamweight", "Women's Featherweight")] <-  "Poids Moyen Femme"

#On compare les combattants avec 5 matchs ou plus

library("dplyr")

dffiltre <- data_all_unique %>% filter(!is.na(categorie_poids) & totalCombat >= 5)
dffiltre <- dffiltre %>% filter(Stance %in% c("Open Stance", "Orthodox", "Southpaw", "Switch"))

table(dffiltre$Stance)

dffiltre$Stance <- as.character(dffiltre$Stance)
dffiltre$Stance <- as.factor(dffiltre$Stance)
#Graphique

#Style de combat
ggplot(dffiltre,aes(x=Stance,y=avg_TOTAL_STR_att))+
  geom_boxplot()+
  ggtitle("Boites à moustaches")+
  xlab("Style de combat")+
  ylab("Coup tenté")

#Catégorie poids
ggplot(dffiltre,aes(x=categorie_poids,y=avg_TOTAL_STR_att))+
  geom_boxplot()+
  ggtitle("Boites à moustaches")+
  xlab("Catégorie poids")+
  ylab("Coup tenté")


#Calcul ANOVA
anova_class_style <- lm(avg_TOTAL_STR_att~categorie_poids + Stance,data=dffiltre)
lm(avg_TOTAL_STR_att~categorie_poids + Stance,data=dffiltre)
summary(anova_class_style)

#Calcul ANOVA avec FactoMineR
install.packages("FactoMineR")
library("FactoMineR")
res_aovsum <- AovSum(avg_TOTAL_STR_att~categorie_poids + Stance,data=dffiltre)
summary(res_aovsum)
anova(res_aovsum)

df$colonne <- relevel(df$colonne, ref = "modalité3")

# tableau de l'analyse de la variance
anova(anova_class_style)

#Calcul de l'ANOVA avec intéraction


























