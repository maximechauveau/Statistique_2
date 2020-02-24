##################################################################################
# 1 - Import des données 
##################################################################################

# Importer la base de données
# data.csv

data <- read.csv("./data/data.csv", header = TRUE, sep = ",")

# Pensez à bien vérifier le format de vos champs !!!


data$date <- as.Date(data$date, format = "%Y-%m-%d")
data$R_fighter <- as.character(data$R_fighter)
data$B_fighter <- as.character(data$B_fighter)
data$Referee <- as.character(data$Referee)
data$location <- as.character(data$location)
data$title_bout <- as.character(data$title_bout)
data$weight_class <- as.character(data$weight_class)

# Pensez à regarder si des erreurs de saisie / valeurs aberrantes sont présentes

##################################################################################
# 2 - Création d'une table de combattant unique
##################################################################################

# Etape 1 : faire un dataframe avec toutes les variables concernant le joueur bleu + Date + Gagnant + Catégorie de poids + Nombre de round

library("dplyr")
library("tidyverse")

### On regarde les différentes modalités de la colonne
levels(data$Winner)

install.packages("questionr")

library(questionr)
freq(data$Winner)

### On enlève les draw

data$Winner <- as.character(data$Winner)

df_regr_log <- data %>%
  filter(data$Winner != 'Draw')

levels(df_regr_log$Winner)
freq(df_regr_log$Winner)

df_regr_log$Winner <- as.factor(df_regr_log$Winner)


#On choisit les gros et la date

df_regr_log <- data %>% filter(weight_class == 'Lightweight' & date >= '2012-01-01')

df_regr_log$Winner <- as.factor(df_regr_log$Winner)


### Calcul du nombre total de match

df_regr_log$B_Nb_total_combat <- df_regr_log$B_wins + df_regr_log$B_losses + df_regr_log$B_draw

df_regr_log$R_Nb_total_combat <- df_regr_log$R_wins + df_regr_log$R_losses + df_regr_log$R_draw


# Création variable
variableRetenue <- c("Winner", "date", "B_Nb_total_combat", "R_Nb_total_combat", "B_Reach_cms", "R_Reach_cms",
                     "B_avg_SIG_STR_landed", 'R_avg_SIG_STR_landed', "B_avg_KD", "R_avg_KD", "B_avg_TD_landed", "R_avg_TD_landed",
                     'B_age', 'R_age', 'B_Weight_lbs', 'R_Weight_lbs', 'B_Height_cms', 'R_Height_cms')

### Création de la base

dfLourd <- df_regr_log[,variableRetenue]

### Calculs des ratios

dfLourd$diff_nb_combat <- round(dfLourd$B_Nb_total_combat - dfLourd$R_Nb_total_combat, 2)

dfLourd$diff_Allonge <- round(dfLourd$B_Reach_cms - dfLourd$R_Reach_cms, 2)

dfLourd$diff_Coup_touche <- round(dfLourd$B_avg_SIG_STR_landed - dfLourd$R_avg_SIG_STR_landed, 2)

dfLourd$diff_takedown <- round(dfLourd$B_avg_KD - dfLourd$R_avg_KD, 2)

dfLourd$diff_soumission <- round(dfLourd$B_avg_TD_landed - dfLourd$R_avg_TD_landed, 2)

dfLourd$diff_age <- round(dfLourd$B_age - dfLourd$R_age, 2)

dfLourd$diff_poids <- round(dfLourd$B_Weight_lbs - dfLourd$R_Weight_lbs, 2)

dfLourd$diff_taille <- round(dfLourd$B_Height_cms - dfLourd$R_Height_cms, 2)

# On récupère nos colonnes

dfLourd <- dfLourd[,c(1,19:26)]



# 

dfLourd <- dfLourd %>% filter(!is.na(diff_nb_combat) &
                                !is.na(diff_Allonge) &
                                !is.na(diff_Coup_touche) &
                                !is.na(diff_soumission) &
                                !is.na(diff_takedown) &
                                !is.na(diff_age) &
                                !is.na(diff_poids) &
                                !is.na(diff_taille))

#On enlève les draw

dfLourd$Winner <- as.character(dfLourd$Winner)

table(dfLourd$Winner)

dfLourd$Winner <- as.factor(dfLourd$Winner)

dfLourd <- dfLourd %>%
  filter(dfLourd$Winner != 'Draw')


model_sature <- glm(Winner~. , data = dfLourd, family = binomial)
summary(model_sature)

step(model_sature)


stepAIC(model_sature)
# 
# model_quali <- glm(formula = Winner ~ diff_nb_combat + ratio_Allonge + ratio_Coup_touche, 
#     family = binomial(link = 'logit'), data = dfLourd)

model_quali <- glm(formula = Winner ~ diff_Allonge + diff_soumission + diff_age, 
                   family = binomial, data = dfLourd)


###############
# Interprétation
summary(model_quali)

#
model_quali$deviance

#
model_quali$aic

#calcul de la vraisemblance
prev <- model_quali$fitted.values #on obtient les pi
vrais <- rep(0,nrow(dfLourd))
vrais[dfLourd$Winner=="Red"] <- prev[dfLourd$Winner=="Red"]
vrais[dfLourd$Winner=="Blue"] <- 1-prev[dfLourd$Winner=="Blue"]
vrais <- prod(vrais) #vrais est la vraisemblance du modele
dev <- -2*log(vrais) 
dev

# MAtrice de confusion
appren.p <- cbind(dfLourd, predict(model_quali, newdata = dfLourd, type = "link", 
                                   se = TRUE))
appren.p <- within(appren.p, {
  PredictedWin <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
appren.p <- cbind(appren.p, pred.chd = factor(ifelse(appren.p$PredictedWin > 0.5, 1, 0)))
(m.confusion <- as.matrix(table(appren.p$pred.chd, appren.p$Winner)))

#Calcul pourcentage
# 


# Courbe ROC
#install.packages("ROCR")
library(ROCR)
pred <- prediction(appren.p$PredictedWin, appren.p$Winner)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# Analyse des résidus
res_dev <- residuals(model_quali) #residus de deviance
res_pear <- residuals(model_quali,type="pearson") #residus de Pearson
res_dev_stand <- rstandard(model_quali) #residu de deviance standardises
H <- influence(model_quali)$hat #diagonale de la hat matrix
res_pear_stand <- res_pear/sqrt(1-H) #residu de Pearson standardises
plot(rstudent(model_quali),type="p",cex=0.5,ylab="Résidus studentisés par VC")
abline(h=c(-2,2))

# Prévision
plot(predict(model_quali),rstudent(model_quali),type="p",cex=0.5,xlab="prévision linéaire",
     ylab="Résidus studentisés par VC")

# Distance de cook
plot(cooks.distance(model_quali),type="h",ylab="Distance de Cook")








install.packages("pacman")
pacman::p_load(blorr)

blr_step_aic_both(model_quali)
step(model_quali)

blr_gains_table(model_quali, data = dfLourd) %>%
  blr_roc_curve()

blr_confusion_matrix(model_quali, data = dfLourd, cutoff = 0)

?blr_confusion_matrix

