# TP DATA EXPLORATION - ANALYSE MULTIVARIÉE & CLUSTERING
# Auteurs : ACHOUR Hajar et BARHILI Samia

# PARTIE 1 : ETUDE UNIVARIEE

# Chargement des données
employes <- read.table("employes.csv", header = TRUE, sep = ",")
str(employes)

# Identification des types

# Variables qualitatives
employes$SEX   <- as.factor(employes$SEX)
employes$CATEG <- as.factor(employes$CATEG)

# 1) ID (identifiant)
# Pas d'analyse statistique pertinente
summary(employes$ID)

# 2) SEX (qualitative nominale)
# Calcul des effectifs et des proportions
tabSEX <- table(employes$SEX)
tabSEX
prop.table(tabSEX)

# Représentation graphique de la répartition par sexe
barplot(tabSEX,
        main = "Répartition des salariés selon le sexe",
        xlab = "Sexe",
        ylab = "Effectif")

# 3) CATEG (qualitative ordinale)

# Tableau des effectifs par catégorie
tabCATEG <- table(employes$CATEG)
tabCATEG
prop.table(tabCATEG)

# Diagramme en barres des catégories socioprofessionnelles
barplot(tabCATEG,
        main = "Répartition des salariés par catégorie",
        xlab = "Catégorie",
        ylab = "Effectif")

# 4) SALDEB (quantitative continue)

# Indicateurs statistiques usuels
summary(employes$SALDEB)
mean(employes$SALDEB)
median(employes$SALDEB)
sd(employes$SALDEB)
quantile(employes$SALDEB)

# Histogramme et boîte de Tuckey du salaire à l'embauche
hist(employes$SALDEB,
     main = "Histogramme du salaire à l'embauche",
     xlab = "Salaire à l'embauche",
     breaks = 20)

boxplot(employes$SALDEB,
        main = "Boxplot du salaire à l'embauche",
        ylab = "Salaire à l'embauche")

# 5) SALACT (quantitative continue)

summary(employes$SALACT)
mean(employes$SALACT)
median(employes$SALACT)
sd(employes$SALACT)
quantile(employes$SALACT)

hist(employes$SALACT,
     main = "Histogramme du salaire actuel",
     xlab = "Salaire actuel",
     breaks = 20)

boxplot(employes$SALACT,
        main = "Boxplot du salaire actuel",
        ylab = "Salaire actuel")

# 6) AGE (quantitative continue)

summary(employes$AGE)
mean(employes$AGE)
median(employes$AGE)
sd(employes$AGE)
quantile(employes$AGE)

hist(employes$AGE,
     main = "Histogramme de l'âge des salariés",
     xlab = "Âge",
     breaks = 20)

boxplot(employes$AGE,
        main = "Boxplot de l'âge",
        ylab = "Âge")

# 7) ANC (ancienneté en mois)

summary(employes$ANC)
mean(employes$ANC)
median(employes$ANC)
sd(employes$ANC)
quantile(employes$ANC)

hist(employes$ANC,
     main = "Histogramme de l'ancienneté",
     xlab = "Ancienneté (mois)",
     breaks = 20)

boxplot(employes$ANC,
        main = "Boxplot de l'ancienneté",
        ylab = "Ancienneté (mois)")

# 8) EXP (expérience en années)

summary(employes$EXP)
mean(employes$EXP)
median(employes$EXP)
sd(employes$EXP)
quantile(employes$EXP)

hist(employes$EXP,
     main = "Histogramme de l'expérience professionnelle",
     xlab = "Expérience (années)",
     breaks = 20)

boxplot(employes$EXP,
        main = "Boxplot de l'expérience",
        ylab = "Expérience (années)")

# 9) NIVED (niveau d'études)

summary(employes$NIVED)
mean(employes$NIVED)
median(employes$NIVED)
sd(employes$NIVED)
quantile(employes$NIVED)

tabNIV <- table(employes$NIVED)
barplot(tabNIV,
        main = "Répartition du niveau d'études",
        xlab = "Nombre d'années d'études",
        ylab = "Effectif")

boxplot(employes$NIVED,
        main = "Boxplot du niveau d'études",
        ylab = "Années d'études")

# PARTIE 2 : Analyse en Composantes Principales (ACP)

# Analyse en Composantes Principales (ACP)

library(FactoMineR)
library(factoextra)


# 1. Lancement de l'ACP
# Variables quantitatives actives :
# SALDEB, SALACT, AGE, EXP, ANC
# Variables qualitatives supplémentaires :
# SEX, CATEG
# Variable quantitative supplémentaire :
# NIVED

#Les données sont centrées et réduites afin de rendre les variables comparables
acpEmployes <- PCA(employes,
                   scale.unit = TRUE,
                   quali.sup = c(which(names(employes) == "SEX"),
                                 which(names(employes) == "CATEG")),
                   quanti.sup = which(names(employes) == "NIVED"),
                   graph = FALSE)

# 2. Valeurs propres

acpEmployes$eig

# 3. Scree plot (choix du nombre d’axes)

eig <- acpEmployes$eig
var_exp <- eig[,2]   # pourcentage de variance expliquée

bp <- barplot(var_exp,
              names.arg = 1:length(var_exp),
              ylim = c(0, max(var_exp) + 5),
              ylab = "Pourcentage de variance expliquée",
              xlab = "Dimensions",
              main = "Scree plot")

# Courbe de décroissance
lines(bp, var_exp, type = "b", pch = 16)

# Pourcentages au-dessus des barres pour visualiser la raptude (règle du coude)
text(bp, var_exp,
     labels = paste0(round(var_exp,1), "%"),
     pos = 3, cex = 0.9)

# 4. Cercle des corrélations

# Représentation des variables quantitatives sur le plan factoriel (1,2)
plot(acpEmployes, choix = "var", axes = c(1,2))

# Qualité de représentation des variables
acpEmployes$var$cos2

# Contribution des variables aux axes
acpEmployes$var$contrib

# 5. Analyse des individus

# Projection des individus sur le plan factoriel (Dim1,Dim2)
plot(acpEmployes, choix = "ind", axes = c(1,2), label = "none")

# Coordonnées factorielles des individus
acpEmployes$ind$coord

# 6. Effet des variables qualitatives supplémentaires

# Catégorie professionnelle
plot(acpEmployes, choix = "ind",
     axes = c(1,2),
     habillage = "CATEG",
     label = "none")

# Sexe
plot(acpEmployes, choix = "ind",
     axes = c(1,2),
     habillage = "SEX",
     label = "none")

# Informations sur les variables qualitatives supplémentaires
acpEmployes$quali.sup

# PARTIE 3.1 : ETUDE BIVARIEE (Quantitatif - Quantitatif)

# 1. Calcul de la corrélation
cor_salaires <- cor(employes$SALDEB, employes$SALACT)
print(paste("Coefficient de corrélation r :", round(cor_salaires, 2)))

# 2. Modèle de régression linéaire (pour avoir le R^2)
modele_sal <- lm(SALACT ~ SALDEB, data = employes)
summary(modele_sal) # Regarder "Multiple R-squared"

# 3. Graphique : Nuage de points avec droite de régression
plot(employes$SALDEB, employes$SALACT,
     main = "Relation Salaire Début vs Salaire Actuel",
     xlab = "Salaire d'embauche",
     ylab = "Salaire Actuel",
     pch = 20,         # Forme des points (ronds pleins)
     col = "darkblue") # Couleur des points

# Ajout de la droite de régression rouge
abline(modele_sal, col = "red", lwd = 2)

# Optionnel : Ajout du texte R^2 sur le graphique
text(x = 10000, y = 40000, 
     labels = paste("R2 =", round(summary(modele_sal)$r.squared, 2)), 
     col = "red", cex = 1.2)

# PARTIE 3.2 : ETUDE BIVARIEE (Qualitatif - Quantitatif)

# 1. Salaire vs Catégorie (Le plus important)
# On utilise un boxplot croisé : SALACT en fonction de CATEG
par(mfrow=c(1,1)) # Un seul graphique
boxplot(SALACT ~ CATEG, data = employes,
        main = "Distribution des Salaires par Catégorie",
        xlab = "Catégorie Professionnelle",
        ylab = "Salaire Actuel",
        col = rainbow(7)) # Une couleur différente par catégorie

# Test ANOVA 
model_anova <- aov(SALACT ~ CATEG, data = employes)
print(summary(model_anova))
# Si la valeur Pr(>F) est < 2e-16 (ce qui sera le cas), le lien est prouvé.

# PARTIE 4 : CLUSTERING PRINCIPAL (K-MEANS)
# Source : TP7 Exercice 1 (Méthode du R² / Kmeans)
# Visualisation : Adaptée avec factoextra (Validé par le prof)

library(factoextra)
library(FactoMineR)

# 1. Préparation des données
# On sélectionne les variables quantitatives actives
vars_actives <- employes[, c("SALDEB", "SALACT", "AGE", "EXP", "ANC")]
# On centre-réduit (Scale) car K-Means est sensible aux échelles
donnees_scaled <- scale(vars_actives)


# ETAPE A : JUSTIFICATION DU NOMBRE DE CLUSTERS (k) ---
# Question : "Justifier le choix du nombre de clusters"
# Méthode : On utilise la fonction Rcarre du TP7 Exercice 1 pour afficher la courbe du R².

# Fonction tirée du TP7 Exercice 1
Rcarre <- function(myData, k){
  res = kmeans(myData, centers = k, nstart = 25)
  # R² = Variance Inter-classes / Variance Totale
  return(res$betweenss / res$totss)
}

# Calcul du R² pour k allant de 1 à 10
valeurs_r2 <- sapply(1:10, function(k) Rcarre(donnees_scaled, k))

# Affichage de la courbe (Comme PlotR2 du TP7)
plot(1:10, valeurs_r2, type = "b", pch = 19, col = "blue",
     xlab = "Nombre de clusters (k)", 
     ylab = "R² (Part de variance expliquée)",
     main = "Justification du choix de k (Courbe du R²)")
# On ajoute une ligne rouge pour montrer la cassure (le coude)
abline(v = 3, col = "red", lty = 2)

# >>> INTERPRÉTATION POUR LE RAPPORT :
# "Nous observons une cassure nette de la courbe à k=3 (Règle du coude).
# Cela signifie qu'ajouter un 4ème groupe n'augmente pas significativement la part de variance expliquée."


# ETAPE B : EXÉCUTION DE L'ALGO & RÉSULTATS ---
set.seed(123) # Pour reproductibilité
k_choisi <- 3
res_kmeans <- kmeans(donnees_scaled, centers = k_choisi, nstart = 25)

# On affiche la valeur exacte du R² pour k=3 (C'est ton "r" ou "R²")
cat("R² obtenu pour k=3 :", round(res_kmeans$betweenss/res_kmeans$totss, 2)*100, "%\n")


# --- ETAPE C : VISUALISATION SUR LE PLAN FACTORIEL ---
# Consigne : "Représenter les individus dans le premier plan factoriel"
# Outil : fviz_cluster

fviz_cluster(res_kmeans, data = donnees_scaled,
             geom = "point",              # Points lisibles
             ellipse.type = "convex",     # Entoure les groupes
             palette = "jco",             # Couleurs académiques
             ggtheme = theme_minimal(),
             main = "Projection des Clusters K-Means sur le plan factoriel (ACP)")


# --- ETAPE D : INTERPRÉTATION DES PROFILS ---
# On calcule les moyennes par groupe pour dire qui est qui
print(aggregate(vars_actives, by = list(Cluster = res_kmeans$cluster), FUN = mean))


# PARTIE 5 (BONUS) : L'AUTRE ALGO (CAH)
# Source : TP7 Exercice 2 (Classification Ascendante Hiérarchique)

library(factoextra)

# 1. Calcul de la CAH (Classification Hiérarchique)
# On utilise hcut (de factoextra) qui fait le travail de hclust + cutree
# hc_method = "ward.D2" est la méthode exigée dans le cours (Ward)
res_cah <- hcut(donnees_scaled, k = 3, hc_method = "ward.D2")

# 2. LE DENDROGRAMME 
fviz_dend(res_cah, 
          rect = TRUE,           # Encadre les groupes en couleurs
          show_labels = FALSE,   
          main = "Dendrogramme de la CAH (Ward)")

# 3. Comparaison visuelle (Projection sur ACP)
fviz_cluster(res_cah, data = donnees_scaled,
             geom = "point",
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal(),
             main = "Comparaison : Classification Hiérarchique (CAH)")