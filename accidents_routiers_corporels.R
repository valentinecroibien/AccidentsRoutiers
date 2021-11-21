# effacer les objets de l'environnement
rm(list = ls())

# bibliotheques
library('dplyr')


########## MISE EN PLACE DE LA BASE
### chargement de l'emplacement
setwd("~/Documents/M2_SEP/SEMESTRE_1/SEP0953/GESTION_PROJET/PROJET/AccidentsRoutiers")

### import des données
carac <- read.csv2(file="data/caracteristiques-2019.csv")
lieux <- read.csv2(file="data/lieux-2019.csv")
usagers <- read.csv2(file="data/usagers-2019.csv")
vehic <- read.csv2(file="data/vehicules-2019.csv")

# on garde les variables utiles
var_carac <- c("Num_Acc","jour","mois","hrmn","lum","com","agg","int","atm","col")
carac <- carac[,var_carac]

var_lieux <- c("Num_Acc","catr","voie","circ","nbv","vosp","prof",
               "plan","larrout","surf","infra","situ","vma")
lieux <- lieux[,var_lieux]

var_vehic <- c("Num_Acc", "id_vehicule","catv","obs","obsm","manv","motor")
vehic <- vehic[,var_vehic]

var_usagers <- c("id_vehicule", "catu","grav",
                 "sexe","an_nais","trajet","secu1","secu2","secu3","locp","actp","etatp")
usagers <- usagers[,var_usagers]


### création de la base finale
# jointures 
carac_lieux <- inner_join(carac,lieux,by="Num_Acc")
usagers_vehic <- inner_join(usagers,vehic,by="id_vehicule")
df <- inner_join(carac_lieux,usagers_vehic,by="Num_Acc")



### formats variables
summary(df)
# numero accident
df$Num_Acc <- as.factor(df$Num_Acc)
# date
df$date <- paste(df$jour,df$mois,"2019",sep="/")
df$date <- as.Date(df$date,format="%d/%m/%Y")
df <- df[,-c(2,3)]
df <- relocate(df,"date",.after="Num_Acc")
# heure 
df$hrmn <- as.factor(df$hrmn)
# lumiere
df$lum <- as.factor(df$lum)
# commune
df$com <- as.factor(df$com)
# agglomeration
df$agg <- as.factor(df$agg)
# intersection
df$int <- as.factor(df$int)
# météo
df$atm <- as.factor(df$atm)
# type de collision
df$col <- as.factor(df$col)
# categorie de route
df$catr <- as.factor(df$catr)
# regime de circulation
df$circ <- as.factor(df$circ)
# voie reservée
df$vosp <- as.factor(df$vosp)
# déclivité
df$prof <- as.factor(df$prof)
# virage
df$plan <- as.factor(df$plan)
# largeur de la route
df$larrout <- as.double(df$larrout)
# etat de la surface
df$surf <- as.factor(df$surf)
# infrastructures
df$infra <- as.factor(df$infra)
# situation de l'accident
df$situ <- as.factor(df$situ)
# id vehicule
df$id_vehicule <- as.factor(df$id_vehicule)
# catégorie du véhicule
df$catv <- as.factor(df$catv)
# obstacle fixe heurté
df$obs <- as.factor(df$obs)
# obstacle mobile heurté
df$obsm <- as.factor(df$obsm)
# manoeuvre avant l'accident
df$manv <- as.factor(df$manv)
# motorisation
df$motor <- as.factor(df$motor)
# catégorie de l'usager
df$catu <- as.factor(df$catu)
# gravité de l'accident
df$grav <- as.factor(df$grav)
# sexe
df$sexe <- as.factor(df$sexe)
# type de trajet
df$trajet <- as.factor(df$trajet)
# utilisation sécurité
df$secu1 <- as.factor(df$secu1)
df$secu2 <- as.factor(df$secu2)
df$secu3 <- as.factor(df$secu3)
# localisation du piéton
df$locp <- as.factor(df$locp)
# action du pieton
df$actp <- as.factor(df$actp)
# piéton seul ou non
df$etatp <- as.factor(df$etatp)

### valeurs aberantes ??








########## STATISTIQUES DESCRIPTIVES ##########




