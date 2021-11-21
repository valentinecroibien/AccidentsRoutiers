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

# Statistiques descriptives des bases initiales
## Base usagers
str(usagers)

# catégorie de l'usager
usagers$catu <- as.factor(usagers$catu)
# gravité de l'accident
usagers$grav <- as.factor(usagers$grav)
# sexe
usagers$sexe <- as.factor(usagers$sexe)
# type de trajet
usagers$trajet <- as.factor(usagers$trajet)
# utilisation sécurité
usagers$secu1 <- as.factor(usagers$secu1)
usagers$secu2 <- as.factor(usagers$secu2)
usagers$secu3 <- as.factor(usagers$secu3)
# localisation du piéton
usagers$locp <- as.factor(usagers$locp)
# action du pieton
usagers$actp <- as.factor(usagers$actp)
# piéton seul ou non
usagers$etatp <- as.factor(usagers$etatp)

str(usagers)
# 12 variables, 132977 usagers

summary(usagers)

# Nombre d'usagers par catégorie d'usagers
# 1 = Conducteur
# 2 = Passager
# 3 = Piéton

tab_catu <- table(usagers$catu)
tab_catu
plot(usagers$catu, main = "Nombre d'usagers par catégorie d'usagers")

# Nombre d'usagers par niveau de gravité
# 1 =  Indemne  
# 2 = Tué  
# 3 = Blessé hospitalisé  
# 4 = Blessé léger  
tab_grav <- table(usagers$grav)
tab_grav

plot(usagers$grav, "Nombre d'usagers par niveau de gravité")

# Nombre d'usagers par sexe
# 1 = Masculin
# 2 = Féminin
tab_sexe <- table(usagers$sexe)
tab_sexe

plot(usagers$sexe, main = "Nombre d'usagers par sexe")

# Nombre d'usagers par type de trajet
# -1 – Non renseigné   
# 0 – Non renseigné  
# 1 – Domicile – travail  
# 2 – Domicile – école  
# 3 – Courses – achats  
# 4 – Utilisation professionnelle  
# 5 – Promenade – loisirs  
# 9 – Autre 
tab_trajet <- table(usagers$trajet)
tab_trajet

plot(usagers$trajet, main = "Nombre d'usagers par type de trajet")




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

### valeurs aberantes ??









########## STATISTIQUES DESCRIPTIVES ##########




