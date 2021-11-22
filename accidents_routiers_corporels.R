# effacer les objets de l'environnement
rm(list = ls())

# bibliotheques
library('dplyr')
library('stringr')
install.packages('kableExtra')
library('kableExtra')





########## MISE EN PLACE DE LA BASE

### Chargement de l'emplacement
setwd("~/Documents/M2_SEP/SEMESTRE_1/SEP0953/GESTION_PROJET/PROJET/AccidentsRoutiers")



### Import des données
carac <- read.csv2(file="data/caracteristiques-2019.csv")
lieux <- read.csv2(file="data/lieux-2019.csv")
usagers <- read.csv2(file="data/usagers-2019.csv")
vehic <- read.csv2(file="data/vehicules-2019.csv")

# On garde les variables utiles
var_carac <- c("Num_Acc","jour","mois","hrmn","lum","com","agg","int","atm","col")
carac <- carac[,var_carac]

var_lieux <- c("Num_Acc","catr","circ","nbv","vosp","prof",
               "plan","surf","infra","situ","vma")
lieux <- lieux[,var_lieux]

var_vehic <- c("Num_Acc", "id_vehicule","catv","obs","obsm","manv","motor")
vehic <- vehic[,var_vehic]

var_usagers <- c("Num_Acc","id_vehicule", "catu","grav",
                 "sexe","an_nais","trajet")
usagers <- usagers[,var_usagers]



### Formatage des données

## Base caracteristiques
# numero accident
carac$Num_Acc <- as.factor(carac$Num_Acc)
lieux$Num_Acc <- as.factor(lieux$Num_Acc)
vehic$Num_Acc <- as.factor(vehic$Num_Acc)
usagers$Num_Acc <- as.factor(usagers$Num_Acc)
# date
carac$date <- paste(carac$jour,carac$mois,"2019",sep="/")
carac$date <- as.Date(carac$date,format="%d/%m/%Y")
carac <- carac[,-c(2,3)]
carac <- relocate(carac,"date",.after="Num_Acc")
# heure 
carac$hrmn <- as.factor(carac$hrmn)
# lumiere
carac$lum <- as.factor(carac$lum)
# commune
carac$com <- as.factor(carac$com)
# agglomeration
carac$agg <- as.factor(carac$agg)
# intersection
carac$int <- as.factor(carac$int)
# météo
carac$atm <- as.factor(carac$atm)
# type de collision
carac$col <- as.factor(carac$col)

## Base lieux
# categorie de route
lieux$catr <- as.factor(lieux$catr)
# regime de circulation
lieux$circ <- as.factor(lieux$circ)
# voie reservée
lieux$vosp <- as.factor(lieux$vosp)
# déclivité
lieux$prof <- as.factor(lieux$prof)
# virage
lieux$plan <- as.factor(lieux$plan)
# etat de la surface
lieux$surf <- as.factor(lieux$surf)
# infrastructures
lieux$infra <- as.factor(lieux$infra)
# situation de l'accident
lieux$situ <- as.factor(lieux$situ)

## Base véhicules
# id vehicule
vehic$id_vehicule <- as.factor(vehic$id_vehicule)
# catégorie du véhicule
vehic$catv <- as.factor(vehic$catv)
# obstacle fixe heurté
vehic$obs <- as.factor(vehic$obs)
# obstacle mobile heurté
vehic$obsm <- as.factor(vehic$obsm)
# manoeuvre avant l'accident
vehic$manv <- as.factor(vehic$manv)
# motorisation
vehic$motor <- as.factor(vehic$motor)

## Base usagers
# catégorie de l'usager
usagers$catu <- as.factor(usagers$catu)
# gravité de l'accident
usagers$grav <- as.factor(usagers$grav)
# sexe
usagers$sexe <- as.factor(usagers$sexe)
# type de trajet
usagers$trajet <- as.factor(usagers$trajet)



### Valeurs manquantes et abérrantes

## Base caracteristiques
which(carac$atm==-1)
which(carac$col==-1)
which(carac$adr==na | carac$adr=="-")
carac<-carac[-c(2247,5578,5782,6796,52842),]

## Base lieux




### Jointures
carac_lieux <- inner_join(carac,lieux,by="Num_Acc")
lieux_usagers <- inner_join(lieux,usagers,by="Num_Acc")
usagers_vehic <- inner_join(usagers,vehic,by="id_vehicule")
colnames(usagers_vehic)[1] <- "Num_Acc"
var_usagers_vehic <- c("id_vehicule","catv","obs","obsm","manv","motor")
usagers_vehic <- usagers_vehic[,var_vehic]
df <- inner_join(carac_lieux,usagers_vehic,by="Num_Acc")





########## STATISTIQUES DESCRIPTIVES

### Base lieux
str(lieux)
summary(lieux)

# Types de routes (catr)
categ_routes <- data.frame(table(lieux$catr)) 
colnames(categ_routes) <- c("Type_route","Nb_acc")
categ_routes$Freq_acc <- round(prop.table(table(lieux$catr))*100,2)
grav_categ_routes <- table(lieux_usagers$catr,lieux_usagers$grav)
categ_routes$Nb_usagers <- apply(grav_categ_routes,1,sum)
categ_routes$Freq_Indemne <- round(grav_categ_routes[,1]/apply(grav_categ_routes,1,sum)*100,2)
categ_routes$Freq_Leger <- round(grav_categ_routes[,4]/apply(grav_categ_routes,1,sum)*100,2)
categ_routes$Freq_Hosp <- round(grav_categ_routes[,3]/apply(grav_categ_routes,1,sum)*100,2)
categ_routes$Freq_Deces <- round(grav_categ_routes[,2]/apply(grav_categ_routes,1,sum)*100,2)
categ_routes$Type_route <- str_replace_all(categ_routes$Type_route, pattern = c("1","2","3","4","5","6","7","9"), 
                                      replacement = c("Autoroute","Nationale","Départementale",
                                                      "Voie Communales","Hors réseau public",
                                                      "Parking","Route metropole urbaine","Autre"))
View(categ_routes)

# Types de régimes de circulation (circ)
regime_circ <- data.frame(table(lieux$circ)) 
colnames(regime_circ) <- c("Type_regime","Nb_acc")
regime_circ$Freq_acc <- round(prop.table(table(lieux$circ))*100,2)
grav_regime_circ <- table(lieux_usagers$circ,lieux_usagers$grav)
regime_circ$Nb_usagers <- apply(grav_regime_circ,1,sum)
regime_circ$Freq_Indemne <- round(grav_regime_circ[,1]/apply(grav_regime_circ,1,sum)*100,2)
regime_circ$Freq_Leger <- round(grav_regime_circ[,4]/apply(grav_regime_circ,1,sum)*100,2)
regime_circ$Freq_Hosp <- round(grav_regime_circ[,3]/apply(grav_regime_circ,1,sum)*100,2)
regime_circ$Freq_Deces <- round(grav_regime_circ[,2]/apply(grav_regime_circ,1,sum)*100,2)
regime_circ$Type_regime <- str_replace_all(regime_circ$Type_regime, pattern = c("-1","1","2","3","4"), 
                                           replacement = c("Non renseigné","A sens unique","Bidirectionnelle",
                                                           "A chaussées séparées","Avec voies d’affectation variable"))
View(regime_circ)

# Nombre de voies
summary(lieux$nbv) # Valeurs aberantes -1

nb_voies <- data.frame(table(lieux$nbv))
colnames(nb_voies) <- c("Nb_voies","Nb_acc")
nb_voies$Freq_acc <- round(prop.table(table(lieux$nbv))*100,2)
grav_nb_voies <- table(lieux_usagers$nbv,lieux_usagers$grav)
nb_voies$Nb_usagers <- apply(grav_nb_voies,1,sum)
nb_voies$Freq_Indemne <- round(grav_nb_voies[,1]/apply(grav_nb_voies,1,sum)*100,2)
nb_voies$Freq_Leger <- round(grav_nb_voies[,4]/apply(grav_nb_voies,1,sum)*100,2)
nb_voies$Freq_Hosp <- round(grav_nb_voies[,3]/apply(grav_nb_voies,1,sum)*100,2)
nb_voies$Freq_Deces <- round(grav_nb_voies[,2]/apply(grav_nb_voies,1,sum)*100,2)
View(nb_voies)

# Types de voies reservées (vosp)
voie_reserv <- data.frame(table(lieux$vosp))
colnames(voie_reserv) <- c("Type_voie","Nb_acc")
voie_reserv$Freq_acc <- round(prop.table(table(lieux$vosp))*100,2)
grav_voie_reserv <- table(lieux_usagers$vosp,lieux_usagers$grav)
voie_reserv$Nb_usagers <- apply(grav_voie_reserv,1,sum)
voie_reserv$Freq_Indemne <- round(grav_voie_reserv[,1]/apply(grav_voie_reserv,1,sum)*100,2)
voie_reserv$Freq_Leger <- round(grav_voie_reserv[,4]/apply(grav_voie_reserv,1,sum)*100,2)
voie_reserv$Freq_Hosp <- round(grav_voie_reserv[,3]/apply(grav_voie_reserv,1,sum)*100,2)
voie_reserv$Freq_Deces <- round(grav_voie_reserv[,2]/apply(grav_voie_reserv,1,sum)*100,2)
voie_reserv$Type_voie <- str_replace_all(voie_reserv$Type_voie, pattern = c("-1","0","1","2","3"), 
                                           replacement = c("Non renseigné","Sans objet","Piste cyclable",
                                                           "Bande cyclable","Voie réservée"))
View(voie_reserv)

# Types de profils (prof)
decliv <- data.frame(table(lieux$prof))
colnames(decliv) <- c("Type_profil","Nb_acc")
decliv$Freq_acc <- round(prop.table(table(lieux$prof))*100,2)
grav_decliv <- table(lieux_usagers$prof,lieux_usagers$grav)
decliv$Nb_usagers <- apply(grav_decliv,1,sum)
decliv$Freq_Indemne <- round(grav_decliv[,1]/apply(grav_decliv,1,sum)*100,2)
decliv$Freq_Leger <- round(grav_decliv[,4]/apply(grav_decliv,1,sum)*100,2)
decliv$Freq_Hosp <- round(grav_decliv[,3]/apply(grav_decliv,1,sum)*100,2)
decliv$Freq_Deces <- round(grav_decliv[,2]/apply(grav_decliv,1,sum)*100,2)
decliv$Type_profil <- str_replace_all(decliv$Type_profil, pattern = c("-1","1","2","3","4"), 
                                         replacement = c("Non renseigné","Plat","Pente",
                                                         "Sommet de côte","Bas de côte"))
View(decliv)

# Types de plan (plan)
virage <- data.frame(table(lieux$plan))
colnames(virage) <- c("Type_plan","Nb_acc")
virage$Freq_acc <- round(prop.table(table(lieux$plan))*100,2)
grav_virage <- table(lieux_usagers$plan,lieux_usagers$grav)
virage$Nb_usagers <- apply(grav_virage,1,sum)
virage$Freq_Indemne <- round(grav_virage[,1]/apply(grav_virage,1,sum)*100,2)
virage$Freq_Leger <- round(grav_virage[,4]/apply(grav_virage,1,sum)*100,2)
virage$Freq_Hosp <- round(grav_virage[,3]/apply(grav_virage,1,sum)*100,2)
virage$Freq_Deces <- round(grav_virage[,2]/apply(grav_virage,1,sum)*100,2)
virage$Type_plan <- str_replace_all(virage$Type_plan, pattern = c("-1","1","2","3","4"), 
                                      replacement = c("Non renseigné","Rectiligne","Gauche",
                                                      "Droite","S"))
View(virage)

# Etat de la surface (surf)
surface <- data.frame(table(lieux$surf))
colnames(surface) <- c("Type_surface","Nb_acc")
surface$Freq_acc <- round(prop.table(table(lieux$surf))*100,2)
grav_surface <- table(lieux_usagers$surf,lieux_usagers$grav)
surface$Nb_usagers <- apply(grav_surface,1,sum)
surface$Freq_Indemne <- round(grav_surface[,1]/apply(grav_surface,1,sum)*100,2)
surface$Freq_Leger <- round(grav_surface[,4]/apply(grav_surface,1,sum)*100,2)
surface$Freq_Hosp <- round(grav_surface[,3]/apply(grav_surface,1,sum)*100,2)
surface$Freq_Deces <- round(grav_surface[,2]/apply(grav_surface,1,sum)*100,2)
surface$Type_surface <- str_replace_all(surface$Type_surface, pattern = c("-1","1","2","3","4","5","6","7","8","9"), 
                                    replacement = c("Non renseigné","Normale","Mouillée","Flaques","Inondée","Enneigée",
                                                    "Boue","Verglacée","Corps gras ou huile","Autre"))
View(surface)

# Infrastructures (infra)
infrastructures <- data.frame(table(lieux$infra))
colnames(infrastructures) <- c("Type_infra","Nb_acc")
infrastructures$Freq_acc <- round(prop.table(table(lieux$infra))*100,2)
grav_infra <- table(lieux_usagers$infra,lieux_usagers$grav)
infrastructures$Nb_usagers <- apply(grav_infra,1,sum)
infrastructures$Freq_Indemne <- round(grav_infra[,1]/apply(grav_infra,1,sum)*100,2)
infrastructures$Freq_Leger <- round(grav_infra[,4]/apply(grav_infra,1,sum)*100,2)
infrastructures$Freq_Hosp <- round(grav_infra[,3]/apply(grav_infra,1,sum)*100,2)
infrastructures$Freq_Deces <- round(grav_infra[,2]/apply(grav_infra,1,sum)*100,2)
infrastructures$Type_infra <- str_replace_all(infrastructures$Type_infra, pattern = c("-1","0","1","2","3","4","5","6","7","8","9"), 
                                        replacement = c("Non renseigné","Aucune","Sous terrain ou tunnel","Pont ou autopont",
                                                        "Bretelle","Voie ferrée","Carrefour aménagé","Zone piétonne",
                                                        "Zone de péage","Corps gras ou huile","Autres"))
View(infrastructures)

# Infrastructures (situ)
situation <- data.frame(table(lieux$situ))
colnames(situation) <- c("Type_situ","Nb_acc")
situation$Freq_acc <- round(prop.table(table(lieux$situ))*100,2)
grav_situ <- table(lieux_usagers$situ,lieux_usagers$grav)
situation$Nb_usagers <- apply(grav_situ,1,sum)
situation$Freq_Indemne <- round(grav_situ[,1]/apply(grav_situ,1,sum)*100,2)
situation$Freq_Leger <- round(grav_situ[,4]/apply(grav_situ,1,sum)*100,2)
situation$Freq_Hosp <- round(grav_situ[,3]/apply(grav_situ,1,sum)*100,2)
situation$Freq_Deces <- round(grav_situ[,2]/apply(grav_situ,1,sum)*100,2)
situation$Type_situ <- str_replace_all(situation$Type_situ, pattern = c("-1","1","2","3","4","5","6","8"), 
                                              replacement = c("Non renseigné","Chaussée","Bande arret urgence",
                                                              "Accotement","Trottoir","Piste cyclable",
                                                              "Autre voie spéciale","Autres"))
View(situation)

# Vitesse max autorisée (vma)
summary(lieux$vma) # valeurs aberrantes -1
mean_vma_indemne <- mean(lieux_usagers$vma[lieux_usagers$grav==1])
mean_vma_indemne
mean_vma_leger <- mean(lieux_usagers$vma[lieux_usagers$grav==4])
mean_vma_leger
mean_vma_hosp <- mean(lieux_usagers$vma[lieux_usagers$grav==3])
mean_vma_hosp
mean_vma_deces <- mean(lieux_usagers$vma[lieux_usagers$grav==2])
mean_vma_deces


### Base usagers
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

plot(usagers$grav, main = "Nombre d'usagers par niveau de gravité")

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
### Base vehicules
str(vehic)
# 7 variables, 100710 véhicules

summary(vehic)

# Nombre de vehicules selon leur catégorie
# Renommer ces colonnes
tab_catv <- table(vehic$catv)
View (tab_catv)
plot(vehic$catv, main = "Nombre de vehicules selon leur catégorie")

# Nombre de vehicules par obstacle heurté fixe
#-1 – Non renseigné
#0 – Sans objet
#1 – Véhicule en stationnement
#2 – Arbre
#3 – Glissière métallique
#4 – Glissière béton
#5 – Autre glissière
#6 – Bâtiment, mur, pile de pont
#7 – Support de signalisation verticale ou poste d’appel d’urgence
#8 – Poteau
#9 – Mobilier urbain
#10 – Parapet
#11 – Ilot, refuge, borne haute
#12 – Bordure de trottoir
#13 – Fossé, talus, paroi rocheuse
#14 – Autre obstacle fixe sur chaussée
#15 – Autre obstacle fixe sur trottoir ou accotement
#16 – Sortie de chaussée sans obstacle
#17 – Buse – tête d’aqueduc
tab_obs <- table(vehic$obs)
View(tab_obs)

plot(vehic$obs, main = "Nombre de vehicules par obstacle heurté fixe")

# Nombre de vehicules par obstacle heurté mobile
#-1 – Non renseigné
#0 – Aucun
#1 – Piéton
#2 – Véhicule
#4 – Véhicule sur rail
#5 – Animal domestique
#6 – Animal sauvage
#9 – Autre
tab_obs_mobil <- table(vehic$obsm)
tab_obs_mobil

plot(vehic$obsm, main = "Nombre de vehicules par obstacle heurté mobile")

# Nombre de vehicules par type de motorisation du véhicule
#-1 – Non renseigné 0 – Inconnue
#1 – Hydrocarbures
#2 – Hybride électrique
#3 – Electrique
#4 – Hydrogène
#5 – Humaine
#6 – Autre
tab_motor<- table(vehic$motor)
View (tab_motor)

plot(vehic$motor, main = "Nombre de vehicules par type de motorisation du véhicule")
