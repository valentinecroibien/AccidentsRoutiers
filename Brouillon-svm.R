# effacer les objets de l'environnement
rm(list = ls())

# bibliotheques
library('dplyr')
library('stringr')
library('kableExtra')
library('e1071') #Pour SVM

########## MISE EN PLACE DE LA BASE

### Chargement de l'emplacement
setwd("C:/Users/lilid/OneDrive/Bureau/MASTER 2 SEP/S1/Conf�rences, Business Intelligence, SAS et VBA (Coeff 4)/M COUSIN/Suivi projet Apprentissage stat")

### Import des donn�es
carac <- read.csv2(file="caracteristiques-2019.csv")
lieux <- read.csv2(file="lieux-2019.csv")
usagers <- read.csv2(file="usagers-2019.csv")
vehic <- read.csv2(file="vehicules-2019.csv")

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



### Formatage des donn�es

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
levels(carac$lum) <- c("Plein Jour","Cr�puscule ou aube","Nuit sans �clairage public"," Nuit avec �clairage public non allum� ","Nuit avec �clairage public allum�")

# commune
carac$com <- as.factor(carac$com)
# agglomeration
carac$agg <- as.factor(carac$agg)
levels(carac$agg) <- c("Hors agglom�ration","En agglom�ration")

# intersection
carac$int <- as.factor(carac$int)
levels(carac$int) <- c("Hors intersection","Intersection en X","Intersection en T",
                       " Intersection en Y ","Intersection � plus de 4 branches ","Giratoire ",
                       " Place ","Passage � niveau"," Autre intersection ")

# m�t�o
carac$atm <- as.factor(carac$atm)
levels(carac$atm) <- c("Non renseign�","Normale","Pluie l�g�re","Pluie forte","Neige-gr�le","Brouillard-fum�e",
                       "Vent fort-temp�te", "Temps �blouissant", "Temps couvert", "Autre")

# type de collision
carac$col <- as.factor(carac$col)
levels(carac$col) <- c("Non renseign�", "Deux v�hicules-frontale","Deux v�hicules-par l'arri�re","Deux v�hicules-par le c�t�",
                       "Trois v�hicules et plus-en cha�ne", "Trois v�hicules et plus-collisions multiples","Autre collision","Sans collision")

## Base lieux
# categorie de route
lieux$catr <- as.factor(lieux$catr)
levels(lieux$catr) <- c("Autoroute","Route nationale","Route d�partementale","Voie communale","Hors r�seau public","Parc de stationnement ouvert � la circulation publique",
                        "Routes de m�tropole urbaine","autre")

# regime de circulation
lieux$circ <- as.factor(lieux$circ)
levels(lieux$circ) <- c("Non renseign�","A sens unique","Bidirectionnelle","A chauss�es s�par�es","Avec voies d'affectation variable")

# voie reserv�e
lieux$vosp <- as.factor(lieux$vosp)
levels(lieux$vosp) <- c("Non renseign�","Sans objet","Piste cyclable","Bande cyclable","Voie r�serv�e")

# d�clivit�
lieux$prof <- as.factor(lieux$prof)
levels(lieux$prof) <- c("Non renseign�","Plat", "Pente","Sommet de c�te","Bas de c�te")

# virage
lieux$plan <- as.factor(lieux$plan)
levels(lieux$plan) <- c("Non renseign�","Partie rectiligne","En courbe � gauche","En courbe � droite","En S")

# etat de la surface
lieux$surf <- as.factor(lieux$surf)
levels(lieux$surf) <- c("Non renseign�","Normale","Mouill�e","Flaques","Inond�e","Enneig�e","Boue","Verglac�e","Corps gras-huile",
                        "Autre")

# infrastructures
lieux$infra <- as.factor(lieux$infra)
levels(lieux$infra) <- c("Non renseign�","Aucun","Souterrain-tunnel","Pont-autopont","Bretelle d'�changeur ou de raccordement","Voie ferr�e","Carrefour amm�nag�",
                         "Zone pi�tonne", "Zone de p�age", "Chantier","Autres")

# situation de l'accident
lieux$situ <- as.factor(lieux$situ)
levels(lieux$situ) <- c("Non renseign�","Aucun","Sur chauss�e","Sur bande d'arr�t d'urgence","Sur accotement","Sur trottoir","Sur piste cyclable","Sur autre voie sp�ciale","Autres")

## Base v�hicules
# id vehicule
vehic$id_vehicule <- as.factor(vehic$id_vehicule)
# cat�gorie du v�hicule
vehic$catv <- as.factor(vehic$catv)
levels(vehic$catv) <- c("Ind�terminable","Bicyclette","Cyclomoteur<50cm3","Voiturette",
                        "VL Seul",
                        "VU seul 1,5T <= PTAC <= 3,5T avec ou sans remorque",
                        "PL seul 3,5T <PTCA <= 7,5T"," PL seul > 7,5T",
                        " PL > 3,5T + remorque",
                        "Tracteur routier seul","Tracteur routier + semi-remorque ",
                        "Engin sp�cial","Tracteur agricole",
                        "Scooter < 50 cm3 ","- Motocyclette > 50 cm3 et <= 125 cm3",
                        " Scooter > 50 cm3 et <= 125 cm3 ","Motocyclette > 125 cm3 ",
                        "Scooter > 125 cm3 ",
                        " Quad l�ger <= 50 cm3 ","- Quad lourd > 50 cm3","Autobus",
                        "Autocar","Train","Tramway","3RM <= 50 cm3 ","3RM > 50 cm3 <= 125 cm3",
                        " 3RM > 125 cm3 ","EDP � moteur","EDP sans moteur",
                        "VAE","Autre v�hicule")
# obstacle fixe heurt�
vehic$obs <- as.factor(vehic$obs)
levels(vehic$obs) <- c("Non renseign�","Sans objet","V�hicule en stationnement","Arbre","Glissi�re m�tallique","Glissi�re b�ton",
                       "Autre glissi�re","B�timent, mur, pile de pont"," Support de signalisation verticale ou poste d'appel d'urgence",
                       "Poteau","Mobilier urbain","Parapet","Ilot,refuge,borne haute","Bordure de trottoir","Foss�, talus, paroi rocheuse"," Autre obstacle fixe sur chauss�e",
                       " Autre obstacle fixe sur trottoir ou accotement","Sortie de chauss�e sans obstacle","Buse-t�te d'aqueduc")

# obstacle mobile heurt�
vehic$obsm <- as.factor(vehic$obsm)
levels(vehic$obsm) <- c("Non renseign�","Aucun","Pi�ton","V�hicule","V�hicule sur rail","Animal domestique","Animal sauvage","Autre")

# manoeuvre avant l'accident
vehic$manv <- as.factor(vehic$manv)
levels(vehic$manv) <- c("Non renseign�","Inconnue","Sans changement de direction","M�me sens, m�me file","Entre 2 files","En marche arri�re","A contresens","En franchissant le terre-plein central",
                        "Dans le couloir bus,dans le m�me sens ","Dans le couloir bus, dans le sens inverse","En s'ins�rant","En faisant demi-tour sur la chauss�e","Changeant de file � gauche","Changeant de file � droite","D�port� � gauche","D�port� � droite",
                        "Tournant � gauche","Tournant � droite","D�passant � gauche","D�passant � droite","Traversant la chauss�e","Manoeuvre de stationnement","Manoeuvre d'�vitement",
                        "Ouverture de porte","Arr�t�(hors stationnement","En stationnement avec occupants","Circulant sur le trottoir","Autres manoeuvres")

# motorisation
vehic$motor <- as.factor(vehic$motor)
levels(vehic$motor) <- c("Non renseign�","Inconnue","Hydrocarbures","Hybride �lectrique","Electrique","Hydrog�ne","Humaine","Autre")


## Base usagers
# cat�gorie de l'usager
usagers$catu <- as.factor(usagers$catu)
levels(usagers$catu) <- c("Conducteur","Passager","Pi�ton")
# gravit� de l'accident
usagers$grav <- as.factor(usagers$grav)
levels(usagers$grav) <- c("Indemne","Tu�","Bless� hospitalis�","Bless� l�ger")

# sexe
usagers$sexe <- as.factor(usagers$sexe)
levels(usagers$sexe) <- c("Masculin","F�minin")
# type de trajet
usagers$trajet <- as.factor(usagers$trajet)
levels(usagers$trajet) <- c("Non renseign�","Non renseign�","Domicile-travail","Domicile-�cole","Courses-achats","Utilisation professionnelle","Promenade-loisirs","Autre")


### Valeurs manquantes et ab�rrantes

## Base caracteristiques
va <- which(carac$atm=="Non renseign�")
va2 <- which(carac$col=="Non renseign�")
va3 <-which(is.nan(carac$adr) | carac$adr=="-")
carac<-carac[-c(va, va2, va3),]

## Base lieux
lieux$surf <- str_replace_all(lieux$surf, pattern = "Non renseign�", 
                              replacement = "Autre")



### Jointures
carac_lieux <- inner_join(carac,lieux,by="Num_Acc")
lieux_usagers <- inner_join(lieux,usagers,by="Num_Acc")
usagers_vehic <- inner_join(usagers,vehic,by="id_vehicule")
colnames(usagers_vehic)[1] <- "Num_Acc"

var_usagers_vehic <- c("Num_Acc","id_vehicule","grav","catu","sexe","trajet","catv","obs","obsm","manv","motor")
usagers_vehic <- usagers_vehic[,var_usagers_vehic]

df <- inner_join(carac_lieux,usagers_vehic,by="Num_Acc")

str(df)
#MODELE SVM

View(df)
library('lubridate')
df$hrmn<-hour(strptime(df$hrmn,format="%H:%M"))
df$hrmn<- cut(df$hrmn, breaks =0:24, labels = NULL,
              include.lowest = TRUE, right = FALSE,
              ordered_result = TRUE)

df$date <-wday(df$date,label=TRUE,abbr=FALSE,week_start="1",locale = Sys.getlocale("LC_TIME"))


summary(df)

#Test sur toutes les donn�es des 500 premi�res lignes
#on partage la base en deux parties : train.set et test.set
index <- 1:nrow(df[1:500,])
l <- 2 #(l-1)/l d'observations pour l'apprentissage et 1/l observations pour le test
test.set.index <- sample(x = index, size = trunc(length(index)/l), replace = FALSE)
test.set <- df[1:500,][test.set.index, ]
train.set <- df[1:500,][- test.set.index, ]

str(test.set)
str(train.set)
#avec kernel lin�aire
tune.out <- tune(svm, grav ~ lum + agg + int + atm + col + catr + circ + nbv + vosp + 
                   prof + plan + surf + infra + situ + vma + catu + 
                   sexe + trajet + catv +obs + obsm +manv + motor, data = train.set, kernel = "linear",
                 scale = TRUE,
                 ranges = list(cost = c(2^(-2) ,2^(-1), 1, 2^2, 2^3)))
summary(tune.out)
str(tune.out)
table(true = test.set[, "grav"], 
      pred = predict(tune.out$best.model, newdata = test.set, 
                     scale = TRUE))
err1 <- mean(!(test.set[, "grav"] == 
                 predict(tune.out$best.model, newdata = test.set, 
                         scale = TRUE)))
err1

#avec kernel radial
tune.out <- tune(svm,grav ~ lum + agg + int + atm + col + catr + circ + nbv + vosp + 
                   prof + plan + surf + infra + situ + vma + catu + 
                   sexe + trajet + catv +obs + obsm +manv + motor, data = train.set,kernel = "radial",
                 scale = TRUE,
                 ranges = list(cost = c(2^(-2) ,2^(-1), 1, 2^2, 2^3), 
                               gamma = c(2^(-2), 2^(-1), 1, 2, 2^2, 2^3))
)
summary(tune.out)
table(true = test.set[, "grav"], 
      pred = predict(tune.out$best.model, newdata = test.set,
                     scale = TRUE))
err2 <- mean(!(test.set[, "grav"] == 
                 predict(tune.out$best.model, newdata = test.set,
                         scale = TRUE)))
err2

#Comparaison
err1
err2

#Test sur 70 % des donn�es pour l'apprentissage

#on partage la base en deux parties : train.set et test.set
index <- nrow(df[1:500,])
set.seed(123)
test.set.index <- sample(1:index, size = 0.7*index, replace = FALSE)
test.set <- df[1:500,][test.set.index, ]
train.set <- df[1:500,][- test.set.index, ]

#avec kernel lin�aire
tune.out <- tune(svm,grav ~lum + agg + int + atm + col + catr + circ + nbv + vosp + 
                   prof + plan + surf + infra + situ + vma + catu + 
                   sexe + trajet + catv +obs + obsm +manv + motor, data = train.set, kernel = "linear",
                 scale = TRUE,
                 ranges = list(cost = c(2^(-2) ,2^(-1), 1, 2^2, 2^3)))
summary(tune.out)
table(true = test.set[,"grav"], 
      pred = predict(tune.out$best.model, newdata = test.set, 
                     scale = TRUE))
err1 <- mean(!(test.set[, "grav"] == 
                 predict(tune.out$best.model, newdata = test.set, 
                         scale = TRUE)))
err1

#avec kernel radial
tune.out <- tune(svm,grav ~ lum + agg + int + atm + col + catr + circ + nbv + vosp + 
                   prof + plan + surf + infra + situ + vma + catu + 
                   sexe + trajet + catv +obs + obsm +manv + motor, data = train.set, kernel = "radial",
                 scale = TRUE,
                 ranges = list(cost = c(2^(-2) ,2^(-1), 1, 2^2, 2^3), 
                               gamma = c(2^(-2), 2^(-1), 1, 2, 2^2, 2^3))
)
summary(tune.out)
table(true = test.set[, "grav"], 
      pred = predict(tune.out$best.model, newdata = test.set,
                     scale = TRUE))
err2 <- mean(!(test.set[, "grav"] == 
                 predict(tune.out$best.model, newdata = test.set,
                         scale = TRUE)))
err2

#Comparaison
err1
err2         
