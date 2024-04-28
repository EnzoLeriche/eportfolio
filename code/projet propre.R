################################################################################
#                                                                              #
#                 CRÉATION DE LA BASE DE DONNÉES                               #
#                                                                              #
################################################################################

df1 <- read.table("C:/Users/User/OneDrive/Documents/data/winter_olympics_medals.csv",
                  header=TRUE,
                  sep=",",
                  na.strings="")


df2 <- read.table("C:/Users/User/OneDrive/Documents/data/countries_gdp_hist.csv",
                  header=TRUE,
                  sep=";",
                  na.strings="")

df3 <- read.csv("C:/Users/User/OneDrive/Documents/data/df3.csv", header=TRUE, na.strings="")

df4 <- read.csv("C:/Users/User/OneDrive/Documents/data/countries.csv",
                  header=TRUE,
                  na.strings="")
df5 <- read.csv("C:/Users/User/OneDrive/Documents/data/nouveau_jeux_de_données.csv",
                header=TRUE,
                na.strings="")

names(df1) <- c("id","année","sport","médaille","id_pays","pays","organisateur")
names(df2) <- c("id_pays","region","sub_region_name", "intermediate_region", "pays", "income_group", "année", "PIB", "PIB_en_million", "PIB_evolution")
names(df3) <- c("année","id_année","pays","id_pays","accès_combustibles","accès_électricité","terres_agricoles", "score_dees", "dépenses_éducation", 
                "espérance_vie", "taux_alphabétisation", "eaux", "densité_pop", "taux_pauvreté","sous-alimentation","superficie_forestière","utilisateurs_internet")
names(df4) <- c("id_pays2","latitude", "longitude", "pays")
names(df5) <- c("année","id_année","pays","id_pays","depenses_santé_par_habitant","depenses_administrations_publiques","Indice_de_gini", "pop_active", "taux_alphabetisation_adultes", 
                "taux_alphabetisation_jeunes", "depenses_militaires", "densité_de_pop", "pop_vivant_bidonvilles", "commerce")

df_joined <- merge(merge(df1, df2, by = c("année", "pays", "id_pays")), df3, by = c("année", "pays", "id_pays"))
df_joined <- merge(df_joined, df4, by="pays", all.x=TRUE)
df_joined <- merge(df_joined, df5, by = c("année", "pays", "id_pays"))

valid_pays <- c("Australia", "Austria", "Belarus", "Belgium", "Bulgaria",
                "Canada", "Croatia", "Czech Republic", "Czechoslovakia",
                "Democratic People's Republic of Korea", "Denmark", "Estonia",
                "Federal Republic of Germany ", "Finland",
                "France", "German Democratic Republic", "Germany",
                "Great Britain", "Hungary", "Italy", "Japan", "Kazakhstan",
                "Latvia", "Liechtenstein", "Luxembourg", "Netherlands",
                "New Zealand", "Norway", "People's Republic of China", "Poland",
                "Republic of Korea", "Romania", "Russian Federation", "Slovakia",
                "Slovenia", "Spain", "Sweden", "Switzerland", "Ukraine",
                "Unified Team (ex USSR)", "United States of America",
                "United Team of Germany", "USSR", "Uzbekistan",
                "Yugoslavia")
valid_codes <- c("AUS","AUT","BEL","BLR","BUL","CAN","CHN","CRO","CZE","DEN",
                 "ESP","EST","EUN","FIN","FRA","GBR","GER","HUN","ITA","JPN","KAZ","KOR","LAT",
                 "LIE","LUX","NED","NOR","NZL","POL","PRK","ROU","RUS","SLO","SUI","SVK","SWE","TCH","UKR","URS","USA","UZB","YUG")


df_joined <- df_joined[which(df_joined$pays %in% valid_pays | df_joined$id_pays%in% valid_codes), ]

valid_annee <- c("1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", 
                 "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010")
df_joined <- df_joined[which(df_joined$année %in% valid_annee), ]
df_joined <- df_joined[order(df_joined$année, df_joined$pays),]

df_joined <- subset(df_joined, select = -c(score_dees, taux_alphabétisation, intermediate_region, income_group, id_pays2))
View(df_joined)


################################################
### Création de colonnes pour les médailles ####
################################################

# Créer une colonne pour le nombre de médailles par pays et par année
df_joined$nb_medailles_par_pays_annee <- ave(df_joined$médaille, df_joined$pays, df_joined$année, FUN = length)
df_joined$nb_medailles_par_pays_annee <- as.numeric(df_joined$nb_medailles_par_pays_annee)

# Créer une colonne pour la somme des médailles par année
df_joined$somme_medailles_par_annee <- ave(df_joined$médaille, df_joined$année, FUN = length)
df_joined$somme_medailles_par_annee <- as.numeric(df_joined$somme_medailles_par_annee)

#créer une colonne pour le pourcentage de medaille gagner par pays et par année
df_joined$pourcent_medailles_par_pays_annee <- (df_joined$nb_medailles_par_pays_annee/df_joined$somme_medailles_par_annee) * 100

# Créer une colonne pour la somme des médailles par pays
df_joined$somme_medailles_par_pays <- ave(df_joined$médaille, df_joined$pays, FUN = length)
df_joined$somme_medailles_par_pays <- as.numeric(df_joined$somme_medailles_par_pays)

#créer une colonne pour le pourcentage de medaille gagner par annee pour chaque pays
df_joined$pourcent_medailles_par_annee_pays <- (df_joined$nb_medailles_par_pays_annee/df_joined$somme_medailles_par_pays) * 100

#Modification de la colonne Indice de Gini pour avoir la valeur entre 0 et 1
df_joined$Indice_de_gini <- as.numeric(df_joined$Indice_de_gini, na.rm = TRUE)
df_joined$Indice_de_gini <- df_joined$Indice_de_gini/100


##########################
### Année 1992 - PIB #####
##########################
#extraire les données pour l'année 1992
donnees_1992 <- subset(df_joined, année == 1992)

# trier les pays par ordre croissant de PIB pour l'année 1992
bottom_pays_PIB_1992 <- donnees_1992[order(donnees_1992$PIB, decreasing=FALSE), "pays"]
# Supprimer les doublons de la liste des pays sélectionnés
bottom_pays_PIB_1992 <- unique(bottom_pays_PIB_1992)[1:5]
# Créer un sous-ensemble des données pour les 5 pays sélectionnés
bottom_data_1992 <- subset(donnees_1992, pays %in% bottom_pays_PIB_1992)
bottom_data_1992 <- bottom_data_1992[!duplicated(bottom_data_1992$pays), ]
# Créer un graphique à barres avec les PIB des 5 pays
barplot(bottom_data_1992$PIB, names.arg = bottom_data_1992$pays, 
        col = "blue", main = "Les 5 pays avec le plus faible PIB en 1992")

aggregate(PIB ~ pays, data = bottom_data_1992, sum)


# trier les pays par ordre croissant de PIB pour l'année 1992
top_pays_PIB_1992 <- donnees_1992[order(donnees_1992$PIB, decreasing = TRUE), "pays"]
# Supprimer les doublons de la liste des pays sélectionnés
top_pays_PIB_1992 <- unique(top_pays_PIB_1992)[1:5]
# Créer un sous-ensemble des données pour les 5 pays sélectionnés
top_data_1992 <- subset(donnees_1992, pays %in% top_pays_PIB_1992)
top_data_1992 <- top_data_1992[!duplicated(top_data_1992$pays), ]
# Créer un graphique à barres avec les PIB des 5 pays
barplot(top_data_1992$PIB, names.arg = top_data_1992$pays, 
        col = "blue", main = "Les 5 pays avec le plus haut PIB en 1992")

aggregate(PIB ~ pays, data = top_data_1992, sum)


##########################
### Année 2010 - PIB #####
##########################
#extraire les données pour l'année 2010
donnees_2010 <- subset(df_joined, année == 2010)

# trier les pays par ordre croissant de PIB pour l'année 2010
bottom_pays_PIB_2010 <- donnees_2010[order(donnees_2010$PIB, decreasing=FALSE), "pays"]
# Supprimer les doublons de la liste des pays sélectionnés
bottom_pays_PIB_2010 <- unique(bottom_pays_PIB_2010)[1:5]
# Créer un sous-ensemble des données pour les 5 pays sélectionnés
bottom_data_2010 <- subset(donnees_2010, pays %in% bottom_pays_PIB_2010)
bottom_data_2010 <- bottom_data_2010[!duplicated(bottom_data_2010$pays), ]
# Créer un graphique à barres avec les PIB des 5 pays
barplot(bottom_data_2010$PIB, names.arg = bottom_data_2010$pays, 
        col = "blue", main = "Les 5 pays avec le plus faible PIB en 2010")

aggregate(PIB ~ pays, data = bottom_data_2010, sum)

# trier les pays par ordre croissant de PIB pour l'année 2010
top_pays_PIB_2010 <- donnees_2010[order(donnees_2010$PIB, decreasing = TRUE), "pays"]
# Supprimer les doublons de la liste des pays sélectionnés
top_pays_PIB_2010 <- unique(top_pays_PIB_2010)[1:5]
# Créer un sous-ensemble des données pour les 5 pays sélectionnés
top_data_2010 <- subset(donnees_2010, pays %in% top_pays_PIB_2010)
top_data_2010 <- top_data_2010[!duplicated(top_data_2010$pays), ]
# Créer un graphique à barres avec les PIB des 5 pays
barplot(top_data_2010$PIB, names.arg = top_data_2010$pays, 
        col = "blue", main = "Les 5 pays avec le plus haut PIB en 2010")

aggregate(PIB ~ pays, data = top_data_2010, sum)


################################
### Année 1992 - Médailles #####
################################

# trier les pays par ordre croissant de médailles pour l'année 1992
bottom_pays_medaille_1992 <- donnees_1992[order(donnees_1992$nb_medailles_par_pays_annee, decreasing=FALSE), "pays"]
# Supprimer les doublons de la liste des pays sélectionnés
bottom_pays_medaille_1992 <- unique(bottom_pays_medaille_1992)[1:5]
# Créer un sous-ensemble des données pour les 5 pays sélectionnés
bottom_data_medailles_1992 <- subset(donnees_1992, pays %in% bottom_pays_medaille_1992)
bottom_data_medailles_1992 <- bottom_data_medailles_1992[!duplicated(bottom_data_medailles_1992$pays), ]
# Créer un graphique à barres avec les PIB des 5 pays
barplot(bottom_data_medailles_1992$nb_medailles_par_pays_annee, names.arg = bottom_data_medailles_1992$pays, 
        col = "blue", main = "Les 5 pays avec le moins de médailles gagnés en 1992")

aggregate(nb_medailles_par_pays_annee ~ pays, data = bottom_data_medailles_1992, sum)


# trier les pays par ordre croissant de PIB pour l'année 1992
top_pays_medailles_1992 <- donnees_1992[order(donnees_1992$nb_medailles_par_pays_annee, decreasing = TRUE), "pays"]
# Supprimer les doublons de la liste des pays sélectionnés
top_pays_medailles_1992 <- unique(top_pays_medailles_1992)[1:5]
# Créer un sous-ensemble des données pour les 5 pays sélectionnés
top_data_medailles_1992 <- subset(donnees_1992, pays %in% top_pays_medailles_1992)
top_data_medailles_1992 <- top_data_medailles_1992[!duplicated(top_data_medailles_1992$pays), ]
# Créer un graphique à barres avec les PIB des 5 pays
barplot(top_data_medailles_1992$nb_medailles_par_pays_annee, names.arg = top_data_medailles_1992$pays, 
        col = "blue", main = "Les 5 pays avec le plus de médailles gagnés en 1992")

aggregate(nb_medailles_par_pays_annee ~ pays, data = top_data_medailles_1992, sum)


################################
### Année 2010 - Médailles #####
################################


# trier les pays par ordre croissant des medailles pour l'année 2010
bottom_pays_medailles_2010 <- donnees_2010[order(donnees_2010$nb_medailles_par_pays_annee, decreasing=FALSE), "pays"]
# Supprimer les doublons de la liste des pays sélectionnés
bottom_pays_medailles_2010 <- unique(bottom_pays_medailles_2010)[1:5]
# Créer un sous-ensemble des données pour les 5 pays sélectionnés
bottom_data_medailles_2010 <- subset(donnees_2010, pays %in% bottom_pays_medailles_2010)
bottom_data_medailles_2010 <- bottom_data_medailles_2010[!duplicated(bottom_data_medailles_2010$pays), ]
# Créer un graphique à barres avec les PIB des 5 pays
barplot(bottom_data_medailles_2010$nb_medailles_par_pays_annee, names.arg = bottom_data_medailles_2010$pays, 
        col = "blue", main = "Les 5 pays avec le moins de médailles gagnés en 2010")

aggregate(nb_medailles_par_pays_annee ~ pays, data = bottom_data_medailles_2010, sum)


# trier les pays par ordre croissant des médailles pour l'année 1992
top_pays_medailles_2010 <- donnees_2010[order(donnees_2010$nb_medailles_par_pays_annee, decreasing = TRUE), "pays"]
# Supprimer les doublons de la liste des pays sélectionnés
top_pays_medailles_2010 <- unique(top_pays_medailles_2010)[1:5]
# Créer un sous-ensemble des données pour les 5 pays sélectionnés
top_data_medailles_2010 <- subset(donnees_2010, pays %in% top_pays_medailles_2010)
top_data_medailles_2010 <- top_data_medailles_2010[!duplicated(top_data_medailles_2010$pays), ]
# Créer un graphique à barres avec les PIB des 5 pays
barplot(top_data_medailles_2010$nb_medailles_par_pays_annee, names.arg = top_data_medailles_2010$pays, 
        col = "blue", main = "Les 5 pays avec le plus de médailles gagnés en 2010")

aggregate(nb_medailles_par_pays_annee ~ pays, data = top_data_medailles_2010, sum)


#autre graphique
aggregate(médaille ~ année, data = df_joined, FUN = length)
plot(aggregate(médaille ~ année, data = df_joined, FUN = length),type="h", main="nombre de médailles par année")

######################
## Test avec ggplot ##
######################
library(ggplot2)

# Créer un graphique à barres  des 5 pays avec le moins  médailles
ggplot(data = bottom_data_medailles_2010, aes(x = pays, y = nb_medailles_par_pays_annee)) +
  geom_bar(stat = "identity", fill = "blue") +
  ggtitle("Les 5 pays avec le moins de médailles gagnés en 2010") +
  xlab("Pays") + ylab("Nombre de médailles")

#Évolution du PIB pour l'Autriche (Austria) en pourcentage
autriche_data <- subset(df_joined, pays == "Austria")
autriche_data$evolution_pourcentage_PIB <- c(NA, diff(autriche_data$PIB) / autriche_data$PIB[-nrow(autriche_data)] * 100)
autriche_data <- autriche_data[!duplicated(autriche_data$année), ]
autriche_data_PIB <- subset(autriche_data, select=c(année, pays, PIB, PIB_en_million, evolution_pourcentage_PIB))

austria_PIB_evolution<- ggplot(autriche_data_PIB, aes(x = année, y = evolution_pourcentage_PIB)) +
  geom_line(color = "blue") +
  labs(title = "Évolution du PIB en pourcentage de l'Autriche de 1992 à 2010",
       x = "Année",
       y = "Évolution du PIB en pourcentage")
austria_PIB_evolution

#Évolution des médailles pour l'autriche en pourcentage

autriche_data$evolution_pourcentage_medailles <- c(NA, diff(autriche_data$somme_medailles_par_annee[autriche_data$pays == "Austria"]) / autriche_data$somme_medailles_par_annee[autriche_data$pays == "Austria"][-nrow(autriche_data)] * 100)
autriche_data_medailles <- autriche_data[!duplicated(autriche_data$année), ]
autriche_data_medailles <- subset(autriche_data, select=c(année, pays, evolution_pourcentage_medailles))


austria_medailles_evolution<- ggplot(autriche_data, aes(x = année, y = evolution_pourcentage_medailles)) +
  geom_line(color = "red") +
  labs(title = "Évolution des médailles en pourcentage de l'Autriche de 1992 à 2010",
       x = "Année",
       y = "Évolution des médailles en pourcentage")
austria_medailles_evolution


#Affiche l'évolution du PIB et des médailles sur le même graphique
ggplot() +
  geom_line(data = autriche_data_medailles, aes(x = année, y = evolution_pourcentage_medailles, color = "Médailles")) +
  geom_line(data = autriche_data_PIB, aes(x = année, y = evolution_pourcentage_PIB, color = "PIB")) +
  scale_color_manual(values = c("red", "blue")) +
  labs(title = "Évolution du PIB et des médailles de l'Autriche de 1992 à 2010",
       x = "Année",
       y = "Évolution en pourcentage",
       color = "Variable") +
  geom_vline(xintercept = c(1998,2002, 2006), linetype = "dashed", color = "red")



###################################################
## Test de mettre 2 graph a coter avec gridExtra ##
###################################################

library(gridExtra)

grid.arrange(austria_PIB_evolution, austria_medailles_evolution, ncol = 2)


######################
## Test avec plotly ##
######################

library(plotly)

# Créer un graphique à barres  des 5 pays avec le moins  médailles
plot_ly(data = top_data_medailles_2010, x = ~pays, y = ~nb_medailles_par_pays_annee, type = "bar", 
        marker = list(color = "skyblue")) %>%
  layout(title = "Les 5 pays avec le moins de médailles gagnés en 2010", 
         xaxis = list(title = "Pays"), yaxis = list(title = "Nombre de médailles"))

#Graphique de l'évolution du PIB et des Médailles
plot_ly() %>% 
  add_lines(data = autriche_data_medailles, x = ~année, y = ~evolution_pourcentage_medailles, color = "Médailles") %>% 
  add_lines(data = autriche_data_PIB, x = ~année, y = ~evolution_pourcentage_PIB, color = "PIB") %>% 
  layout(title = "Évolution du PIB et des médailles de l'Autriche de 1992 à 2010",
         xaxis = list(title = "Année"),
         yaxis = list(title = "Évolution en pourcentage"),
         colorway = c("sky blue", "dark grey","red"),
         legend = list(title = "Variable"))


#Graphique à bulles
plot_ly(data = df_joined, x = ~pays, y = ~somme_medailles_par_pays, 
        color = ~pays, size = ~somme_medailles_par_pays,
        marker = list(opacity = 1, sizemode='diameter'),
        colors = "Dark2") %>%
  add_markers() %>%
  layout(title = "Nombre total de médailles par pays",
         xaxis = list(title = "Pays"),
         yaxis = list(title = "Nombre total de médailles"))
         


################################################################################
#                                                                              #
#                       ANALYSES DESCRIPTIVES                                  #
#                                                                              #
################################################################################
# Colonnes à convertir en format numeric
columns_to_convert <- c("accès_électricité",
  "terres_agricoles",
  "dépenses_éducation",
  "espérance_vie",
  "eaux",
  "densité_pop",
  "taux_pauvreté",
  "sous-alimentation",
  "superficie_forestière",
  "utilisateurs_internet"
)

# Conversion des colonnes en format numeric
df_joined[, columns_to_convert] <- lapply(df_joined[, columns_to_convert], as.numeric)
library(psych)
#faire un tableau descriptif 
variables <- data.frame(df_joined[, 9:22])
variables <- subset(variables, select = -c(PIB_en_million, id_année, accès_combustibles, sub_region_name))
resume_table <- describe(variables, fast = TRUE)
options(scipen = 999, digits = 10)


library(pander)
## Afficher le tableau descriptif 
resume_table
pander(resume_table, split.table = Inf)


################################################################################
#                                                                              #
#                       ANALYSES / GRAPHIQUES                                  #
#                                                                              #
################################################################################
library(plotly)
library(dplyr)

##############################################################
# Couleurs pour chaque pays a reprendre pour tous les graphs #
##############################################################

# Correspondance pays-couleurs
couleurs <- c("Norway" = "#87CEEB", "Canada" = "#FFA500", "Austria" = "#2ca02c",
              "Russian Federation" = "#9467bd", "Italy" = "#FF6961", "France" = "#8c564b",
              "Finland" = "#e377c2", "Sweden" = "#1f77b4", "Japan" = "#ff7f0e",
              "Poland" = "#17becf", "Belarus" = "#FF6A6A", "Australia" = "#FF9896",
              "Estonia" = "#FFD700", "Kazakhstan" = "#ffbb78", "Ukraine" = "#9467bd",
              "Luxembourg" = "#1f77b4", "Uzbekistan" = "#ff7f0e", "Spain" = "#d62728",
              "New Zealand" = "#8c564b", "Belgium" = "#7f7f7f")

# Liste de couleurs pour les bulles
couleurs_pays <- couleurs[as.character(df_joined$pays)]


#######################################################
#                                                     #
#  Nombre total de médailles par pays de 1992 à 2010  #
#                                                     #
#######################################################


df_joined <- df_joined %>%
  mutate(pays = reorder(pays, somme_medailles_par_pays)) %>%
  arrange(somme_medailles_par_pays)

plot_ly(data = df_joined, x = ~pays, y = ~somme_medailles_par_pays, 
        color = ~pays, size = ~somme_medailles_par_pays,
        marker = list(opacity = 1, sizemode = 'diameter',
                      color = couleurs_pays,
                      line = list(color = couleurs_pays, width = 1)),
        colors = unique(unlist(couleurs_pays))) %>%
  add_markers() %>%
  layout(title = "Nombre total de médailles par pays de 1992 à 2010",
         xaxis = list(title = "Pays"),
         yaxis = list(title = "Nombre total de médailles")) %>%
  layout(legend = list(traceorder = "reversed"))

table_medailles <- data.frame(pays = df_joined$pays, somme_medailles_par_pays = df_joined$somme_medailles_par_pays)
table_medailles <- table_medailles[!duplicated(table_medailles$pays), ]

table_medailles

#######################################################
#                                                     #
#              PIB  par pays en 2010                  #
#                                                     #
#######################################################

# Filtrer les données pour l'année 2010
donnees_2010_V2 <- donnees_2010[!duplicated(donnees_2010$pays), ]

donnees_2010_V2 <- donnees_2010_V2 %>%
  mutate(pays = reorder(pays, PIB)) %>%
  arrange(PIB)

# Liste de couleurs pour les bulles
couleurs_pays <- couleurs[as.character(donnees_2010_V2$pays)]

graph_2010 <- plot_ly(data = donnees_2010_V2, x = ~pays, y = ~PIB, 
        color = ~pays, size = ~PIB,
        marker = list(opacity = 1, sizemode = 'diameter',
                      color = couleurs_pays,
                      line = list(color = couleurs_pays, width = 1)),
        colors = unique(unlist(couleurs_pays))) %>%
  add_markers() %>%
  layout(title = "PIB de chaque pays en 2010",
         xaxis = list(title = "Pays"),
         yaxis = list(title = "PIB")) %>%
  layout(legend = list(traceorder = "reversed"))
graph_2010
#######################################################
#                                                     #
#              PIB par pays en 1992                   #
#                                                     #
#######################################################

# Filtrer les données pour l'année 1992
donnees_1992_V2 <- donnees_1992[!duplicated(donnees_1992$pays), ]

donnees_1992_V2 <- donnees_1992_V2 %>%
  mutate(pays = reorder(pays, PIB)) %>%
  arrange(PIB)

# Liste de couleurs pour les bulles
couleurs_pays <- couleurs[as.character(donnees_1992_V2$pays)]

graph_1992 <-plot_ly(data = donnees_1992_V2, x = ~pays, y = ~PIB, 
        color = ~pays, size = ~PIB,
        marker = list(opacity = 1, sizemode = 'diameter',
                      color = couleurs_pays,
                      line = list(color = couleurs_pays, width = 1)),
        colors = unique(unlist(couleurs_pays))) %>%
  add_markers() %>%
  layout(title = "PIB de chaque pays en 1992",
         xaxis = list(title = "Pays"),
         yaxis = list(title = "PIB")) %>%
  layout(legend = list(traceorder = "reversed"))
graph_1992
#######################################################
#                                                     #
#          PIB par pays en 1992 et 2010               #
#                                                     #
#######################################################
subplot_title <- "PIB de chaque pays 1992 - 2010"
subplot <- subplot(graph_1992, graph_2010, nrows = 1, margin = 0.05)
layout_subplot <- layout(subplot, title = subplot_title, showlegend = TRUE)

subplot(layout_subplot)


library(plotly)

# Filtrer les données pour inclure uniquement les variables nécessaires
df_pib_esperance_vie <- df_joined[, c("pays", "PIB", "espérance_vie")]
df_pib_esperance_vie <- df_pib_esperance_vie[!duplicated(df_pib_esperance_vie$pays), ]

# Liste de couleurs pour les bulles
couleurs_pays <- couleurs[as.character(df_pib_esperance_vie$pays)]

# Créer la figure plotly
fig <- plot_ly(data = df_pib_esperance_vie, x = ~PIB, y = ~espérance_vie, text = ~pays, 
               color = ~pays, colors = couleurs,
               type = "scatter", mode = "markers",
               marker = list(size = 8, opacity = 0.8)) %>%
  layout(title = "Relation entre le PIB et l'espérance de vie",
         xaxis = list(title = "PIB"),
         yaxis = list(title = "Espérance de vie"))

# Afficher la figure
fig

##############################################################################
#                                                                            #
#                           nb_medailles                                     #
#                                                                            #
##############################################################################
##############################################################################
#                                                                            #
#             nb medailles par rapport a année  pour chaque Pays             #
#                                                                  DANIEL    #
##############################################################################
df_nb_medailles <- df_nb_medailles %>%
  mutate(pays = reorder(pays, nb_medailles_par_pays_annee)) %>%
  arrange(nb_medailles_par_pays_annee)

# Créer la figure plotly avec légende triée
fig <- plot_ly(data = df_nb_medailles, x = ~année, y = ~nb_medailles_par_pays_annee, text = ~paste(pays, année),
               color = ~pays, colors = couleurs,
               type = "scatter", mode = "markers",
               marker = list(size = 8, opacity = 0.8)) %>%
  layout(title = "Relation entre l'année et le nombre de médailles remportées par pays",
         xaxis = list(title = "année"),
         yaxis = list(title = "nombre de médailles"),
         legend = list(traceorder = "reversed"))

# Afficher la figure
fig




# Calculer la corrélation entre le PIB et l'espérance de vie
correlation <- cor(df_joined$année, df_joined$nb_medailles_par_pays_annee)

# Afficher le résultat
print(correlation)

# Calcul le nombre moyen de médailles par année
moyenne_medailles_par_annee <- tapply(df_joined$nb_medailles_par_pays_annee, df_joined$année, mean)

# Afficher le résultat
print(moyenne_medailles_par_annee)

table_medailles <- unique(df_nb_medailles[, c("pays", "année", "nb_medailles_par_pays_annee")])
print(table_medailles)


##############################################################################
#                                                                            #
#          nb medailles par rapport esperance de vie  pour chaque Pays       #
#                                                                     MEHDI  #
##############################################################################
library(plotly)

# Liste de couleurs pour les bulles
couleurs_pays <- couleurs[as.character(df_nb_medailles$pays)]

# Créer la figure plotly
fig <- plot_ly(data = df_nb_medailles, x = ~espérance_vie, y = ~nb_medailles_par_pays_annee, text = ~paste(pays, année),
               color = ~pays, colors = couleurs,
               type = "scatter", mode = "markers",
               marker = list(size = 8, opacity = 0.8)) %>%
  layout(title = "Relation entre l'espérance de vie et le nombre de médailles remportées par pays",
         xaxis = list(title = "esperance de vie" , tickvals = c(65, 70, 75, 80)),
         yaxis = list(title = "nombre de medailles"))

# Afficher la figure
fig

df_joined$espérance_vie <- as.numeric(df_joined$espérance_vie)
# Calculer la corrélation entre le PIB et l'espérance de vie
correlation <- cor(df_joined$espérance_vie, df_joined$nb_medailles_par_pays_annee)

# Afficher le résultat
print(correlation)

##############################################################################
#                                                                            #
# nb medailles par rapport a la depenses pour l'éducation   pour chaque Pays #
#                                                                            #
##############################################################################

# Liste de couleurs pour les bulles
couleurs_pays <- couleurs[as.character(df_nb_medailles$pays)]

# Créer la figure plotly
fig <- plot_ly(data = df_nb_medailles, x = ~dépenses_éducation, y = ~nb_medailles_par_pays_annee, text = ~paste(pays, année),
               color = ~pays, colors = couleurs,
               type = "scatter", mode = "markers",
               marker = list(size = 8, opacity = 0.8)) %>%
  layout(title = "Relation entre les dépenses pour l'éducation et le nombre de médailles remportées par pays",
         xaxis = list(title = "dépenses education" , tickvals = c(7.5, 10, 12.5, 15, 17.5)),
         yaxis = list(title = "nb_medailles"))

# Afficher la figure
fig

df_joined$dépenses_éducation <- as.numeric(df_joined$dépenses_éducation)
# Calculer la corrélation en ignorant les valeurs manquantes
correlation <- cor(df_joined$dépenses_éducation, df_joined$nb_medailles_par_pays_annee, use = "complete.obs")

# Afficher le résultat
print(correlation)


##############################################################################
#                                                                            #
#         nb medailles par rapport a l'accès a l'eau   pour chaque Pays      #  
#                                                           ininteressant    #
##############################################################################

# Liste de couleurs pour les bulles
couleurs_pays <- couleurs[as.character(df_nb_medailles$pays)]

# Créer la figure plotly
fig <- plot_ly(data = df_nb_medailles, x = ~eaux, y = ~nb_medailles_par_pays_annee, text = ~paste(pays, année),
               color = ~pays, colors = couleurs,
               type = "scatter", mode = "markers",
               marker = list(size = 8, opacity = 0.8)) %>%
  layout(title = "Relation entre l'accès à l'eau et le nombre de médailles remportées par pays",
         xaxis = list(title = "eaux"),
         yaxis = list(title = "nb_medailles"))

# Afficher la figure
fig

# Calculer la corrélation en ignorant les valeurs manquantes
correlation <- cor(df_joined$eaux, df_joined$nb_medailles_par_pays_annee, use = "complete.obs")

# Afficher le résultat
print(correlation)


##############################################################################
#                                                                            #
#    nb medailles par rapport a la densité de la pop   pour chaque Pays      #  
#                                                                            #
##############################################################################

# Liste de couleurs pour les bulles
couleurs_pays <- couleurs[as.character(df_nb_medailles$pays)]

# Créer la figure plotly
fig <- plot_ly(data = df_nb_medailles, x = ~densité_de_pop, y = ~nb_medailles_par_pays_annee, text = ~paste(pays, année),
               color = ~pays, colors = couleurs,
               type = "scatter", mode = "markers",
               marker = list(size = 8, opacity = 0.8)) %>%
  layout(title = "Relation entre la densité de la population et le nombre de médailles remportées par pays",
         xaxis = list(title = "densité population", tickvals=c(15,25,35,45,55,100,200,300,400,500)),
         yaxis = list(title = "nb_medailles"))

# Afficher la figure
fig

df_joined$densité_de_pop <- as.numeric(df_joined$densité_de_pop)
# Calculer la corrélation en ignorant les valeurs manquantes
correlation <- cor(df_joined$densité_de_pop, df_joined$nb_medailles_par_pays_annee, use = "complete.obs")

# Afficher le résultat
print(correlation)

##############################################################################
#                                                                            #
#      nb medailles par rapport a l'accès a internet  pour chaque Pays       #  
#                                                          pas interessant   #
##############################################################################

# Liste de couleurs pour les bulles
couleurs_pays <- couleurs[as.character(df_nb_medailles$pays)]

# Créer la figure plotly
fig <- plot_ly(data = df_nb_medailles, x = ~utilisateurs_internet, y = ~nb_medailles_par_pays_annee, text = ~paste(pays, année),
               color = ~pays, colors = couleurs,
               type = "scatter", mode = "markers",
               marker = list(size = 8, opacity = 0.8)) %>%
  layout(title = "Relation entre l'accès à internet et le nombre de médailles remportées par pays",
         xaxis = list(title = "densité population"),
         yaxis = list(title = "nb_medailles"))

# Afficher la figure
fig

# Calculer la corrélation en ignorant les valeurs manquantes
correlation <- cor(df_joined$utilisateurs_internet, df_joined$nb_medailles_par_pays_annee, use = "complete.obs")

# Afficher le résultat
print(correlation)



##########################################################################################
#                                                                                        #
# nb medailles par rapport au depense pour l'administrations publiques  pour chaque Pays #  
#                                                                                        #
##########################################################################################

# Liste de couleurs pour les bulles
couleurs_pays <- couleurs[as.character(df_nb_medailles$pays)]

# Créer la figure plotly
fig <- plot_ly(data = df_nb_medailles, x = ~depenses_administrations_publiques, y = ~nb_medailles_par_pays_annee, text = ~paste(pays, année),
               color = ~pays, colors = couleurs,
               type = "scatter", mode = "markers",
               marker = list(size = 8, opacity = 0.8)) %>%
  layout(title = "Relation entre l'administrations publiques et le nombre de médailles remportées par pays",
         xaxis = list(title = "densité population", tickvals=c(10,12.5,15,17.5,20,22.5,25,27.5,30)),
         yaxis = list(title = "nb_medailles"))

# Afficher la figure
fig

df_joined$depenses_administrations_publiques <- as.numeric(df_joined$depenses_administrations_publiques)
# Calculer la corrélation en ignorant les valeurs manquantes
correlation <- cor(df_joined$depenses_administrations_publiques, df_joined$nb_medailles_par_pays_annee, use = "complete.obs")

# Afficher le résultat
print(correlation)


##########################################################################################
#                                                                                        #
#           nb medailles par rapport a l'indice de Gini  pour chaque Pays                #  
#                                                                                  ENZO  #
##########################################################################################
# Liste de couleurs pour les bulles
couleurs_pays <- couleurs[as.character(df_nb_medailles$pays)]

library(ggplot2)

# Supprimer les observations avec des valeurs manquantes dans Indice_de_gini
df_nb_medailles <- df_nb_medailles[!is.na(df_nb_medailles$Indice_de_gini), ]

# Calculer le nombre moyen de médailles
medailles_mean <- mean(df_nb_medailles$nb_medailles_par_pays_annee)
gini_mean <- mean(df_nb_medailles$Indice_de_gini)

# Créer la figure ggplot avec le bon espacement des barres
fig <- ggplot(data = df_nb_medailles, aes(x = Indice_de_gini, y = nb_medailles_par_pays_annee, fill = pays)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.4)) +
  scale_fill_manual(values = couleurs) +
  labs(title = "Relation entre l'indice de Gini et le nombre de médailles remportées par pays",
       x = "Indice de gini",
       y = "nombre de médailles") +
  theme_bw() +
  geom_vline(xintercept = gini_mean, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = gini_mean, y = max(df_nb_medailles$nb_medailles_par_pays_annee),
           label = "Indice de Gini moyen", vjust = -1, color = "red") +
  geom_hline(yintercept = medailles_mean, color = "blue", linetype = "dashed", size = 1) +
  annotate("text", x = Inf, y = medailles_mean,
           label = "Nombre moyen de médailles", hjust = -0.2, color = "blue") +
  coord_cartesian(xlim = c(0.15, 0.40))

correlation <- cor(df_joined$Indice_de_gini, df_joined$nb_medailles_par_pays_annee, use = "complete.obs")
# Afficher le résultat
print(correlation)
print(gini_mean)
print(medailles_mean)
gini_mean <- mean(df_nb_medailles$Indice_de_gini)
print(gini_mean)

max_indice_gini <- max(df_nb_medailles$Indice_de_gini)
print(max_indice_gini)

# Afficher la figure
fig



##########################################################################################
#                                                                                        #
#                nb medailles par rapport au commerce pour chaque Pays                   #  
#                                                % du PIB               pas interessant  #
##########################################################################################

# Liste de couleurs pour les bulles
couleurs_pays <- couleurs[as.character(df_nb_medailles$pays)]

# Créer la figure plotly
fig <- plot_ly(data = df_nb_medailles, x = ~commerce, y = ~nb_medailles_par_pays_annee, text = ~paste(pays, année),
               color = ~pays, colors = couleurs,
               type = "scatter", mode = "markers",
               marker = list(size = 8, opacity = 0.8)) %>%
  layout(title = "Relation entre le commerce et le nombre de médailles remportées par pays",
         xaxis = list(title = "commerce"),
         yaxis = list(title = "nb_medailles"))

# Afficher la figure
fig

df_joined$commerce <- as.numeric(df_joined$commerce)
# Calculer la corrélation en ignorant les valeurs manquantes
correlation <- cor(df_joined$commerce, df_joined$nb_medailles_par_pays_annee, use = "complete.obs")

# Afficher le résultat
print(correlation)


##### test rapido
df_joined$depenses_militaires <- as.numeric(df_joined$depenses_militaires)
# Calculer la corrélation en ignorant les valeurs manquantes
correlation <- cor(df_joined$depenses_militaires, df_joined$nb_medailles_par_pays_annee, use = "complete.obs")

# Afficher le résultat
print(correlation)




##########################################################################################
#                                                                                        #
#              nb medailles par rapport au pop active pour chaque Pays                   #  
#                                                                               MEHDI    #
##########################################################################################

# Liste de couleurs pour les bulles
couleurs_pays <- couleurs[as.character(df_nb_medailles$pays)]

# Créer la figure plotly
fig <- plot_ly(data = df_nb_medailles, x = ~pop_active, y = ~nb_medailles_par_pays_annee, text = ~paste(pays, année),
               color = ~pays, colors = couleurs,
               type = "scatter", mode = "markers",
               marker = list(size = 8, opacity = 0.8)) %>%
  layout(title = "Relation entre le population active et le nombre de médailles remportées par pays",
         xaxis = list(title = "population active"),
         yaxis = list(title = "nombre de medailles"))

# Afficher la figure
fig

df_joined$commerce <- as.numeric(df_joined$commerce)
# Calculer la corrélation en ignorant les valeurs manquantes
correlation <- cor(df_joined$pop_active, df_joined$nb_medailles_par_pays_annee, use = "complete.obs")

# Afficher le résultat
print(correlation)


##########################################################################################
#                                                                                        #
#                   nb medailles par rapport au PIB pour chaque Pays                     #  
#                                                                          DANIEL        #
##########################################################################################

# Liste de couleurs pour les bulles
couleurs_pays <- couleurs[as.character(df_nb_medailles$pays)]

# Créer la figure plotly
fig <- plot_ly(data = df_nb_medailles, x = ~PIB, y = ~nb_medailles_par_pays_annee,
               color = ~pays, colors = couleurs,
               type = "scatter", mode = "markers",
               marker = list(size = 10, opacity = 0.8)) %>%
  layout(title = "Relation entre le PIB et le nombre de médailles remportées par pays",
         xaxis = list(title = "PIB"),
         yaxis = list(title = "nombre de medailles"))

# Ajouter les points
fig <- fig %>%
  add_markers(x = c(3.26e11, 1.41e12, 2.75e12, 4.68e12),
              y = c(16.35, 16.43, 8.62, 6.73),
              color = I("red"), size = 1,
              hovertext = c("Point 1", "Point 2", "Point 3", "Point 4"),
              hoverinfo = "text",
              showlegend = FALSE)

# Ajouter la droite reliant les points avec la légende renommée
fig <- fig %>%
  add_lines(x = c(3.26e11, 1.41e12, 2.75e12, 4.68e12),
            y = c(16.35, 16.43, 8.62, 6.73),
            color = I("red"), size = 0.5, opacity = 0.6, showlegend = FALSE)

# Afficher la figure
fig


# Calculer la corrélation en ignorant les valeurs manquantes
correlation <- cor(df_joined$PIB, df_joined$nb_medailles_par_pays_annee, use = "complete.obs")

# Afficher le résultat
print(correlation)

library(dplyr)

# Définir les plages de PIB
plages_pib <- c("[0;1T]", "(1T;2T]", "(2T;4T]", "(4T;6T]")

# Créer une nouvelle variable pour les tranches de PIB
df_joined <- df_joined %>%
  mutate(Tranche_PIB = cut(PIB, breaks = c(0, 1e12, 2e12, 4e12, 6e12), labels = plages_pib, right = FALSE))

# Calculer la moyenne des médailles par tranche de PIB
moyennes_medailles <- df_joined %>%
  group_by(Tranche_PIB) %>%
  summarize(Moyenne_Medailles = mean(nb_medailles_par_pays_annee, na.rm = TRUE))

# Calculer la moyenne du PIB par tranche de PIB
moyennes_pib <- df_joined %>%
  group_by(Tranche_PIB) %>%
  summarize(Moyenne_PIB = mean(PIB, na.rm = TRUE))

# Afficher les moyennes des médailles et du PIB par tranche de PIB
moyennes <- merge(moyennes_medailles, moyennes_pib, by = "Tranche_PIB")
moyennes






################################################################################
#
#
#                             médaille ~ Région
#                                                                 FRANCK
################################################################################
# Créer un tableau croisé des variables "médaille" et "région"
table_cross <- table(df_joined$médaille, df_joined$region)

# Effectuer le test du khi-deux
result <- chisq.test(table_cross)

# Afficher les résultats du test
print(result)

# Création du tableau de contingence
table_contingency <- table(df_joined$region, df_joined$médaille)

# Affichage du tableau de contingence
print(table_contingency)

library(ggplot2)
library(dplyr)

# Créer un dataframe avec les données de médaille et région
df_plot <- data.frame(region = rep(c("Americas", "Asia", "Europe", "Oceania"), times = 3),
                      medaille = rep(c("bronze", "silver", "gold"), each = 4),
                      count = c(29, 15, 212, 3, 38, 10, 184, 5, 37, 12, 189, 2))

# Liste des couleurs pour chaque région
couleurs_region <- c("Americas" = "#D8BFD8", "Asia" = "slategrey", "Europe" = "lightskyblue", "Oceania" = "tan2")

# Calculer les proportions de médailles par région
df_plot <- df_plot %>%
  group_by(medaille) %>%
  mutate(prop = count / sum(count) * 100)

# Créer le graphique à barres empilées avec ggplot2
p <- ggplot(df_plot, aes(x = medaille, y = prop, fill = region)) +
  geom_bar(stat = "identity", position = "stack", color = "black", width = 0.7) +
  scale_fill_manual(values = couleurs_region) +
  labs(title = "Répartition des médailles par région",
       x = "Médaille",
       y = "Proportion de médailles (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F0F0F0"),
        plot.background = element_rect(fill = "#FFFFFF"),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))

# Ajouter les étiquettes de pourcentage
p + geom_text(aes(label = paste0(round(prop, 1), "%")), position = position_stack(vjust = 0.5), size = 4)



#######################
# carte
library(ggplot2)
library(maps)
data_pays <- df_joined
data_pays <- subset(data_pays, select=c(année, pays, id_pays, region, PIB, somme_medailles_par_pays, latitude, longitude))
data_pays <- data_pays[order(data_pays$année, decreasing=TRUE),]
data_pays <- data_pays[!duplicated(data_pays$pays), ]
data_pays <- subset(data_pays, pays != "Russian Federation")

world_map <- map_data("world")
world_map <- world_map[order(world_map$region), ]


merged_data <- merge(world_map, data_pays, by.x = "region", by.y = "pays", all.x = TRUE)
merged_data <- merged_data[order(merged_data$order), ]
# Créer la carte avec les polygones


maps1 <- ggplot() +
  geom_polygon(data = merged_data, aes(x = long, y = lat, group = group, fill = somme_medailles_par_pays)) +
  geom_text(data = data_pays, aes(x = longitude, y = latitude, label = pays), size = 3, color = "black") +
  scale_fill_gradient(low = "#CCE5FF", high = "#003366", na.value = "#DDDDDD") +
  labs(title = "Somme de médailles par pays") +
  theme_void()

print(maps1)
