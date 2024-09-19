Spotify <- read.csv(file = 'songs_normalize.csv', header=TRUE, sep=",")


Spotify$duration_ms <- Spotify$duration_ms / 1000
boxplot(Spotify$duration_ms ~ Spotify$year, data = Spotify, 
        main = "Comparaison de la durée en fonction de l'année",
        xlab = "année", ylab = "durée (s)",
        col = "lightblue", border = "black", 
        notch = TRUE, notch.frac = 0.5, outline = FALSE)
Spotify <- subset(Spotify, Spotify$year <2020)
Spotify <- subset(Spotify, Spotify$year >1998)
Spotify <- subset(Spotify, Spotify$popularity >=78)
#corrélation
library(dplyr)
library(corrplot)


# Filtrer les variables numériques uniquement
variables_numeriques <- Spotify %>%
  select_if(is.numeric)

# Calculer la matrice de corrélation
matrice_correlation <- cor(variables_numeriques)

corrplot(matrice_correlation, 
         type = "upper", # Montrer seulement la moitié supérieure de la matrice
         tl.col = "black", # Couleur des labels
         tl.srt = 45, # Angle de rotation des labels
         tl.cex = 0.6, # Taille de police des labels
         col = colorRampPalette(c("blue", "white", "red"))(100), # Couleurs de la heatmap
         title = "Heatmap de Corrélation entre les variables", # Titre de la heatmap
)
#annee
variance_par_annee <- Spotify %>%
  group_by(Spotify$year) %>%
  summarise(variance = var(duration_ms))

variance_par_annee$variance[1]=0

plot(variance_par_annee$`Spotify$year`, variance_par_annee$variance, 
     type = "b",
     main = "variance de la durée en fonction\nde l'année",
     xlab = "année", ylab = "variance de la durée",
     col = "blue", lwd = 0.1)
#degré dansant
variance_par_annee <- Spotify %>%
  group_by(Spotify$year) %>%
  summarise(variance = var(danceability))

variance_par_annee$variance[1]=0

plot(variance_par_annee$`Spotify$year`, variance_par_annee$variance, 
     type = "b",
     main = "variance du degré dansant en fonction\nde l'année",
     xlab = "année", ylab = "variance du degré dansant",
     col = "blue", lwd = 0.1)
#energie
variance_par_annee <- Spotify %>%
  group_by(Spotify$year) %>%
  summarise(variance = var(energy))

variance_par_annee$variance[1]=0

plot(variance_par_annee$`Spotify$year`, variance_par_annee$variance, 
     type = "b",
     main = "variance de l'énergie en fonction\nde l'année",
     xlab = "année", ylab = "variance de l'énergie",
     col = "blue", lwd = 0.1)

#volume sonore
variance_par_annee <- Spotify %>%
  group_by(Spotify$year) %>%
  summarise(variance = var(loudness))

variance_par_annee$variance[1]=0

plot(variance_par_annee$`Spotify$year`, variance_par_annee$variance, 
     type = "b",
     main = "variance du volume sonore en fonction\nde l'année",
     xlab = "année", ylab = "variance du volume sonore",
     col = "blue", lwd = 0.1)

#degré de parole
variance_par_annee <- Spotify %>%
  group_by(Spotify$year) %>%
  summarise(variance = var(speechiness))

variance_par_annee$variance[1]=0

plot(variance_par_annee$`Spotify$year`, variance_par_annee$variance, 
     type = "b",
     main = "variance degré de parole en fonction\nde l'année",
     xlab = "année", ylab = "variance du degré de parole",
     col = "blue", lwd = 0.1,ylim=c(0,1))

#acoustique
variance_par_annee <- Spotify %>%
  group_by(Spotify$year) %>%
  summarise(variance = var(acousticness))

variance_par_annee$variance[1]=0

plot(variance_par_annee$`Spotify$year`, variance_par_annee$variance, 
     type = "b",
     main = "variance de l'acoustique en fonction\nde l'année",
     xlab = "année", ylab = "variance de l'acoustique",
     col = "blue", lwd = 0.1)
#degré d'instrumentalité
variance_par_annee <- Spotify %>%
  group_by(Spotify$year) %>%
  summarise(variance = var(instrumentalness))

variance_par_annee$variance[1]=0

plot(variance_par_annee$`Spotify$year`, variance_par_annee$variance, 
     type = "b",
     main = "variance du degré d'instrumentalité en fonction\nde l'année",
     xlab = "année", ylab = "variance du degré d'instrumentalité",
     col = "blue", lwd = 0.1)

#degré de présence
variance_par_annee <- Spotify %>%
  group_by(Spotify$year) %>%
  summarise(variance = var(liveness))

variance_par_annee$variance[1]=0

plot(variance_par_annee$`Spotify$year`, variance_par_annee$variance, 
     type = "b",
     main = "variance du degré de présence en fonction\nde l'année",
     xlab = "année", ylab = "variance du degré de présence",
     col = "blue", lwd = 0.1)

#durée
variance_par_annee <- Spotify %>%
  group_by(Spotify$year) %>%
  summarise(variance = mean(duration_ms))

variance_par_annee$variance[1]=0

plot(variance_par_annee$`Spotify$year`, variance_par_annee$variance, 
     type = "b",
     main = "évolution la moyenne de la durée en fonction\nde l'année",
     xlab = "année", ylab = "moyenne de la durée (s)",
     col = "blue", lwd = 0.1, ylim=c(0,max(variance_par_annee$variance)+50))
#positivité
variance_par_annee <- Spotify %>%
  group_by(Spotify$year) %>%
  summarise(variance = var(valence))

variance_par_annee$variance[1]=0

plot(variance_par_annee$`Spotify$year`, variance_par_annee$variance, 
     type = "b",
     main = "variance du degré de positivité en fonction\nde l'année",
     xlab = "année", ylab = "variance du degré de positivité",
     col = "blue", lwd = 0.1,ylim = c(0,1))
#tempo
variance_par_annee <- Spotify %>%
  group_by(Spotify$year) %>%
  summarise(variance = var(tempo))

variance_par_annee$variance[1]=0

plot(variance_par_annee$`Spotify$year`, variance_par_annee$variance, 
     type = "b",
     main = "variance du tempo en fonction\nde l'année",
     xlab = "année", ylab = "variance du tempo",
     col = "blue", lwd = 0.1)



Spotify <- Spotify %>% arrange(desc(Spotify$popularity))

library(ggplot2)
ggplot(Spotify, aes(y = genre)) +
  geom_bar(stat = "count", fill = "steelblue") +
  labs(title = "Nombre d'apparitions par style de musique", x = "Nombre d'apparitions", y = "Style de musique",cex=0.5) +
  theme_minimal()+theme(axis.text = element_text(size = 8))


library(tidyverse)

# Créer un dataframe pour le top 5 des styles de musique
top5_styles <- df %>%
  group_by(track_genre) %>%
  summarise(count = n()) %>%
  top_n(5, wt = count) %>%
  arrange(desc(count))

top_5_styles$track_genre <- factor(top_5_styles$track_genre, levels = rev(top5_styles$track_genre))

ggplot(Spotify, aes(x = genre)) +
  geom_bar(stat = "count", fill = "lightblue") +
  labs(title = "top 5 des styles de musiques", x = "Style de musique", y = "Nombre d'apparitions") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8)) +  # Changer la taille de police des labels
  scale_x_discrete(limits = top_5_styles)  # Restreindre les labels de l'axe x aux cinq styles de musique les plus fréquents



shapiro.test(Spotify$danceability)


# Calculer la densité pour la "danceability"
densities <- density(Spotify$danceability)

# Créer un dataframe avec les valeurs de "danceability" et les densités calculées
df_danceability_density <- data.frame(danceability = densities$x, density = densities$y)

# Afficher le dataframe
print(df_danceability_density)

plot(df_danceability_density$danceability,df_danceability_density$density, 
     type = "l",
     main = "densité du degré dansant\npour les 20 dernières années",
     xlab = "degré dansant", ylab = "densité",
     col = "blue", lwd = 0.1)
mean(Spotify$danceability)
var(Spotify$danceability)



# Charger la bibliothèque ggplot2
library(ggplot2)

# Agréger les données par année en calculant les moyennes
agg_data <- aggregate(cbind(acousticness, danceability,energy,instrumentalness,liveness,speechiness,valence) ~ year, data = Spotify, FUN = mean)

# Créer le graphique avec les courbes de moyenne de danceability et energy en fonction du temps
ggplot(data = agg_data, aes(x = year)) +
  geom_line(aes(y = acousticness, color = "Acousticness")) +
  geom_line(aes(y = danceability, color = "Danceability")) +
  geom_line(aes(y = energy, color = "Energy")) +
  geom_line(aes(y = instrumentalness, color = "Instrumentalness")) +
  geom_line(aes(y = liveness, color = "Liveness")) +
  geom_line(aes(y = speechiness, color = "Speechiness")) +
  geom_line(aes(y = valence, color = "Valence")) +
  scale_color_manual(values = c("Acousticness" = "red", "Danceability" = "orange","Energy"="darkgreen","Instrumentalness"="lightblue","Liveness"="blue","Speechiness"="purple","Valence"="pink"), 
                     labels = c("Acoustique","Dansant", "Energie", "Instrumentalité","réel","parole","positivité")) +
  labs(title = "Carctéristiques musicales en fonction du temps",
       x = "Temps", y = "Valeur moyenne",color="couleurs") +
  theme_minimal() +
  theme(legend.position = "top")








plot(Spotify$energy ~ Spotify$loudness, data = Spotify,
     type = "n", pch = 19, col = "blue",
     main = "Comparaison de l'énergie  en fonction du volume sonore",
     xlab = "volume sonore", ylab = "énergie")
points(Spotify$loudness, Spotify$energy, pch = 19, col = "blue", cex = 0.1)


plot(Spotify$valence ~ Spotify$danceability, data = Spotify,
     type = "n", pch = 19, col = "blue",
     main = "Comparaison de la positivité  en fonction du degré dansant",
     xlab = "degré dansant", ylab = "positivité")
points(Spotify$danceability, Spotify$valence, pch = 19, col = "blue", cex = 0.4)


plot(Spotify$energy ~ Spotify$acousticness, data = Spotify,
     type = "n", pch = 19, col = "blue",
     main = "Comparaison de la énergie  en fonction de l'acoustique",
     xlab = "acoustique", ylab = "énergie")
points(Spotify$acousticness, Spotify$energy, pch = 19, col = "blue", cex = 0.4)






# Calculer la densité pour la "durée"
densities <- density(Spotify$duration_ms)

# Créer un dataframe avec les valeurs de "danceability" et les densités calculées
df_duration_density <- data.frame(duration_ms = densities$x, density = densities$y)

# Afficher le dataframe
print(df_danceability_density)

plot(df_duration_density$duration_ms,df_duration_density$density, 
     type = "l",
     main = "densité des durées des musiques à forte popularité\nau cours des 20 dernières années",
     xlab = "durée (s)", ylab = "densité",
     col = "blue", lwd = 0.1)
mean(Spotify$duration_ms)
var(Spotify$duration_ms)
Spotify <- subset(Spotify, Spotify$popularity >= 70)


library(ggplot2)
ggplot(Spotify, aes(y = genre)) +
  geom_col(data= Spotify, FUN = "count", fill = "steelblue") +
  labs(title = "Nombre d'apparitions par style de musique", x = "Nombre d'apparitions", y = "Style de musique",cex=0.5) +
  theme_minimal()+theme(axis.text = element_text(size = 8))


# Compter le nombre d'occurrences de chaque genre
genre_compte <- table(Spotify$genre)

# Convertir le résultat en un data frame
genre_df <- data.frame(genre = names(genre_compte), count = as.numeric(genre_compte))

# Trier les genres par ordre décroissant de leur proportion
genre_df <- genre_df[order(-genre_df$count), ]

# Créer le graphique circulaire avec la proportion des genres
ggplot(data = genre_df, aes(x = "", y = count, fill = genre)) +
  geom_col(width = 1, color="black",show.legend = TRUE) +
  coord_polar(theta = "y") +
  ggtitle("Proportion des genres de musique") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) +
  labs(x = NULL, y = NULL)
  

# Charger les bibliothèques nécessaires
library(dplyr)

# Compter le nombre d'occurrences de chaque genre
genre_compte <- table(Spotify$genre)

# Convertir le résultat en un data frame
genre_df <- data.frame(genre = names(genre_compte), count = as.numeric(genre_compte), stringsAsFactors = FALSE)

# Calculer le pourcentage pour chaque genre
genre_df <- genre_df %>%
  mutate(percentage = round(count / sum(count) * 100, 1))

# Trier les genres par ordre décroissant de leur proportion
genre_df <- genre_df[order(-genre_df$count), ]

genre_df <- subset(genre_df, genre_df$percentage >= 1)


# Créer le graphique avec ggplot2
ggplot(data = genre_df, aes(x = count, y = reorder(genre, -count), fill = genre)) +
  geom_bar(stat = "identity", color = "black",show.legend = FALSE) +
  labs(title = "Nombre d'apparitions par style de musique", y = "Style de musique", x = "Nombre d'apparitions") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8)) +
  geom_text(aes(label = paste0(percentage, "%")), 
            position = position_dodge(width = 3), 
            vjust = -0.5,
            size = 4)

# Agréger les données par année en calculant le nombre total de chansons explicites
agg_data <- aggregate(explicit ~ year, data = Spotify, FUN = function(x) sum(x == "True"))



# Créer le graphique avec ggplot2
ggplot(data = agg_data, aes(x = year, y = explicit)) +
  geom_line(color = "steelblue") +
  labs(title = "Évolution du nombre de chansons explicites", x = "Année", y = "Nombre de chansons explicites") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))


agg_data <- aggregate(popularity ~ year, data = Spotify, FUN = mean)

# Créer le graphique avec ggplot2
ggplot(data = agg_data, aes(x = year, y = popularity)) +
  geom_line(color = "steelblue") +
  labs(title = "Évolution de la moyenne de popularité", x = "Année", y = "Moyenne de popularité") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))+
  coord_cartesian(ylim = c(0, 100))

#moyenne de l'énergie en fonction de l'année
agg_data <- aggregate(energy ~ year, data = Spotify, FUN = mean)

ggplot(data = agg_data, aes(x = year, y = energy)) +
  geom_line(color = "steelblue") +
  labs(title = "Évolution de la moyenne d'énergie", x = "Année", y = "Moyenne d'énergie") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))+
  coord_cartesian(ylim = c(0, 1))

#moyenne du degré dansant en fonction de l'année
agg_data <- aggregate(danceability ~ year, data = Spotify, FUN = mean)

ggplot(data = agg_data, aes(x = year, y = danceability)) +
  geom_line(color = "steelblue") +
  labs(title = "Évolution de la moyenne de degré dansant", x = "Année", y = "Moyenne de degré dansant") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))+
  coord_cartesian(ylim = c(0, 1))

#Comptage des modes

ggplot(Spotify, aes(x = factor(mode), fill = factor(mode))) +
  geom_bar(stat = "count") +
  scale_x_discrete(labels = c("0" = "Mineur", "1" = "Majeur")) +
  labs(title = "Nombre de chansons par mode", x = "Mode", y = "Nombre de chansons") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14)) +
  scale_fill_discrete(labels = c("0" = "Mineur", "1" = "Majeur"))
