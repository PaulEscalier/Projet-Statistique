Spotify <- read.csv(file = 'dataset.csv', header=TRUE, sep=",")

#épuration des données
Spotify <- subset(Spotify, Spotify$popularity >= 70)
mon_tableau <- subset(Spotify, select = Spotify$track_id,drop=drop)


#corrélation

# Calculer la corrélation entre deux variables
correlation <- cor(Spotify$acousticness, Spotify$popularity)

# Afficher la corrélation
print(correlation)
plot(Spotify$loudness ~ (Spotify$energy), data = Spotify,
     type = "n", pch = 19, col = "blue",
     main = "Comparaison du volume sonore en fonction\nde l'énergie",
     xlab = "energie", ylab = "volume sonore")
points(Spotify$loudness, Spotify$energy, pch = 19, col = "blue", cex = 0.5)
correlation<- cor(Spotify$explicit,Spotify$speechiness)

#explicit
bwplot(Spotify$popularity~Spotify$explicit,data=Spotify,col=c("green","red"),main = "Répartition des notes en fonction des paroles (explicit ou non)",
       xlab = "Explicit", ylab = "Popularité",key=list(text=list(c("True","False")),points=list(pch=c(15,17),col=c("green","red"))))


#durée
Spotify$duration_ms <- Spotify$duration_ms / 1000
plot(Spotify$popularity ~ (Spotify$duration_ms), data = Spotify,
     type = "n", pch = 19, col = "blue",
     main = "Comparaison de la popularité en fonction de la durée",
     xlab = "Durée (s)", ylab = "popularité")
points(Spotify$duration_ms, Spotify$popularity, pch = 19, col = "blue", cex = 0.5)
axis(1, at = seq(0, max(Spotify$duration_ms), by = 500))

#Degré dansant
plot(Spotify$popularity ~ (Spotify$danceability), data = Spotify,
     type = "n", pch = 19, col = "blue",cex=0.01,
     main = "Comparaison de la popularité en fonction du degré dansant",
     xlab = "degré dansant", ylab = "popularité")
points(Spotify$danceability, Spotify$popularity, pch = 19, col = "blue", cex = 0.5)

library(ggplot2)
ggplot(data = Spotify, aes(x =Spotify$danceability  , y = Spotify$popularity)) +
  geom_density_2d() +
  labs(x = "Popularite", y = "Degre Dansant",
       title = "Contour Plot de Popularite en fonction du Degre Dansant")


#Energy
plot(Spotify$popularity ~ (Spotify$danceability), data = Spotify,
     type = "n", pch = 19, col = "blue",cex=0.01,
     main = "Comparaison de la popularité en fonction de l'énergie",
     xlab = "énergie", ylab = "popularité")
points(Spotify$energy, Spotify$popularity, pch = 19, col = "blue", cex = 0.1)


plot(Spotify$popularity ~ Spotify$energy, data = Spotify, 
     main = "Moyenne de popularité en fonction de l'énergie",
     xlab = "Énergie", ylab = "Moyenne de popularité",type="n",
     pch = 19, col = "blue")

# Calculer la moyenne de popularité par groupe d'énergie
moyenne_popularite <- tapply(Spotify$popularity, Spotify$energy, mean)

# Ajouter une ligne représentant la moyenne de popularité pour chaque groupe d'énergie
abline(h = moyenne_popularite, col = "red", lty = 2)


#Tonalité
plot(Spotify$popularity ~ Spotify$key, data = Spotify,
     type = "n", pch = 19, col = "blue",cex=0.01,
     main = "Comparaison de la popularité en fonction de la tonalité",
     xlab = "tonalité", ylab = "popularité")
boxplot(Spotify$popularity ~ Spotify$key, data = Spotify, 
        main = "Comparaison de la popularité en fonction de la tonalité",
        xlab = "Tonalité", ylab = "Popularité",
        col = "lightblue", border = "black", 
        notch = TRUE, notch.frac = 0.5, outline = FALSE)
#Volume Sonore
plot(Spotify$popularity ~ Spotify$loudness, data = Spotify,
     type = "n", pch = 19, col = "blue",
     main = "Comparaison de la popularité en fonction du volume sonore",
     xlab = "popularité", ylab = "volume sonore", xlim = c(-50, 10))
points(Spotify$loudness, Spotify$popularity, pch = 19, col = "blue", cex = 0.1)
mean(Spotify$loudness)

#Mode

boxplot(Spotify$popularity ~ Spotify$mode, data = Spotify, 
        main = "Comparaison de la popularité en fonction \n de si le morceau est à la mode",
        xlab = "à la mode (0 non, 1 oui)", ylab = "Popularité",
        col = "lightblue", border = "black", 
        notch = TRUE, notch.frac = 0.5, outline = FALSE)
#degré de parole
plot(Spotify$popularity ~ Spotify$speechiness, data = Spotify,
     type = "n", pch = 19, col = "blue",
     main = "Comparaison de la popularité en fonction\ndu degré de parole",
     xlab = "degré de parole", ylab = "popularité")
points(Spotify$speechiness, Spotify$popularity, pch = 19, col = "blue", cex = 0.1)
scale_x_continuous(expand = c(0.1, 0))
par(mar = c(5, 4, 4, 6))

#degré d'acoustique

# Calculer la moyenne de popularité par degré d'acoustique
moyenne_popularite <- aggregate(Spotify$popularity, by = list(Spotify$acousticness), FUN = mean)

# Tracer la courbe de la moyenne de popularité en fonction de l'acoustique
plot(Spotify$acousticness, Spotify$popularity, 
     type = "n",
     main = "popularité en fonction\ndu degré d'acoustique",
     xlab = "Degré d'acoustique", ylab = "popularité",
     col = "blue", lwd = 0.1)
points(Spotify$acousticness,Spotify$popularity, pch = 19, col = "blue", cex = 0.1)

#degré d'instrumentalité

# Calculer la moyenne de popularité par degré d'instrumentalité
moyenne_popularite <- aggregate(Spotify$popularity, by = list(Spotify$instrumentalness), FUN = mean)

# Tracer la courbe de la moyenne de popularité en fonction de l'instrumentalité
plot(moyenne_popularite$Group.1, moyenne_popularite$x, 
     type = "n",
     main = "Moyenne de popularité en fonction\ndu degré d'instrumentalité",
     xlab = "Degré d'instrumentalité", ylab = "Moyenne de popularité",
     col = "blue", lwd = 0.1, ylim=c(0,100))
points(moyenne_popularite$Group.1, moyenne_popularite$x, pch = 19, col = "blue", cex = 0.1)


#degré de présence en direct

# Calculer la moyenne de popularité par degré de présence en direct
moyenne_popularite <- aggregate(Spotify$popularity, by = list(Spotify$liveness), FUN = mean)

# Tracer la courbe de la moyenne de popularité en fonction de la présence en direct
plot(moyenne_popularite$Group.1, moyenne_popularite$x, 
     type = "n",
     main = "Moyenne de popularité en fonction\ndu degré de présence en direct",
     xlab = "degré de présence en direct", ylab = "Moyenne de popularité",
     col = "blue", lwd = 0.1, ylim=c(0,100))
points(moyenne_popularite$Group.1, moyenne_popularite$x, pch = 19, col = "blue", cex = 0.1)


#degré de positivité

# Calculer la moyenne de popularité par degré de positivité
moyenne_popularite <- aggregate(Spotify$popularity, by = list(Spotify$valence), FUN = mean)

# Tracer la courbe de la moyenne de popularité en fonction de la positivité
plot(moyenne_popularite$Group.1, moyenne_popularite$x, 
     type = "n",
     main = "Moyenne de popularité en fonction\ndu degré de positivité",
     xlab = "degré de positivité", ylab = "Moyenne de popularité",
     col = "blue", lwd = 0.1, ylim=c(0,100))
points(moyenne_popularite$Group.1, moyenne_popularite$x, pch = 19, col = "blue", cex = 0.1)


#Tempo

# Calculer la moyenne de popularité en fonction du tempo
moyenne_popularite <- aggregate(Spotify$popularity, by = list(Spotify$tempo), FUN = mean)

# Tracer la courbe de la moyenne de popularité en fonction du tempo
plot(moyenne_popularite$Group.1, moyenne_popularite$x, 
     type = "n",
     main = "Moyenne de popularité en fonction\ndu tempo",
     xlab = "tempo", ylab = "Moyenne de popularité",
     col = "blue", lwd = 0.1, ylim=c(0,100))
points(moyenne_popularite$Group.1, moyenne_popularite$x, pch = 19, col = "blue", cex = 0.1)

#signature rythmique
boxplot(Spotify$popularity ~ Spotify$time_signature, data = Spotify, 
        main = "Comparaison de la popularité en fonction \n de la signature rythmique",
        xlab = "signature rythmique", ylab = "Popularité",
        col = "lightblue", border = "black", 
        notch = TRUE, notch.frac = 0.5, outline = FALSE)
#Genre
boxplot(Spotify$popularity ~ Spotify$track_genre, data = Spotify, 
        main = "Comparaison de la popularité en fonction \n du genre",
        xlab = "genre", ylab = "Popularité",
        col = "lightblue", border = "black", 
        notch = TRUE, notch.frac = 0.5, outline = FALSE)
elements_distincts <- unique(Spotify$track_genre)
print(elements_distincts)

#Fréquence des éléments
#explicit
freq_explicit <- table(Spotify$explicit)
print(freq_explicit)
pie(freq_explicit, labels = c("Non explicites", "explicites"), col = c("red", "green"),
    main = "Paroles explicites dans les musique\nà forte popularité (>=70)", cex.main = 1.5)


#time signature
freq_time_signature <-table(Spotify$time_signature)
print(freq_time_signature)
pie(freq_time_signature, labels = c("0","1","3","4","5"), col = c("white", "lightgrey","grey","black"),
    main = "Signatures rythmiques dans les musique\nà forte popularité (>=70)", cex.main = 1.5,cex=0.5)
#Durée
boxplot(Spotify$duration_ms, main = "Répartition de la durée dans\nles titres les plus populaires", ylab = "Durée",col = "grey", border = "black")
#dansabilité
boxplot(Spotify$danceability, main = "Répartition du degré dansant dans\nles titres les plus populaires", ylab = "Degré dansant",col = "lightblue", border = "black")




moyenne_popularite <- aggregate(Spotify$popularity, by = list(Spotify$track_genre), FUN = mean)

# Renommer les colonnes du résultat
colnames(moyenne_popularite) <- c("genre", "moyenne popularité")

# Afficher le résultat
print(moyenne_popularite)


#corrélation
library(dplyr)
library(corrplot)

# Charger le tableau de données dans une variable (exemple)

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
