library(ggplot2)
library(dplyr)
library(reshape2)

# Beispel Spotify Top 100 aus 2017
top100<-read.csv("featuresdf.csv")

# Nummerierung einf??gen
top100$platz<-1:100

# Visualisierung einzelner Faktoren ####

#Platzierung und Danceability
plot(top100$platz, top100$danceability, pch = 16)
plot(top100$platz, top100$energy)
plot(top100$platz, top100$key)
plot(top100$platz, top100$loudness)
plot(top100$platz, top100$mode)
plot(top100$platz, top100$speechiness)
plot(top100$platz, top100$acousticness)
plot(top100$platz, top100$instrumentalness)
plot(top100$platz, top100$liveness)
plot(top100$platz, top100$valence)
plot(top100$platz, top100$tempo)
plot(top100$platz, top100$duration_ms)
plot(top100$platz, top100$time_signature)

# Ziel: Herausfinden wie stark einzelne Faktoren auf die Platzierung des Songs Einfluss haben ####

Regression_top100<-lm(data = top100, platz~key+valence)
summary(Regression_top100)

# Ergebnis:
# 1.      Key hat meisten Einfluss auf die Platzierung
# 2.      Valence (die Fröhlichkeit des Tracks) hat auch Einfluss auf die Platzierung


# 1. Key
Regression_top100_key<-lm(top100$key~top100$platz)
summary(Regression_top100_key)

# Visualierung
plot(top100$platz, jitter(top100$key), pch = 16)
abline(Regression_top100_key)

ggplot(data = top100, mapping = aes(x = platz, y = key)) +
  geom_jitter() +
  geom_smooth(method = lm)


# 2. Valence
Regression_top100_valence<-lm(top100$valence~top100$platz)
summary(Regression_top100_valence)

plot(top100$platz, top100$valence, pch = 16)
abline(Regression_top100_valence)

ggplot(data = top100, mapping = aes(x = platz, y = valence)) +
  geom_jitter() +
  geom_smooth(method = lm)


# Idee: Wird ein bestimmer Key mit mehr Fr??hlichkeit verbunden?    ####

Regression_top100_keyXvalence<-lm(top100$valence~top100$key+I(top100$key^2)+I(top100$key^3)+I(top100$key^4))
summary(Regression_top100_keyXvalence)
abline(Regression_top100_keyXvalence)
# -> Keine Regression

# Means der einzelnen Keys
mean(top100$valence[top100$key == "1"])     # 0.556
mean(top100$valence[top100$key == "2"])     # 0.516
mean(top100$valence[top100$key == "3"])     # 0.777
mean(top100$valence[top100$key == "4"])     # 0.438
mean(top100$valence[top100$key == "5"])     # 0.483
mean(top100$valence[top100$key == "6"])     # 0.469
mean(top100$valence[top100$key == "7"])     # 0.583
mean(top100$valence[top100$key == "8"])     # 0.513
mean(top100$valence[top100$key == "9"])     # 0.557
mean(top100$valence[top100$key == "10"])    # 0.6252
mean(top100$valence[top100$key == "11"])    # 0.46

top100_meanValence<-c(mean(top100$valence[top100$key == "0"]), mean(top100$valence[top100$key == "1"]),
                      mean(top100$valence[top100$key == "2"]),mean(top100$valence[top100$key == "3"]),
                      mean(top100$valence[top100$key == "4"]),mean(top100$valence[top100$key == "5"]),
                      mean(top100$valence[top100$key == "6"]),mean(top100$valence[top100$key == "7"]),
                      mean(top100$valence[top100$key == "8"]),mean(top100$valence[top100$key == "9"]),
                      mean(top100$valence[top100$key == "10"]),mean(top100$valence[top100$key == "11"]))

plot(top100$key, top100$valence, pch = 16)
points(0:11, top100_meanValence, pch = 16, col = "red", type = "line")

ggplot() +
  geom_line(mapping = aes(x= 0:11, y = top100_meanValence), col = "red") +
  geom_point(mapping = aes(x = top100$key, y = top100$valence)) +
  theme_get()

# Ergebnis: Nein, die Valence ist nicht durch die Tonart bedingt


# Idee: Heatmap aller Faktoren untereinander um zu sehen was sich bestimmt ####

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}


top100_corMatrix <- get_lower_tri(cor(top100[,4:17]))
top100_corMatrix_melt <- melt(top100_corMatrix, na.rm = T)

ggplot(data = top100_corMatrix_melt, mapping = aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", high = "green", mid = "white", midpoint = 0, 
                       limit = c(-1,1), space = "Lab") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  coord_fixed()


# Ergebnis: 
# Die gr????ten Signifikanzen findet man bei:
#   Energy x Loudness        ++       check
#   Speechness x Loudness    --       check
#   Valence x Danceability   +        check
#   Tempo x Valence          -        check
#   Tempo x Danceability     -        check
#   Valence x Loudness       +        check

# Energy 
Regression_top100_energy<-lm(data = top100, energy~acousticness + loudness)
summary(Regression_top100_energy)

ggplot(mapping = aes(x = top100$loudness, y = top100$energy)) +
  geom_point() +
  #geom_abline(intercept = 0.980971, slope = 0.053030, col = "red") +
  geom_smooth(method = lm, se = F, col = "red") +
  theme_get()

ggplot(mapping = aes(x = top100$acousticness, y = top100$energy)) +
  geom_point() +
  #geom_abline(intercept = 0.69583, slope = -0.21128, col = "red") +
  geom_smooth(method = lm, se = F, col = "red")
  theme_get()


# Loudness
Regression_top100_loudness<-lm(data = top100, loudness~speechiness)
summary(Regression_top100_loudness)

ggplot() +
  geom_point(mapping = aes(x = top100$valence, y = top100$loudness)) +
  geom_abline(intercept = -6.4208, slope = 3.0205, col = "red") +
  theme_get()
  
ggplot() +
  geom_point(mapping = aes(x = top100$speechiness, y = top100$loudness)) +
  geom_abline(intercept = -4.7793, slope = -8.4, col = "red") +
  theme_get()
  

# Danceability
Regression_top100_dance<-lm(data = top100, danceability~tempo+valence)
summary(Regression_top100_dance)

ggplot(mapping = aes(x = top100$valence, y = top100$danceability)) +
  geom_point() +
  #geom_abline(intercept = 0.7337221, slope = 0.2002868, col = "red") +
  geom_smooth(method = lm) +
  theme_get()

ggplot(mapping = aes(x = top100$tempo, y = top100$danceability)) +
  geom_point() +
  #geom_abline(intercept = 0.7337221, slope = -0.0011783, col = "red") +
  geom_smooth(method = lm) +
  theme_get()

# valence
Regression_top100_valence<-lm(data = top100, valence~tempo)
summary(Regression_top100_valence)

ggplot(mapping = aes(x = top100$tempo, y = top100$valence)) +
  geom_point() +
  #geom_abline(intercept = 0.7864430, slope = -0.0022600, col = "red") +
  geom_smooth(method = lm, col="red") +
  theme_get()


# Idee: H??ngen die Tonart und die Fr??hlichkeit zusammen? ####

mean(top100$valence[top100$mode == "1"])
mean(top100$valence[top100$mode == "0"])

plot(top100$mode, top100$valence)
points(0:1, c(0.5127966, 0.5229214), col = "red", pch = 16)

# Nicht wirklich, eine Dur Tonart ist nur minimal "fr??hlicher" als Moll




# Idee: Wie ist das Tempo der Top 100 verteilt? Welches ist das beste zum Tanzen? Welches hat die meiste Energy?  ####
plot(top100$tempo, top100$valence)
plot(top100$tempo, top100$danceability)
plot(top100$tempo, top100$acousticness)

mean(top100$tempo)
hist(top100$tempo, nclass = 20)

Regression_top100_tempo<-lm(data = top100, tempo~danceability+I(danceability^2))
summary(Regression_top100_tempo)

ggplot(mapping = aes(x = top100$tempo, y = top100$danceability)) +
  geom_point() +
  geom_smooth() +
  theme_get()

ggplot(mapping = aes(x = top100$tempo, y = top100$valence)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_get()

ggplot(mapping = aes(x = top100$tempo, y = top100$acousticness)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_get()

# Ergebnis: 
# Das Tempo hat gro??en Einfluss auf die Tanzbarkeit, Fr??hlichkeit und Acousticness des Songs


# GGplot2 ??ben ####

ggplot(data = top100) +
  geom_point(mapping = aes(x = platz, y = valence, color = artists))






# Endergebnis:       ##### 
# Der Platz in den Top 100 ist nur durch zwei Faktoren signifkikant bedingt:
# 1.  Die Tonart
# 2.  Die wahrgenommene Fr??hlichkeit
#
# Demnach gilt, je näher die Tonart an C dran ist, desto besser ist der Song in den Top 100 platziert
# (H wäre hier am weitesten entfernt, obwohl es nur einen Ton unter C liegt)
#
# Je fr??hlicher ein Song wahrgenommen wird (durch z.B. den Text) desto besser ist er in den Top 100 platziert
#
#
# Zudem bedingen sich Teile der Faktoren gegenseitig. Logische Zusammenh??nge zeigen sich z.B. bei der 
# starken Korelation aus Loudness und Energy (je lauter ein Song ist, desto mehr Energie hat er) oder 
# je mehr Sprache (nicht Gesang oder Rap) ein Song hat desto leiser ist er






