# Libraries
library(tidyverse)
library(reshape2)
library(extrafont)
font_import() # Used for Roboto

# ------------------------- #
# Spotify Top 100 Songs 2017 Analysis
# ------------------------- #

# 1. Loading the Data #### 
top100 <- read.csv("featuresdf.csv") %>% 
  mutate(position = 1:100, # Adding the Rank of the Song
         duration = duration_ms / 1000 / 60) %>% 
  select(-duration_ms)

tibble(top100)


# 2. Exploratory Visualization #####

# Plotting each Variable against the Position inside the Top 100
top100_explore <- top100 %>% 
  select(-id) %>% 
  pivot_longer(cols = c(3:14, 16), names_to = "variable", values_to = "value") %>% 
  mutate(value = round(value, 4))

# Visual to see if there are strong correlation between position and a singel Variable
p <- ggplot(top100_explore, aes(x = position, y = value)) +
  geom_point(alpha = 0.6, ) +
  geom_smooth(color = "#0096c7") +
  facet_wrap(vars(variable), scales = "free_y", ) +
  
  theme_minimal() +
  theme(text=element_text(family="Roboto Medium", size = 18),
        panel.spacing = unit(8, "mm")) +
  labs(title = "Expl. Visualization of Top 100 Variables", subtitle = "Note: Different Y Scale for each Plot", 
       x = "Position of the Song", y = "Value of the Variable")

ggsave(plot = p, "Plots/expl_allVariable_roboto.jpeg", width = 16, height = 10)

# 3. Linear Regression ####

# Using all Factors
Regression_top100 <- lm(data = top100, position ~ 
                          acousticness + key + valence + danceability + duration + energy +
                          instrumentalness + liveness + loudness + mode + speechiness + tempo + 
                          time_signature)
summary(Regression_top100)

# Only the most important
Regression_top100 <- lm(data = top100, position ~ 
                          key + valence)
summary(Regression_top100)

# 1.  Key is the most significant Factor, with a p-value of 0.0271 however not significant enough
# 2.  Valence of a track is the other close to significant factor, having a much bigger positive impact on the position (Estimate -24.1)

# _ 3.1. Key ####
Regression_top100_key<-lm(top100$key~top100$position)
summary(Regression_top100_key)

plot(top100$position, jitter(top100$key), pch = 16)
abline(Regression_top100_key)

# Or
ggplot(data = top100, mapping = aes(x = position, y = key)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = lm, se = FALSE, color = "#0096c7") + 
  
  theme_minimal() +
  theme(text=element_text(family="Roboto Medium")) +
  scale_y_continuous(limits = c(0,11), breaks = seq(0, 11, 1)) +
  labs(title = "Lin. Regr. between Key and Top 100 Position", subtitle = "1 = C, 2 = D, ..., 7 = B",
       y = "Key Value", x = "Position", caption = "Regression is not significant, but the most sign. in the Dataset (p-value 0.026)")

ggsave("Plots/lm_key.jpeg", width = 6, height = 4)

# _ 3.2. Valence ####
Regression_top100_valence<-lm(top100$valence~top100$position)
summary(Regression_top100_valence)

plot(top100$platz, top100$valence, pch = 16)
abline(Regression_top100_valence)

# Or
ggplot(data = top100, mapping = aes(x = position, y = valence)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = lm, se = FALSE, color = "#0096c7") + 
  
  theme_minimal() +
  scale_y_continuous(limits = c(0,1)) +
  theme(text=element_text(family="Roboto Medium")) +
  labs(title = "Lin. Regr. between Valence and Top 100 Position", subtitle = "1 = C, 2 = D, ..., 7 = B",
       y = "Valence Value", x = "Position", caption = "Regression is not significant, but has the biggest impact and is slightly sign. (p-value 0.0656)")

ggsave("Plots/lm_valence.jpeg", width = 6, height = 4)



# 4. Hypothesis: Is a certain Key happier than others? ####

# _ 4.1 Key and Valence ####
# Trying a Linear Regression
Regression_top100_keyXvalence<-lm(top100$valence~top100$key + top100$mode)
summary(Regression_top100_keyXvalence)
# -> Not significant

# Mean of a certain key
top100_meanKey <- top100 %>% 
  group_by(key) %>% 
  summarize(mean = mean(valence),
            sd = sd(valence),
            n = n())

ggplot(data = top100, mapping = aes(x = key, y = valence)) +
  geom_point(alpha = 0.8) +
  geom_line(data = top100_meanKey, aes(x = key, y = mean),color = "#0096c7") +
  theme_minimal() +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(limits = c(0,11), breaks = seq(0, 11, 1)) +
    
  theme(text=element_text(family="Roboto Medium")) +
  labs(title = "All Keys can be happy", subtitle = "Key and Mean Valence Value",
       x = "Key", y = "Valence", caption = "No sign. Correlation found")

ggsave("Plots/valence_key.jpeg", width = 6, height = 4)

# No viewable correlation between Key and Valence

# _ 4.2 Adding the mode ####

top100_meanKey <- top100 %>% 
  group_by(key, mode) %>% 
  summarize(mean = round(mean(valence), 2),
            sd = sd(valence),
            n = n())


ggplot(top100_meanKey, aes(x = key, y = mode)) +
  geom_tile(aes(fill = mean)) +
  geom_text(aes(label = mean)) +
  
  theme_minimal() +
  scale_y_continuous(limits = c(-0.5,1.5), breaks = c(0,1)) +
  scale_x_continuous(limits = c(-0.5,11.5), breaks = seq(0, 11, 1)) +
  
  theme(text=element_text(family="Roboto Medium"), 
        legend.position = "none") +
  labs(title = "Can Mode change the View?", subtitle = "Key+Mode and Mean Valence Value",
       x = "Key", y = "Mode", caption = "No sign. Correlation found")

ggsave("Plots/valence_keymode.jpeg", width = 6, height = 4)

# 5. Hypothesis: Certain Variables are strong predictors for others ####

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

top100_corMatrix <- get_lower_tri(cor(top100[,4:17]))
top100_corMatrix_melt <- melt(top100_corMatrix, na.rm = T)

ggplot(data = top100_corMatrix_melt, mapping = aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 2.3) +
  scale_fill_gradient2(low = "red", high = "green", mid = "white", midpoint = 0, 
                       limit = c(-1,1), space = "Lab") +
  
  coord_fixed() +
  
  theme_minimal() +
  theme(text=element_text(family="Roboto Medium"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Cor. Matrix between all Variables", caption = "Hidden upper Trinangle due to duplicate Information",
       x = NULL, y = NULL)
  
ggsave("Plots/cor_matrix Variables.jpeg", width = 8, height = 6)

# Ergebnis: 
# Die gr????ten Signifikanzen findet man bei:
#   Energy x Loudness        ++ 
#   Speechness x Loudness    -- 
#   Valence x Danceability   +  
#   Tempo x Valence          -  
#   Tempo x Danceability     -
#   Valence x Loudness       +      

# 6. What is the best BPM to dance to? ####

ggplot(data = top100, aes(x = tempo, y = danceability)) +
  geom_point() +
  geom_smooth(se = F, color = "#0096c7") +
  
  scale_y_continuous(limits = c(0,1)) +
  theme_minimal() +
  theme(text=element_text(family="Roboto Medium"), 
        legend.position = "none") +
  labs(title = "Around 120BPM has the best danceability", subtitle = "BPM/Tempo to Danceability",
       x = "BPM", y = "Danceability")

ggsave("Plots/bpm_dance.jpeg", width = 6, height = 4)
# A BPM of around 120 has the best Danceability
