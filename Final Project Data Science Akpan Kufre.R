install.packages('tidyverse')
library(tidyverse)
#install remotes
install.packages("remotes")
library(remotes)

#install data
remotes::install_github("allisonhorst/palmerpenguins")
library(palmerpenguins)
penguins

library(tidyverse)
glimpse(penguins)

#exploring import
unique(penguins$species)
unique(penguins$island)

penguins %>%
  count(species)

#Variable levels
levels(penguins$sex)
levels(penguins$body_mass_g)
levels(penguins$species)
levels(penguins$island)
levels(penguins$flipper_length_mm)
levels(penguins$bill_length_mm)
levels(penguins$bill_depth_mm)

#data that is missing
is.na(penguins)
is.na(penguins$flipper_length_mm)
is.na(penguins$sex)

#Analysis with NA value
penguins %>%
  group_by(island) %>%
  summarise(mean(bill_length_mm))

penguins %>%
  drop_na() %>%
  count(sex ='male', species) %>%
  ggplot() + geom_col(aes(x = species, y = n, fill = species)) +
  geom_label(aes(x = species, y = n, label = n)) +
  scale_fill_manual(values = c("red","blue","grey")) +
  theme_minimal() +
  labs(title = 'Gender')

# Bar graph counts > colorblind palettes 
penguins %>%
  count(species) %>%
  ggplot() + geom_col(aes(x = species, y = n, fill = species)) + 
  geom_label(aes(x = species, y = n, label = n)) +
  scale_fill_manual(values = c("black","light blue","light green")) +
  theme_minimal() +
  labs(title = 'Species Frequency')

library(palmerpenguins)
library(tidyverse)
library(dplyr)
library(ggplot2)


View(penguins)

ggplot(data =penguins, aes(x = flipper_length_mm, y = body_mass_g))+
  geom_point(aes(color = island, shape = species), size = 3, alpha = 0.8) +
  scale_color_manual(values = c("red", "blue","grey"))+
  labs(title = "Sex vs Mass, Palmer Station LTER",
       subtitle = "Gender and body mass for each island",
       x = "n of Sex",
       y = "Body Mass (g)",
       color = "Penguin Island",
       shape = "Penguin Species") +
  theme_minimal()

distribTable <- table(penguins$sex, penguins$species)
barplot(distribTable,
        ylab = 'Frequency of Gender',
        xlab = 'species', 
        col = 8:10, 
        legend = rownames(distribTable),
        main = 'Frequency Table of Penguins each Species')
