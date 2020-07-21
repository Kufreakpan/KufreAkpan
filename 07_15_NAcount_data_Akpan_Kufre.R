#Data
library(remotes)
remotes::install_github("allisonhorst/palmerpenguins")
library(palmerpenguins)
library(tidyverse)

#Variable class
class(penguins$sex)
class(penguins$body_mass_g)

#Variable levels
levels(penguin$sex)

#Missing Data
is.na(penguins)
is.na(penguins$flipper_length_mm)
is.na(penguins$sex)

#Analysis with NA value
penguins %>%
  group_by(island) %>%
  summarise(mean(bill_length_mm))

#NA counts bar graph
penguins %>%
  #group_by(species) %>%
  select(everything()) %>%
  summarise_all(funs(sum(is.na(.)))) %>%
  pivot_longer(cols = 1:7, names_to = 'columns', values_to = 'NA_count') %>%
  arrange(desc(NA_count)) %>%
  ggplot(aes(y = columns, x = NA_count)) + geom_col(fill = '#F0E442') +
  geom_label(aes(label = NA_count)) +
  #   scale_fill_manual(values = c("darkorange","purple","cyan4")) +
  theme_minimal() +
  labs(title = "Palmer Penguins NA Count")

#Summary
summary(penguins)












































#variable class
class(penguins$sex)
class(penguins$body_mass_g)
class(penguins$species)
class(penguins$island)
class(penguins$flipper_length_mm)
class(penguins$bill_length_mm)
class(penguins$bill_depth_mm)


levels(penguins$sex)
levels(penguins$body_mass_g)
levels(penguins$species)
levels(penguins$island)
levels(penguins$flipper_length_mm)
levels(penguins$bill_length_mm)
levels(penguins$bill_depth_mm)

#summary
summary(penguins$sex)
summary(penguins$body_mass_g)
summary(penguins$species)
summary(penguins$island)
summary(penguins$flipper_length_mm)
summary(penguins$bill_length_mm)
summary(penguins$bill_depth_mm)

# Bar graph counts > colorblind palettes
penguins %>%
  count(species) %>%
  ggplot() + geom_col(aes(x = species, y = n, fill = species)) + 
  geom_label(aes(x = species, y = n, label = n)) + 
  scale_fill_manual(values = c("grey","light blue","red")) +
  theme_minimal() + 
  labs (title = 'Frequency of Species')

