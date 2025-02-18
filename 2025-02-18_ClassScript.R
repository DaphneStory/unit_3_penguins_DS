install.packages("tidyverse")

#DPS
#2.18.25

library(tidyverse)
tidyverse_packages()


install.packages("palmerpenguins")
library(palmerpenguins)

head(penguins)
summary(penguins)

glimpse(penguins)

#Using dplyr

gentoo = filter(penguins, species=="Gentoo")
summary(gentoo)

gentoo_ladies = filter(penguins, species=="Gentoo", sex=="female")
summary(gentoo_ladies)

gentoo_ladies = filter(gentoo, sex=="female")


##pipe tool

###  %>%

gentoo = penguins %>% 
  filter(species == "Gentoo") %>%
  filter(sex=="female")
summary(gentoo)


female_mass = penguins %>%
  filter(sex=="female") %>%
  summarize(mean_mass_g = mean(body_mass_g), mean_bill_depth_mm=mean(bill_depth_mm))
female_mass


##in base r
female_mass = mean(penguins$body_mass_g[which(penguins$sex=="female")])
female_mass

species_mean_mass_g = penguins %>%
  filter(!is.na(sex)) %>%
  group_by(species, sex, island) %>%
  summarize(mean_mass_g = mean(body_mass_g, na.rm=TRUE), count=n()) %>%
  print()

write.csv(file="data/processed/mass_table_with_a_dot.csv", x=species_mean_mass_g)
write_csv(file="data/processed/mass_table_with_an_underscore.csv", x=species_mean_mass_g)


temp = read_csv("data/processed/mass_table_with_an_underscore.csv")


penguins_for_america = penguins %>%
  mutate(body_mass_lb = body_mass_g*0.0022) ##conversion is 0.0022 lb/g
penguins_for_america

islands = penguins %>% 
  distinct(island)

islands

penguins_brief = penguins %>%
  select(-body_mass_g) %>% ##hyphen allows you to exclude variables
  print()

penguins_sorted = penguins %>%
  arrange(body_mass_g, bill_depth_mm) %>%
  print()

##big to small
penguins_sorted = penguins %>%
  arrange(desc(body_mass_g), desc(bill_depth_mm)) %>%
  print()


##Exercise 1.3

mean_bill_length = penguins %>%
  filter(island %in% c("Dream", "Biscoe"), species=="Adelie")%>%
  mutate(bill_length_in = bill_length_mm*0.0393701) %>%
  summarize(mean_bill_length_in = mean(bill_length_in)) %>% ##combine line below
  summarize(sd_bill_length_in=sd(bill_length_in))
  print()
