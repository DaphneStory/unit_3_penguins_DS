#DPS 

#02.25.2025

library(tidyverse)
library(palmerpenguins)
library(rstatix)

head(penguins)

gentoo =  penguins %>%
  filter(species == "Gentoo")

ggplot() +
  geom_histogram(aes(x=body_mass_g), data=gentoo)

# QQ plot

ggplot() +
  stat_qq(aes(sample=body_mass_g), data=gentoo)
dim(gentoo %>% filter(!is.na(body_mass_g)))

lit_body_mass_g = 5500 #EOL
my_t_test = t.test(gentoo$body_mass_g, mu = lit_body_mass_g)
my_t_test
summary(my_t_test)
class(my_t_test)

gentoo %>%
  t_test(body_mass_g ~ 1, mu = lit_body_mass_g)

#independent sample ttest

#no relationship between observations

data_for_t_test = penguins %>%
  filter(species %in% c("Gentoo", "Adelie"),
         !is.na(body_mass_g)) %>%
  select(species, body_mass_g)
head(data_for_t_test)

data_for_t_test%>%
  group_by(species) %>%
  summarize(avg=mean(body_mass_g), sd=sd(body_mass_g))

ggplot() +
  geom_histogram(aes(x=body_mass_g, fill=species), data=data_for_t_test) +
  facet_wrap(~species)

ggplot() +
  stat_qq(aes(sample=body_mass_g, color=species), data=data_for_t_test) +
  facet_wrap(~species, scales ="free")

data_for_t_test %>%
  levene_test(body_mass_g ~ species)

zoo =t.test(data_for_t_test$body_mass_g ~ data_for_t_test$species, var.equal=T)
summary(zoo)

data_for_t_test %>%
  t_test(body_mass_g ~ species)

# Correlation bill depth vs. bill length

ggplot() +
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm), data = gentoo)

ggplot() +
  stat_qq(data = gentoo, aes(sample=bill_depth_mm))

ggplot() +
  geom_histogram(aes(bill_length_mm), data = gentoo)


test =cor(x=gentoo$bill_depth_mm, y=gentoo$bill_length_mm, use="complete.obs") # use complete.obs - rids of na
class(test)
test
hand_test = cor.test(x=gentoo$bill_depth_mm,  y=gentoo$bill_length_mm, use="complete.obs")
class(hand_test)
summary(hand_test)


gentoo %>%
  cor_test(bill_length_mm, bill_depth_mm)

head(gentoo)
cor(gentoo[,3:6], use="complete.obs")


library(GGally)

gentoo %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  ggpairs()

penguins %>%
  select(body_mass_g, ends_with("_mm"), species) %>%
  ggpairs(aes(color=species))

##Correlation does not imply causation

  