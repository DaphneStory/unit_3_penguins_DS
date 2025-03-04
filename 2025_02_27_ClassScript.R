#DPS

#02.27.2025

##Linear Regression
  #simple way to model data
## y =B1 +B2*x + Error

##5 key assumptions
  ###linear relationship
  #### normality of model residuals
  ##### no or little multicollinearity
  ###### no auto-correlation
  ####### homoscedasticity



library(tidyverse)
library(palmerpenguins)
library(GGally)

head(penguins)

penguins %>% 
  select(bill_length_mm, bill_depth_mm) %>%
  GGally::ggpairs()


##Build Model
lm_1 = lm(bill_depth_mm ~ bill_length_mm, data=penguins)
 class(lm_1)
summary(lm_1) 

  #lm function
lm_1$coefficients ##coef is accepted form of coefficients
lm_1$coefficients[1]
lm_1$residuals

ggplot(data=penguins, aes(x=bill_length_mm, y=bill_depth_mm)) +
  geom_point() +
  geom_smooth(method="lm")


gentoo = penguins %>%
  filter(species=="Gentoo")

gentoo %>%
  select(bill_length_mm, bill_depth_mm) %>%
  ggpairs()

lm_2 = lm(bill_depth_mm ~ bill_length_mm, data=gentoo)
summary(lm_2)

ggplot(data=gentoo, aes(x=bill_length_mm, y=bill_depth_mm)) +
  geom_point() +
  geom_smooth(method="lm")


ggplot(data=penguins) +
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm, color=species)) +
  geom_smooth(aes(x=bill_length_mm, y =bill_depth_mm), method = "lm", color = "black") +
  geom_smooth(aes(x=bill_length_mm, y=bill_depth_mm, color= species), method ="lm")


#Exercise 5.1

lm_gbd = lm(bill_depth_mm ~ flipper_length_mm, data=gentoo)

ggplot(data=gentoo, aes(x=flipper_length_mm, y=bill_depth_mm)) +
  geom_point() +
  geom_smooth(method ="lm")