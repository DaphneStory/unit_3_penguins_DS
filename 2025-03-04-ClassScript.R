#DPS
#03.04.2025

##continued from Thursday class notes

library(tidyverse)
library(palmerpenguins)
library(GGally)


#Building a model

###Multiple regression

penguins_lm_3 = penguins %>%
  filter(!is.na(bill_depth_mm),
         !is.na(bill_length_mm),
         !is.na(species))
lm_3 = lm(bill_depth_mm ~ bill_length_mm + species, data=penguins_lm_3)
summary(lm_3)

coef(lm_3)

lm_3$coefficients
anova(lm_3)

popper = broom::tidy(lm_3, conf.int = TRUE, conf.level=0.95)
popper$std.error


library(ggiraph)

library(ggiraphExtra)

ggPredict(lm_3, se=T, interactive=T) ###se = standard error

### model predictions in base r

lm3_predictions = predict(lm_3, interval="confidence", level=0.95)
head(lm3_predictions)
penguins_lm_3_predict = cbind(penguins_lm_3, lm3_predictions)
head(penguins_lm_3_predict)

#plot it
ggplot(data=penguins_lm_3_predict, 
       aes(x=bill_length_mm, y=bill_depth_mm, color=species)) +
  geom_point() + 
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill = species, color=NULL), alpha=0.5) +
  geom_line(aes(y=fit), linewidth=0.75) +
  theme_bw()

# make prediction on new data
min_bill= min(penguins$bill_length_mm, na.rm=T)
max_bill =max(penguins$bill_length_mm, na.rm=T)

newdata_bill_length_mm = seq(from=min_bill, to = max_bill, by =0.1)
head(newdata_bill_length_mm)
tail(newdata_bill_length_mm)

##used to create fake x-variables to feed into the model
newdata = expand.grid(bill_length_mm = newdata_bill_length_mm, 
                      species = unique(penguins$species))

head(newdata)
tail(newdata)
summary(newdata)

new_predictions = predict(lm_3, interval = "confidence", newdata = newdata)
head(new_predictions)
##bind the model to the fake x-values to predict. Combin true data used to make predictions with the predictions generated
newdata_predict_lm_3 = cbind(newdata, new_predictions)
head(newdata_predict_lm_3)


#This plot allows us to predict outside the bounds of our data
ggplot() +
  geom_point(data=penguins_lm_3, 
             aes(x=bill_length_mm, y=bill_depth_mm, color =species)) +
  geom_ribbon(data=newdata_predict_lm_3, 
              aes(x=bill_length_mm, ymin=lwr, ymax=upr, fill=species, color=NULL),
              alpha=0.5) +
  geom_line(data=newdata_predict_lm_3, 
            aes(x=bill_length_mm, y=fit, color = species))

#do it the tidyverse way

lm3_predict = lm_3 %>%
  broom::augment(data=penguins_lm_3, se_fit=T, interval="confidence")
head(lm3_predict)
glimpse(lm3_predict)

ggplot(aes(x=bill_length_mm, y=bill_depth_mm, color=species),
       data=lm3_predict) +
  geom_point() +
  geom_ribbon(aes(ymin=.lower, ymax=.upper, fill=species, color=NULL), 
              alpha=0.5)+
  geom_line(aes(y=.fitted))

penguins%>%
  arrange(bill_length_mm) %>%
  print()

newdata= penguins_lm_3 %>%
  tidyr::expand(bill_length_mm, species)
head(newdata)

lm3_predict=lm_3 %>%
  broom::augment(newdata=newdata, se_fit=T, interval="confidence")
head(lm3_predict)

ggplot(aes(x=bill_length_mm, y=bill_depth_mm, color=species),
       data=lm3_predict) +
  geom_point(data=penguins_lm_3) +
  geom_ribbon(aes(ymin=.lower, ymax=.upper, fill=species, color=NULL), 
              alpha=0.5)+
  geom_line(aes(y=.fitted))
