#DPS

#2.20.25


library(tidyverse)
library(palmerpenguins)

find("filter")

gentoo = penguins %>%
  dplyr::filter(species=="Gentoo") %>%
  dplyr::select(-body_mass_g) %>%
  print()


head(penguins)

penguins_no_nas = penguins %>%
  filter(!is.na(body_mass_g)) %>%
  filter(!is.na(sex))

ggplot(data=penguins_no_nas) +
  geom_point(aes(x=flipper_length_mm, y=body_mass_g, color=bill_length_mm, shape=sex)) + ###aes = aesthetics || anything from the data must be put into the aes portion
  geom_smooth(aes(x=flipper_length_mm, y=body_mass_g), method = "lm") +
  xlab("Flipper length (mm)") +
  ylab("Body mass (g)") +
  ggtitle("Penguins are cute") +
  theme_bw()

penguins_ts = penguins %>%
  group_by(species, year) %>%
  summarize(count = n())

ggplot((data=penguins_ts)) +
  geom_line(aes(x=year, y=count, color=species)) +
  theme_classic()


# histogram --1 dimensional

x_plot = ggplot(data=penguins) +
  geom_histogram(aes(x = flipper_length_mm, fill=species),
                 binwidth = 2,  ##bin width ==> for every two mm there is a bar
                 position = "identity",
                 alpha=0.7) + ## transparency value | 0 = transparent | 1= opaque
  scale_fill_manual(values=c("darkorange", "darkorchid", "cyan4"))

penguins %>% distinct(species)
unique(penguins$species)


#Box Plots

ggplot(data=penguins) +
  geom_boxplot(aes(y=flipper_length_mm, x=species)) + ## explain your box plot in your figure caption
  geom_point(aes(y=flipper_length_mm, x=species)) 

ggplot(data=penguins) +
  geom_boxplot(aes(y=flipper_length_mm, x=species)) + 
  geom_jitter(aes(y=flipper_length_mm, x=species, color=species), width=0.3, height=0) 

# Bar Charts 

ggplot(data=penguins) +
  geom_bar(aes(x=sex, fill=species)) +
  facet_wrap(~species) #+
  ##coord_flip()

# save plot
ggsave("figures/penguins_islands_species.png", plot =x_plot, device="png", units="in", width=5, height = 7, dpi=300)
  

## Exercise 2.2
ggplot(data=penguins) +
  geom_point(aes(x=bill_depth_mm, y=bill_length_mm, color=sex), scales="free") +
  facet_wrap(~species) +
  theme_classic()+
  scale_color_manual(values=c("", ""))
