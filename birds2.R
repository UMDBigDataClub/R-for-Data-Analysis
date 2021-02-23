setwd(r"{C:\Users\salba\Documents\Big Data Club\R For Data Analysis}")

### Load the Tidyverse
# install.packages("tidyverse")
library(tidyverse)

#Also load a personal favorite - gridExtra
library(gridExtra)

df <- read_csv("birds.csv")

head(df)

colnames(df)

summary(df)

g1 <- ggplot(df) + geom_density(aes(x = wingspan), fill = "lightblue") + xlab("Wingspan") + theme_light()
g2 <- ggplot(df) + geom_histogram(aes(x = egg_capacity), fill = "black") + xlab("Egg Capacity") + theme_light()

grid.arrange(g1, g2)

pred <- df %>% group_by(predator) %>% summarize(Wingspan = mean(wingspan, na.rm = TRUE), se = sd(wingspan, na.rm = TRUE) / sqrt(n()))

ggplot(pred) + geom_col(aes(x = predator, y = Wingspan), fill = "lightblue") +
  geom_errorbar(aes(x = predator, ymin = Wingspan - se, ymax = Wingspan + se), width = 0.2) +
  theme_light()

summary(df)

hab <- df %>% select(common_name, forest, grassland, wetland, egg_capacity) %>% 
  mutate(nhab = forest + grassland + wetland) %>%
  filter(nhab == 1)

forest <- hab %>% filter(forest) %>% summarize(forest = mean(egg_capacity))

grassland <- hab %>% filter(grassland) %>% summarize(grassland = mean(egg_capacity))

df2 <- bind_cols(forest, grassland) %>% gather()
 
ggplot(df2) + geom_col(aes(x = key, y = value)) + xlab("Habitat") + theme_light()

