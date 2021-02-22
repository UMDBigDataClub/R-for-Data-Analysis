setwd(r"{C:\Users\salba\Documents\Big Data Club\R For Data Analysis}")

### Load the Tidyverse
# install.packages("tidyverse")
library(tidyverse)

#Also load a personal favorite - gridExtra
library(gridExtra)

### Read in the data ###

#Readr

df <- read_csv("birds.csv")

#View the dataframe - you can also use the environment tab
head(df)

#View the columns
colnames(df)

#Summarize the attributes
summary(df)

### Investigate a question: How are the main numeric variables, egg_capacity and wingspan, distributed? ###

#Use a basic geom from ggplot2

ggplot(df) + geom_histogram(aes(x = egg_capacity))

#Try a fancier plot, maybe with both attributes using grid.arrange?
p1 <- ggplot(df) + 
  geom_histogram(aes(x = egg_capacity)) + 
  xlab("Egg Capacity") + 
  theme_light()

p2 <- ggplot(df) + 
  geom_density(aes(x = wingspan), fill = "lightblue") + 
  xlab("Wingspan") + 
  theme_light()

grid.arrange(p1, p2)

ggplot(df) + geom_jitter(aes(x = egg_capacity, y = wingspan), color = "darkgreen") + theme_light()
  

### Question: What is the average wingspan for predators compared to nonpredators?

# Make sure to remove NA values using tidyr, and use dplyr to group and summarize

pred_vs_nonpred <- df %>% 
  drop_na(wingspan) %>% 
  drop_na(predator) %>%
  group_by(predator) %>% 
  summarize(wingspan = mean(wingspan))

ggplot(pred_vs_nonpred) + geom_col(aes(x = predator, y = wingspan))

#Let's make this plot fancier
ggplot(pred_vs_nonpred) + geom_col(aes(x = predator, y = wingspan), fill = "lightgray") +
  scale_x_discrete(labels = c("Predator","Nonpredator")) +
  xlab("") +
  ylab("Wingspan") +
  theme_light()

#Let's add some error bars. Keep in mind when using multiple values in summarize, variables can be overridden
pred_vs_nonpred <- df %>% drop_na(wingspan) %>% drop_na(predator) %>% group_by(predator) %>% summarize(Wingspan = mean(wingspan), se = sd(wingspan) / sqrt(n()))

ggplot(pred_vs_nonpred) + geom_col(aes(x = predator, y = Wingspan), fill = "lightgray") + 
  geom_errorbar(aes(x = predator, ymin = Wingspan - se, ymax = Wingspan + se), width = 0.2) +
  scale_x_discrete(labels = c("Predator","Nonpredator")) +
  xlab("") +
  theme_light()

#Then we can run a statistical test to compare
t.test(filter(df, predator)$wingspan, filter(df, !predator)$wingspan)

### Question: What is the difference in egg size between the terrain types?

cats <- df %>% select(common_name, scientific_name, egg_capacity, forest, grassland, wetland)

#Let's make sure these categories are mutually exclusive

cats %>% mutate(mutual = (forest + grassland + wetland) > 1) %>% filter(mutual)

#They are not. So let's compare single-environment birds to multi-environment, ignoring two-environment

cats <- cats %>% mutate(env_count = (forest + grassland + wetland)) %>% filter(env_count != 2)
cats_gb <- cats %>% drop_na(egg_capacity) %>% group_by(env_count) %>% summarize(eggs = mean(egg_capacity))

ggplot(cats_gb) + geom_col(aes(x = env_count, y = eggs))

### Suppose we want a count of the logical variables. How can we do this?

roles <- df %>% select(anatomist, cartographer, historian, photographer) %>% 
  summarize(anatomist = sum(anatomist), cartographer = sum(cartographer), historian = sum(historian), photographer = sum(photographer)) %>%
  gather()


ggplot(roles) + geom_col(aes(x = key, y = value, fill = key)) + scale_fill_viridis_d() + theme_light()
