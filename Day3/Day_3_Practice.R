
setwd("~/GitHub/REALshortcourse_Summer2021/Day3")

library(tidyverse)
library(naniar)
library(janitor)
library(here)
library(skimr)
library(janitor)
library(here)
library(palmerpenguins
        
homerange <- read_csv("Tamburelloetal_HomeRangeDatabase.csv")
glimpse(homerange)       
naniar::miss_var_summary(homerange)
names(homerange)

ggplot(data = homerange, mapping = aes(x = log10.mass, y = log10.hra)) +
  geom_jitter()

ggplot(data=homerange, mapping=aes(x=log10.mass, y=log10.hra)) +
  geom_point()+
  geom_smooth(method=lm, se=T)#adds the regression line, `se=TRUE` will add standard error

homerange %>% 
  filter(family=="salmonidae") %>% #includes only salmonidae#
  ggplot(aes(x=common.name, y=log10.mass))+
  geom_col()

homerange %>% 
  group_by(class) %>% 
  summarize(mean_body_wt=mean(log10.mass)) %>% 
  ggplot(aes(x=class, y=mean_body_wt))+
  geom_col()

#Make a bar plot that shows the relative numbers of herbivores or carnivores in our dataset
#(e.g. Are there more herbivores or carnivores in mammals?)

homerange %>% 
  filter(class=="mammalia") %>% 
  group_by(trophic.guild) %>% 
  count()

homerange %>% 
  filter(class=="mammalia") %>% 
  group_by(trophic.guild) %>% 
  count() %>% 
  ggplot(aes(y=trophic.guild, x=n))+
  geom_col()

#Boxplots help us visualize a range of values. So, on the x-axis we typically have something categorical
#and the y-axis is the range. In the case below, we are plotting log10.mass by taxonomic class in the homerange
#data. geom_boxplot() is the geom type for a standard box plot. The center line in each box represents the median, not the mean.

homerange %>% 
  group_by(class) %>% 
  summarize(min_log10.mass=min(log10.mass),
            max_log10.mass=max(log10.mass),
            median_log10.mass=median(log10.mass))

homerange %>% 
  ggplot(aes(x = class, y = log10.mass)) +
  geom_boxplot()

homerange %>% 
  group_by(trophic.guild) %>% 
  summarize(min_log10.mass=min(log10.mass),
            max_log10.mass=max(log10.mass),
            mean_log10.mass=mean(log10.mass),
            median_log10.mass=median(log10.mass),
            total_n=n())

homerange %>% 
  ggplot(aes(x=trophic.guild, y=log10.mass))+
  geom_boxplot()

homerange %>% 
  filter(taxon=="mammals" & trophic.guild=="carnivore") %>% 
  summarize(min_log10.mass=min(log10.mass),
            max_log10.mass=max(log10.mass),
            mean_log10.mass=mean(log10.mass),
            median_log10.mass=median(log10.mass),
            total_n=n()) %>% 
  pivot_longer(cols=everything(), #made a new table with 2 columns#
               names_to="measurement",
               values_to="value")
homerange %>% 
  filter(taxon=="mammals" & trophic.guild=="carnivore") %>% 
  select(family, log10.mass) 

homerange %>% 
  filter(taxon=="mammals" & trophic.guild=="carnivore") %>% 
  select(family, log10.mass) %>% 
  ggplot(aes(x=family, y=log10.mass))+
  geom_boxplot()+
  coord_flip()+
  labs(title="Family vs. Log10.mass") #labeled axis, title

ggplot(data=homerange, mapping=aes(x=log10.mass, y=log10.hra)) +
  geom_point()+
  geom_smooth(method=lm, se=T)

ggplot(data=homerange, mapping=aes(x=log10.mass, y=log10.hra)) +
  geom_point()+
  geom_smooth(method=lm, se=T)+
  labs(title="Log 10 Mass vs. Log 10 Home Range Area",
       x="Log 10 Mass (g)",
       y="Log 10 Home Range Area (m2)")

ggplot(data=homerange, mapping=aes(x=log10.mass, y=log10.hra)) +
  geom_point()+
  geom_smooth(method=lm, se=T)+
  labs(title="Log 10 Mass vs. Log 10 Home Range Area",
       x="Log 10 Mass (g)",
       y="Log 10 Home Range Area (m2)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))

ggplot(data=homerange, mapping=aes(x=log10.mass, y=log10.hra)) +
  geom_point()+
  geom_smooth(method=lm, se=T)+
  labs(title="Log 10 Mass vs. Log 10 Home Range Area",
       x="Log 10 Mass",
       y="Log 10 Home Range Area (m2)") +
  theme(plot.title = element_text(size = rel(1.5), hjust = 0.5))

ggplot(data=homerange, aes(x=realm, fill=realm))+geom_bar()+
  labs(title = "# Species by Realm",
       x = "realm",
       y = NULL) +
  theme(plot.title = element_text(size = rel(1.5), hjust = 0.5))

ggplot(data=homerange, aes(x=log10.hra, y=log10.mass, size=log10.hra))+
  geom_point(na.rm=T)

p <- homerange %>% 
  ggplot(aes(x= log10.mass, y= log10.hra))
p +geom_point(size=1)
p+geom_point(aes(shape=thermoregulation, color=thermoregulation), size=1.75)
