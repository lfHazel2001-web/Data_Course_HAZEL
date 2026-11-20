## Assignment 7
## Utah Religions by County

library(tidyverse)
library(janitor)

## Import the data using a relative path
religion = read.csv('Data/Utah_Religions_by_County.csv')

## Examine the data
View(religion)
str(religion)
dim(religion)
names(religion)
head(religion)

## Clean column names
religion = religion %>%
  clean_names()

## Look at the updated data
names(religion)
View(religion)

## I want to compare each specific religious group's proportion
## with county population and the proportion of non-religious people.
## Pivoting the religious groups into long format makes these
## comparisons easier to graph.

religion_long = religion %>%
  pivot_longer(
    cols = c(assemblies_of_god, episcopal_church,
             pentecostal_church_of_god, greek_orthodox, lds,
             southern_baptist_convention, united_methodist_church,
             buddhism_mahayana, catholic, evangelical, muslim,
             non_denominational, orthodox),
    names_to = 'religion_group',
    values_to = 'proportion'
  )

View(religion_long)

## Compare county population with the proportion of each religion.
## Each panel represents a different religious group.

ggplot(religion_long, aes(x = pop_2010, y = proportion)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  facet_wrap(~religion_group, scales = 'free_y') +
  labs(x = 'County Population (2010)',
       y = 'Religious Group Proportion',
       title = 'County Population vs. Religious Group Proportions')

## Calculate the correlation between county population
## and the proportion of each religious group.

population_correlations = religion_long %>%
  group_by(religion_group) %>%
  summarise(correlation = cor(pop_2010, proportion))

population_correlations

## the proportion of each specific religious group.

ggplot(religion_long, aes(x = non_religious, y = proportion)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  facet_wrap(~religion_group, scales = 'free_y') +
  labs(x = 'Non-Religious Population Proportion',
       y = 'Religious Group Proportion',
       title = 'Non-Religious vs. Religious Group Proportions')

## Calculate the correlation between the non-religious
## proportion and each specific religious group's proportion.

nonreligious_correlations = religion_long %>%
  group_by(religion_group) %>%
  summarise(correlation = cor(non_religious, proportion))

nonreligious_correlations

## Question 1, Muslim population have the strongest correlation with county population
## Most other religious groups have weak correlations with county population. LDS had almost no correlation
## as did non-denominational groups. 

## Question 2, LDS population had the strongest relationship with the proportion of non-religious people.
## This suggest that counties where there are higher number of LDS residents have a 
## lower proportion of non-religious residents. Episcopal church has a positive correlation.