library(tidyverse)

unicef = read.csv("https://raw.githubusercontent.com/gzahn/BIOL3100_Exams/master/Exam_2/unicef-u5mr.csv")
View(unicef)
str(unicef)
dim(unicef)
names(unicef)

write.csv(unicef, "unicef-u5mr.csv", row.names = FALSE)

## Convert the yearly columns into long format.
## Each row will represent one country in one year.

unicef_long = unicef %>%
  pivot_longer(
    cols = starts_with('U5MR.'),
    names_to = 'Year',
    values_to = 'U5MR'
  ) %>%
  mutate(Year = as.numeric(str_remove(Year, 'U5MR\\.')))

## Examine the tidy data
View(unicef_long)
str(unicef_long)

## Plot the mortality rate over time for each country.
## Group by country so each country has its own line.
## Separate the plots by continent.

plot1 = unicef_long %>%
  drop_na(U5MR) %>%
  ggplot(aes(x = Year, y = U5MR, group = CountryName)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~ Continent) +
  labs(
    x = 'Year',
    y = 'Under-5 Mortality Rate',
    title = 'Under-5 Mortality Rate by Country and Continent'
  )

plot1

ggsave('HAZEL_Plot_1.png', plot = plot1)

## Group the data by continent and year.
## Calculate the mean mortality rate for each group.

continent_mean = unicef_long %>%
  drop_na(U5MR) %>%
  group_by(Continent, Year) %>%
  summarise(mean_U5MR = mean(U5MR))

View(continent_mean)

## Plot the average mortality rate over time.
## Color each line by continent.

plot2 = continent_mean %>%
  ggplot(aes(x = Year, y = mean_U5MR, color = Continent)) +
  geom_line() +
  labs(
    x = 'Year',
    y = 'Mean Under-5 Mortality Rate',
    title = 'Average Under-5 Mortality Rate by Continent',
    color = 'Continent'
  )

plot2

ggsave('HAZEL_Plot_2.png', plot = plot2)

## Remove missing mortality values before modeling.
## Convert continent into a categorical variable.

unicef_model = unicef_long %>%
  drop_na(U5MR) %>%
  mutate(Continent = as.factor(Continent))

## Model 1: Mortality rate predicted by year only.

mod1 = glm(
  data = unicef_model,
  formula = U5MR ~ Year
)

## Model 2: Mortality rate predicted by year and continent.

mod2 = glm(
  data = unicef_model,
  formula = U5MR ~ Year + Continent
)

## Model 3: Mortality rate predicted by year, continent,
## and their interaction.

mod3 = glm(
  data = unicef_model,
  formula = U5MR ~ Year * Continent
)
Year + Continent + Year:Continent

## Compare the performance of all three models.

library(easystats)

compare_performance(mod1, mod2, mod3) %>%
  plot()

summary(mod1)
summary(mod2)
summary(mod3)

## Create a dataset containing the country, continent, and year.
## Use all three models to calculate predicted mortality rates.

prediction_data = unicef_model %>%
  select(Continent, Year, CountryName) %>%
  distinct()

prediction_data$pred1 = predict(mod1, prediction_data)

prediction_data$pred2 = predict(mod2, prediction_data)

prediction_data$pred3 = predict(mod3, prediction_data)

View(prediction_data)

## Convert the three prediction columns into long format.

predictions_long = prediction_data %>%
  pivot_longer(
    cols = starts_with('pred'),
    names_to = 'model',
    values_to = 'prediction'
  )

## Summarize predictions by continent, year, and model.

predictions_mean = predictions_long %>%
  group_by(Continent, Year, model) %>%
  summarise(prediction = mean(prediction))

## Plot predictions from all three models.

ggplot(predictions_mean,
       aes(x = Year, y = prediction, color = Continent)) +
  geom_line() +
  facet_wrap(~ model) +
  labs(
    x = 'Year',
    y = 'Predicted U5MR',
    title = 'Predicted Under-5 Mortality Rates by Model'
  )