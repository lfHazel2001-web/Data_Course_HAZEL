library(tidyverse)
getwd()
## 1)read cleaned data
df <- read.csv("Exam_1/cleaned_covid_data.csv")
## 2)A States
A_states<-df %>%
  filter(grepl("^A", Province_State))
unique(A_states$Province_State)
## 3) Scatterplot of deaths over time, by state
A_states%>%
  ggplot(aes(x= Last_Update, y= Deaths))+
  geom_point()+
  geom_smooth(method = "loess", se= FALSE)+
  facet_wrap(~Province_State, scales = "free")+
  labs(
    title = "COVID-19 Deaths Over Time in States Beginning with A",
    x = "Date",
    y= "Death"
  )+
  theme_minimal()
## 4) Maximum case fatality ratio for each state
state_max_fatality_rate<-df%>%
  group_by(Province_State)%>%
  summarize(
    Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE),
    .groups="drop"
    )%>%
  arrange(desc(Maximum_Fatality_Ratio))
state_max_fatality_rate
## 5) Bar plot of max fatality
state_max_fatality_rate %>%
  mutate(
    Province_State = factor(
      Province_State,
      levels = state_max_fatality_rate$Province_State
    )
  ) %>%
  ggplot(aes(x = Province_State, y = Maximum_Fatality_Ratio)) +
  geom_col() +
  labs(
    title = "Maximum COVID-19 Case Fatality Ratio by State",
    x = "State",
    y = "Maximum Fatality Ratio (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  )
## 6) BONUS
us_deaths <- df %>%
  group_by(Last_Update) %>%
  summarize(
    Total_Deaths = sum(Deaths, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Last_Update)
