library(tidyverse)
library(forcats)
library(here)
library(dplyr)


data(gss_cat)
exists("gss_cat")

names(gss_cat)

tv_hours_table <- gss_cat %>%
  filter(age >= 30) %>%
  group_by(marital) %>%
  summarise(mean_tv_hours = mean(tvhours, na.rm = TRUE))

tv_hours_table

write.csv(tv_hours_table,
          here("Tv_hour_by_marital_status.csv"),
          row.names = FALSE)
head(tv_hours_table)

gss_cat %>%
  summarise(
    Mean = mean(tvhours, na.rm = TRUE),
    Median = median(tvhours, na.rm = TRUE),
    SD = sd(tvhours, na.rm = TRUE),
    Minimum = min(tvhours, na.rm = TRUE),
    Maximum = max(tvhours, na.rm = TRUE)
  )


tv_gender <- gss_cat %>%
  group_by(race) %>%
  summarise(
    Mean_TV = mean(tvhours, na.rm = TRUE)
  )

tv_gender

names(gss_cat)

gss_cat %>%
  group_by(age) %>%
  summarise(mean_tv = mean(tvhours, na.rm = TRUE))

gss_cat %>%
  mutate(age_group = cut(age,
                         breaks = c(18, 30, 45, 60, 100),
                         labels = c("18-30", "31-45", "46-60", "60+"))) %>%
  group_by(age_group) %>%
  summarise(mean_tv = mean(tvhours, na.rm = TRUE))


gss_cat %>%
  group_by(race) %>%
  summarise(mean_tv = mean(tvhours, na.rm = TRUE))


library(ggplot2)

ggplot(gss_cat, aes(x = tvhours)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Distribution of TV Hours")


ggplot(gss_cat, aes(x = marital, y = tvhours)) +
  geom_boxplot() +
  labs(title = "TV Hours by Marital Status")
