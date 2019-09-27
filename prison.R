library(tidyverse)
library(lubridate)
library(tsibble)
library(feasts)
library(fable)

prison <- readr::read_csv("prison_population.csv") %>%
  mutate(quarter = yearquarter(date)) %>%
  select(-date) %>%
  as_tsibble(key = c(state, gender, legal, indigenous), index = quarter)

# Combine into state and gender categories only
prison <- prison %>%
  group_by(gender, state) %>%
  summarise(count = sum(count)) %>%
  ungroup()

prison %>% autoplot(count)

# Fit some models

fit_prison <- prison %>%
  model(
    ets = ETS(count),
    arima = ARIMA(log(count))
  )

# Produce some forecasts
fc_prison <- forecast(fit_prison, h="2 years")

fc_prison
fc_prison %>% filter(state=="NSW", gender=="Male", .model=='arima')

fc_prison %>% filter(state=="NSW") %>% autoplot(prison, level=90)

