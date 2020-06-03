source("load_data.R")
library('ggplot2')

# Suma czasu przejechanego w poszczególnych miesiącach roku w rozróżnieniu na płcie
write_to_csv <- function(df, file_name){
  write.csv(df,paste0("results/", file_name, ".csv"))
}

res1 <- data %>%
  filter(gender != 0) %>%
  group_by(month, gender) %>%
  summarize(TripTimeTotal = sum(tripduration/3600)) %>%
  mutate(Gender = ifelse(gender==1,"Male","Female"))
write_to_csv(res1, "monthly_trip_times")

res2 <- data %>%
  group_by(end.station.id) %>%
  mutate(NumOfTrips = n()) %>% top_n(1, tripduration) %>% ungroup() %>%
  select(end.station.id, end.station.name, end.station.latitude, end.station.longitude, NumOfTrips) %>%
  arrange(end.station.id)
write_to_csv(res2, "stations")

res3 <- data %>%
  group_by(DayOfWeek, hour) %>%
  summarize(Count = n())
write_to_csv(res3, "week_hours")

early_hours_week_data <- data %>%
  filter(DayOfWeek >= 1 & DayOfWeek <= 5 & hour >=7 & hour <= 9)

res4 <- early_hours_week_data %>%
  group_by(start.station.id) %>%
  mutate(NumOfTrips = n()) %>% top_n(1, tripduration) %>% ungroup() %>%
  select(id = start.station.id, start.station.name, latitude = start.station.latitude, longitude = start.station.longitude, NumOfTrips) %>%
  arrange(id)
write_to_csv(res4, "start_stations_early")

res5 <- early_hours_week_data %>%
  group_by(end.station.id) %>%
  mutate(NumOfTrips = n()) %>% top_n(1, tripduration) %>% ungroup() %>%
  select(id = end.station.id, end.station.name, latitude = end.station.latitude, longitude = end.station.longitude, NumOfTrips) %>%
  arrange(id)
write_to_csv(res5, "end_stations_early")
