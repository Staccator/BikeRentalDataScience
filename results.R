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

g <- ggplot(data = res1, aes(x=as.factor(month), y=TripTimeTotal, fill=as.factor(Gender)))
g + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Dark2", name = "Gender") +
  xlab("Month") + ylab("Total Trip Duration (hours)") + theme(legend.position = 'bottom')


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
