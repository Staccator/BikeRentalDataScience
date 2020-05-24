options(stringsAsFactors=FALSE)
library("dplyr")

# Data for Year 2018 for Jersey City
months <- list()
for (m in 1:12){
  year <- 2018
  file_name <- paste0('JC-', as.character(year), ifelse(m < 10,'0', ''), as.character(m), '-citibike-tripdata.csv')
  months[[m]] <- read.csv(paste0('data/', file_name))
}
data <- months[[1]]
for (m in 2:12){
  data <- bind_rows(data, months[[m]])
}

data <- data %>% mutate(month = as.integer(substring(starttime,6,7)))
data <- data %>% mutate(DayOfWeek = as.POSIXlt(starttime)$wday)
data <- data %>% mutate(hour = as.integer(substring(starttime,12,13)))
data$DayOfWeek[data$DayOfWeek == 0] <- 7
data$DayOfWeek <- as.integer(data$DayOfWeek)