data <- read.csv("Atlantic.csv")
years <- data$year

data.agg <- aggregate(cyclone_of_the_year ~ year, data, max)
plot(data.agg)


reg <- lm(data.agg$cyclone_of_the_year ~ data.agg$year)
abline(reg)
summary(reg)
