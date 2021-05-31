data <- read.csv("test_out.csv")
plot(data)
reg <- lm(data$count ~ data$year)
abline(reg,col='red')
summary(reg)

years <- data$year

data.agg <- aggregate(cyclone_of_the_year ~ year, data, max)
plot(data.agg)

big_only <- subset(data, data$max_sustained_wind >= 100)

big.agg <- aggregate(cyclone_of_the_year ~ year, big_only, FUN=function(x) length(unique(x)))
for (entry in big.agg){
  print(entry)
}
plot(big.agg,ylab='Cyclones per year')
reg <- lm(big.agg$cyclone_of_the_year ~ big.agg$year)
abline(reg,col='red')
summary(reg)
