data <- read.csv("Atlantic.csv")
years <- data$year
summary(years)
years.freq <- table(years)
plot(c(1851:2015), years.freq, ytick = 50, ylim = c(0,1000), xlab = "Year", ylab = "Number of entries", main = "Number of entries per year")
axis(side=2, at = c(0,250,500,750,1000))
plot(c(1851:2015), years.freq)
cor(c(1851:2015), years.freq)
cor.test(c(1851:2015), years.freq)
length(c(1851,2015))
length(years.freq)
dates <- data$date
months <- c()
for (val in dates) {
  month <- (val%%1e4)%/%100
  months<- c(months, month)
}
print(months)
months.percent<- round(prop.table(table(months))*100, digits =2)
months.freq <- table(months)
barplot(months.freq, main = "Number of entries per month", xlab = "Month", ylab = "Number of entries", ylim = c(0,20000))
print(months.percent)

status <- data$status_of_system
status.freq <- table(status)
barplot(status.freq)

winds <- data$max_sustained_wind
boxplot(winds)

hurricanes <- data[status == "HU",]
winds <- hurricanes$max_sustained_wind
boxplot(winds)
summary(winds)

scatter.smooth(x=data$central_pressure, y=data$max_sustained_wind)
lmhur <- lm(max_sustained_wind~central_pressure, data = data)
summary(lmhur)

plot(x=data$central_pressure,y=data$max_sustained_wind)

abline(lmhur, col="blue")

plot(lmhur)






