data <- read.csv("hw1_data.csv", header=TRUE)
colnames(data)
names(data)

nrow(data)
ncol(data)
dim(data)
data[1:2, 1]
data[(nrow(data)-1):nrow(data), ]
tail(data) # to extract last few rows
data[47, "Ozone"]


x <- data[, "Ozone"]  #get ozone data to x vector
bad <- is.na(x)  # gets bad logical vector with TRUE where NA
length(x[bad]) # gives length of vector with only bad values
good <- x[!bad]
mean(good)

subset(data, Ozone > 31)

solarSub <- subset(data, Ozone > 31 & Temp > 90)
solar <- solarSub[, "Solar.R"]
mean(solar)
subset(data,Month ==6)

mean (subset(data, Month==6)[, "Temp"])

max(subset(data, Month==5)[, "Ozone"], na.rm=TRUE)



