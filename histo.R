outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)  ##46
nrow(outcome)  ##4706
names(outcome)

outcome[, 11]  <- as.numeric(outcome[, 11]) # col. 11 is mortality from heart attack
hist(outcome[,11])