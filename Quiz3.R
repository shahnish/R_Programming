library(datasets)
data()
data(iris)
?iris

subset(iris,Species=="virginica")
mean(subset(iris,Species=="virginica")[, "Sepal.Length"])   # can use tapply as well

tapply(iris$Sepal.Length, iris$Species, mean)

apply(iris[, 1:4], 2, mean)

data(mtcars)
?mtcars
mtcars


mean(subset(mtcars,cyl==4)[, "mpg"])


tapply(mtcars$mpg, mtcars$cyl, mean)
#or
sapply(split(mtcars$mpg,mtcars$cyl),mean)


mean(subset(iris,Species=="virginica")[, "Sepal.Length"]) 
