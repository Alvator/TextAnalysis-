

install.packages("sklearn")

data("iris")

attach(iris)

View(iris)


iris2 <- iris


iris2$Sepal.Length

iris2$Petal.Length

iris2$Sepal.Width

iris2$Petal.Length

summary(iris2)

dim(iris2)
names(iris)

unique(iris2$Species)

hist(iris$Sepal.Length,
     col='red',
     main='Histogram',
     xlab='Length',
     ylab='Frequency')


hist(iris2$Sepal.Width,
     col='yellow',
     main='Histogram',
     xlab='Length',
     ylab='Frequency')

hist(iris2$Petal.Length,
     col='blue',
     main='Histogram',
     xlab='Length',
     ylab='Frequency')

hist(iris2$Petal.Width,
     col='pink',
     main='Histogram',
     xlab='Length',
     ylab='Frequency')

plot(iris2$Sepal.Width, iris2$Sepal.Length,
     col='green',
     main='Scatterplot',
     xlab='Sepal Width',
     ylab='Sepal Length',
     pch=20)


boxplot(Sepal.Length~Species,
        data=iris,
        main='Sepal Length by Species',
        xlab='Species',
        ylab='Sepal Length',
        col='steelblue',
        border='black')


