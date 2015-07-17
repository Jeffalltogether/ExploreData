## Subsetting data by levels with `gl` function

## generate data
x <- rnorm(100)
y <- rnorm(100)

## create levels
g <- gl(2, 50, labels = c("Male", "Female"))

## generate empty plot
plot(x,y, type= "n")

## subset and plot data by levels
points(x[g=="Male"], y[g=="Male"], col = "green")
points(x[g=="Female"], y[g=="Female"], col = "blue", pch = 19)