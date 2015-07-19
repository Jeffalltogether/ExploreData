## ggplot2 Introduction
library(ggplot2)

## Quick Plotting funciton is qplot()

## Detailed Plotting Function is ggplot()

##Enter Graph Data into ggplot 
g <- ggplot(iris, aes(Sepal.Length, Petal.Length))
print(g)   #Nothing, because plot information is not added yet
summary(g)  

## Add Geom information to plot
g + geom_point()   #defaults for geom_point() are used

## Add regression
g + geom_point() + geom_smooth()   #Uses defualt regression method
g + geom_point() + geom_smooth(method = lm)

## Separate data in pannel plots by thrid varriable
g + geom_point() + facet_grid(. ~ Species) + geom_smooth(method = lm)
g + geom_point() + facet_grid(Species ~ .) + geom_smooth(method = lm)

## Using Colors instead of pannels
g + geom_point(color = "steelblue", size = 4, alpha = 1/2)   #all these options are set in geom_point function and they are all constants
g + geom_point(aes(color = Species), size = 4, alpha = 1/2)  #similar to above, but color is now assigned to a data variable by wrapping in the aes function

## Modifying Labels
g + geom_point(aes(color = Species)) + labs(title = "Iris Data") + labs(x = expression("Sepal " * Length[Length]), y = expression("Petal" * Length[Length])) 

## Customize Smoother
g + geom_point(aes(color = Species)) + 
        labs(title = "Iris Data") + 
        labs(x = expression("Sepal " * Length[Length]), y = expression("Petal" * Length[Length]))  +
        geom_smooth(size = 4, linetype = 3, method = "lm", se = FALSE)
        
## Customize Theme
g + geom_point(aes(color = Species)) + 
        labs(title = "Iris Data") + 
        labs(x = expression("Sepal " * Length[Length]), y = expression("Petal" * Length[Length]))  +
        geom_smooth(size = 4, linetype = 3, method = "lm", se = FALSE) +
        theme_bw(base_family = "Times")  ## Changes theme to Black & White and font to Times

## Axis Limits
testdata <- data.frame(x = seq(1,100,by=1), y = rnorm(100))
testdata[50,2] <- 100

# in the base plot function, this works fine to not stretch the ylimit to include the outlier
plot(testdata$x, testdata$y, type = "l", ylim = c(-3,3))

g <- ggplot(testdata, aes(x = x, y = y)) + geom_line()
g

# In ggplot, this creates a break in the data because it will subset the data to only include what is within the ylimit
g + ylim(-3, 3)

# to get the correct plot, we must call the coord_cartesian() function
g + coord_cartesian(ylim = c(-3, 3))

## What to do when the third variable is not categoricle
# binning a continuous dataset

# Calculate the deciles of the data
cutpoints <- quantile(iris$Sepal.Width, seq(0, 1, length = 4), na.rm = TRUE)

# cut the data at the deciles and create a new factor variable
iris$sepalWdthQuant <- cut(iris$Sepal.Width, cutpoints)

# see the leves of the newly created factor varialbe
levels(iris$sepalWdthQuant)

# Plot the data
g <- ggplot(iris, aes(Sepal.Length, Petal.Length))

g + geom_point(alpha=1/3) +
  facet_wrap(Species ~ sepalWdthQuant, ncol = 3, nrow = 4) +  # remember difference between facet_grid & facet_wrap!
  geom_smooth(method = "lm", se = FALSE, col = "steelblue") +
  theme_bw(base_family = "Avenir", base_size = 10) +
  labs(x = "Sepal Length") +
  labs(y = "Petal Length") +
  labs(title = "Iris Sepal vs. Petal Length by Sepal Width")

