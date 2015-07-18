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


