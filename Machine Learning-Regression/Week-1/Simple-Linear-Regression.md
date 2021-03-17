Simple Linear Regression
================
Jose\_A
17/3/2021

## Regression Week 1: Simple Linear

Regression Assignment Predicting House Prices (One Feature) In this
notebook we will use data on house sales in King County, where Seattle
is located, to predict house prices using simple (One Feature) linear
regression. We will: \> \* Compute summary statistics – Write function
to compute Simple Linear Regression weights using the closed form
solution – Write functions to make predictions of the output given input
features – Turn the regression around to predict the input/feature given
the output • Compare two different models for predicting house prices

##### 1\. Load data

``` r
train_data = read.csv("Data/kc_house_train_data.csv", header=T, sep=",")
test_data = read.csv("Data/kc_house_test_data.csv", header=T, sep=",")
```

## Including Plots

You can also embed plots, for example:

![](Simple-Linear-Regression_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
