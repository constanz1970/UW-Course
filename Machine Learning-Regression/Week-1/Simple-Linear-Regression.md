Simple Linear Regression
================
Jose\_A
17/3/2021

## Regression Week 1: Simple Linear

Regression Assignment Predicting House Prices (One Feature) In this
notebook we will use data on house sales in King County, where Seattle
is located, to predict house prices using simple (One Feature) linear
regression. We will:

  - Compute summary statistics
  - Write function to compute Simple Linear Regression weights using the
    closed form solution
  - Write functions to make predictions of the output given input
    features
  - Turn the regression around to predict the input/feature given the
    output
  - Compare two different models for predicting house prices

##### 1\. Load data

``` r
trainData = read.csv("Data/kc_house_train_data.csv", header=T, sep=",")
testData = read.csv("Data/kc_house_test_data.csv", header=T, sep=",")
```

##### 2\. Write a generic function that accepts a column of data (e.g a vector) ‘input\_feature’ and another column ‘output’ and returns the Simple Linear Regression parameters ‘intercept’ and ‘slope’. Use the closed form solution from lecture to calculate the slope and intercept.

``` r
SLR_Intercept_Slope <- function(feature, output) {
        inputMean <- mean(feature)
        outputMean <- mean(output)
        
        covar <- sum((feature - inputMean)*(output - outputMean)) / nrow(trainData)
        varia <- sum((feature - inputMean)**2) / nrow(trainData)
        slope <- covar / varia
        intercept <- outputMean - slope*inputMean
        
        return(list(slope = slope, intercept = intercept))
}
```

##### 3\. Use the function to calculate the estimated slope and intercept on the training data to predict price given sqft\_living

``` r
sqft <- SLR_Intercept_Slope(trainData$sqft_living, trainData$price)
sqft
```

    ## $slope
    ## [1] 281.9588
    ## 
    ## $intercept
    ## [1] -47116.08

##### 4\. Write a function that accepts a column of data input\_features the slope and the intercept you learned, and returns a column of predictions ‘predicted\_output’ for each entry in the input column.

``` r
get_predictions <- function(feature, intercept, slope) {
        return(intercept + slope*feature)
}
```

##### 5\. QUIZ QUESTION: Using the slope and intercept from (4), what is the predicted price for a house with 2,650 sqft.

``` r
predictedPrice <- get_predictions(2650, sqft$intercept, sqft$slope)
predictedPrice
```

    ## [1] 700074.8

##### 6\. Write a function that accepts a column of data: input\_features and output and the regression parameters slope and intercept and returns the Residual Sum of Squares (RSS).

``` r
getRSS <- function(feature, output, intercept, slope) {
        calculatedRSS <- sum((output - (intercept + slope*feature))**2)
        return(calculatedRSS)
}
```

##### 7\. QUIZ QUESTION: According to this function and the slope and intercept from (4) what is the RSS for the simple linear regression using sqft to predict prices on the TRAINING data?

``` r
rss_sqft_living <- getRSS(trainData$sqft_living, trainData$price, sqft$intercept, sqft$slope)
rss_sqft_living
```

    ## [1] 1.201918e+15

##### 8\. Write a function that accepts a column of data output and the regression parameters slope and intercept and outputs the colum of data estimated\_input.

``` r
get_estimated_Inputs <- function(output, intercept, slope) {
        estimatedInputs <- (output - intercept)/slope
        return(estimatedInputs)
}
```

##### 9\. QUIZ QUESTION: According to this function and the regression slope and intercept from (3) what is the estimated sqft for a house costing $800,000?

``` r
estInputs <- get_estimated_Inputs(800000, sqft$intercept, sqft$slope)
estInputs
```

    ## [1] 3004.396

##### 10\. Use the function from (3) to calculate the Simple Linear Regression parameters slope and intercept for estimating price based on number of bedrooms. Save this slope and intercept for later.

``` r
numberOfBedrooms <- SLR_Intercept_Slope(trainData$bedrooms, trainData$price)
numberOfBedrooms
```

    ## $slope
    ## [1] 127589
    ## 
    ## $intercept
    ## [1] 109473.2

##### 11\. Compute RSS from both models using TEST data

``` r
rss_sqft_living <- getRSS(testData$sqft_living, testData$price, sqft$intercept, sqft$slope)
rss_bedrooms <- getRSS(testData$bedrooms, testData$price, numberOfBedrooms$intercept, numberOfBedrooms$slope)
c(rss_sqft_living, rss_bedrooms)
```

    ## [1] 2.754029e+14 4.933646e+14

##### 12\. Compare the RSS from both models, which model has the smallest residual sum of squares? Why do you think this is the case?

###### The model that uses house size (square feet) has the smallest RSS. This is likely do to the fact that square footage is more predictive of housing price than the number of bedrooms.
