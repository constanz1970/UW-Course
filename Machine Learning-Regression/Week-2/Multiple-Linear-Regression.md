Multiple Linear Regression
================
Jose\_A
18/3/2021

## Regression Week 2: Multiple Linear

Estimating Multiple Regression Coefficients (Gradient Descent)

In this first notebook we explored multiple regression using lm. Now we
will implement our own functions to estimate model parameters using
gradient descent.

In this notebook we will cover estimating multiple linear regression
weights via gradient descent. We will:

  - Add a constant column of 1s to the feature data
  - Converte these features from a data frame to a matrix
  - Write a predict\_output() function
  - Write a function to compute the derivative of the regression weights
    with respect to a single feature
  - Write gradient descent function to compute the regression weights
    given an initial weigth vector, step size and tolerance
  - Use the gradient descent function to estimate regression weights for
    multiple features

##### 1\. Load train and test data

``` r
trainData = read.csv("../Week-1/Data/kc_house_train_data.csv", header=T, sep=",")
testData = read.csv("../Week-1/Data/kc_house_test_data.csv", header=T, sep=",")
```

##### 2\. Write a function that take a data set, a list of features to be used as inputs, and a name of the output (e.g. price). This function should return a features\_matrix consisting of first a column of ones followed by columns containing the values of the input features in the data set in the same order as the input list, it should also return an output\_array which is an array of the values of the output i the data set (e.g. price)

``` r
get_matrices_list <- function(dataset, features, output) {
        dataset$ones <- 1
        features_matrix <- as.matrix(dataset[, c('ones', features)])
        output_matrix <- as.matrix(dataset[, output])
        return(list(features_matrix = features_matrix, output_matrix = output_matrix))
        
}
```

##### 3\. If the features matrix (including the column of 1s for the constant) is stored as a 2D array (matrix) and the regression weights are stored as a 1D array, then the predicted output is just the dot product between the features matrix and the weights. Write a function predict\_output() which accepts a 2D array feature\_matrix and a 1D array weights and returns a 1D array predictions

``` r
predict_output <- function(features_matrix, weights) {
        return(features_matrix %*% weights)
}
```

##### 4\. If we have the values of a single input feature in an array feature and the prediction errors (predictions - output) then the derivative of the regression cost function with respect to the weight of feature is just twice the dot product between feature and errors. Write a fuction that accepts a feature array and the error array and return the derivative (a single number).

``` r
feature_derivative <- function(feature_array, errors_array) {
        return(t(feature_array) %*% errors_array)
}
```

##### 5\. Now we will use predict\_output() and feature\_derivative() to write a gradient descent function. Although we can compute the derivative for all the features simultaneously (the gradient) we will explicitly loop over the features individually for simplicity. Write a gradient descent function that does the following:

  - Accepts a feature\_matrix, a 1D output array, an array of initial
    weights, step\_size, and a convergence tolerence.
  - While not converged updates each feature weight by subtracting the
    step size times the derivative for that feature given the current
    weights
  - At each step computes the magnitude/length of the gradient (square
    root of the sum of squared components)
  - When the magnitude of the gradient is smaller than the input
    tolerance, return the final weight vector

Note: instead of using the function feature\_derivative() to compute the
gradient for each feature I used the formula:

``` 
            ▽RSS=−2HT(y−Hw)
```

``` r
get_gradient_descent <- function(features_matrix, output_array, initial_weights, step_size, tolerance) {
        converged = FALSE 
        weights = initial_weights
        while(!converged){
                predictions = predict_output(features_matrix, weights)
                error = predictions - output_array
                gradient = -2*t(features_matrix) %*% error
                gradient_magnitude = sqrt(sum(gradient**2))
                weights = weights + step_size * gradient
                if(gradient_magnitude < tolerance){
                        converged = TRUE
                }
        }
        return(weights) 
}
```

##### 6\. Now we will run the regression\_gradient\_descent function on some acutal data. In particular we will use the gradient descent to estimate the model from Week 1 using just an intercept and slope. Use the following parameters:

  - Features: sqft\_living
  - Output: price
  - initial\_weights = \[1,-47000\]
  - step\_size = 7e-12
  - tolerance = 2.5e7

<!-- end list -->

``` r
initial_weights <- c(-47000, 1)
step_size <- 7e-12
tolerance <- 2.5e7
listMatrices_trainData <- get_matrices_list(trainData, 'sqft_living', 'price')
simple_weights_trainData <- get_gradient_descent(listMatrices_trainData$features_matrix, 
                                listMatrices_trainData$output_matrix, initial_weights, step_size, tolerance)
round(simple_weights_trainData)
```

    ##               [,1]
    ## ones        -47000
    ## sqft_living    282

##### 7\. QUIZ QUESTION: What is the value of the weight for sqft\_living the second element of simple\_weights (rounded to 1 decimal place)

``` r
round(simple_weights_trainData['sqft_living',],1)
```

    ## sqft_living 
    ##       281.9

##### 8\. Now build a corresponding test\_simple\_feature\_matrix() and test\_output() function using test\_data. Using test\_simple\_feature\_matrix() and simple\_weights compute the predicted house prices on all the test data.

``` r
listMatrices_testData <- get_matrices_list(testData, 'sqft_living', 'price')
predictions <- predict_output(listMatrices_testData$features_matrix, simple_weights_trainData)
```

##### 9\. QUIZ QUESTION: What is the predicted price for the 1st house in the TEST data set for model 1 (rounded to the nearest dollar)

``` r
predictions[1,]
```

    ## [1] 356134.4

##### 10\. Now compute the RSS on all test data for this model. Record the value and store it for later.

``` r
rss_testData <- sum((testData$price - predictions)**2)
rss_testData
```

    ## [1] 2.754e+14

##### 11\. Now we will use the gradient descent to fit a model with more than 1 predictor variable (and an intercept). Use the following parameters:

  - features = \[‘sqft\_living’, ‘sqft\_living15’\]
  - output = ‘price’
  - initial\_weights = \[-100000, 1, 1,\] (intecept, sqft\_living,
    sqft\_living\_15)
  - step\_size = 4e-12
  - tolerance=1e9

<!-- end list -->

``` r
new_features <- c('sqft_living', 'sqft_living15')
output <- 'price'
new_initial_weights <- c(-100000, 1, 1)
new_step_size = 4e-12
new_tolerance <- 1e9
listMatrices_trainData_2V <- get_matrices_list(trainData, new_features, output)
simple_weights_trainData_MV <- get_gradient_descent(listMatrices_trainData_2V$features_matrix, 
                                                    listMatrices_trainData_2V$output_matrix,
                                                    new_initial_weights, new_step_size, new_tolerance)
simple_weights_trainData_MV
```

    ##                       [,1]
    ## ones          -99999.96885
    ## sqft_living      245.07260
    ## sqft_living15     65.27953

##### 12\. Use the regression weights from this second model (using sqft\_living and sqft\_living\_15) and predict the outcome of all the house prices on TEST data.

``` r
listMatrices_testData_2V <- get_matrices_list(testData, new_features, output)
new_predictions <- predict_output(listMatrices_testData_2V$features_matrix, simple_weights_trainData_MV)
```

##### 13\. QUIZ QUESTION: What is the predicted price for the 1st house in the TEST data set for model 2 (round to the nearest dollar)

``` r
new_predictions[1,]
```

    ## [1] 366651.4

##### 14\. What is the actual price for the 1st house in the TEST data

``` r
testData[1,3]
```

    ## [1] 310000

##### 15\. QUIZ QUESTION: Which estimate was closer to the true price for the 1st house on the TEST data set, model 1 or model 2

Model 1

##### 16\. Now compute RSS on all TEST data for the second model. Record the value and store it for later

``` r
rss_testData_m2 <- sum((testData$price - new_predictions)**2)
rss_testData_m2
```

    ## [1] 2.702634e+14

##### 17\. QUIZ QUESTION: Which model (1 or 2) has lowest RSS on all the TEST DATA

Model 2 with RSS of 2.702634e14
