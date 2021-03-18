

trainData = read.csv("C:/Users/constanz1970/Desktop/Coursera Projects/UW-Course/Machine Learning-Regression/Week-1/Data/kc_house_train_data.csv", header=T, sep=",")
testData = read.csv("C:/Users/constanz1970/Desktop/Coursera Projects/UW-Course/Machine Learning-Regression/Week-1/Data/kc_house_test_data.csv", header=T, sep=",")

get_matrices_list <- function(dataset, features, output) {
        dataset$ones <- 1
        features_matrix <- as.matrix(dataset[, c('ones', features)])
        output_matrix <- as.matrix(dataset[, output])
        return(list(features_matrix = features_matrix, output_matrix = output_matrix))
        
}



predict_output <- function(features_matrix, weights) {
        return(features_matrix %*% weights)
}

feature_derivative <- function(feature_array, errors_array) {
        return(t(feature_array) %*% errors_array)
}

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

initial_weights <- c(-47000, 1)
step_size <- 7e-12
tolerance <- 2.5e7
listMatrices_trainData <- get_matrices_list(trainData, 'sqft_living', 'price')
simple_weights_trainData <- get_gradient_descent(listMatrices_trainData$features_matrix, listMatrices_trainData$output_matrix, initial_weights, step_size, tolerance)
simple_weights_trainData


listMatrices_testData <- get_matrices_list(testData, 'sqft_living', 'price')
simple_weights_testData <- get_gradient_descent(listMatrices_testData$features_matrix, listMatrices_testData$output_matrix, initial_weights, step_size, tolerance)
simple_weights_testData

class(simple_weights_testData)

test_predictions <- predict_output(listMatrices_testData$features_matrix, simple_weights_testData)
test_predictions[1,]

rss_testData <- sum((testData$price - test_predictions)**2)
rss_testData
