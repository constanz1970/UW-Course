

trainData = read.csv("C:/Users/constanz1970/Desktop/Coursera Projects/UW-Course/Machine Learning-Regression/Week-1/Data/kc_house_train_data.csv", header=T, sep=",")
testData = read.csv("C:/Users/constanz1970/Desktop/Coursera Projects/UW-Course/Machine Learning-Regression/Week-1/Data/kc_house_test_data.csv", header=T, sep=",")
testData
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
round(simple_weights_trainData['sqft_living',],1)

initial_weights <- c(-47000, 1)
step_size <- 7e-12
tolerance <- 2.5e7
listMatrices_trainData <- get_matrices_list(trainData, 'sqft_living', 'price')
simple_weights_trainData <- get_gradient_descent(listMatrices_trainData$features_matrix, listMatrices_trainData$output_matrix, initial_weights, step_size, tolerance)
simple_weights_trainData


listMatrices_testData <- get_matrices_list(testData, 'sqft_living', 'price')
predictions <- predict_output(listMatrices_testData$features_matrix, simple_weights_trainData)
predictions[1,]


rss_testData <- sum((testData$price - predictions)**2)
rss_testData


new_features <- c('sqft_living', 'sqft_living15')
output <- 'price'
new_initial_weights <- c(-100000, 1, 1)
new_step_size = 4e-12
new_tolerance <- 1e9
listMatrices_trainData_2V <- get_matrices_list(trainData, new_features, output)
simple_weights_trainData_MV <- get_gradient_descent(listMatrices_trainData_2V$features_matrix, listMatrices_trainData_2V$output_matrix,
                                                    new_initial_weights, new_step_size, new_tolerance)
simple_weights_trainData_MV


listMatrices_testData_2V <- get_matrices_list(testData, new_features, output)
new_predictions <- predict_output(listMatrices_testData_2V$features_matrix, simple_weights_trainData_MV)
new_predictions[1,]
testData[1,3]

rss_testData_m2 <- sum((testData$price - new_predictions)**2)
rss_testData_m2
