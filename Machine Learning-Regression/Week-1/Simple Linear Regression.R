

trainData = read.csv("C:/Users/constanz1970/Desktop/Coursera Projects/UW-Course/Machine Learning-Regression/Week-1/Data/kc_house_train_data.csv", header=T, sep=",")
testData = read.csv("C:/Users/constanz1970/Desktop/Coursera Projects/UW-Course/Machine Learning-Regression/Week-1/Data/kc_house_test_data.csv", header=T, sep=",")
str(trainData)

SLR_Intercept_Slope <- function(feature, output) {
        inputMean <- mean(feature)
        outputMean <- mean(output)
        
        covar <- sum((feature - inputMean)*(output - outputMean)) / nrow(trainData)
        varia <- sum((feature - inputMean)**2) / nrow(trainData)
        slope <- covar / varia
        intercept <- outputMean - slope*inputMean
        
        return(list(slope = slope, intercept = intercept))
}

sqft <- SLR_Intercept_Slope(trainData$sqft_living, trainData$price)
sqft

get_predictions <- function(feature, intercept, slope) {
        return(intercept + slope*feature)
}

predictedPrice <- get_predictions(2650, sqft$intercept, sqft$slope)
predictedPrice

getRSS <- function(feature, output, intercept, slope) {
        calculatedRSS <- sum((output - (intercept + slope*feature))**2)
        return(calculatedRSS)
}

rss_sqft_living <- getRSS(trainData$sqft_living, trainData$price, sqft$intercept, sqft$slope)
rss_sqft_living

get_estimated_Inputs <- function(output, intercept, slope) {
        estimatedInputs <- (output - intercept)/slope
        return(estimatedInputs)
}

estInputs <- get_estimated_Inputs(800000, sqft$intercept, sqft$slope)
estInputs


numberOfBedrooms <- SLR_Intercept_Slope(trainData$bedrooms, trainData$price)
numberOfBedrooms


rss_sqft_living <- getRSS(testData$sqft_living, testData$price, sqft$intercept, sqft$slope)
rss_bedrooms <- getRSS(testData$bedrooms, testData$price, numberOfBedrooms$intercept, numberOfBedrooms$slope)
c(rss_sqft_living, rss_bedrooms)




lm(price~sqft_living, trainData)

inputMean <- mean(trainData$sqft_living)
outputMean <- mean(trainData$price)

difMean <- sum(((trainData$sqft_living - inputMean)*(trainData$price - outputMean))) / nrow(trainData)
difMean
