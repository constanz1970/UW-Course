

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

SLR_Intercept_Slope(trainData$sqft_living, trainData$price)

lm(price~sqft_living, trainData)

inputMean <- mean(trainData$sqft_living)
outputMean <- mean(trainData$price)

difMean <- sum(((trainData$sqft_living - inputMean)*(trainData$price - outputMean))) / nrow(trainData)
difMean
