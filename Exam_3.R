library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggcorrplot)
library(caret)
library(Metrics)
#load data into dataframe, check if file exists, if not, download it
crime <- read.csv("crime.csv")
#Explore the dataset
#1.1. Print the first 5 rows of the data
head(crime)
#1.2. Print the number of rows and columns in the data
dim(crime)
#1.3. Print the column names of the data
names(crime)
#1.4. Print the number of missing values in the data
sum(is.na(crime))
#1.5. Print the summary statistics of the data
summary(crime)
#2. Rename the column @computed_region_f5dn_yrer as Region 1.
names(crime)[names(crime) == "X..computed_region_f5dn_yrer"] <- "Region_1"
#3. Rename the column @computed_region_yeji_bk3q as Region 2.
names(crime)[names(crime) == "X..computed_region_yeji_bk3q"] <- "Region_2"
#4. Rename the column @computed_region_92fq_4b7q as Region 3.
names(crime)[names(crime) == "X..computed_region_92fq_4b7q"] <- "Region_3"
#5. Rename the column @computed_region_sbqj_enih as Region 4.
names(crime)[names(crime) == "X..computed_region_sbqj_enih"] <- "Region_4"
#6. See if there are any columns that are unnamed and drop them.
if (any(colnames(crime) == "X")) {
    crime <- crime[, -which(names(crime) %in% "X")] 
}
#7. Prep_sex column has M or F as values change them to 1 or 0 accordingly
crime$perp_sex[crime$perp_sex == "M"] <- "1"
crime$perp_sex[crime$perp_sex == "F"] <- "0"
#8. Check for null and impute with most common values in that column.
cbind(lapply(lapply(crime, is.na), sum))
crime$Region_1[is.na(crime$Region_1)] <- unique(which.max(tabulate(match(crime$Region_1, unique(crime$Region_1))))) 
crime$Region_2[is.na(crime$Region_2)] <- unique(which.max(tabulate(match(crime$Region_2, unique(crime$Region_2))))) 
crime$Region_3[is.na(crime$Region_3)] <- unique(which.max(tabulate(match(crime$Region_3, unique(crime$Region_3))))) 
crime$Region_4[is.na(crime$Region_4)] <- unique(which.max(tabulate(match(crime$Region_4, unique(crime$Region_4))))) 
#9.Summarize the data and check for outliers and either correct them or remove them as implied. 
summary(crime) 
crime_dist <- crime[, c("Region_1", "Region_2", "Region_3", "Region_4")]
boxplot(crime_dist)
sum(duplicated(crime))
crime$age_group <- factor(crime$age_group)
out <- boxplot(crime$age_group, plot=FALSE)$out
#delete outliers from the dataframe
crime <- crime[-out,]
#11.Which borough of NYC has highest crime rate?
high_cr <- crime %>% group_by(ofns_desc) %>% summarize(count=n()) %>% arrange(count)  %>% tail(1) 
#12.What are the ranges for various crimes? 
crime_types <- unique(crime$ofns_desc)
#create dataframes with the each crime type, and boxplot the data
for (i in crime_types) {
    crime_type <- crime[crime$ofns_desc == i,]
    boxplot(crime_type$age_group)
    file <- paste("crime_", i, ".png", sep="")
    print(file)
    save.image(file)
}
#13.Have the crimes increased or decreased over time?
crime_date <- crime %>% mutate(arrest_date = as.Date(arrest_date)) %>% arrange(arrest_date)
hist(crime_date$arrest_date, breaks=seq(min(crime_date$arrest_date), max(crime_date$arrest_date), length=10))
#14. Use regression to predict “arrest_precint” values.
#14.1. Plot the correlations to get an idea about key features for regression.
correlations <- ggcorrplot(crime %>% select_if(is.numeric) %>% cor(.))
#14.2. Split the data into training and testing data (80:20)
train <- crime[sort(sample(nrow(crime), nrow(crime)*.8)),]
test <- crime[-sort(sample(nrow(crime), nrow(crime)*.8)),]
#14.3. Build and test a regression model using only the features that have high correlation with “arrest_precinct” column. 
#(arrest_key, latitude, longitude, arrest_precinct, jurisdiction_code, region_4)
model <- lm(data = train,arrest_precinct ~ arrest_key+latitude+longitude+jurisdiction_code+Region_4)
summary(model)
#14.4. Calculate the accuracy of this model for both training and testing data.
accuracy_train <- summary(model)$adj.r.squared
accuracy_test <- summary(model, data = test)$adj.r.squared
#15.Now use all the features and see if the R-squared value is improved.
model <- lm(data = crime,arrest_precinct ~ arrest_key+latitude+longitude+jurisdiction_code+Region_4)
#16. By adding and removing features in your model, identify the key features that contribute to the model. Basically the features that will improve R squared value. Make sure you test the accuracy for both training and testing data as you add and delete features in your model. 
#16.1. Add and remove features in your model.
model <- lm(data = crime,arrest_precinct ~ Region_4)
#16.2. Calculate the accuracy of this model for both training and testing data.
summary(model, data = train)$adj.r.squared
summary(model, data = test)$adj.r.squared
#17. Once a satisfying model is built plot test vs predicted values to see how your model has performed. 
predicted <- predict(model, test)
plot(predicted, test$arrest_precinct, xlab="Predicted", ylab="Actual")
