#check if package is installed, if not, install it.
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
library(ggplot2)
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(tidyverse)
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
library(tidyr)
if(!require(ggcorrplot)) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
library(ggcorrplot)
if(!require(caTools)) install.packages("carTools", repos = "http://cran.us.r-project.org")
library(caTools)
#1. Load housing data (house_data.csv) from moodle into R.
if(file.exists("house_data.csv")){    #check if the file exists, if not create it.
  house = read.csv("house_data.csv")
}else{
    url = "https://cs-devel.potsdam.edu/caiy203/CIS325_Assignment_Simple_Linear_Regression/raw/branch/main/house_data.csv"
    #The url from moodle website where the data is stored can't be accessed directly, so I pushed it to cs-devel.potsdam.edu and then access it from there.
    house = read.csv(url, header=TRUE)
    write.csv(house, "house_data.csv")
    house[, -1]
    house$column_name = NULL
    }
#2. Explore the data
#2.1. Print the first 5 rows of the data
head(house)
#2.2. Print the number of rows and columns in the data
dim(house)
#2.3. Print the column names of the data
colnames(house)
#2.4. Print the number of rows in the data
nrow(house)
#2.5. Print the number of columns in the data
ncol(house)
#2.6. Print the number of missing values in the data
sum(is.na(house))
#missmap(house) mismatch between the data and the header
#3. Make sure to check if there are any NA’s
## contain 0 NA values in the data in summary
#4. Remove the columns – lat, long and view which do not contribute to the model.
house <- select(house, -lat, -long, -view)
#5. Take a subset of data if you can’t load everything may be 5000 rows
subset_house <- subset(house, select=c(id, price, bedrooms, bathrooms, sqft_living, sqft_lot, floors, waterfront, condition, grade, sqft_above, sqft_basement, yr_built, yr_renovated, zipcode))
#6. Use the glimpse function to explore the data
glimpse(subset_house)
#7. Get a summary of the data
summary(house)
#8. Plot a histogram of price distribution
price_hist <- ggplot(subset_house, aes(x=price)) + geom_histogram()
#9. Plot a histogram of number of bedroom distribution
bedrooms_hist <- ggplot(subset_house, aes(x=bedrooms)) + geom_histogram()
#10. Get a count of frequency of houses with the number of bedrooms.
#Basically how many houses with 1 bedroom, 2 bedrooms and so on.
bedrooms_count <- table(subset_house$bedrooms)
#11. Get a box plot to see how the price and number of bedrooms are associated.
bedrooms_box <- ggplot(data = subset_house, aes(x = as.factor(subset_house$bedrooms), y = price)) + geom_boxplot()+ ylim(0,1000000) + xlab("Bedrooms") + ylab("Price")
#12. Get a box plot to see how the price and number of bathrooms are associated.
bathrooms_box <- ggplot(data = subset_house, aes(x = as.factor(subset_house$bathrooms), y = price)) + geom_boxplot()+ ylim(0,1000000) + xlab("Bathrooms") + ylab("Price")
#13. Plot price against squrefeet
price_sqft <- ggplot(data = subset_house, aes(x = sqft_living, y = price)) + geom_point() + xlab("Square feet") + ylab("Price")
#14. Plot price against number of bedrooms
price_bedrooms <- ggplot(data = subset_house, aes(x = bedrooms, y = price)) + geom_point() + xlab("Bedrooms") + ylab("Price")
#15. Create a linear regression model with all the variables.
#Create a linear regression model with all the variables.
bedrooms_lm <- ggplot(data = subset_house, aes(x = bedrooms, y = price)) + geom_point() + stat_smooth(method = "lm") + xlab("Bedrooms") + ylab("Price")
bathrooms_lm <- ggplot(data = subset_house, aes(x = bathrooms, y = price)) + geom_point() + stat_smooth(method = "lm") + xlab("Bathrooms") + ylab("Price")
sqft_lm <- ggplot(data = subset_house, aes(x = sqft_living, y = price)) + geom_point() + stat_smooth(method = "lm") + xlab("Square feet") + ylab("Price")
sqft_lot_lm <- ggplot(data = subset_house, aes(x = sqft_lot, y = price)) + geom_point() + stat_smooth(method = "lm") + xlab("Square feet lot") + ylab("Price")
floors_lm <- ggplot(data = subset_house, aes(x = floors, y = price)) + geom_point() + stat_smooth(method = "lm") + xlab("Floors") + ylab("Price")
waterfront_lm <- ggplot(data = subset_house, aes(x = waterfront, y = price)) + geom_point() + stat_smooth(method = "lm") + xlab("Waterfront") + ylab("Price")
condition_lm <- ggplot(data = subset_house, aes(x = condition, y = price)) + geom_point() + stat_smooth(method = "lm") + xlab("Condition") + ylab("Price")
grade_lm <- ggplot(data = subset_house, aes(x = grade, y = price)) + geom_point() + stat_smooth(method = "lm") + xlab("Grade") + ylab("Price")
sqft_above_lm <- ggplot(data = subset_house, aes(x = sqft_above, y = price)) + geom_point() + stat_smooth(method = "lm") + xlab("Square feet above grade") + ylab("Price")
sqft_basement_lm <- ggplot(data = subset_house, aes(x = sqft_basement, y = price)) + geom_point() + stat_smooth(method = "lm") + xlab("Square feet basement") + ylab("Price")
yr_built_lm <- ggplot(data = subset_house, aes(x = yr_built, y = price)) + geom_point() + stat_smooth(method = "lm") + xlab("Year built") + ylab("Price")
yr_renovated_lm <- ggplot(data = subset_house, aes(x = yr_renovated, y = price)) + geom_point() + stat_smooth(method = "lm") + xlab("Year renovated") + ylab("Price")
zipcode_lm <- ggplot(data = subset_house, aes(x = zipcode, y = price)) + geom_point() + stat_smooth(method = "lm") + xlab("Zipcode") + ylab("Price")
model <- lm(data=subset_house,price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+condition+grade+sqft_above+sqft_basement+yr_built+yr_renovated+zipcode)
summary(model)
#16. Identify the significant variables.
#from the R square value and the Pr value, we can see that variables are significant are bedrooms, bathrooms, sqft_living, waterfront, grade, yr_built.
#17. Explain the coefficients. Which variable has high impact.
#The coefficient Pr value is 2e-16, which means the variable has a very high impact on the price. The coefficient Rsquare value greater when the variable has a high impact.(bedrooms, bathrooms, sqft_living, waterfront, grade, yr_built)
#18. Plot the correlations using ggcorrplot
correlations_ggcorr <- ggcorrplot(cor(data.frame(select(subset_house, -id))))