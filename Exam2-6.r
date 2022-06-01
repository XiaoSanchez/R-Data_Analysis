#-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
#main. The goal of this exam is to get the data, 
#plot it and graph it.
#
#@author Cai Student
#@email caiy203@potsdam.edu
#@course CIS 235 Data Analysis and Visualization
#@Midterm 2 - 6
#@due 04/05/2021
#-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
if (!require(dplyr)) { 
	install.packages("dplyr") 
	library(dplyr) 
	}
if (!require(ggplot2)) { 
	install.packages("ggplot2") 
	library(ggplot2) 
	}
fZip <- "Exam2Data.zip"
# unzipping the file
unzip(fZip)
AirPollutdata <- "DATA/AirPollutantClassification.rds"
PM25data <- "DATA/PM25data.rds"
PM25 <- readRDS(PM25data)
APT <- readRDS(AirPollutdata)
NYMotor <- subset(PM25, PM25$fips == "36061" & PM25$type == "ON-ROAD")
totalNYMotor <- aggregate(Emissions ~ year, NYMotor, sum)
# plot
ggplot(totalNYMotor, aes(year, Emissions)) + geom_line() + geom_point() + ggtitle("NY PM 2.5 Motor Vehicle Emissions by Year")