#-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
#main. The goal of this exam is to get the data, 
#plot it and graph it.
#
#@author Cai Student
#@email caiy203@potsdam.edu
#@course CIS 235 Data Analysis and Visualization
#@Midterm 2 - 2
#@due 04/05/2021
#-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
if (!require(dplyr)) { 
	install.packages("dplyr") 
	library(dplyr) 
	}
fZip <- "Exam2Data.zip"
# unzipping the file
unzip(fZip)
PM25data <- "DATA/PM25data.rds"
PM25 <- readRDS(PM25data)
NY <- subset(PM25, PM25$fips == "36061")
totalNY <- aggregate(Emissions ~ year, NY, sum)
#plot
plot(totalNY$year, totalNY$Emissions, type = "b", main ="Total NY PM 2.5 Emissions by Year", xlab = "Year", ylab ="Emissions")