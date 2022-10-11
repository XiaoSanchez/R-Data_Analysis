#-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
#main. The goal of this exam is to get the data, 
#plot it and graph it.
#
#@author Cai Student
#@email caiy203@potsdam.edu
#@course CIS 235 Data Analysis and Visualization
#@Midterm 2 - 4
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
APTGas <- APT[grepl("Natural Gas", APT$Short.Name, ignore.case = T),]
PM25Gas <- PM25[PM25$SCC %in% APTGas$SCC,]
totalGas <- aggregate(Emissions~year, PM25Gas, sum)
ggplot(totalGas, aes(year, Emissions)) +  geom_line() + geom_point() + ggtitle("Total US PM 2.5 Natural Gas Emission by Type and Year")