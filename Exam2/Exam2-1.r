#-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
#main. The goal of this exam is to get the data, 
#plot it and graph it.
#
#@author Cai Student
#@email caiy203@potsdam.edu
#@course CIS 235 Data Analysis and Visualization
#@Midterm 2 - 1
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
totalPM25 <- aggregate(Emissions~year, PM25, sum)
# plot
plot(totalPM25$year, totalPM25$Emissions, type = "b", col = 1, main ="Total US PM 2.5 Emissions by Year", ylab ="Emissions", xlab ="Year")