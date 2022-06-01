#-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
#main. The goal of this exam is to get the data, 
#plot it and graph it.
#
#@author Cai Student
#@email caiy203@potsdam.edu
#@course CIS 235 Data Analysis and Visualization
#@Midterm 2 - 3
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
PM25data <- "DATA/PM25data.rds"
PM25 <- readRDS(PM25data)
totalNY <- PM25 %>% filter(fips == "36061") %>% group_by(type, year) %>% filter(year == 1999|2008) %>% summarize(Total.Emissions = sum(Emissions), .groups = 'drop')
# plot 
Plots <- ggplot(data = totalNY, aes(x = year, y = Total.Emissions, color = type)) + facet_grid(.~type) + geom_line() + geom_point() + ggtitle("Total Emissions in NewYork City, 1999-2008") 
print(Plots)