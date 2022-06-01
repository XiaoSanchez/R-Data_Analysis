#-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
#main. The goal of this exam is to get the data, 
#plot it and graph it.
#
#@author Cai Student
#@email caiy203@potsdam.edu
#@course CIS 235 Data Analysis and Visualization
#@Midterm 2 - 5
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
APTCoal <- APT[grepl("Coal", APT$Short.Name, ignore.case = T),]
PM25Gas <- PM25[PM25$SCC %in% APTGas$SCC,]
PM25Coal <- PM25[PM25$SCC %in% APTCoal$SCC,]
totalGas <- aggregate(Emissions~year, PM25Gas, sum)
totalCoal <- aggregate(Emissions~year, PM25Coal, sum)
totals <- totalGas %>%  mutate(Type = 'Natural Gas') %>% bind_rows(totalCoal %>% mutate(Type = 'Coal'))
# plot
ggplot(totals, aes(x = year, y = Emissions, col = Type)) +  geom_line() + geom_point() + ggtitle("Total US PM 2.5 Natural Gas Emission by Type and Year")
