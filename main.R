## Integer vector ID indicates the 332 monitor files 
Mean_Airpollutant<- function(directory, pollutant, id = 1:332) {
    p <- c()
    for(monitor in id){
    	## get path, indicate the location of the CSV files
        path <- paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "")
        ##get monitor data
        monitor_data <- read.csv(path)
        interested_data <- monitor_data[pollutant]
        ##get pollution mean
        p <- c(p, interested_data[!is.na(interested_data)])
    }
    ## Return the mean of the pollutant for called monitors list
    mean(p)
}

GoodData <- function(directory, id = 1:332) {
	##get the number of complete cases
	count_nobs <- function(fname) sum(complete.cases(read.csv(fname)))
	##get frames
	fnames <- list.files(directory, full.names=TRUE)[id]
	## the monitor ID and 'nobs' is the number of complete cases
	data.frame(id = id, nobs = unlist(lapply(fnames, count_nobs)))
}

PollutantCorrelation<-function(directory,threshold=0){
	##get path
	files<-list.files(directory,full.names = TRUE)
	##get monitor data
	dat <- vector(mode = "numeric", length = 0)
	for(i in 1:length(files)){
		temp<- read.csv(files[i],header=TRUE)
		temp<-temp[complete.cases(temp),]
		var<-nrow(temp)
		if(var>threshold){
			dat<-c(dat,cor(temp$nitrate,temp$sulfate))
		}
	}
	return(dat)
}