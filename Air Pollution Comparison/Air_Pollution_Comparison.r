# Read dataset 1999 and 2012
Air1999 <- read.table("Air1999.txt", comment.char = "#", header = FALSE, sep = "|", na.strings = "")
Air2012 <- read.table("Air2012.txt", comment.char = "#", header = FALSE, sep = "|", na.strings = "", nrow =1304290)
# Define the column headers
columns <- readLines("Air1999.txt", 1)
columns <- strsplit(columns, "|", fixed = TRUE)
names(Air2012) <- names(Air1999) <- make.names(columns[[1]])
# All sample values in 1999 and 2012
bplot1999 <- Air1999$Sample.Value
bplot2012 <- Air2012$Sample.Value
# Detremined the mean of PM for each state in 1999 and 2012.
mean1999 <- with(Air1999, tapply(Sample.Value, State.Code, mean, na.rm = TRUE))
mean2012 <- with(Air2012, tapply(Sample.Value, State.Code, mean, na.rm = TRUE))
# Make data frames for states and years
dFrame1999 <- data.frame(state = names(mean1999), mean = mean1999)
dFrame2012 <- data.frame(state = names(mean2012), mean = mean2012)
# Merge them into one plot data
merges <- merge(dFrame1999, dFrame2012, by = "state")
# Plot the datasets to compare
par(mfrow = c(1, 2))
# Boxplots of all values in 1999 and 2012.
boxplot(bplot1999, bplot2012, names=c("1999", "2012"), xlab = "Year", ylab = "PM Sample Data")
# Comparsion of 1999 and 2012 mean by states
with(merges, plot(rep(1, 52), merges[, 2], xlim = c(0.5, 2.5), ylim = range(merges[, 2], merges[, 3]), xaxt = "n", xlab = "Year", ylab = "Mean PM by States"))
with(merges, points(rep(2, 52), merges[, 3], xlim = c(0.5, 2.5), ylim = range(merges[, 2], merges[, 3]), xaxt = "n", xlab = "Year", ylab = "Mean PM by States"))
# Connect mean1999 and mean2012 to make it clear and near the example shown in class.
segments(rep(1, 52), merges[, 2], rep(2, 52), merges[, 3])
axis(1, c(1, 2), c("1999", "2012"))
mtext("Conclusion: Most states' PM levels from 1999 to 2012 has decayed. But some outliers haven't.", side = 3, line = -2, outer = TRUE)