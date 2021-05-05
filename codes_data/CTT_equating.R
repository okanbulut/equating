
install.packages("equate")
library("equate")

# Test form X
formx <- read.csv("formx.csv", header = TRUE)
formy <- read.csv("formy.csv", header = TRUE)

# Preview the data sets
head(formx)
head(formy)

# Combine the forms
formxy <- rbind(formx, formy)

# Add score frequencies to the data
data <- as.data.frame(table(formxy$score, formxy$form))
names(data) <- c("total", "form", "count")
head(data)

# Restructure the data as a frequency table
data_x <- as.freqtab(data[data$form == "x", c("total", "count")], scales = 20:50)
data_y <- as.freqtab(data[data$form == "y", c("total", "count")], scales = 20:50)

head(data_x)
head(data_y)

# Descriptive summary of the forms
rbind(form_x = summary(data_x), form_y = summary(data_y))

plot(data_x, main = "Bar plot of the test scores on form X")
plot(data_y, main = "Bar plot of the test scores on form Y")


### Mean Equating
mean_yx <- equate(data_x, data_y, type = "mean")
mean_yx

head(mean_yx$concordance)

# Save the concordance table
form_yx <- mean_yx$concordance

# Rename the first column to total
colnames(form_yx)[1] <- "total"

# Merge the concordance table to form x
data_xy <- merge(data_x, form_yx)
head(data_xy)

### Linear Equating
linear_yx <- equate(data_x, data_y, type = "linear")
linear_yx

plot(linear_yx)

linear_yx_boot <- equate(data_x, data_y, type = "linear", boot = TRUE, reps = 5)
plot(linear_yx_boot, out = "se")


### Equipercentile Equating
equi_yx <- equate(data_x, data_y, type = "equipercentile")

plot(equi_yx$concordance$yx ~ equi_yx$concordance$scale, type = "p", xlab = "Form X scores", 
     ylab = "Adjusted X Scores on Form Y", ylim = c(20, 55))
points(linear_yx$concordance$yx ~ linear_yx$concordance$scale, pch = 4)

equismooth_yx <- equate(data_x, data_y, type = "equipercentile", smooth = "loglin", degree = 3)

# Compare equating functions
plot(equi_yx, equismooth_yx, addident = FALSE)


# Nonequivalent Groups
negd <- read.csv("negd.csv", header = TRUE)
head(negd)

# Calculate total scores based on unique items
negd$total <- rowSums(negd[, 1:25])

# Calculate scores based on anchor items
negd$anchor <- rowSums(negd[, 26:35])

# Create frequency tables (total score range: 0-25; anchor score range: 0-10)
negd_x <- freqtab(negd[1:1000, c("total", "anchor")], scales = list(0:25, 0:10))
negd_y <- freqtab(negd[1001:2000, c("total", "anchor")], scales = list(0:25, 0:10))

plot(negd_x, xlab = "Total Scores Form X", ylab = "Common Anchor Scores Form X")

smooth_x <- presmoothing(negd_x, smoothmethod = "loglinear")
smooth_y <- presmoothing(negd_y, smoothmethod = "loglinear")

plot(smooth_x, xlab = "Total Scores Form X", ylab = "Common Anchor Scores Form X")


## Linear Tucker Equating
negd_tucker <- equate(negd_x, negd_y, type = "linear", method = "tucker")
negd_tucker$concordance


## Comparing Multiple Methods
# Nominal method with mean equating
negd_nom <- equate(negd_x, negd_y, type = "mean", method = "nom")

# Frequency method with equipercentile
negd_freq <- equate(negd_x, negd_y, type = "equip", method = "freq")

# Braun method with linear equating
negd_braun <- equate(negd_x, negd_y, type = "linear", method = "braun")

# Compare equated scores
round(cbind(xscale = 0:25, 
            nominal = negd_nom$concordance$yx,
            tucker = negd_tucker$concordance$yx, 
            freq = negd_freq$concordance$yx, 
            braun = negd_braun$concordance$yx), 2)

# Plot the results
plot(negd_tucker, negd_nom, negd_freq, negd_braun, lty=c(1,2,3,4),
     col=c("blue", "black", "red", "forestgreen"), addident = FALSE)




