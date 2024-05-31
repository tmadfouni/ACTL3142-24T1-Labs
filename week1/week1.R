
# Reading in College.csv
# Remember to ensure your working directory (wd) matches where the csv is located
# Use setwd() to change it
getwd()
college = read.csv("College.csv")



# Gets the first column of college and puts it in the rownames
rownames(college) <- college[, 1]
View(college)

# Gets all the columns of college EXCEPT the 1st one, and re-assigns it to college
# Basically removing the first column
college <- college[, -1]
View(college)

# Provides numerical summary of different columns
summary(college)

# Produces matrix of pair-wise scatter plots of colums 2 to 10
# 2:10 notation creates a vector c(2,3,4,5,6,7,8,9,10)
pairs(college[,2:10])

# Initially plotting this doesn't work because Private is of the "character"
# type which means it's just a bunch of letters
# Using `factor` or `as.factor` converts it to a categorical variable (in R)
# which then allows us to plot it
plot(college$Private, college$Outstate)
college$Private = factor(college$Private)
plot(college$Private, college$Outstate)

# 2 different methods for creating Elite vector
# 2nd is simpler but the 1st introduces the more widely applicable concept
# of using logical statements (trues and falses) to index a vector (retrieve
# only some values of a vector).

# Elite <- rep("No", nrow(college))
# Elite[college$Top10perc > 50] <- "Yes"

Elite = ifelse(college$Top10perc > 50, "Yes", "No")

#Convert to factor
Elite <- as.factor(Elite)

# 3 different ways of adding the column Elite to the dataframe
# college <- data.frame(college, Elite)
# college = cbind(college, Elite)
college["Elite"] = Elite

# Creates 2x2 matrix of different histograms
# Intentionally looks bad, can use the col and bg arguments
# to change the colours :))
par(mfrow = c(2, 2), bg = "red")
hist(college$Outstate, breaks = 20)
hist(college$Outstate, breaks = 40, col = "blue")
hist(college$Outstate, breaks = 80, col = "black")
hist(college$Outstate, breaks = 100, col = "purple")


# If we read like this, it doesn't detect any NAs which we can check by functions below
auto = read.csv("Auto.csv")

is.na(auto)
sum(is.na(auto))
colSums(is.na(auto))

# Turns out horsepower was read as a character type, which doesn't seem correct
# By checking the values it turns out that this dataset represents NAs by ? rather than NA
# So we use the na.strings argument to change what R interprets as NAs
auto = read.csv("Auto.csv", na.strings = "?")

# Removes any rows with any NAs
auto = na.omit(auto)

# Can verify NAs have disappeared
colSums(is.na(auto))

# Origin is numeric, but should be a categorical variable (can try using plot() like
# we did with college$Private to see what happens before and after using factor())
auto$origin = factor(auto$origin)

# Applies the function `mean` over the 2nd margin (refer to help docs, just means over the columns)
# on the 3 specified columns
# The notation of auto[c("mpg", "cylinders")] is useful. Keep it in mind.
quant_var = c("mpg", "cylinders", "displacement")
apply(auto[quant_var], 2, mean)

