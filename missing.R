
#Missing Data Handling in R

#Install Necessary Packages

#Install and Load Relevant Packages
#install.packages("mice")     #ignore if already installed
#install.packages("VIM")    #ignore if already installed
#install.packages("Amelia")    #ignore if already installed

library(VIM)
library(mice)
library(MASS)
library(tidyverse)
library(Amelia)


#Load and View birthwt Data from MASS library

data("birthwt")
View(birthwt)
nrow(birthwt) # Number of rows in the data
sum(is.na(birthwt$bwt)) # Check how many missing observations are there in bwt


#Introduce Random Missing Values in bwt column of the data (For Learning Purposes)
set.seed(555)
miss_row = sample(nrow(birthwt), 15, replace = FALSE)

data1 = birthwt %>% 
  mutate(bwt = replace(bwt, miss_row, NA))

sum(is.na(data1$bwt)) # Now bwt has 15 missing values

#visualizing missing values in the data
aggr(data1)

#Let us move on to handling these missing values

#Solution 1: Removing Missing Values (Not Recommended)
data1 %>%
  drop_na(bwt) %>%
  View()

#we have not made changes in the data with the previous code


# Solution 2: Replacing Missing Values with Mean/Median
#Replace with Mean
m = mean(data1$bwt, na.rm = TRUE)

data2 = data1 %>% 
  mutate(bwt = ifelse(is.na(bwt), m, bwt))

#let us plot the density plot with and without NA
# Create the density plot
plot(density(data2$bwt), 
     main = "Density Plot of Birth Weight",
     xlab = "Birth Weight",
     ylab = "Density",
     col = "black", 
     lwd = 1.5,
     xlim = c(min(data2$bwt, na.rm = TRUE), max(data2$bwt, na.rm = TRUE)),
     ylim = c(0, max(c(density(data2$bwt)$y, density(data1$bwt, na.rm = TRUE)$y))))
# Add the second density plot
lines(density(data1$bwt, na.rm = TRUE), 
      col = "red", 
      lty = 3, 
      lwd = 2)

# Add a legend
legend("topleft", 
       c("Without NA", "With NA"), 
       col = c("black", "red"), 
       lty = c(1, 3), 
       lwd = c(1.5, 2), 
       bty = "n",
       cex = 0.7,
       pt.cex = 0.7,
       )

#If your data is skewed then you may like to use median instead of mean as imputation
#Replace with Median
med = median(data1$bwt, na.rm = TRUE)

dataMed = data1 %>% 
  mutate(bwt = replace(bwt, is.na(bwt), med))


###### Solution - 2

#Using linear regression for predicting missing values
data1$race = as.factor(data1$race)     #convert race to factor variable

#run initial regression (expluding the 'low' variable)
m1 <- lm(bwt ~ . -low, data  = data1, na.action = na.omit)
summary(m1)

#remove insignificant variables from the model
m2 <- lm(bwt ~ lwt + race + smoke + ht + ui, data  = data1, na.action = na.omit)
summary(m2)

#our model is ready to make predictions

na_rows <- which(is.na(data1$bwt))      #rows with NA values in bwt
data_pred = data1[na_rows, -which(names(data1) == "bwt")]  #creating dataset for which bwt has NA values

predicted_bwt = predict(m2, data_pred)

data3 = data1 %>% 
  mutate(bwt = replace(bwt, na_rows, predicted_bwt))

# Density Plot for Linear Regression Replacement
plot(density(data3$bwt), 
     main = "Density Plot of Birth Weight (Regression Imputation)",
     xlab = "Birth Weight",
     ylab = "Density",
     col = "black", 
     lwd = 1.5)

lines(density(data1$bwt, na.rm = TRUE), 
      col = "red", 
      lty = 3, 
      lwd = 2)

legend("topleft", 
       c("Without NA", "With NA"), 
       col = c("black", "red"), 
       lty = c(1, 3), 
       lwd = c(1.5, 2), 
       bty = "n",
       cex = 0.8,
       pt.cex = 0.8)


##############################################
          #Solution 4
#K-Nearest Neighbor (kNN) Imputation
# Imputation using kNN method
#We will use VIM package for this

data4 <- kNN(data1, k = 5, variable = "bwt")


#############################################

             #Solution 5

#Multiple Imputation Using MICE


impu_vals = mice(data1, method = "pmm", m = 5)

#this uses Predictive Mean Matching Algorithm 
#this command created 5 datasets with different imputations
#we can choose any suitable one
#or we can take mean of imputed values in the five interations

data5 = complete(impu_vals, 1) #imputing from the first imputed dataset

###########################################
   #Multiple Imputation Using Amelia


#remove race from the data as it is a factor variables

data_wo_race = data1 %>% 
  dplyr::select(-race)

#main command
imps = amelia(data_wo_race, m = 5)

#this command uses expectation minimization (EM) and expectation minimization with bootstrapping (EMB) algorithm
#this command also created 5 datasets stored in imps

na_rows <- which(is.na(data1$bwt))  #rows with NA values

# Calculate the mean of the imputed values for each row
imputed_bwt_values <- sapply(imps$imputations, function(x) x$bwt[na_rows])


# Calculate the mean of the imputed values for each row
mean_imputed_bwt <- rowMeans(imputed_bwt_values)


# Replace the missing values in the original data with the mean imputed values

data6 = data1
data6$bwt[na_rows] <- mean_imputed_bwt


###########################################################

#plotting results from different methods

plot(density(data1$bwt , na.rm = T),
     main = "Density Plot of Birthweight",
     xlab = "Biethweight",
     Ylab = "Density",
     col = "black",
     lwd = 1.5,
     xlim = c(min(data2$bwt, na.rm = T), max(data2$bwt, na.rm = T)),
     ylim = c(0, max(c(density(data2$bwt)$y)))
)

lines(density(data2$bwt),
      col = "red",
      lty = 2,
      lwd = 1.5
)

lines(density(data3$bwt),
      col = "blue",
      lty = 3,
      lwd = 1.5
)

lines(density(data4$bwt),
      col = "cyan",
      lty = 4,
      lwd = 1.5)

lines(density(data5$bwt),
      col = "green",
      lty = 5,
      lwd = 1.5
)

lines(density(data6$bwt),
      col = "magenta",
      lty = 6,
      lwd = 1.5)

# Add a legend
legend("topleft", 
       c("Without NA", "mean replacement", "regression", "kNN", "MICE", "Amelia"), 
       col = c("black", "red", "blue", "cyan", "green", "magenta"), 
       lty = c(1,2,3,4,5,6), 
       lwd = c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5), 
       bty = "n",
       cex = 0.6,
       pt.cex = 0.6)

####



