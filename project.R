rm(list=ls())
opar <- par(no.readonly=TRUE)
#install.packages("gmodels")
#install.packages("Hmisc")
#install.packages("ggplot2")
#install.packages("vcd")
#install.packages("car")
library(gmodels)
library(Hmisc)
library(ggplot2)
library(vcd)
library(car)
#getwd()
#settign the new current directory
#setwd("C:/Users/Kushu/Desktop/FRE")

#The following code setd the current directory to 
wd <- setwd(".")
setwd(wd)
#loading the data sets
#Using this data set we will try to predict the bitterness of the beer 
# using the alcohol content
beers = read.csv("beers.csv")
head(beers)

#Viewing the structure of the data
dim(beers)
#we can see that there are 8 columns and 2410 rows
str(beers)

#Let us look at the names of the columns
names(beers)
#Here,
#abv = alcoholic by volume
#ibu = international bitterness unit
unique(beers$style)
summary(beers)

#Now let us look at the NA values in the dataset
head(beers)
#looking at the head values we can see that there are a few NA values
colSums(sapply(beers, is.na))
#there are a lot of NAs in ibu and abv columns

#In the future we will be using these columns a lot
#if we simply ignore the rows with NA, we will lose most of the dataset
#Now, we will replace the NA values with the median values of both the columns
medABV <- median(beers$abv, na.rm = T)
medIBU <- median(beers$ibu, na.rm = T)
beers[is.na(beers$abv), "abv"] <- medABV
beers[is.na(beers$ibu),"ibu"] <- medIBU

#user defined function to print the mean, length and stddev of data for EDA
mystats <- function(x){
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  return(c(n=n, mean=m, stdev=s))
}


#mystats(beers$abv)
#mystats(beers$ibu)
myvars <- c("abv","ibu")
sapply(beers[myvars], mystats)

new_cols <- c("abv","ibu")
beers[new_cols]
#Printing the complete statistical description of the data with cols = abv & ibu
describe(beers[new_cols])

hist(beers$abv)
#Here, the data is more normal, it is almost a perfect bell curve

hist(beers$ibu)
#here we see that the data is right skewed
#Most of the data is in the range of 30-40

boxplot(beers$abv)
boxplot(beers$ibu)
#Here we can see that there are a lot of outliers in the data
barplot(table(beers$ibu), col="blue")

#Linear regression between abv & ibu
fit <- lm(ibu ~ abv, data=beers)
#summary of he fitted mddel
summary(fit)

#From the summary we can see that
#intercept = -6.769
#Residual standard error (RSE) is 17.31
#Therefore, we can generate the following formula
# yhat = -6.769 + 775.28*X + RSE

#plotting the graph to better understand  the data
plot(beers$abv,beers$ibu,
     main="abv vs ibu", 
     xlab="ibu", 
     ylab="abv")
abline(fit, col="red", lwd=2, lty=1)

lines(lowess(beers$abv, beers$ibu), 
      col="blue", lwd=2, lty=2)

fitted(fit)
residuals(fit)

#aggregating the types of beers with their styles
aggregate(beers[myvars], by=list(beers$style),mean)

#generating frequency tables
with(beers, table(ibu,abv))
mytable <- xtabs(~ ibu+abv, data=beers)
mytable # frequencies

margin.table(mytable,1) #row sums #ie across first dimension
margin.table(mytable, 2) # column sums

CrossTable(beers$ibu, beers$abv)

mytable <- xtabs(~ ibu+abv, data=beers)
mytable
chisq.test(mytable)
#p-value < .05 
#we reject null hypothesis 
#variables are not independent

scatterplotMatrix(beers, spread=FALSE, 
                  smoother.args=list(lty=2), 
                  main="Scatter Plot Matrix")
#Checking normality of the data

qqPlot(lm(ibu ~ abv, data=beers), 
       simulate=TRUE, 
       main="Q-Q Plot", labels=FALSE)
#We can see that the data is fairly normal

#outliertest
outlierTest(fit)
#This suggest that the observation in row 2391 and 148 are the most extreme.

b <- ggplot(beers)
b+ geom_point(aes(x = beers$abv,y=beers$ibu, size = beers$ounces, 
                  color = as.factor(beers$ounces)), alpha = 0.3) +
                  ggtitle("abv vs ibu") + 
                  xlab("abv")+ ylab( "ibu")

