#------------ Things to test from the ADAR scripts -----------------------------------------

#### Introduction scripts####

#------------- descriptives and Plotting ---------------------------------------------------

# necessary packages
library(MASS)                                  #package for basic statistics

# using iris data to test
data(iris)                                     
attach(iris)

# preprocessing the data
tenure.cat.10 <- cut(tenure, breaks = seq(0, 60, 10))             #0 is not included, missing values are generated!
tenure.cat.10
tenure.cat.10 <- cut(tenure, breaks = seq(0, 60, 10), include.lowest = TRUE)
tenure.cat.10
tenure.tab.10 <- table(tenure.cat.10)

#creating tables to investigate categorical variables
tenure.age <- table(tenure.cat.10, age.cat.10)                    #Contingency Table/Crossclassification
tenure.age
margin.table(tenure.age, 1)                                       #row sums
margin.table(tenure.age, 2)                                       #column sums
addmargins(tenure.age)                                            #frequency table including row/column sums and total (not useful for prop.table below!)

prop.table(tenure.age)                                            #relative freqencies
round(prop.table(tenure.age), digits = 3)                         #relative frequencies, rounded to 3 digits
round(prop.table(tenure.age) * 100, digits = 3)                   #total percentages
round(prop.table(tenure.age, 1), digits = 3)                      #table of relative frequencies(row-wise)
tenure.age.rel <- round(prop.table(tenure.age, 2), digits = 3)    #table of relative freqencies (column-wise)

# ---------------- missing values -------------------------------------------------------------
is.na(x)                             #logical condition for missing values in x
any(is.na(x) == TRUE)                #Any missing values in x?
which(is.na(x) == TRUE)              #Position of the missing values in x

#### Linear models ####

#necessary packages
if (!require(car)) install.packages("car"); library(car)                # Companion to Applied Regression: Package by John Fox accompanying his book; provides additional functions and plots for lm's and glm's
if (!require(effects)) install.packages("effects"); library(effects)    # graphical and tabular effect displays for fitted models
if (!require(corrplot)) install.packages("corrplot"); library(corrplot) # used to plot a correlation matrix
if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)    # graphics package 

library(foreign) # Read Data Stored by Minitab, S, SAS, SPSS, Stata, Systat, Weka, dBase, ...
library(lattice) # graphics package

#----------------- exploring the data --------------------------------------------------------
#required data
dataceo <- read.table("http://statmath.wu.ac.at/~wurzer/ADAR/Data/dataceo.txt", header = TRUE)

complete.cases(dataceo) # are the single cases (= observational units = rows) complete (no NA's)?
table(complete.cases(dataceo)) # table giving TRUE (complete) and FALSE (NA's present) cases

xyplot(salary ~ assets)       # scatterplot using lattice package (could also be enhanced)