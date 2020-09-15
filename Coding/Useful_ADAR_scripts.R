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

######################################### LINEAR MODELS ####

#necessary packages
if (!require(car)) install.packages("car"); library(car)                # Companion to Applied Regression: Package by John Fox accompanying his book; provides additional functions and plots for lm's and glm's
if (!require(effects)) install.packages("effects"); library(effects)    # graphical and tabular effect displays for fitted models
if (!require(corrplot)) install.packages("corrplot"); library(corrplot) # used to plot a correlation matrix
if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)    # graphics package 
if (!require(estimability)) install.packages("estimability"); library(estimability)    # effect plots

library(foreign) # Read Data Stored by Minitab, S, SAS, SPSS, Stata, Systat, Weka, dBase, ...
library(lattice) # graphics package

#----------------- exploring the data - possibly for pre matching analysis --------------------
#required data
dataceo <- read.table("http://statmath.wu.ac.at/~wurzer/ADAR/Data/dataceo.txt", header = TRUE)

complete.cases(dataceo) # are the single cases (= observational units = rows) complete (no NA's)?
table(complete.cases(dataceo)) # table giving TRUE (complete) and FALSE (NA's present) cases

xyplot(salary ~ assets)       # scatterplot using lattice package (could also be enhanced)

             
pairs(dataceo[c(1, 3, 4, 7)]) # scatterplot matrix of the involved variables (pairwise)     
pairs(dataceo[c(1, 3, 4, 7)], panel = panel.smooth) # add lowess smooth to each panel of the display
pairs(dataceo[c(1, 3, 4, 7)], lower.panel = panel.smooth, upper.panel = NULL) # no panels above the diagonal
# to see how one can further customize the panel functions, see ?pairs for examples
scatterplotMatrix(~ dataceo$salary + dataceo$tenure + dataceo$age + dataceo$assets) # 'car' package version
scatterplotMatrix(~ dataceo$salary + dataceo$tenure + dataceo$age + dataceo$assets, diagonal = list(method = "boxplot")) # boxplots instead of 

#----- this could be quite useful for pre matching analysis - comparing the two groups --------
cor.dataceo <- cor(dataceo[c(1, 3, 4, 7)]) # compute a correlation matrix
corrplot(cor.dataceo, method = "ellipse") # visualize correlation matrix ('corrplot' package)
corrplot(cor.dataceo, method = "number", type = "lower") # see ?corrplot for possible customizations

#----------------- Regression Diagnostics - use this to argue for logged DV/count DV -----------------------------------------------------------

regfit2.1 <- lm(salary ~ tenure + age + assets, data = dataceo)

qqnorm(residuals(regfit2.1)) # QQ-Plot to identify nonnormal errors
qqline(residuals(regfit2.1)) # add a line to the QQ-Plot that represents a normal distribution
shapiro.test(residuals(regfit2.1)) # highly significant -> indication of nonnormal errors
ks.test(residuals(regfit2.1), "pnorm") # highly significant -> indication of nonnormal errors, too

qqPlot(regfit2.1) # includes a pointwise 95% confidence envelope based on a parametric bootstrap
qqPlot(regfit2.1, simulate = FALSE) # no parametric bootstrap, the confidence envelope looks quite different now -> interesting: highlights outliers

# this won't work too well as I hardly have any metric variables
residualPlots(regfit2.1) # bottom right plot: curved trend (should be linear) -> fit not adequate; all lack-of-fit tests (for linearity) 

#--------------- Outlier Diagnostics - use this to identify outliers in my own research -----------------------------------------------
regfit2.3 <- lm(log(salary) ~ log1p(tenure) + age + log(assets), data = dataceo)

residualPlots(regfit2.3, id = TRUE)
qqPlot(regfit2.3, id = TRUE)
outlierTest(regfit2.3)

#test with felm package
library(lfe)

regfit2.3_lfe <- felm(log(salary) ~ log1p(tenure) + age + log(assets), data = dataceo)

#the below don't work in conjunction with lfe package
residualPlots(regfit2.3_lfe, id = TRUE)
qqPlot(regfit2.3_lfe, id = TRUE)
outlierTest(regfit2.3_lfe)



#-------------------------------------- THESE DO work with LFE--------------------------------------------------------------------------------------
qqnorm(residuals(regfit2.3_lfe))
qqline(residuals(regfit2.3_lfe))

# Creating the projection matrix to determine the leverage of each observation
X <- model.matrix(regfit2.3_lfe)
P = X %*% solve(t(X) %*% X) %*% t(X)

if (!require(faraway)) install.packages("faraway"); library(faraway)   
# Create `labs` (labels) for 1 through 1704 observations
halfnorm(diag(P), labs = 1:447, ylab = 'Leverages', nlab = 1)

#Using the broom package
if (!require(broom)) install.packages("broom"); library(broom) 


broom::tidy(regfit2.3_lfe)    #getting the summary statistics in a table form that can be plotted
augment(regfit2.3_lfe)        #receiving overview of a number of important information on outliers
glance(regfit2.3_lfe)         #to get a number of fit statistics

#### back to ADAR script #####

infIndexPlot(regfit2.3, id = TRUE)  #for analysing the leverage points in linear regressions. Description in onenote diagnostics
influencePlot(regfit2.3, id = TRUE) # all in one: x-axis: hat values; y-axis: studentized residuals; areas of circles: proportional to the value of Cook's distance, 
# points with largest hat values, Cook's distances, or studentized residuals are flagged

#----------------- Refining the models --------------------#

regfit2.4 <- update(regfit2.3, subset = -c(100)) # take previous model regift2.3, but without observation no. 100
compareCoefs(regfit2.3, regfit2.4) # compare the coefficients of the two models
cC <- as.data.frame(compareCoefs(regfit2.3, regfit2.4, print = FALSE))
round(cbind(coef = cC$'Model 2' / cC$'Model 1', se = cC$'SE 2' / cC$'SE 1') * 100, 2)
# Eliminating just this single one out of 447 CEO's increases the log(assets) coefficient by 5%, the 
# log1p(tenure) coefficient by almost 10% and the age coefficient by over 40%. Standard errors 
# aren't affected that much, but all go down by about 5% -> higher precision of the estimation 

#---------------------- interpreting the model -------------------------#


confint(regfit2.3_lfe)    #getting the 5% confidence intervals for the model

plot(effect("age", regfit2.4))
eff.regfit2.4 <- allEffects(regfit2.3_lfe) #good for understanding moderator effects/interaction terms
eff.regfit2.4
plot(allEffects(regfit2.4))   #issue here is that the x-axis is still on normal scale
# using the log-scale on the x-axis leads to a graphical representation of the linear relationship used in the model
plot(effect("log(assets)", regfit2.4), transform.x = list(assets = list(trans = log, inverse = function(x) exp(x))), 
     ticks.x = list(assets = list(at = c(1e2, 1e3, 1e4, 1e5))))
# but log and exp cause problems with the plot for tenure:
plot(effect("log1p(tenure)", regfit2.4), transform.x = list(tenure = list(trans = log, inverse = function(x) exp(x))), 
     ticks.x = list(tenure = list(at = c(10, 20, 30, 40, 50, 60))))
# solution: use log1p instead of log and expm1 instead of exp(x)
plot(effect("log1p(tenure)", regfit2.4), transform.x = list(tenure = list(trans = log1p, inverse = function(x) expm1(x))), 
     ticks.x = list(tenure = list(at = c(10, 20, 30, 40, 50, 60))))
plot(allEffects(regfit2.4), transform.x = list(tenure = list(trans = log1p, inverse = function(x) expm1(x)), 
                                               assets = list(trans = log, inverse = function(x) exp(x))), 
     ticks.x = list(tenure = list(at = c(0, 10, 20, 30, 40, 60)), assets = list(at = c(1e2, 1e3, 1e4, 1e5))))

#-------------------- prediction ---------------------------------------#

predict(regfit2.4) # prediction for all observations in the data set
comp.salary <- data.frame(obs = log(dataceo$salary)[-100], est = predict(regfit2.4)) # compare observed and fitted/predicted values
head(comp.salary)
head(resid(regfit2.4)) # difference between observed and predicted values

# ----------------------------------3. Analysis Of Variance - Potentially for hypothesis tests -------------------------------------------

# 3.1 One-Factor Anova:
dataceo.aov <- data.frame(dataceo[c(2, 5:6)])
# generate artificial data set with categorized sales and profits:
dataceo.aov$sales <- cut(dataceo.aov$sales, breaks = c(0, 10000, 20000, max(dataceo.aov$sales)), include.lowest = TRUE, labels = c("low", "medium", "high"))
dataceo.aov$profits <- cut(dataceo.aov$profits, breaks = c(min(dataceo.aov$profits), 0, max(dataceo.aov$profits)), include.lowest = TRUE, labels = c("no", "yes"))
head(dataceo.aov)

boxplot(totcomp ~ profits, data = dataceo.aov) # totcomp = total compensation; higly skewed
plot(totcomp ~ profits, data = dataceo.aov)    # boxplot is automatically chosen for these variable types
boxplot(log(totcomp) ~ profits, data = dataceo.aov)

leveneTest(log(totcomp) ~ profits, data = dataceo.aov) # H0 of equal variances cannot be rejected

anovafit3.1 <- aov(log(totcomp) ~ profits, data = dataceo.aov) # assumptions: normal distribution in all groups, equal variances      
summary(anovafit3.1)                                           # F-Test

anovafit3.2 <- lm(log(totcomp) ~ profits, data = dataceo.aov) # regression perspective
summary(anovafit3.2)

# # # # # Keyword: Design Matrix/Coding schemes -> accompanying slides# # # # # 
dataceo.aov.rel <- dataceo.aov
dataceo.aov.rel$profits <- relevel(dataceo.aov.rel$profits, ref = "yes")
(anovafit3.2.rel <- summary(lm(log(totcomp) ~ profits, data = dataceo.aov.rel)))

dataceo.aov$profits
as.numeric(dataceo.aov$profits)
dataceo.aov.rel$profits             # releveled variable - the level names stay the same, but the numeric codes have changed:
as.numeric(dataceo.aov.rel$profits)
# # # # # 

anovafit3.3 <- oneway.test(log(totcomp) ~ profits, data = dataceo.aov) # no equal variances assumed   
anovafit3.3

# t-test
anovafit3.4 <- t.test(log(totcomp) ~ profits, data = dataceo.aov) # assumption: unequal variances -> compare test statistic to anovafit3.3
anovafit3.4

anovafit3.5 <- t.test(log(totcomp) ~ profits, data = dataceo.aov, var.equal = TRUE) # compare test statistic and p-value to anovafit3.1 and anovafit3.2
anovafit3.5
summary(anovafit3.1)
summary(anovafit3.2)

# 3.2 Two-Factor Anova - can use this to plot group and treatment interaction?:
boxplot(log(totcomp) ~ sales + profits, horizontal = TRUE, data = dataceo.aov) # outliers, unequal group sizes
abline(v = median(log(dataceo.aov$totcomp)), col = "red")
abline(h = 3.5)
bartlett.test(log(totcomp) ~ interaction(sales, profits), data = dataceo.aov) # Null hypothesis of equal variances has to be rejected 
# (we continue here, but usually, would have to check the problematic observations)

anovafit3.6 <- lm(log(totcomp) ~ sales*profits, data = dataceo.aov) # Full model/saturated model including all possible effects 
# -> starting point for backward selection of terms
summary(anovafit3.6)                                        
par(mfrow = c(2, 1))
with(dataceo.aov, interaction.plot(sales, profits, log(totcomp))) # interaction plot
with(dataceo.aov, interaction.plot(profits, sales, log(totcomp))) # interaction plot, x.factor and trace.factor exchanged (see ?interaction.plot)
par(mfrow = c(1, 1))

plot(allEffects(anovafit3.6)) # effect plots for categorical variables; confidence bars instead of bands
anova(anovafit3.6) # sequential "type-I tests" - problematic in unbalanced designs
Anova(anovafit3.6) # "type-II tests" - better choice than anova, each term is tested after all the others 


