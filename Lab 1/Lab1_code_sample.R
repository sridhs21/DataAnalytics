library(readr)
library(EnvStats)

# set working directory (relative path)
setwd("C:/Users/swaro/OneDrive/Desktop/Classes/Data Analytics/Lab 1")

# read data
epi.data <- read_csv("epi_results_2024_pop_gdp.csv")

# view dataframe
View(epi.data)

# print summary of variables in dataframe
summary(epi.data$EPI.new)

# print values in variable
epi.data$EPI.new


######## Optional ########
## If you want to reference the variable without using the dataframe:

# attach dataframe
attach(epi.data)

# print values in variable
EPI.new

########################



### Explore Variable ###

EPI <- epi.data$EPI.new

# find NAs in variable - outputs vector of logical values, true if NA, false otherwise
NAs <- is.na(EPI)

EPI[which(NAs)]

# print values in variable
MHP <- epi.data$MHP.new

MHP

# find NAs inv variavle - outputs vector of logical values, true if NA, false otherwise
NAs <- is.na(MHP)

# print NAs
MHP[which(NAs)]

# take subset of NOT NAs from variable
MHP.noNA <- MHP[!NAs]

MHP.noNA

# filter for only values above 30
MHP.above30 <- MHP.noNA[MHP.noNA>30]

MHP.above30
  
# stats
summary(MHP.above30)

# boxplot of variable(s)
boxplot(EPI, MHP.above30, names = c("EPI","MHP"))


### Histograms ###

# histogram (frequency distribution)
hist(EPI)

# define sequence of values over which to plot histogram
x <- seq(20., 80., 10)
  
# histogram (frequency distribution) over range
hist(EPI, x, prob=TRUE)

# print estimated density curve for variable
lines(density(EPI,na.rm=TRUE,bw=1.)) # or try bw=“SJ”

# print rug
rug(EPI)

x <- seq(20., 80., 5)

# histogram (frequency distribution) over rabge
hist(EPI, x, prob=TRUE) 

# print estimated density curve for variable
lines(density(EPI,na.rm=TRUE, bw="SJ"))

# print rug
rug(EPI)


# histogram (frequency distribution) over rabge
hist(EPI.new, x, prob=TRUE) 

# range
x1<-seq(20,80,1)

# generate probability density values for a normal distribution with given mean and sd
d1 <- dnorm(x1,mean=45, sd=11,log=FALSE)

# print density values
lines(x1,d1)

# generate probability density values for a normal distribution with given mean and sd
d2 <- dnorm(x1,mean=64, sd=11,log=FALSE) 

# print density values
lines(x1,d2) 

# print density values
lines(x1,.5*d2)

### Empirical Cumulative Distribution Function ###

# plot ecdfs
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) 

plot(ecdf(MHP), do.points=FALSE, verticals=TRUE) 


### Quantile-quantile Plots ###

# print quantile-quantile plot for variable with theoretical normal distribuion
qqnorm(EPI); qqline(EPI)


# print quantile-quantile plot for random numbers from a normal distribution with theoretical normal distribution
x <- rnorm(500)
qqnorm(x); qqline(x)


# print quantile-quantile plot for variable with any theoretical distribution
qqplot(rnorm(180), EPI.new.sub, xlab = "Q-Q plot for norm dsn") 
qqline(EPI.new.sub)

# print quantile-quantile plot for 2 variables
qqplot(EPI, MHP, xlab = "Q-Q plot for EPI vs MHP") 

qqplot(x, EPI, xlab = "Q-Q plot for EPI vs MHP") 
qqline(EPI)

y <- rnorm(500)

qqplot(x, y, xlab = "Q-Q plot for EPI vs MHP") 
qqline(y)


## Statistical Tests

x <- rnorm(500)
y <- rnorm(500)

hist(x)
hist(y)

shapiro.test(x)
shapiro.test(y)

ad.test(x)
ad.test(y)

ks.test(x,y)

wilcox.test(x,y)

var.test(x,y)
t.test(x,y)

#Student Version

#assigning
ECO <- epi.data$ECO.new
BDH <- epi.data$BDH.new
summary(ECO)
summary(BDH)

#boxplots
boxplot(ECO)
boxplot(BDH)

BDH.noNA <- BDH[!is.na(BDH)]
BDH.above20 <- BDH.noNA[BDH.noNA>20]
BDH.above20

summary(BDH.above20)

boxplot(ECO, BDH.above20, names = c("ECO","BDH"))

#norm histograms
hist(ECO)
hist(BDH)

#histogram freq dist for ECO
x <- seq(floor(min(ECO)), ceiling(max(ECO)), 1)
hist(ECO, x, prob=TRUE)
lines(density(ECO,na.rm=TRUE,bw=1.))
rug(ECO)

x1 <- seq(floor(min(ECO, na.rm=TRUE)), ceiling(max(ECO, na.rm=TRUE)), 1)
d1 <- dnorm(x1, mean=mean(ECO, na.rm=TRUE), sd=sd(ECO, na.rm=TRUE), log=FALSE)
lines(x1, d1)

#histogram freq dist for BDH
x <- seq(floor(min(BDH)), ceiling(max(BDH)), 3)
hist(BDH, x, prob=TRUE)
lines(density(BDH,na.rm=TRUE,bw=3.))
rug(BDH)

x1 <- seq(floor(min(BDH, na.rm=TRUE)), ceiling(max(BDH, na.rm=TRUE)), 1)
d1 <- dnorm(x1, mean=mean(BDH, na.rm=TRUE), sd=sd(BDH, na.rm=TRUE), log=FALSE)
lines(x1, d1)

#ECDF plots
plot(ecdf(ECO), do.points=FALSE, verticals=TRUE) 
plot(ecdf(BDH), do.points=FALSE, verticals=TRUE) 


#QQ plots
qqnorm(ECO); qqline(ECO)
qqnorm(BDH); qqline(BDH)
qqplot(ECO, BDH, xlab = "Q-Q plot for ECO vs BDH") 

ECO.clean <- ECO[!is.na(ECO)]
BDH.clean <- BDH[!is.na(BDH)]

#norm stat tests

shapiro.test(ECO.clean)
shapiro.test(BDH.clean)

ad.test(ECO.clean)
ad.test(BDH.clean)

#stat tests
ks.test(ECO.clean, BDH.clean)

wilcox.test(ECO.clean, BDH.clean)

var.test(ECO.clean, BDH.clean)

t.test(ECO.clean, BDH.clean)
