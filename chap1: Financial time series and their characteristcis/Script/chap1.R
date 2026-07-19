##Environment setting


setwd("~/Admission for Phd/DATA/tsay/chap1")

library(pacman)

p_load(tidyverse,ggplot2,fBasics,dplyr,lubridate)


# Analysis of Financial time series ----

## Chapter 1 : Financial timeseries and their characteristics ----

### Example 1 : Understanding Returns----

data <- read.table(header = TRUE,file = "d-3stocks9908.txt")

str(data)

data$date= as.Date(as.character(data$date),format="%Y%m%d") # Specify the structure of the date

####  Express the simple returns in percentages. Compute the sample mean,standard deviation, skewness, excess kurtosis, minimum, and maximum of the percentage simple returns.-----

data$pct_r=data$axp * 100 #Transform the simple returns in percentages

basicStats(data$axp) #Compute the statistics

data$l_r=log(1+data$axp) # Transform the simple returns to log returns

data$pct_lr=data$l_r*100

basicStats(data$pct_lr)

t.test(data$pct_lr) #The result shows that the zero expected mean cannot be rejected 


### Example 2 : Understanding Returns level 2----

data <- read.table(header = TRUE,file = "m-gm3dx7508.txt")

data$date= as.Date(as.character(data$date),format="%Y%m%d") # Specify the structure of the date

#### What is the average annual log return over the data span?

data$l_r= log(data$gm+1)

mean(data$l_r)*12

#### Assume that there were no transaction costs. If one invested $1.00 on the S&P composite index at the beginning of 1975, what was the value of the investment at the end of 2008? ----

tail(data[c(1,5)])

exp((2008-1975+1)*mean(data$l_r)*12)

### Example 3 : Distributional properties using asset returns ----

data <- read.table(header = TRUE,file = "d-3stocks9908.txt")

#### Test the null hypothesis that the skewness measure of the returns is zero.  ----

data$l_r=log(1+data$axp) # Transform the simple returns to log returns

t=skewness(data$l_r)/sqrt(6/nrow(data))

pv <- 2 * pnorm(-abs(t))

pv # This shows that the daily log returns of American express is significant skewed  to the left

#### Test the null hypothesis that the kurtosis measure of the returns is zero.  ----

t1= kurtosis(data$l_r) / sqrt(24/nrow(data))

pv1= 2*(1-pnorm(t1))

pv1 # The p-value obtained is showing that the daily log returns of American express are significantly leptokurtic

normalTest(data$l_r,"jb")

t3= ((skewness(data$l_r))^2/sqrt(6/nrow(data))) + ((kurtosis(data$l_r)^2) / sqrt(24/nrow(data))) # we reject the normality assumption on the returns of American Express

### Example 4 : Distributional properties using asset returns Visually ----

data= read.table("d-caus.txt",header = T)

#### Compute the daily log return of each exchange rate.----

data$d_lr= log(data$rate/lag(data$rate))

data= na.omit(data)

#### Compute the sample mean, standard deviation, skewness, excess kurtosis, minimum, and maximum of the log returns of each exchange rate.----

basicStats(data$d_lr)

#### Discuss the empirical characteristics of the log returns of exchange rates----

normalTest(data$d_lr,"jb") # this is showing that the daily log returns of the exchange rate is not normaly distributed. We do have some negative extreme value more than positive due to its skeweness. It's also leptokurtic which shows that crash seem to occur 

#### Obtain a density plot of the daily long returns of dollar–euro exchange rate.----

ggplot(data=data) + 
  geom_density(aes(d_lr))+
  theme_bw()+
  labs(x="Daily log returns",title="Distribution of daily log returns of exchange rate between CAD and USD")
