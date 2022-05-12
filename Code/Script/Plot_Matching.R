### Load packages ###
library(tableone)
library(Matching)
library(MatchIt)

### Read data ###
data <- read.csv(file='C:/Users/amirr/Downloads/Temp Backup/municipalities.csv')
# data <- read.csv(file='C:/Users/amirr/Downloads/Temp Backup/districts.csv')
# data <- read.csv(file='C:/Users/amirr/Downloads/Temp Backup/cantons.csv')

### Define variables ###
income_seq <- seq(48000, 88000, by=500)
output <- rep(NA, length(income_seq))
p_values <- rep(NA, length(income_seq)) 

for (index in 1:length(income_seq))
{
  ### Create a data-set with just these variables, for simplicity ###
  treatment <- as.numeric(data$income < income_seq[index])
  houses <- data$houses
  elec <- data$electricity_per_household
  c_elec <- data$consumed_electricity
  m_c_elec <- data$max_consumed_electricity
  area <- data$area_per_household
  people <- data$persons_per_household
  debt_rate <- data$debt_rate
  debt <- data$debt

  ### New data-set ###
  mydata<-cbind(treatment, houses, elec, c_elec, m_c_elec, area, people, debt_rate, debt)
  mydata<-data.frame(mydata)

  ### Covariates we will use ###
  xvars<-c("houses", "elec", "c_elec", "m_c_elec", "area", "people")

  ### Fit a propensity model, logistic regression ###
  psmodel<-glm(treatment~houses+elec+c_elec+m_c_elec+area+people+debt_rate+debt,
               family = binomial(), data=mydata)
  
  # summary(psmodel)

  ### Create propensity score ###
  pscore<-psmodel$fitted.values

  ### Do greedy matching ###
  psmatch<-Match(Tr=mydata$treatment, M=1, X=pscore, replace=FALSE, caliper=0.1)
  matched<-mydata[unlist(psmatch[c("index.treated", "index.control")]), ]
  matchedtab1<-CreateTableOne(vars=xvars, strata="treatment", data=matched, test=FALSE)


  # print(matchedtab1, smd=TRUE)

  ### Outcome analysis ###
  y_trt<-matched$debt_rate[matched$treatment==1]
  y_con<-matched$debt_rate[matched$treatment==0]

  ### Pairwise difference ###  
  diffy<-y_trt-y_con

  ### Paired t.test
  res <- t.test(diffy)
  output[index] <- res$estimate
  p_values[index] <- res$p.value
}

### Plots ###
plot(income_seq, output, 
     main = "Causality of Income over Debt within Swiss Municipalities (Matching)",
     ylab = "Difference in Debt Rate",
     xlab = "Income Threshold")

plot(income_seq, p_values, 
     main = "P-values in study over Swiss Municipalities (Matching)",
     ylab = "P-value",
     xlab = "Income Threshold")





