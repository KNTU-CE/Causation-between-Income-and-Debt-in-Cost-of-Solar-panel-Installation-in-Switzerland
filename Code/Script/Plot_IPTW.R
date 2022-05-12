### Load packages ###
library(tableone)
library(Matching)
library(MatchIt)
library(ipw)
library(survey)
library(sandwich)

### Define variables ###
income_seq <- seq(58000, 88000, by=1000)
output <- rep(NA, length(income_seq))
p_values <- rep(NA, length(income_seq)) 


for (index in 1:length(income_seq))
{
  ### Read data ###
  data <- read.csv(file='C:/Users/amirr/Downloads/Temp Backup/municipalities.csv')
  # data <- read.csv(file='C:/Users/amirr/Downloads/Temp Backup/districts.csv')
  # data <- read.csv(file='C:/Users/amirr/Downloads/Temp Backup/cantons.csv')
  
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
  psmodel <- glm(treatment~houses+elec+c_elec+m_c_elec+area+people,
                 family = binomial(link = "logit"))
  
  ### Value of propensity score for each subject ###
  ps <-predict(psmodel, type="response")
  
  ### Create weights ###
  weight<-ifelse(treatment==1,1/(ps),1/(1-ps))
  
  ### Apply weights to data ###
  weighteddata<-svydesign(ids = ~ 1, data=mydata, weights = ~ weight)
  
  ### Weighted table 1
  weightedtable <-svyCreateTableOne(vars = xvars, strata = "treatment",
                                    data = weighteddata, test=FALSE)
  
  weightmodel <- ipwpoint(exposure = treatment, family = "binomial", link = "logit",
                          denominator = ~ houses+elec+c_elec+m_c_elec+area+people,
                          data = mydata, trunc = 0.01)
  summary(weightmodel$weights.trun)
  ipwplot(weights = weightmodel$weights.trun, logscale = FALSE,
          main = "weights", xlim = c(0, 12))
  mydata$wt<-weightmodel$weights.trun
  msm <- (svyglm(debt ~ treatment, design = svydesign(~ 1, weights = ~wt, data =mydata)))
  output[index] <- coef(msm)
  p_values[index] <- summary(msm)$coefficients[8]
  # confint(msm)
}  

### Plots ###
plot(income_seq, output, 
     main = "Causality of Income over Debt within Swiss Municipalities (IPTW)",
     ylab = "Difference in Debt",
     xlab = "Income Threshold")
print(mean(output))
plot(income_seq, p_values, 
     main = "P-values in study over Swiss Municipalities (IPTW)",
     ylab = "P-value",
     xlab = "Income Threshold")
