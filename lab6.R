data_treatvscontrol=read.csv("noncompliance_treat.csv")
data_treatvscontrol
nrow(data_treatvscontrol)
summary(lm(purchased_aug2008 ~ treatment_attempt_call, data=data_treatvscontrol))
alpha = mean(data_treatvscontrol$contacted[data_treatvscontrol$treatment_attempt_call==1])
summary(lm(purchased_aug2008 ~ treatment_attemp_call, data=data_treatvscontrol))$coefficient[2,1:2]
summary(lm(purchased_aug2008 ~ treatment_attempt_call, data=data_treatvscontrol))$coefficient[2,1:2]/alpha
install.package("AER")
library((AER))
summary(ivreg(purchased_Aug2008 ~ contacted | treatment_attempt_call, data=data_treatvscontrol))
summary(lm(purchased_aug2008 ~ / alpha))