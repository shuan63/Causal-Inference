setwd("~/Downloads")
data<- read.csv("rosca_cleaned.csv")
names(data)
#safebox-control
mean(data$outcome_amtinvest_healthproducts[data$treat_safe_box==1]) - mean(data$outcome_amtinvest_healthproducts[data$any_treatment==0])
mean(data$outcome_amtinvest_healthproducts[data$treat_locked_box==1])-mean(data$outcome_amtinvest_healthproducts[data$any_treatment==0])
mean(data$outcome_amtinvest_healthproducts[data$treat_health_pot==1])-mean(data$outcome_amtinvest_healthproducts[data$any_treatment==0])
mean(data$outcome_amtinvest_healthproducts[data$treat_health_savings==1])-mean(data$outcome_amtinvest_healthproducts[data$any_treatment==0])
my.lm <- lm(outcome_amtinvest_healthproducts ~ treat_safe_box + treat_locked_box + treat_health_pot + treat_health_savings, data = data) 
summary(my.lm)

cl   <- function(fm, cluster){
require(sandwich, quietly = TRUE)
require(lmtest, quietly = TRUE)
M <- length(unique(cluster))
N <- length(cluster)
K <- fm$rank
dfc <- (M/(M-1))*((N-1)/(N-K))
uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
coeftest(fm, vcovCL) }

cl(my.lm, data$rosca_id)

cl = function(ate, se) return(c(ate+1.96*se, ate-1.96*se))
cl(405.42, 80.56)
cl(405.42, 118.696)

my.lm2 <- lm(outcome_amtinvest_healthproducts ~ covariate_female + covariate_b1_age + covariate_provider + covariate_hyperbolic + covariate_pat_now_impat_later + covariate_max_discount + covariate_n_roscas, data = data)
summary(my.lm2)

my.lm3 <- lm(outcome_amtinvest_healthproducts ~ treat_safe_box + treat_locked_box + treat_health_pot + treat_health_savings + covariate_female + covariate_b1_age + covariate_provider + covariate_hyperbolic + covariate_pat_now_impat_later + covariate_max_discount + covariate_n_roscas, data = data)
cl(my.lm3, data$rosca_id)

