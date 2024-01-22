load("nlsy97.rdata")
dat = subset(
  nlsy97,
  wage > 0 &
    ASVAB > 0 & 
    hdegree > 0 &
    weight2017 > 0 &
    health2017 > 0 &
    height2002 > 0 &
    tenure2017 >0 &
    male == 1
)


dat$weight = dat$weight2017
dat$height = dat$height2002 / 100
dat$BMI = dat$weight / (dat$height)^2
dat$white_BMI = dat$white * dat$BMI
dat$black_BMI = dat$black *dat$BMI
dat$hispanic_BMI = dat$hispanic * dat$BMI
dat$mixed_BMI = dat$mixed * dat$BMI
dat$ln_wage = log(dat$wage)
dat$tenure = dat$tenure2017
dat$tenure_sq = (dat$tenure)^2

if(T){
  model_male = lm( 
    ln_wage ~ 
      1 + height + BMI + black + hispanic + mixed + black_BMI + hispanic_BMI + mixed_BMI + hdegree + ASVAB + tenure + tenure_sq,
    data = dat
  )
}

sink("summary_male_model")
summary(model_male)
sink()
