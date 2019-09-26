options(scipen = 999)

WTP = 1000 # willingness to pay threshold
WTP_compare1 = 500
Num_SIm = 10000

################## PSA #####################################

serial = c(seq(1,Num_SIm))
simdata = as.data.frame(serial)

## treatment difference in effect
simdata$Effect_prop_diff_A_minus_B = rnorm(
  n = Num_SIm, mean = 0.7398587 * 100, sd = 0.03579797 * 100) -
  rnorm(n = Num_SIm, mean = 0.654222 * 100, sd = 0.03861657 * 100)

## ER visits
simdata$ER_visits_Treatment_A = sample(c(0,1,1,1,2,2,2,3), size = Num_SIm, replace = T)
simdata$ER_visits_Treatment_B = sample(c(0,1,1,1,2,2,2,3), size = Num_SIm, replace = T)
simdata$ER_visits_cost = rnorm(n=Num_SIm, mean = 600, sd = 50)
simdata$ER_visits_cost_Treatment_A = simdata$ER_visits_Treatment_A * simdata$ER_visits_cost ###Treatment_A1
simdata$ER_visits_cost_Treatment_B = simdata$ER_visits_Treatment_B * simdata$ER_visits_cost  

## Hospitalization
simdata$ALOS_Treatment_A = sample(c(0,1,2), size = Num_SIm, replace = T)
simdata$ALOS_Treatment_B = sample(c(0,1,2), size = Num_SIm, replace = T)
simdata$LOS_cost = rnorm(n=Num_SIm, mean = 1600, sd = 150)
simdata$ALOS_cost_Treatment_A = simdata$ALOS_Treatment_A * simdata$LOS_cost ###Treatment_A2 
simdata$ALOS_cost_Treatment_B = simdata$ALOS_Treatment_B * simdata$LOS_cost

## Medication
simdata$units_Treatment_A = sample(c(5,6,7,8,9), size = Num_SIm, replace = T)
simdata$units_Treatment_B = sample(c(5,6,7,8,9), size = Num_SIm, replace = T)
simdata$Treatment_A_unit_cost = rnorm(n=Num_SIm, mean = 481, sd = 10) 
simdata$Treatment_B_unit_cost = rnorm(n=Num_SIm, mean = 170, sd = 10)

simdata$unitS_cost_Treatment_A = simdata$units_Treatment_A * simdata$Treatment_A_unit_cost  ### Treatment_A3
simdata$unitS_cost_Treatment_B = simdata$units_Treatment_B * simdata$Treatment_B_unit_cost

## total Cost
simdata$total_cost_Treatment_A = simdata$unitS_cost_Treatment_A + simdata$ALOS_cost_Treatment_A + simdata$ER_visits_cost_Treatment_A
simdata$total_cost_Treatment_B = simdata$unitS_cost_Treatment_B + simdata$ALOS_cost_Treatment_B + simdata$ER_visits_cost_Treatment_B
simdata$cost_diff = simdata$total_cost_Treatment_A - simdata$total_cost_Treatment_B

## calculate ICER
simdata$icer = simdata$cost_diff/simdata$Effect_prop_diff



write.csv(simdata, "simdata.csv")


simdata$model = WTP * simdata$Effect_prop_diff 

simdata$model_true = simdata$model - simdata$cost_diff 

simdata$CE = ifelse(test = simdata$model_true>0,yes = 1, no = 0 )

simdata$CE_col = ifelse(test =simdata$CE == 0,yes = 2, no = 3 )
table(simdata$CE)

plot(simdata$cost_diff ~ simdata$Effect_prop_diff, col=simdata$CE_col, cex=.8,pch=3,
     xlim=c(-30,30), ylim=c(-8000,8000))
abline(h = 0, lwd=2 )
abline(v=0, lwd=2 )

abline(c(0,WTP),col = 4, lwd = 3)


#abline(c(0,WTP_compare1), lwd=3)
table(simdata$CE)