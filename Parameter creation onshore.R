
library(data.table)

# Cost calculations

# Medical consultation costs (MBS website)
c.gp.b.vr <- 38.20
c.gp.b.nonvr <- 21.00
c.gp.b.afterhours <- 49.80
c.gp.c.vr <- 73.95
c.gp.c.nonvr <- 38.00
c.gp.c.afterhours <- 85.30
c.spec.first <- 155.60
c.spec.review <- 77.90

# Medical assessment costs (MBS website)
c.qft.git <- 34.90
c.tst <- 11.20
c.cxr <- 47.15
c.liver <- 17.70
c.mcs <- 43.00

# Mid Medicine costs
c.inh.mid <- 14.60 # GDF 
c.rifamp.mid <- 13.04 # VTP
c.rifapent.mid <- 21.90  # GDF

# Low (currently the same as the mid) Medicine costs
c.inh.low <- 14.60 # GDF 
c.rifamp.low <- 13.04 # VTP
c.rifapent.low <- 21.90  # GDF
 
# High Medicine costs
c.inh.high <- 22.11 # PBS 
c.rifamp.high <- 115.77 # PBS
c.rifapent.high <- 35.04 # US$1 per tablet = US$24 = AUD$105.12

# Create table of parameters
p <- c("rradj", "att", "begintrt", "snqftgit", "spqftgit",
     "sntst15", "sptst15", "sntst10", "sptst10", "treatr3HP",
     "treatr4R", "treatr6H", "treatr9H", "ttt3HP", "ttt4R", 
     "ttt6H", "ttt9H", "saemr", "cattend", "cattendspec", 
     "csae", "cscreenqft", "cscreentst", "ctb", "ctreat3HP", 
     "cparttreat3HP", "ctreat4R", "cparttreat4R",  
     "ctreat6H", "cparttreat6H", "ctreat9H", 
     "cparttreat9H", "ctreatspec3HP", "cparttreatspec3HP", 
     "ctreatspec4R", "cparttreatspec4R", "ctreatspec6H", 
     "cparttreatspec6H", "ctreatspec9H", 
     "cparttreatspec9H", "uactivetb", "uactivetbr", 
     "uhealthy", "ultbi3HP", "ultbipart3HP", "ultbi4R", 
     "ultbipart4R", "ultbi6H", "ultbipart6H", 
     "ultbi9H", "ultbipart9H", "ultbitreatsae")
params <- data.frame(p)
params <- as.data.table(p)
params[, mid := 0]
params[, low := 0]
params[, mid := 0]
params[, high := 0]

c.gp.first.mid <- (c.gp.c.vr + c.gp.c.nonvr + c.gp.c.afterhours)/3
c.gp.first.low <- c.gp.c.nonvr
c.gp.first.high <- c.gp.c.afterhours

c.gp.review.mid <- (c.gp.b.vr + c.gp.b.nonvr + c.gp.b.afterhours)/3
c.gp.review.low <- c.gp.b.nonvr
c.gp.review.high <- c.gp.b.afterhours

chance.of.needing.mcs <- 0.1

# Cost of active TB
params[p == "ctb", mid := 12550.52] 
params[p == "ctb", low := 6330.73] 
params[p == "ctb", high := 185047.81] #18491.84

# Cost of sae
params[p == "csae", mid := 1222.64] 
params[p == "csae", low := 500] 
params[p == "csae", high := 10000] 

# Cost of screening
params[p == "cscreenqft", mid := 110.33] 
params[p == "cscreenqft", low := 78.34] 
params[p == "cscreenqft", high := 133.24] 

params[p == "cscreentst", mid := 113.28] 
params[p == "cscreentst", low := 70.20] 
params[p == "cscreentst", high := 146.30] 

# Cost of initial appointment after positive screen

cattendspec <- c.spec.first + (c.mcs * chance.of.needing.mcs) +
  c.liver + c.cxr

proportion.needing.spec <- 0.135 # Loutet et al 2018 UK study

params[p == "cattend",
       mid := (cattendspec * proportion.needing.spec) +
         (c.gp.review.mid + (c.mcs * chance.of.needing.mcs) + 
            c.liver + c.cxr) * (1 - proportion.needing.spec)]
params[p == "cattend",
       low := (cattendspec * proportion.needing.spec) +
         (c.gp.review.low + (c.mcs * chance.of.needing.mcs) + 
            c.liver + c.cxr) * (1 - proportion.needing.spec)]
params[p == "cattend",
       high := (cattendspec * proportion.needing.spec) +
         (c.gp.review.high + (c.mcs * chance.of.needing.mcs) + 
            c.liver + c.cxr) * (1 - proportion.needing.spec)]

# These specify how much of the appointment and medicine
# costs are applied for the partial costs and treatment
part.appt <- 2
part.med <- 3

# Cost of 3HP latent TB treatment
med.reviews <- 2
inh.packets <- 1
rpt.packets <- 3

appt.mid <- med.reviews * c.gp.review.mid
appt.low <- med.reviews * c.gp.review.low
appt.high <- med.reviews * c.gp.review.high

spec.all <- c.spec.review * med.reviews

med.mid <- inh.packets * c.inh.mid + rpt.packets * c.rifapent.mid
med.low <- inh.packets * c.inh.low + rpt.packets * c.rifapent.low
med.high <- inh.packets * c.inh.high + rpt.packets * c.rifapent.high

params[p == "ctreat3HP", mid := appt.mid + med.mid] 
params[p == "ctreat3HP", low := appt.low + med.low]
params[p == "ctreat3HP", high := appt.high + med.high] 

params[p == "cparttreat3HP",
       mid := appt.mid / part.appt + med.mid / part.med] 
params[p == "cparttreat3HP",
       low := appt.low / part.appt + med.low / part.med]
params[p == "cparttreat3HP",
       high := appt.high / part.appt + med.high / part.med] 

params[p == "ctreatspec3HP", mid := spec.all + med.mid] 
params[p == "ctreatspec3HP", low := spec.all + med.low]
params[p == "ctreatspec3HP", high := spec.all + med.high] 

params[p == "cparttreatspec3HP",
       mid := spec.all / part.appt + med.mid / part.med] 
params[p == "cparttreatspec3HP",
       low := spec.all / part.appt + med.low / part.med]
params[p == "cparttreatspec3HP",
       high := spec.all / part.appt + med.high / part.med] 

# Cost of 4R latent TB treatment
med.reviews <- 3
med.packets <- 3

appt.mid <- med.reviews * c.gp.review.mid
appt.low <- med.reviews * c.gp.review.low
appt.high <- med.reviews * c.gp.review.high

spec.all <- c.spec.review * med.reviews

med.mid <- c.rifamp.mid * med.packets
med.low <- c.rifamp.low * med.packets
med.high <- c.rifamp.high * med.packets

params[p == "ctreat4R", mid := appt.mid + med.mid] 
params[p == "ctreat4R", low := appt.low + med.low]
params[p == "ctreat4R", high := appt.high + med.high] 

params[p == "cparttreat4R",
       mid := appt.mid / part.appt + med.mid / part.med] 
params[p == "cparttreat4R",
       low := appt.low / part.appt + med.low / part.med]
params[p == "cparttreat4R",
       high := appt.high / part.appt + med.high / part.med] 

params[p == "ctreatspec4R", mid := spec.all + med.mid] 
params[p == "ctreatspec4R", low := spec.all + med.low]
params[p == "ctreatspec4R", high := spec.all + med.high] 

params[p == "cparttreatspec4R",
       mid := spec.all / part.appt + med.mid / part.med] 
params[p == "cparttreatspec4R",
       low := spec.all / part.appt + med.low / part.med]
params[p == "cparttreatspec4R",
       high := spec.all / part.appt + med.high / part.med] 


# Cost of 6H latent TB treatment
med.reviews <- 3
med.packets <- 6

appt.mid <- med.reviews * c.gp.review.mid
appt.low <- med.reviews * c.gp.review.low
appt.high <- med.reviews * c.gp.review.high

spec.all <- c.spec.review * med.reviews

med.mid <- c.inh.mid * med.packets
med.low <- c.inh.low * med.packets
med.high <- c.inh.high * med.packets

params[p == "ctreat6H", mid := appt.mid + med.mid] 
params[p == "ctreat6H", low := appt.low + med.low]
params[p == "ctreat6H", high := appt.high + med.high] 

params[p == "cparttreat6H",
       mid := appt.mid / part.appt + med.mid / part.med] 
params[p == "cparttreat6H",
       low := appt.low / part.appt + med.low / part.med]
params[p == "cparttreat6H",
       high := appt.high / part.appt + med.high / part.med] 

params[p == "ctreatspec6H", mid := spec.all + med.mid] 
params[p == "ctreatspec6H", low := spec.all + med.low]
params[p == "ctreatspec6H", high := spec.all + med.high] 

params[p == "cparttreatspec6H",
       mid := spec.all / part.appt + med.mid / part.med] 
params[p == "cparttreatspec6H",
       low := spec.all / part.appt + med.low / part.med]
params[p == "cparttreatspec6H",
       high := spec.all / part.appt + med.high / part.med] 


# Cost of 9H latent TB treatment
med.reviews <- 4
med.packets <- 9

appt.mid <- med.reviews * c.gp.review.mid
appt.low <- med.reviews * c.gp.review.low
appt.high <- med.reviews * c.gp.review.high

spec.all <- c.spec.review * med.reviews

med.mid <- c.inh.mid * med.packets
med.low <- c.inh.low * med.packets
med.high <- c.inh.high * med.packets

params[p == "ctreat9H", mid := appt.mid + med.mid] 
params[p == "ctreat9H", low := appt.low + med.low]
params[p == "ctreat9H", high := appt.high + med.high] 

params[p == "cparttreat9H",
       mid := appt.mid / part.appt + med.mid / part.med] 
params[p == "cparttreat9H",
       low := appt.low / part.appt + med.low / part.med]
params[p == "cparttreat9H",
       high := appt.high / part.appt + med.high / part.med] 

params[p == "ctreatspec9H", mid := spec.all + med.mid] 
params[p == "ctreatspec9H", low := spec.all + med.low]
params[p == "ctreatspec9H", high := spec.all + med.high] 

params[p == "cparttreatspec9H",
       mid := spec.all / part.appt + med.mid / part.med] 
params[p == "cparttreatspec9H",
       low := spec.all / part.appt + med.low / part.med]
params[p == "cparttreatspec9H",
       high := spec.all / part.appt + med.high / part.med] 

params[p == "rradj", mid := 0.9]
params[p == "rradj", low := 0.875]
params[p == "rradj", high := 0.952]

params[p == "att", mid := 0.684]
params[p == "att", low := 0.646]
params[p == "att", high := 0.721]

params[p == "begintrt", mid := 0.596]
params[p == "begintrt", low := 0.262]
params[p == "begintrt", high := 0.762]

params[p == "snqftgit", mid := 0.6104]
params[p == "snqftgit", low := 0.4925]
params[p == "snqftgit", high := 0.7195]

params[p == "spqftgit", mid := 0.95820]
params[p == "spqftgit", low := 0.95700]
params[p == "spqftgit", high := 0.95948]

params[p == "sntst15", mid := 0.6753]
params[p == "sntst15", low := 0.5590]
params[p == "sntst15", high := 0.7777]

params[p == "sptst15", mid := 0.95117]
params[p == "sptst15", low := 0.94978]
params[p == "sptst15", high := 0.95255]

params[p == "sntst10", mid := 0.7532]
params[p == "sntst10", low := 0.6418]
params[p == "sntst10", high := 0.8444]

params[p == "sptst10", mid := 0.82227]
params[p == "sptst10", low := 0.81780]
params[p == "sptst10", high := 0.82686]

params[p == "treatr3HP", mid := 0.543]
params[p == "treatr3HP", low := 0.221]
params[p == "treatr3HP", high := 0.749]

params[p == "treatr3HP", low.treat.complete := 0.538]
params[p == "treatr3HP", high.treat.complete := 0.632]
params[p == "treatr3HP", low.treat.eff := 0.218]
params[p == "treatr3HP", high.treat.eff := 0.642]

params[p == "treatr4R", mid := 0.604]
params[p == "treatr4R", low := 0.301]
params[p == "treatr4R", high := 0.79]

params[p == "treatr4R", low.treat.complete := 0.4565]
params[p == "treatr4R", high.treat.complete := 0.615]
params[p == "treatr4R", low.treat.eff := 0.3820]
params[p == "treatr4R", high.treat.eff := 0.774]

params[p == "treatr6H", mid := 0.557]
params[p == "treatr6H", low := 0.224]
params[p == "treatr6H", high := 0.642]

params[p == "treatr6H", low.treat.complete := 0.352]
params[p == "treatr6H", high.treat.complete := 0.599]
params[p == "treatr6H", low.treat.eff := 0.331]
params[p == "treatr6H", high.treat.eff := 0.596]

params[p == "treatr9H", mid := 0.561]
params[p == "treatr9H", low := 0.248]
params[p == "treatr9H", high := 0.812]

params[p == "treatr9H", low.treat.complete := 0.402]
params[p == "treatr9H", high.treat.complete := 0.618]
params[p == "treatr9H", low.treat.eff := 0.332]
params[p == "treatr9H", high.treat.eff := 0.718]

params[p == "ttt3HP", mid := 0.375]
params[p == "ttt3HP", low := 0.292]
params[p == "ttt3HP", high := 0.500]

params[p == "ttt4R", mid := 0.458]
params[p == "ttt4R", low := 0.375]
params[p == "ttt4R", high := 0.583]

params[p == "ttt6H", mid := 0.625]
params[p == "ttt6H", low := 0.542]
params[p == "ttt6H", high := 0.750]

params[p == "ttt9H", mid := 0.875]
params[p == "ttt9H", low := 0.792]
params[p == "ttt9H", high := 1.00]

# CONSTANT????????????
params[p == "uhealthy", mid := 0.873333333]
params[p == "uhealthy", low := 0.873333333]
params[p == "uhealthy", high := 0.873333333]

params[p == "uactivetb", mid := 0.780649583]
params[p == "uactivetb", low := 0.686612833]
params[p == "uactivetb", high := 0.872468854]

params[p == "uactivetbr", mid := 0.873333333]
params[p == "uactivetbr", low := 0.849333333]
params[p == "uactivetbr", high := 0.873333333]

params[p == "ultbi3HP", mid := 0.873333333]
params[p == "ultbi3HP", low := 0.8600]
params[p == "ultbi3HP", high := 0.873333333]
params[p == "ultbipart3HP", mid := 0.873333333]
params[p == "ultbipart3HP", low := 0.8667]
params[p == "ultbipart3HP", high := 0.873333333]

params[p == "ultbi4R", mid := 0.873333333]
params[p == "ultbi4R", low := 0.8558]
params[p == "ultbi4R", high := 0.873333333]
params[p == "ultbipart4R", mid := 0.873333333]
params[p == "ultbipart4R", low := 0.8650]
params[p == "ultbipart4R", high := 0.873333333]

params[p == "ultbi6H", mid := 0.873333333]
params[p == "ultbi6H", low := 0.8475]
params[p == "ultbi6H", high := 0.873333333]
params[p == "ultbipart6H", mid := 0.873333333]
params[p == "ultbipart6H", low := 0.8650]
params[p == "ultbipart6H", high := 0.873333333]

params[p == "ultbi9H", mid := 0.873333333]
params[p == "ultbi9H", low := 0.8267]
params[p == "ultbi9H", high := 0.873333333]
params[p == "ultbipart9H", mid := 0.873333333]
params[p == "ultbipart9H", low := 0.8650]
params[p == "ultbipart9H", high := 0.873333333]

params[p == "ultbitreatsae", mid := 0.8525000]
params[p == "ultbitreatsae", low := 0.8420833]
params[p == "ultbitreatsae", high := 0.8629167]

params[p == "saemr", mid := 0.000813]
params[p == "saemr", low := 0]
params[p == "saemr", high := 0.0316]

# Add a distribution and shap column with
# defaults of pert and 4
params[, distribution := "pert"]
params[, shape := 4]

# Write the table to clipboard so I can 
# paste it into my Excel spreadsheet
write.table(params, file = "clipboard-16384", 
            sep = "\t", row.names = FALSE)

# Save this table to file
setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
saveRDS(params, "params onshore.rds")









