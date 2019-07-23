

POP <- 0.6
TSTSN <- 0.7
TSTSP <- 0.7
ATTEND <- 0.8
BEGINTREAT <- 0.4
RR <- 0.0003
SAE <- 0.00001
RRADJUST <- 0.9
TREATR <- 0.7
EMIGRATE <- 0.0002
MR <- 0.000004
TIMETOTREAT <- 0.4

# p.sus branch


# p.sus to p.sus.nf transition
a <- 1 - (POP * (1-TESTSP) * ATTEND)

# p.sus to p.sus.nbt transition
b <- POP * (1 - TSTSP) * ATTEND * (1 - BEGINTREAT)

# p.sus to p.sus.nct transition
c <- POP * (1 - TSTSP) * ATTEND * BEGINTREAT * (1 - TREATR - SAE)

# p.sus to p.sus.sae transition 
d <- POP * (1 - TSTSP) * ATTEND * BEGINTREAT * SAE

# p.sus to p.sus.tc transition 
e <- POP * (1 - TSTSP) * ATTEND * BEGINTREAT * TREATR

           


# p.ltbi branch

# working out the proportion of the entire population that could reactivate in the first year
# after migration, i.e. those that didn't have  effective treatment,
# i.e. total population minus the effectively treated population/population

PROPTREATED <- POP * TSTSN * ATTEND * BEGINTREAT * TREATR

# p.ltbi to p.ltbi.nf transition
a <- 1 - (POP * TSTSN * ATTEND) - (RR * RRADJUST * (1 - PROPTREATED * (1 - TIMETOTREAT))) 

# p.ltbi to p.tb transition 
b <- RR * RRADJUST * (1 - PROPTREATED * (1 - TIMETOTREAT))

# p.ltbi to p.ltbi.tp.a transition
c <- POP * TSTSN * ATTEND 

a + b + c


# p.ltbi to p.ltbi.nbt transition
d <- POP * TSTSN * ATTEND * (1 - BEGINTREAT)

# p.ltbi to p.ltbi.nct transition
e <- POP * TSTSN * ATTEND * BEGINTREAT * (1 - TREATR - SAE)

# p.ltbi to p.ltbi.sae transition 
f <- POP * TSTSN * ATTEND * BEGINTREAT * SAE

# p.ltbi to p.ltbi.tc transition 
g <- POP * TSTSN * ATTEND * BEGINTREAT * TREATR


### Writing them out in long form...


# p.sus branch


# p.sus to p.sus.nf transition
a <- 1 - (param$POP * (1-param$TESTSP) * param$ATTEND)

# p.sus to p.sus.nbt transition
b <- param$POP * (1 - param$TESTSP) * param$ATTEND * (1 - param$BEGINTREAT)

# p.sus to p.sus.nct transition
c <- param$POP * (1 - param$TESTSP) * param$ATTEND * param$BEGINTREAT * (1 - param$TREATR - param$SAE)

# p.sus to p.sus.sae transition 
d <- param$POP * (1 - param$TESTSP) * param$ATTEND * param$BEGINTREAT * param$SAE

# p.sus to p.sus.tc transition 
e <- param$POP * (1 - param$TESTSP) * param$ATTEND * param$BEGINTREAT * param$TREATR


# p.ltbi branch


PROPTREATED <- (param$POP * param$TSTSN * param$ATTEND * param$BEGINTREAT * param$TREATR)

# p.ltbi to p.ltbi.nf transition
a <- 1 - (param$POP * param$TESTSN * param$ATTEND) - (param$RR * param$RRADJUST * (1 - (param$POP * param$TESTSN * param$ATTEND * param$BEGINTREAT * param$TREATR) * (1 - param$TIMETOTREAT))) 

# p.ltbi to p.tb transition 
b <- param$RR * param$RRADJUST * (1 - (param$POP * param$TESTSN * param$ATTEND * param$BEGINTREAT * param$TREATR) * (1 - param$TIMETOTREAT))

# p.ltbi to p.ltbi.nbt transition
d <- param$POP * param$TESTSN * param$ATTEND * (1 - param$BEGINTREAT)

# p.ltbi to p.ltbi.nct transition
e <- param$POP * param$TESTSN * param$ATTEND * param$BEGINTREAT * (1 - param$TREATR - param$SAE)

# p.ltbi to p.ltbi.sae transition 
f <- param$POP * param$TESTSN * param$ATTEND * param$BEGINTREAT * param$SAE

# p.ltbi to p.ltbi.tc transition 
g <- param$POP * param$TESTSN * param$ATTEND * param$BEGINTREAT * param$TREATR

