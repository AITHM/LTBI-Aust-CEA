

POP <- 0
TESTSN <- 0.7
TESTSP <- 0.7
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


# p.sus to p.sus.notest transition
a <- 1 - POP

# p.sus to p.sus.nf transition
b <- POP * (1 - (1-TESTSP) * ATTEND)

# p.sus to p.sus.nbt transition
c <- POP * (1 - TESTSP) * ATTEND * (1 - BEGINTREAT)

# p.sus to p.sus.nct transition
d <- POP * (1 - TESTSP) * ATTEND * BEGINTREAT * (1 - TREATR - SAE)

# p.sus to p.sus.sae transition 
e <- POP * (1 - TESTSP) * ATTEND * BEGINTREAT * SAE

# p.sus to p.sus.tc transition 
f <- POP * (1 - TESTSP) * ATTEND * BEGINTREAT * TREATR

a + b + c + d + e + f          


# p.ltbi branch

# working out the proportion of the entire population that could reactivate in the first year
# after migration, i.e. those that didn't have  effective treatment,
# i.e. total population minus the effectively treated population/population

PROPTREATED <- TESTSN * ATTEND * BEGINTREAT * TREATR


# p.ltbi to p.ltbi.notest transition
a <- (1 - POP) * (1 - (RR * RRADJUST))
                  
# p.ltbi to p.tb transition 
b <- ((1 - POP) * RR * RRADJUST) + (POP * RR * RRADJUST * (1 - PROPTREATED * (1 - TIMETOTREAT)))

# p.ltbi to p.ltbi.nf transition
c <- POP * (1 - (TESTSN * ATTEND) - (RR * RRADJUST * (1 - PROPTREATED * (1 - TIMETOTREAT))))

# p.ltbi to p.ltbi.tp.a transition
d <- POP * TESTSN * ATTEND 

a + b + c + d


# p.ltbi to p.ltbi.nbt transition
e <- POP * TESTSN * ATTEND * (1 - BEGINTREAT)

# p.ltbi to p.ltbi.nct transition
f <- POP * TESTSN * ATTEND * BEGINTREAT * (1 - TREATR - SAE)

# p.ltbi to p.ltbi.sae transition 
g <- POP * TESTSN * ATTEND * BEGINTREAT * SAE

# p.ltbi to p.ltbi.tc transition 
h <- POP * TESTSN * ATTEND * BEGINTREAT * TREATR

a + b + c + e + f + g + h


### Writing them out in long form...

# p.sus branch


# p.sus to p.sus.notest transition
a <- 1 - param$POP

# p.sus to p.sus.nf transition
b <- param$POP * (1 - (1-param$TESTSP) * param$ATTEND)

# p.sus to p.sus.nbt transition
c <- param$POP * (1 - param$TESTSP) * param$ATTEND * (1 - param$BEGINTREAT)

# p.sus to p.sus.nct transition
d <- param$POP * (1 - param$TESTSP) * param$ATTEND * param$BEGINTREAT * (1 - param$TREATR - param$SAE)

# p.sus to p.sus.sae transition 
e <- param$POP * (1 - param$TESTSP) * param$ATTEND * param$BEGINTREAT * param$SAE

# p.sus to p.sus.tc transition 
f <- param$POP * (1 - param$TESTSP) * param$ATTEND * param$BEGINTREAT * param$TREATR

a + b + c + d + e + f          


# p.ltbi branch

# working out the proportion of the entire population that could reactivate in the first year
# after migration, i.e. those that didn't have  effective treatment,
# i.e. total population minus the effectively treated population/population

PROPTREATED <- (param$TESTSN * param$ATTEND * param$BEGINTREAT * param$TREATR)


# p.ltbi to p.ltbi.notest transition
a <- (1 - param$POP) * (1 - (param$RR * param$RRADJUST))

# p.ltbi to p.tb transition 
b <- ((1 - param$POP) * param$RR * param$RRADJUST) + (param$POP * param$RR * param$RRADJUST * (1 - (param$TESTSN * param$ATTEND * param$BEGINTREAT * param$TREATR) * (1 - param$TIMETOTREAT)))

# p.ltbi to p.ltbi.nf transition
c <- param$POP * (1 - (param$TESTSN * param$ATTEND) - (param$RR * param$RRADJUST * (1 - (param$TESTSN * param$ATTEND * param$BEGINTREAT * param$TREATR) * (1 - param$TIMETOTREAT))))

# p.ltbi to p.ltbi.tp.a transition
d <- param$POP * param$TESTSN * param$ATTEND 


# p.ltbi to p.ltbi.nbt transition
e <- param$POP * param$TESTSN * param$ATTEND * (1 - param$BEGINTREAT)

# p.ltbi to p.ltbi.nct transition
f <- param$POP * param$TESTSN * param$ATTEND * param$BEGINTREAT * (1 - param$TREATR - param$SAE)

# p.ltbi to p.ltbi.sae transition 
g <- param$POP * param$TESTSN * param$ATTEND * param$BEGINTREAT * param$SAE

# p.ltbi to p.ltbi.tc transition 
h <- param$POP * param$TESTSN * param$ATTEND * param$BEGINTREAT * param$TREATR

a + b + c + e + f + g + h
