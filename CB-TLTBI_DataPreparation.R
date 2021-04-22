
#'===========================================================================================================
#' This script simply contains a couple of functions for the model run, including the one that creates
#' the migrant cohort popualtion table. They are called by the "Model run" file, but we
#' didn't really need a separate script for these, I
#' just never got around to incorporating them into others.
#' 
#' Inputs:
#' None
#' 
#' Output:
#' Only functions
#' 
#' Coding style
#' https://google.github.io/styleguide/Rguide.xml
#'===========================================================================================================


#' Creates a default set of states and values
CreateStates <- function(state.names) {

    for (i in state.names) {
        assign(i, pos = 1, DefineStates(cost = COST, utility = UTILITY))
    }

}

#' Creates a master migrant population table
CreatePopulationMaster <- function(Modify = FALSE) {
  pop.master <- aust[, .(AGEP, ISO3, YARP, NUMP, LTBP, AGERP = AGEP - (2016L - YARP), SEXP)]
  
  #' Also creating migrant cohort arrivals for YARP > 2016. i.e. 2017 to 2025.
  #' again this is for validating the model at runtime.
  
  #' deleted 2016 due to it being a census year with 1/2 half.
  pop.master <- pop.master[YARP != 2016]
  
  #' Create new arrival cohorts
  #' Putting the years I want into a list
  new.year.list <- c(2016:2050)

  #' Creating a function that will create a new arrival cohort for each of the years in my list
  add.more.years.function <- function(new.year) {
    pop.master.extra <- pop.master[YARP == 2015, .(AGEP = AGEP - 1, ISO3, YARP = new.year, NUMP, LTBP, AGERP, SEXP),]
    pop.master.extra
  }
  pop.master.extra.years <- lapply(new.year.list, add.more.years.function)
  pop.master.extra.years <- rbindlist(pop.master.extra.years)
  pop.master <- rbind(pop.master, pop.master.extra.years)

  #' Must order the pop.master table by YARP due to sub-setting and recombining. 
  setkey(pop.master, YARP, SEXP, AGEP, ISO3)
  
  #' Multiply the population by 1.7 because the current population represents the net migrant arrivals
  #' rather than all migrant arrivals, so the total number of arrivals needs to be higher
  
  pop.master <- pop.master[, NUMP := NUMP * (migrant.inflow.size/252808) ]
  pop.master <- pop.master[, LTBP := LTBP * (migrant.inflow.size/252808) ]
  
  #' Remove the populations who arrived under the age of 11 years and over 100
  
  pop.master <- subset (pop.master, AGERP > 10)
  
  #' Calculate the susceptible and latent population
  
  pop.master <- pop.master[, cycle := as.integer(NA)]
  pop.master <- pop.master[, (new.state.names) := as.numeric(NA)]
  
  pop.master <- pop.master[, (state.names) := .(NUMP - LTBP, 0, 0, 0, 0, 0,
                                                0, 0,
                                                0,
                                                LTBP, 0, 0, 0, 0, 0,
                                                0, 0, 
                                                0, 0,
                                                0, 0, 0, 0, 0)]
  
  #' Because we are running the model from 2020 the retrospective cohort must be aged from 2016 to 2020
  pop.master[YARP <= 2016, AGEP := AGEP + 4] # Census was taken in 2016
  pop.master[YARP == 2017, AGEP := AGEP + 3]
  pop.master[YARP == 2018, AGEP := AGEP + 2]
  pop.master[YARP == 2019, AGEP := AGEP + 1]
  
  pop.master
  
}
