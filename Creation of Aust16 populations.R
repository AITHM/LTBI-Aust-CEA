
# This script creates the population files for the CEA
# including the ones for Table 1/Figure 1 and the PSA


# This script imports the Australian 2016 census data by cob, yoa and age, 
# tidies it, calculates the LTBI prevalence 
# and then calculates a lot of LTBI estimates between the 25th and 75th percentiles
# based on a random number generated from 
# with upper and lower limits and saves it as "aust".
# This creates the population input for the cost-effectiveness analysis 
# that is run using Milinda's github repository titled "MH---CB-LTBI".

# I need to create a population dataset with 10 variables:
# AGEP: int  0 1 1 2 2 2 3 3 3 3 ...
# YARP: int  2016 2015 2016 2014 2015 2016 2013 2014 2015 2016 ...
# YOBP: int  2016 2015 2015 2014 2014 2014 2013 2013 2013 2013 ...
# SACC: Factor w/ 292 levels "1000","1100",..: 6 6 6 6 6 6 6 6 6 6 ...
# BPLP: chr  "New Zealand" "New Zealand" "New Zealand" "New Zealand" ...
# ISO3: chr  "NZL" "NZL" "NZL" "NZL" ...
# CNSY: num  2016 2016 2016 2016 2016 ...
# SEXP: chr  "Male" "Male" "Male" "Male" ...
# NUMP: int  100 178 116 178 194 81 233 230 161 85 ...
# LTBP: num  0.022 0 0.0134 0 0.0206 ...

# I plan to aggregate the sexes and therefore SEXP will simply equal "Both.
# I will include TB incidence in country of birth in the ISO3 variable, instead of iso3.
# I also don't know what SACC is used for, so will simply duplicate the 
# TB incidence in country of birth for this variable.

library(data.table)
library(reshape2)
library(countrycode)
library(matrixStats)
library(dplyr)
library(mc2d)

#set directory to the folder where the file is to be saved
setwd("H:/Katie/PhD/LTBI to active TB project/R/")

#Loading the the Australian Census data - choose relevant location
#MAKE SURE THAT THE FILES TO BE SAVED IN THE CODE BELOW REFLECT THE DATA THAT IS LOADED HERE
####################################################################
Aust16 <- read.csv("H:\\Katie\\PhD\\ABS data\\Australia 2016\\Australia 2016.csv", skip = 10, header = T)
####################################################################

#####Tidying the census data
Austall <- Aust16

##Function for tidying and reshaping and adding iso3 column
censustidyfunc <- function(df){
  df <- as.data.frame(df)
  #Replace the first two column names to become age and yoa
  colnames(df)[1] <- "age"
  colnames(df)[2] <- "yoa"
  # unique(df$age)
  # unique(df$yoa)
  df <- as.data.table(df)
  # Function for filling down the blank rows in the age column
  filltheblanks <- function(x, missing = ""){
    rle <- rle(as.character(x))
    empty <- which(rle$value == missing)
    rle$values[empty] <- rle$value[empty - 1] 
    inverse.rle(rle)
  }
  df$age <- filltheblanks(df$age)
  # unique(df$age)
  # Getting rid of rows that aren't needed
  df <- df[age != "Age in Single Years (AGEP)", ]
  df <- df[age != "AGEP - Age in Single Years", ]
  df <- df[age != "AGEP Age", ]
  df <- df[age != "(c) Commonwealth of Australia 2016", ]
  df <- df[age != "(c) Commonwealth of Australia 2017", ]
  df <- df[yoa != "YARP Year of Arrival in Australia", ]
  df <- df[yoa != "Year of Arrival in Australia (YARP)", ]
  df <- df[age != "Total", ]
  df <- df[age != "Dataset: 2006 Census of Population and Housing", ]
  df <- df[age != "Dataset: 2011 Census of Population and Housing", ]
  df <- df[age != "Dataset: 2016 Census of Population and Housing", ]
  df <- df[age != "INFO", ]
  df <- df[age != "ABS data licensed under Creative Commons, see abs.gov.au/ccby", ]
  df <- df[age != "Copyright Commonwealth of Australia, 2018, see abs.gov.au/copyright", ]
  df <- df[age != "Data Source: Census of Population and Housing, 2016, TableBuilder", ]
  df <- df[yoa != "Cells in this table have been randomly adjusted to avoid the release of confidential data. No reliance should be placed on small cells.", ]
  # Making age numeric
  df$age <- as.numeric(df$age)
  # unique(df$age)
  # Creating a variable with 2006/2011/2016 in it, 
  # so I can minus the age values from it later on
  # to obtain year of birth (yob)
  df$yoa <- as.character(df$yoa)
  df$yoa2 <- df$yoa
  df$yoa2 <- gsub("\\D", "", df$yoa2)
  df$yoa2 <- as.numeric(df$yoa2)
  # unique(df$yoa2)
  censusyear <- max(df$yoa2, na.rm = T)
  df$yoa2 <- NULL
  #Sorting year of arrival column
  df$yoa <- as.character(df$yoa)
  df$yoa <- gsub("\\D", "", df$yoa)
  df$yoa <- as.numeric(df$yoa)
  # unique(df$yoa)
  # Reshape, of country of birth becomes one column
  df <- melt(df, id = c("age", "yoa"))
  # Renaming country of birth (cob) variable
  names(df)[names(df) == "variable"] <- "cob"
  names(df)[names(df) == "value"] <- "pop"
  # unique(df$cob)
  df$pop <- as.numeric(df$pop)
  # sum(df$pop)
  # 23,297,139
  # Creating a censusdate variable so I can
  # minus the age values from it to obtain year of birth (yob)
  df$censusdate <- censusyear
  df$yob <- df$censusdate - df$age
  df[, "censusdate" := NULL]
  ##Creating a column of iso3 codes 
  df$iso3 <- countrycode(df$cob, "country.name", "iso3c")
  df
}

Austall <- censustidyfunc(Austall)

sum(Austall$pop, na.rm = TRUE)
# 23,297,139

# Fixing iso3 codes for those that didn't convert, and changing those
# that don't appear in Houben and Dodd's dataset
source('ISO3 fix functions.R')
Austall <- iso3fixfunc(Austall)

# Removing any rows with no population 
Austall <- Austall[pop != 0]

# Function for checking for countries that 
# don't have an iso3 match in ARTI data
# loading the ARTI/hazard data from Houben and Dodd
load("H:/Katie/PhD/LTBI project/R/Houben and Dodd/200repLARI.Rdata")
tbhaz <- as.data.table(rundata)
rm(rundata)
iso3list <- unique(tbhaz$iso3)
'%!in%' <- function(x,y) ! ('%in%'(x,y))
m <- Austall[Austall$iso %!in% iso3list,]
unique(m$cob)
rm(m, tbhaz)

sum(Austall$pop, na.rm = TRUE)
# 23,297,139

#Creating a year variable
Austall[, year := 2016]

AustA <- subset(Austall, iso3 == "AUS")
sum(AustA$pop)
# 15,615,550

Austm <- subset(Austall, iso3 != "AUS" | is.na(iso3))
sum(Austm$pop)
# 7,681,589
sum(Austm$pop) + sum(AustA$pop)
# 23,297,139

# aggregate all the rows that have ended being the same
Austm[, cob := NULL]
Austm[, year := NULL]
Austm <- Austm[, list(pop = sum(pop)), 
               by = c("age", "yoa", "yob", "iso3")]
Austm[, cob := iso3]
Austm[, year := 2016]
sum(Austm$pop)
# 7,681,589


#####MISSING DATA EXPLORATION

sum(Austm$pop[is.na(Austm$yoa)], na.rm = TRUE)
# Missing yoa only, not Australian: 
# 1,832,421

sum(Austm$pop[is.na(Austm$iso3) & !is.na(Austm$yoa)], na.rm = TRUE)
# Missing iso3 only: 
# 11,553

sum(Austm$pop[is.na(Austm$iso3) & is.na(Austm$yoa)], na.rm = TRUE)
# Missing both yoa and iso3 only: 
# 1,624,443

##Subsetting all rows with NA in yoa, but a country of birth, 
#and all those with yoa, but not country of birth
#i.e. they were born overseas but we don't know when 
#they migrated, or where they migrated from.
AustNA <- subset(Austm, (is.na(Austm$yoa) & Austm$iso != "AUS") | is.na(Austm$iso3))
sum(AustNA$pop)
#2016 - 1,843,974

##Removing all rows with anything missing 
Austm <- subset(Austm, !is.na(Austm$yoa) & !is.na(Austm$iso3))
sum(Austm$pop,na.rm = TRUE)
# 5,837,615
rm(AustNA)

TBriskcalc_5K200rep <- function(DT){
  DT <- as.data.table(DT)
  year <- DT$year[1]
  DT[, mani :=  "tb"]
  DT[, sex := "Both"]
  censusyear <- DT$year[1]
  ## Create the 5000 rep and 200 rep datasets 
  newcnty <- c("CHN", "GBR", "IND", "PHL", "VNM")
  DT5k <- DT[DT$iso3 %in% newcnty,]
  '%!in%' <- function(x, y)!('%in%'(x, y))
  DT200 <- DT[DT$iso3 %!in% newcnty, ]
  ### calculate the risks using 5000 an 200 rep dataset
  # Sourcing the functions
  setwd("H:/Katie/PhD/LTBI to active TB project/R/")
  source('ARTIcalc functions.R')
  DT5k <- TBriskcalc_onarrival(DT5k, 5000)
  DT200 <- TBriskcalc_onarrival(DT200, 200)
  DT200[, year := year]
  DT5k[, year := year]
  DT <- list(DT200, DT5k)
}
Austm <- TBriskcalc_5K200rep (Austm)

Austc <- copy(Austm)

# Defining the number of simulations we want
Num_SIm <- 100

dt <- Austm[[2]]

repquantiles <- function(dt) {
  if(ncol(dt) > 4999) {
    repnumber <- "5000"
  } else {
    repnumber <- "200"
  }
  firstrep <- "V1"
  lastrep <- paste0(c("V"), repnumber)
  Vcolnam <- paste0(c("V"), 1 : repnumber)
  dt <- as.data.table(dt)
  dt[, "cob" := NULL]
  g <- which(colnames(dt) == firstrep)
  h <- which(colnames(dt) == lastrep)
  headcols <- g - 1
  head <- dt[, 1:headcols]
  m <- dt[, .SD, .SDcols = Vcolnam]
  m <- as.matrix(m)
  probs <- c(20:80)
  probs <- probs/100
  qt <- rowQuantiles(m, probs = probs, drop = FALSE)
  qt <- as.data.table(qt)
  dt <- cbind(head, qt)
  dt
  # if(ncol(dt) > 4999) {
  #   repnumber <- "5000"
  # } else {
  #   repnumber <- "200"
  # }
  # firstrep <- "V1"
  # lastrep <- paste0(c("V"), repnumber)
  # Vcolnam <- paste0(c("V"), 1 : repnumber)
  # dt <- as.data.table(dt)
  # dt[, "cob" := NULL]
  # g <- which(colnames(dt) == firstrep)
  # h <- which(colnames(dt) == lastrep)
  # headcols <- g - 1
  # head <- dt[, 1:headcols]
  # m <- dt[, .SD, .SDcols = Vcolnam]
  # m <- as.matrix(m)
  # set.seed(10000)
  # probs <- rpert(Num_SIm, min = 0.25, mode = 0.50, max = 0.75)
  # probs <- round(probs, digits = 2)
  # qt <- rowQuantiles(m, probs = probs, drop = FALSE)
  # qt <- as.data.table(qt)
  # head <- cbind(head, qt)
  # dt
}

# Apply function to create quantiles of replicates
Austm <- lapply(Austm, repquantiles)
Austm <- rbindlist(Austm)
sum(Austm$pop, na.rm = "TRUE")
# 5,837,615
Austkeep <- copy (Austm)

Austm[, mani := NULL]
Austm[, id := NULL]
Austm[, medrisk := Austm$'50%']
Austm[, tfrisk := Austm$'25%']
Austm[, sfrisk := Austm$'75%']


# Apply function that merges the TB incidences in countries of birth
# for each census year form 2000 onwards, and applies the 2000 values for all prior years
addISO3TBincidfunc <- function(dt) {
  setwd("H:\\Katie\\PhD\\CEA\\Data") 
  cobtb <- read.csv("TB_burden_countries_2019-04-23.csv", header = T)
  cobtb <- as.data.table(cobtb)
  cobtb[, iso3 := as.character(iso3)]
  setnames(cobtb, "e_inc_100k", "cob.incid")
  cobtb <- cobtb[, c("iso3", "cob.incid", "year")]
  # unique(cobtb$cob.incid)
  # unique(cobtb$year)
  # Merging the cobtb data into the
  cobtb[, yoa := as.numeric(year)]
  dt[, yoa := as.character(yoa)]
  dt[, yoa := as.numeric(yoa)]
  dt[, yoa2 := yoa]  
  dt[yoa2 < 2000, yoa2 := 2000] 
  dt[iso3 == "TLS" & yoa2 < 2002, yoa2 := 2002] 
  dt[iso3 == "SSD" & yoa2 < 2011, yoa2 := 2011] 
  dt[iso3 == "SRB" & yoa2 < 2005, yoa2 := 2005] 
  dt[iso3 == "MNE" & yoa2 < 2005, yoa2 := 2005] 
  dt[iso3 == "CUW" & yoa2 < 2010, yoa2 := 2010] 
  setnames(cobtb, "year", "yoa2")
  dt <- merge(dt, cobtb, by = c("iso3", "yoa2"), all.x = TRUE)
  # dt<-cobifunc(dt)
  dt
}

Austm <- addISO3TBincidfunc(Austm)
Austm[, yoa2 := NULL]
Austm[, yoa.y := NULL]
setnames(Austm, "yoa.x", "YARP")

sum(Austm$pop, na.rm = "TRUE")
# 5,837,615

# Choose the TB incidence in country of birth cut-offs here:

cobi4func <- function(dt) {
  dt$cobi <- as.factor(cut(dt$cob.incid, breaks = c(-1, 39, 99, 149, 4000),
                           labels = c('0-39', '40-99', '100-149', '150+' )))
  dt
}

cobi5func <- function(dt) {
  dt$cobi <- as.factor(cut(dt$cob.incid, breaks = c(-1, 39, 99, 
                                                    149, 199, 4000),
                           labels = c('0-39', '40-99', 
                                      '100-149', '150-199','200+' )))
  dt
}

cobi6func <- function(dt) {
  dt$cobi <- as.factor(cut(dt$cob.incid, breaks = c(-1, 50, 100, 150,
                                                    200, 250, 4000),
                           labels = c('0-50', '51-100', '101-150', 
                                      '151-200', '201-250','251+' )))
  dt
}

#######################################################################################
# CHOOSE TB INCIDENCE AGGREGATION
cobifunc <- cobi5func
#######################################################################################

Austm <- cobifunc(Austm)
Austm <- as.data.table (Austm)
setnames(Austm, "pop", "NUMP")
setnames(Austm, "cobi", "ISO3")
Austm[, ISO3 := as.character(ISO3)]
setnames(Austm, "age", "AGEP")


# Saving a version of the this dataset for Table 1
tab1 <- copy(Austm)

tab1 <- tab1[, c("medrisk", "tfrisk", "sfrisk", "AGEP", "iso3", "cob.incid",
                 "sfrisk", "tfrisk",
                   "NUMP", "YARP", "ISO3")]
tab1 <- as.data.table(tab1)
tab1[, CNSY := 2016]
tab1[, ysa := CNSY - YARP]
tab1 <- subset(tab1, YARP == 2015)
# set directory to the folder where the file is to be saved
setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
# Save the  object
saveRDS(tab1, "Data/tab1data.rds")


percentilecols <- colnames(Austm)
percentilecols <- percentilecols[8:68]

check <- copy(Austm)
# check <- check[, c("medrisk", "tfrisk", "sfrisk", "AGEP",
#                    "NUMP", "YARP", "ISO3", "CNSY", "SEXP")]

#converting all risks to populations so that I can sum them appropriately
check[ , c(percentilecols) := lapply(.SD, "*", NUMP), 
       .SDcols = c(percentilecols)]

check[, LTBP := medrisk * NUMP]
check[, tfnum := tfrisk * NUMP]
check[, sfnum := sfrisk * NUMP]
check[, medrisk := NULL]
check[, tfrisk := NULL]
check[, sfrisk := NULL]
check[, iso3 := NULL]
check[, sex := NULL]

check <- check %>%
  group_by(YARP, ISO3, AGEP) %>% 
  summarise_all(list(sum))
sum(check$NUMP)
# 5,837,615

# AGEP: int  0 1 1 2 2 2 3 3 3 3 ...
# YARP: int  2016 2015 2016 2014 2015 2016 2013 2014 2015 2016 ...
# YOBP: int  2016 2015 2015 2014 2014 2014 2013 2013 2013 2013 ...
# SACC: Factor w/ 292 levels "1000","1100",..: 6 6 6 6 6 6 6 6 6 6 ...
# BPLP: chr  "New Zealand" "New Zealand" "New Zealand" "New Zealand" ...
# ISO3: chr  "NZL" "NZL" "NZL" "NZL" ...
# CNSY: num  2016 2016 2016 2016 2016 ...
# SEXP: chr  "Male" "Male" "Male" "Male" ...
# NUMP: int  100 178 116 178 194 81 233 230 161 85 ...
# LTBP: num  0.022 0 0.0134 0 0.0206 ...
check <- as.data.table(check)
check[, SACC := ISO3]
check[, BPLP := ISO3]
check[, SEXP := "Both"]
check[, CNSY := 2016]

sum(check$NUMP)
# 5,837,615

#str(check)

unique(check$ISO3)

check <- as.data.table(check)
check[, YOBP := CNSY - AGEP]
check[, year := NULL]
check[, yob := NULL]
check[, cob.incid := NULL]
#set directory to the folder where the file is to be saved
setwd("H:/Katie/PhD/CEA/MH---CB-LTBI/Data")
# Save the  object
saveRDS(check, "Aust16.psa.rds")

check[, twentyrisk := check$'20%']
check[, eightyrisk := check$'80%']
check <- as.data.table(check)
check[, c(percentilecols) := NULL]

#set directory to the folder where the file is to be saved
setwd("H:/Katie/PhD/CEA/MH---CB-LTBI/Data")
# Save the  object
saveRDS(check, "Aust16byTBincid.rds")

# library (ggplot2)
# 
# # Checking the dataset to make sure it makes sense
# 
# dev.off()
# ggplot(check, aes(x = YARP, y = NUMP, colour = ISO3, 
#                   group = ISO3))+
#   facet_grid(ISO3 ~ . )+
#   geom_line()+
#   labs(x = "Year of arrival",
#        y = "population") 
# 
# 
# dev.off()
# ggplot(check, aes(x = AGEP, y = NUMP, colour = ISO3, 
#                   group = ISO3))+
#   geom_line()+
#   labs(x = "Year of arrival",
#        y = "population") 
# 
# 
# dev.off()
# ggplot(check, aes(x = AGEP, y = NUMP))+
#   geom_line()+
#   labs(x = "Age",
#        y = "population") 
# 
# 
# 
# 
# che <- copy(check)
# che <- che[,c("NUMP", "AGEP")]
# che <- che %>%
#   group_by(AGEP) %>% # aggregating by ysa 
#   summarise_all(list(sum))
# 
# 
# dev.off()
# ggplot(che, aes(x = AGEP, y = NUMP))+
#   geom_bar(stat="identity", position = position_dodge(width = 0.7))+
#   labs(x = "Years since migration", 
#        y = "TB rate per 100,000",
#        fill="Gender")
# 
# 
# 
# che <- copy(check)
# che <- che[,c("NUMP", "YARP", "ISO3")]
# che <- che %>%
#   group_by(YARP, ISO3) %>% # aggregating by ysa 
#   summarise_all(list(sum))
# 
# 
# dev.off()
# ggplot(che, aes(x = YARP, y = NUMP, 
#                 colour = ISO3, group = ISO3))+
#   geom_line()+
#   labs(x = "Years since migration", 
#        y = "TB rate per 100,000",
#        colour = "ISO3")
# 
# 
# 
# 
#   geom_text(aes(label=year, x = Inf, y = Inf, 
#                 hjust = 3, vjust = 1.5),size=6, 
#             colour = "black") +
#   facet_grid(.~year)+
#   scale_y_continuous(breaks = seq(0, 1000, 10))+
#   theme_bw()+
#   #coord_cartesian(ylim = c(0,1500))+
#   theme(text = element_text(size=20),
#         legend.position = c(0.80, 0.8),
#         axis.text.x = element_text(angle=45, hjust=1),
#         strip.text = element_blank())
# 
# sum(che$NUMP)
# # 5,837,615
# 
# 
# 
