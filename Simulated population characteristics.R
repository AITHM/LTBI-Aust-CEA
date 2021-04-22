library(data.table)
library(purrr)
library(reshape2)
library(dplyr)
library(countrycode)
library(ggplot2)
library(scales)
library(gridExtra)
library(egg)
# Summarise the population data 
setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
source("CB-TLTBI_DataPreparation.R")
source("CB-TLTBI Functions.R")

# Read the data file
aust <- readRDS("Data/Aust16.rds") # baseline
aust <- as.data.table(aust)
dt <- subset(aust, YARP == 2015)
dt[, AGERP := AGEP - (2016L - YARP)]
sum(dt$NUMP)
252808
migrant.inflow.size <- 434340 
mult <- migrant.inflow.size/252808
dt[, NUMP := NUMP * mult]
sum(dt$NUMP)
434340
dt <- subset(dt, AGERP > 10)
sum(dt$NUMP)
371621.7

# Load the object with country of birth information
raw <- readRDS("tab1data.rds")
raw[, AGERP := AGEP - (CNSY - YARP)]
raw <- subset(raw, AGERP > 10)
sum(raw$NUMP) * 1.7
371621.7

raw[, LTBP := medrisk * NUMP]
setnames(raw, "ISO3", "COBI")
setnames(raw, "iso3", "ISO3")



#Functions:
options(scipen=2)
agetab <- function(df) {
  df <- as.data.table(df)
  tot <- sum(df$NUMP) 
  df$AAAG <- as.factor(cut(dt$AGERP, breaks=c(-1,14,19,24,29,
                                                  34,39,44,49,54,
                                                  59,64, 120),
                                 labels=c('11-14','15-19','20-24',
                                          '25-29', '30-34','35-39','40-44',
                                          '45-49','50-54','55-59','60-64',
                                          '65+')))
  tabage <- df[, c("NUMP", "AAAG")]
  tabage[, AAAG := as.character(AAAG)]
  tabage <- tabage %>%
    group_by(AAAG) %>%
    summarise_all(list(sum))
  tabage <- as.data.table(tabage)
  tabage[ , percent := (NUMP / tot) * 100]
  tabage
}



iso3tab <- function(df) {
  df <- as.data.table(df)
  tot <- df[, sum(NUMP, na.rm = TRUE), by = COBI]
  sumtot <- sum(df$NUMP)
  tabc <- df[, c("NUMP", "ISO3", "COBI")]
  tabc <- tabc %>%
    group_by(ISO3, COBI) %>%
    summarise_all(list(sum))
  tabc <- as.data.table(tabc)
  tabc <- tabc[order(COBI, NUMP),]
  keep <- tabc[, tail(ISO3, n=5), by = COBI]
  setnames(keep, 2, "ISO3")
  check <- merge(keep, tabc, by = c("ISO3", "COBI"), all.x = TRUE)
  check <- merge(check, tot, by = c("COBI"), all.x = TRUE)
  check[, percentofall := (NUMP / sumtot) * 100]
  check[, percent := (NUMP / V1) * 100]
  check <- check[order(COBI, - NUMP),]
  check$ISO3 <- countrycode(check$ISO3, "iso3c", "country.name")
  check$COBI <- factor(check$COBI, levels = c('0-39', '40-99', 
                                                '100-149', '150-199','200+'))
  check <- check[order(check$COBI), ]
  check
}


overall <- function(df) {
  df <- as.data.table(df)
  tot <- sum(df$NUMP) 
  df[, sum(NUMP, na.rm = TRUE), by = ISO3]
  tabage <- df[, c("NUMP", "ISO3")]
  tabage <- tabage %>%
    group_by(ISO3) %>%
    summarise_all(list(sum))
  tabage <- as.data.table(tabage)
  tabage[, percent := (NUMP/tot) * 100]
  tabage[, ISO3 := as.factor(ISO3)]
  tabage$ISO3 <- factor(tabage$ISO3, levels = c('0-39', '40-99', 
                                                '100-149', '150-199','200+'))
  tabage <- tabage[order(tabage$ISO3), ]
  tabage
}
  
check <- agetab(dt)
write.table(check, "clipboard", sep = "\t", row.names = FALSE)
check <- iso3tab(raw)
write.table(check, "clipboard", sep = "\t", row.names = FALSE)
check <- overall(dt)
write.table(check, "clipboard", sep = "\t", row.names = FALSE)

df <- check[[1]]

# Creating the figure
agetabfig <- function(df) {
  df <- as.data.table(df)
  tot <- sum(df$NUMP)
  lab <- df$ISO3[1]
  df$AAAG <- as.factor(cut(df$AGERP, breaks=c(-1, 14, 19, 24, 29,
                                              34,39,44,49,54,
                                              59,64, 120),
                           labels = c('11-14','15-19','20-24',
                                    '25-29', '30-34','35-39','40-44',
                                    '45-49','50-54','55-59','60-64',
                                    '65+')))
  tabage <- df[, c("NUMP", "AAAG")]
  tabage[, AAAG := as.character(AAAG)]
  tabage <- tabage %>%
    group_by(AAAG) %>%
    summarise_all(list(sum))
  tabage <- as.data.table(tabage)
  tabage[ , percent := (NUMP/tot)*100]
  tabage[, ISO3:= lab]
  tabage
}


check <- split(dt, dt$ISO3)
check <- lapply(check, agetabfig)
check <- rbindlist(check)
check$ISO3 <- ordered(check$ISO3, levels = c('0-39', '40-99', 
                                    '100-149', '150-199','200+'))

# dev.off()
plot1 <- 
  ggplot(check, aes(x = AAAG, colour = ISO3, y = NUMP, group = ISO3))+
  geom_line(size = 2) +
  labs(x = "Age group at migration (years)", 
       y = "Number",
       colour = "TB incidence in country of birth\n at migration, per 100,000") +
  scale_y_continuous(labels = comma) +
  theme_bw() +
  scale_color_brewer(palette="Dark2") +
  # coord_cartesian(ylim = c(0,120)) +
  guides(colour = guide_legend(
    keywidth = 0.2,
    keyheight = 0.2,
    default.unit = "inch"))+
  theme(text = element_text(size = 15),
        legend.position = c(0.75, 0.57),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        strip.text = element_blank())
plot2 <- 
  ggplot(check, aes(x = AAAG, colour = ISO3, y = percent, group = ISO3))+
  geom_line(size = 2) +
  labs(x = "Age group at migration (years)", 
       y = "Percent of each group",
       colour = "TB incidence in country of birth\n at migration, per 100,000") +
  scale_y_continuous(labels = comma) +
  theme_bw() +
  scale_color_brewer(palette="Dark2") +
  # coord_cartesian(ylim = c(0,120)) +
  guides(colour = guide_legend(
    keywidth = 0.2,
    keyheight = 0.2,
    default.unit = "inch"))+
  theme(text = element_text(size = 15),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_blank())

tiff('Figures/Figure 1.tiff', units = "in", width = 10, height = 5,
     res = 100)
egg::ggarrange(plot1, plot2, nrow = 2)
dev.off()

