# This script creates a cost-effectivenes plane, showing incremental cost and effectiveness (in QALYs) of
# different intervention strategies.
# To be used in the cost-effectivness model for latent TB screening and treatment program.
# The files CB-TLTBI.R and CEA analyses need to be run to create the output, which was then added into
# an excel spreadsheet ("Model paramenter") into the sheet names "Table 3"
# and then these results are transferred into the sheet called "CEA plane", which is read by this script.

library(xlsx)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(scales)
library(egg)
library(cowplot)
library(grid)
library(gridExtra)
# Need to obtain chance of having sae with different treatment regimens.
# I have researched this and it is in an excel file in "Model parameters"

ylimupper <- 2620000/1000
ylimlower <- -50000/1000
xlimupper <- -5
xlimlower <- 20
  
# Reading in the data 
setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
#setwd("C:/Users/Robin/Documents/Katie/PhD/CEA/LTBI-Aust-CEA")
data <- readRDS("agetarget.rds")
data <- readRDS("agetargetonshore.rds")
data <- as.data.table(data)

data <- subset(data, strategy != "0_12...rds")
data <- data[, c("age.low", "age.high", "Percentage.of.all.TB.cases.prevented",
                 "Incremental.QALYS", "total.additional.cost")]
setnames(data, "Incremental.QALYS", "incremental.qalys")
setnames(data, "total.additional.cost", "incremental.cost")
setnames(data, "Percentage.of.all.TB.cases.prevented", "tb.prev.percent")

data[ , strategy := do.call(paste, c(.SD, sep = "-")), .SDcols = c("age.low", "age.high")]

percent <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(x, format = format, digits = digits, ...), "%")
}

data$tb.prev.percent <- percent(data$tb.prev.percent)

# Get the colour palatte
# I need 4 fill colours
getPalette<-brewer.pal(8, "Spectral")
getPalette

textsize <- 17
geomtextsize <- 5
options(scipen = 5)
#dev.off()
myplot1 <-  
  ggplot(data, aes(x = incremental.qalys, y = incremental.cost/1000,
                   fill = strategy,
                   shape =  strategy)) +
  geom_point(size = 7, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (50000)/1000,
              colour = "grey",
              size = 1.5) +
  labs(x = "Incremental QALYs", 
       y = "Incremental cost (AUD$thousands)",
       fill = "Target by:\nage group\n(years)",
       shape = "Target by:\nage group\n(years)") +
  # scale_size_continuous(limits = c(250, 450), 
  #                       range = c(5, 12), 
  #                       breaks = c(250, 350,
  #                                  450))+
  scale_shape_manual(values = c(21, 21,
                                21, 21,
                                21, 21)) +
  scale_fill_manual(values = c(getPalette, getPalette)) +
  geom_text_repel (aes(label = tb.prev.percent),
                   hjust = 0.5, vjust = -1,
                   segment.color = "transparent",
                   size = geomtextsize) +
  # geom_text(aes(label="More costly\nLess effective", x = -Inf, y = Inf),
  #           hjust = -0.03, vjust = 1.5, size = geomtextsize,
  #           colour = "black") +
  # geom_text(aes(label="More costly\nMore effective", x = Inf, y = Inf),
  #           hjust = 1, vjust = 1.5, size = geomtextsize,
  #           colour = "black") +
  # geom_text(aes(label="Less costly\nLess effective", x = -Inf, y = -Inf),
  #           hjust = -0.03, vjust = -0.7, size = geomtextsize,
  #           colour = "black") +
  # geom_text(aes(label="Less costly\nMore effective", x = Inf, y = -Inf),
  #           hjust = 1, vjust = -0.7, size = geomtextsize,
#           colour = "black") +
scale_y_continuous(breaks = seq(-600000/1000, 20000000/1000, 500000/1000),
                   label = comma) +
  scale_x_continuous(breaks = seq(-10, 50, 5)) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimlower, xlimupper), 
                  ylim = c(ylimlower, ylimupper)) +
  theme(text = element_text(size = textsize),
        # axis.title.x = element_blank(),
        legend.position = "none",
        panel.border = element_blank())
#legend.position = c(0.80, 0.8),
#axis.text.x = element_text(angle=45, hjust=1),

tiff('Figures/ceaplaneagetalk.tiff', units = "in", width = 15, height = 7,
     res = 200)
myplot1
dev.off()

# Reading in the data without emigration
data <- readRDS("Data/agetargetnoemig.rds")
data <- readRDS("Data/agetargetonshorenoemig.rds")
data <- as.data.table(data)

data <- subset(data, strategy != "0_12...rds")
data <- data[, c("age.low", "age.high", "Percentage.of.all.TB.cases.prevented",
                 "Incremental.QALYS", "total.additional.cost")]
setnames(data, "Incremental.QALYS", "incremental.qalys")
setnames(data, "total.additional.cost", "incremental.cost")
setnames(data, "Percentage.of.all.TB.cases.prevented", "tb.prev.percent")

data[ , strategy := do.call(paste, c(.SD, sep = "-")), .SDcols = c("age.low", "age.high")]

percent <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(x, format = format, digits = digits, ...), "%")
}

data$tb.prev.percent <- percent(data$tb.prev.percent)

# Get the colour palatte
# I need 4 fill colours
getPalette<-brewer.pal(8, "Spectral")
getPalette

options(scipen = 5)
#dev.off()
myplot2 <-  
  ggplot(data, aes(x = incremental.qalys, y = incremental.cost/1000,
                   fill = strategy,
                   shape =  strategy)) +
  geom_point(size = 7, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (50000)/1000,
              colour = "grey",
              size = 1.5) +
  labs(x = "Incremental QALYs", 
       y = "Incremental cost (in thousands, AUD$)",
       fill = "Target by:\nage group\n(years)",
       shape = "Target by:\nage group\n(years)") +
  # scale_size_continuous(limits = c(250, 450), 
  #                       range = c(5, 12), 
  #                       breaks = c(250, 350,
  #                                  450))+
  scale_shape_manual(values = c(21, 21,
                                21, 21,
                                21, 21)) +
  scale_fill_manual(values = c(getPalette, getPalette)) +
  geom_text_repel (aes(label = tb.prev.percent),
                   hjust = 0.5, vjust = -1,
                   segment.color = "transparent",
                   size = geomtextsize) +
  # geom_text(aes(label="More costly\nLess effective", x = -Inf, y = Inf),
  #           hjust = -0.03, vjust = 1.5, size = textsize, 
  #           colour = "black") +
  # geom_text(aes(label="More costly\nMore effective", x = Inf, y = Inf),
  #           hjust = 1, vjust = 1.5, size = textsize, 
  #           colour = "black") +
  # geom_text(aes(label="Less costly\nLess effective", x = -Inf, y = -Inf),
  #           hjust = -0.03, vjust = -0.7, size = textsize, 
  #           colour = "black") +
  # geom_text(aes(label="Less costly\nMore effective", x = Inf, y = -Inf),
  #           hjust = 1, vjust = -0.7, size = textsize, 
#           colour = "black") +
scale_y_continuous(breaks = seq(-600000/1000, 20000000/1000, 500000/1000),
                   label = comma) +
  scale_x_continuous(breaks = seq(-10, 50, 5)) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimlower, xlimupper), 
                  ylim = c(ylimlower, ylimupper)) +
  theme(text = element_text(size = textsize),
        axis.title.y = element_blank(),
        #legend.position = "none",
        panel.border = element_blank())

# Extract legend
grobs <- ggplotGrob(myplot2)$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]

# Resave the plot without the legend
myplot2 <-  
  ggplot(data, aes(x = incremental.qalys, y = incremental.cost/1000,
                   fill = strategy,
                   shape =  strategy)) +
  geom_point(size = 7, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (50000)/1000,
              colour = "grey",
              size = 1.5) +
  labs(x = "Incremental QALYs", 
       y = "Incremental cost (in thousands, AUD$)",
       fill = "Target by:\nage group\n(years)",
       shape = "Target by:\nage group\n(years)")  +
  # scale_size_continuous(limits = c(250, 450), 
  #                       range = c(5, 12), 
  #                       breaks = c(250, 350,
  #                                  450))+
  scale_shape_manual(values = c(21, 21,
                                21, 21,
                                21, 21)) +
  scale_fill_manual(values = c(getPalette, getPalette)) +
  geom_text_repel (aes(label = tb.prev.percent),
                   hjust = 0.5, vjust = -1,
                   segment.color = "transparent",
                   size = geomtextsize) +
  # geom_text(aes(label="More costly\nLess effective", x = -Inf, y = Inf),
  #           hjust = -0.03, vjust = 1.5, size = textsize, 
  #           colour = "black") +
  # geom_text(aes(label="More costly\nMore effective", x = Inf, y = Inf),
  #           hjust = 1, vjust = 1.5, size = textsize, 
  #           colour = "black") +
  # geom_text(aes(label="Less costly\nLess effective", x = -Inf, y = -Inf),
  #           hjust = -0.03, vjust = -0.7, size = textsize, 
  #           colour = "black") +
  # geom_text(aes(label="Less costly\nMore effective", x = Inf, y = -Inf),
  #           hjust = 1, vjust = -0.7, size = textsize, 
#           colour = "black") +
scale_y_continuous(breaks = seq(-600000/1000, 20000000/1000, 500000/1000),
                   label = comma) +
  scale_x_continuous(breaks = seq(-10, 50, 5)) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimlower, xlimupper), 
                  ylim = c(ylimlower, ylimupper)) +
  theme(text = element_text(size = textsize),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.border = element_blank())


plotty <- plot_grid(myplot1, myplot2, legend, ncol = 3, 
                    rel_widths = c(1, 1, .3),
                    labels = c("a)", "b)", " "))

tiff('Figures/ceaplaneageonshore.tiff', units = "in", width = 15, height = 5,
     res = 200)
plotty
dev.off()

