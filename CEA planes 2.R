# This script creates a figure with six cost-effectivenes planes,
# showing incremental cost and effectiveness (in QALYs) of
# different intervention strategies.
# To be used in the cost-effectivness model for latent TB screening and treatment program.
# The files CB-TLTBI.R and CEA analyses need to be run to create the output, which was then added into
# csv spreadsheets of various names, which are read in below

library(xlsx)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(egg)
# Need to obtain chance of having sae with different treatment regimens.
# I have researched this and it is in an excel file in "Model parameters"

# # Reading in the data from excel
# setwd("H:/Katie/PhD/CEA")
# data <- read.xlsx("Model parameters.xlsx", 
#                   sheetName = "CEA plane input",
#                   startRow = 2)

# Reading in the data from excel
setwd("H:/Katie/PhD/CEA/Data")

off11.35 <- read.csv("cea plane off11_35.csv")
off11.35_60yrs <- read.csv("cea plane off11-35_100_60yrs.csv")
noemigoff <- read.csv("cea plane off_noemig.csv")
ultbioff <- read.csv("cea plane off_ultbi.csv")

sortformatfunc <- function(dt){
  dt <- as.data.table(dt)
  dt <- dt[, c('strategy', 'total.additional.cost', 'Incremental.QALYS')]
  setnames(dt, 'Incremental.QALYS', "incremental.qalys")
  setnames(dt, 'total.additional.cost', "incremental.cost")
  dt[strategy == "0_12...rds", strategy := "Baseline"]
  #dt[, incremental.cost := as.numeric(gsub('\\D+','', incremental.cost))]
  dt[is.na(incremental.cost), incremental.cost := 0]
  dt[is.na(incremental.qalys), incremental.qalys := 0]
  dt[, incremental.qalys := as.character(incremental.qalys)]
  dt[, incremental.qalys := as.numeric(incremental.qalys)]
}

plot1 <- sortformatfunc(off11.35)
plot2 <- sortformatfunc(off11.35_60yrs)
plot3 <- sortformatfunc(noemigoff)
plot4 <- sortformatfunc(ultbioff)

# Get the colour palatte
# I need 4 fill colours
getPalette<-brewer.pal(4, "Spectral")
getPalette

ylimmin <- -2
ylimmax <- 7
xlimmin <- -4
xlimmax <- 40

pointsize <- 2.5
textsize <- 4
textsize2 <- 10

options(scipen=5)
myplot1 <-
  ggplot(plot1, aes(x = incremental.qalys, y = incremental.cost/1000000,
                    fill = strategy,
                    shape =  strategy)) +
  geom_point(size = pointsize, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (50000/1000000)/1,
              colour = "grey",
              size = 1) +
  labs(x = "Incremental QALYs", 
       y = "Incremental cost (AUD$millions)",
       fill = "Strategy",
       shape = "Strategy") +
  scale_shape_manual(values = c(24, 24, 24, 24,
                                21, 21, 21, 21,
                                22, 22, 22, 22, 19)) +
  annotate("text", x = 20, y = 6.5, 
           label = "30 year time horizon") +
  scale_fill_manual(values = c(getPalette, 
                               getPalette,
                               getPalette, 19)) +
  # geom_text(aes(label="100+/100,000; 11-35 year olds", x = Inf, y = Inf),
  #                     hjust = "middle", vjust = "middle", size = textsize,
  #                     colour = "black") +
  # geom_text(aes(label="More costly\nLess effective", x = -Inf, y = Inf),
  #           hjust = -0.03, vjust = 1.2, size = textsize, 
  #           colour = "black") +
  # geom_text(aes(label="More costly\nMore effective", x = Inf, y = Inf),
  #           hjust = 1, vjust = 1.2, size = textsize, 
  #           colour = "black") +
  # geom_text(aes(label="Less costly\nLess effective", x = -Inf, y = -Inf),
  #           hjust = -0.03, vjust = -0.2, size = textsize, 
#           colour = "black") +
# geom_text(aes(label="Less costly\nMore effective", x = Inf, y = -Inf),
#           hjust = 1, vjust = -0.2, size = textsize, 
#           colour = "black") +
scale_y_continuous(breaks = seq(-10, 250, 1)) +
  scale_x_continuous(breaks = seq(-10, 1000, 5)) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax)) +
  theme(text = element_text(size = textsize2),
        panel.border = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank())
#axis.text.x = element_text(angle=45, hjust=1),
myplot2 <-
  ggplot(plot2, aes(x = incremental.qalys, y = incremental.cost/1000000,
                    fill = strategy,
                    shape =  strategy)) +
  geom_point(size = pointsize, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (50000/1000000)/1,
              colour = "grey",
              size = 1) +
  labs(x = "Incremental QALYs", 
       y = "Incremental cost (AUD$millions)",
       fill = "Strategy",
       shape = "Strategy") +
  scale_shape_manual(values = c(24, 24, 24, 24,
                                21, 21, 21, 21,
                                22, 22, 22, 22, 19)) +
  annotate("text", x = 20, y = 6.5, 
           label = "60 year time horizon") +
  scale_fill_manual(values = c(getPalette, 
                               getPalette,
                               getPalette, 19)) +
  # geom_text(aes(label="More costly\nLess effective", x = -Inf, y = Inf),
  #           hjust = -0.03, vjust = 1.2, size = textsize, 
  #           colour = "black") +
  # geom_text(aes(label="More costly\nMore effective", x = Inf, y = Inf),
  #           hjust = 1, vjust = 1.2, size = textsize, 
  #           colour = "black") +
  # geom_text(aes(label="Less costly\nLess effective", x = -Inf, y = -Inf),
  #           hjust = -0.03, vjust = -0.2, size = textsize, 
  #           colour = "black") +
  # geom_text(aes(label="Less costly\nMore effective", x = Inf, y = -Inf),
  #           hjust = 1, vjust = -0.2, size = textsize, 
#           colour = "black") +
scale_y_continuous(breaks = seq(-10, 250, 1)) +
  scale_x_continuous(breaks = seq(-10, 1000, 5)) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax)) +
  theme(text = element_text(size = textsize2),
        panel.border = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank())
myplot3 <-
  ggplot(plot3, aes(x = incremental.qalys, y = incremental.cost/1000000,
                    fill = strategy,
                    shape =  strategy)) +
  geom_point(size = pointsize, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (50000/1000000)/1,
              colour = "grey",
              size = 1) +
  labs(x = "Incremental QALYs", 
       y = "Incremental cost (AUD$millions)",
       fill = "Strategy",
       shape = "Strategy") +
  scale_shape_manual(values = c(24, 24, 24, 24,
                                21, 21, 21, 21,
                                22, 22, 22, 22, 19)) +
  scale_fill_manual(values = c(getPalette, 
                               getPalette,
                               getPalette, 19)) +
  annotate("text", x = 20, y = 6.5, 
           label = "Assuming no emigration") +
  # geom_text(aes(label="More costly\nLess effective", x = -Inf, y = Inf),
  #           hjust = -0.03, vjust = 1.2, size = textsize, 
  #           colour = "black") +
  # geom_text(aes(label="More costly\nMore effective", x = Inf, y = Inf),
  #           hjust = 1, vjust = 1.2, size = textsize, 
  #           colour = "black") +
  # geom_text(aes(label="Less costly\nLess effective", x = -Inf, y = -Inf),
  #           hjust = -0.03, vjust = -0.2, size = textsize, 
  #           colour = "black") +
  # geom_text(aes(label="Less costly\nMore effective", x = Inf, y = -Inf),
  #           hjust = 1, vjust = -0.2, size = textsize, 
#           colour = "black") +
scale_y_continuous(breaks = seq(-10, 250, 1)) +
  scale_x_continuous(breaks = seq(-10, 1000, 5)) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax)) +
  theme(text = element_text(size = textsize2),
        panel.border = element_blank(),
        legend.position = "none")
myplot4 <-
  ggplot(plot4, aes(x = incremental.qalys, y = incremental.cost/1000000,
                    fill = strategy,
                    shape =  strategy)) +
  geom_point(size = pointsize, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (50000/1000000)/1,
              colour = "grey",
              size = 1) +
  labs(x = "Incremental QALYs", 
       y = "Incremental cost (AUD$millions)",
       fill = "Strategy",
       shape = "Strategy") +
  annotate("text", x = -250, y = 6.5, 
           label = "Applying health utility values for those on\nLTBI treatment from Bauer et al 2015") +
  scale_shape_manual(values = c(24, 24, 24, 24,
                                21, 21, 21, 21,
                                22, 22, 22, 22, 19)) +
  scale_fill_manual(values = c(getPalette, 
                               getPalette,
                               getPalette, 19)) +
  # geom_text(aes(label="More costly\nLess effective", x = -Inf, y = Inf),
  #           hjust = -0.03, vjust = 1.2, size = textsize, 
  #           colour = "black") +
  # geom_text(aes(label="More costly\nMore effective", x = Inf, y = Inf),
  #           hjust = 1, vjust = 1.2, size = textsize, 
  #           colour = "black") +
  # geom_text(aes(label="Less costly\nLess effective", x = -Inf, y = -Inf),
  #           hjust = -0.03, vjust = -0.2, size = textsize, 
  #           colour = "black") +
  # geom_text(aes(label="Less costly\nMore effective", x = Inf, y = -Inf),
  #           hjust = 1, vjust = -0.2, size = textsize, 
#           colour = "black") +
scale_y_continuous(breaks = seq(-10, 250, 1)) +
  scale_x_continuous(breaks = seq(-500, 1000, 100)) +
  theme_bw() +
  coord_cartesian(xlim = c(-440, 5), ylim = c(ylimmin, ylimmax)) +
  theme(text = element_text(size = textsize2),
        panel.border = element_blank(),
        legend.position = "none")
#legend.position = c(0.80, 0.8),
#axis.text.x = element_text(angle=45, hjust=1),


grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = "bottom")+
                    guides(fill = guide_legend(ncol = 4),
                           shape = guide_legend(ncol = 4)))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position = "none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

dev.off()
grid_arrange_shared_legend(myplot1, myplot2,
                           myplot3, myplot4,
                           # myplot7, myplot8,
                           nrow = 2)


# setwd("H:/Katie/PhD/CEA/Health eco conference")
# tiff('ceaplane1.tiff', units = "in", width = 10, height = 5,
#      res = 400)
# myplot1
# dev.off()