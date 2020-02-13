# This script creates a figure with six cost-effectivenes planes,
# showing incremental cost and effectiveness (in QALYs) of
# different intervention strategies.
# To be used in the cost-effectivness model for latent TB screening and treatment program.
# The files CB-TLTBI.R and CEA analyses need to be run to create the output, which was then added into
# csv spreadsheets of various names, which are read in below


library(data.table)
library(ggplot2)
library(RColorBrewer)
library(egg)
library(cowplot)
library(grid)
library(gridExtra)

# Need to obtain chance of having sae with different treatment regimens.
# I have researched this and it is in an excel file in "Model parameters"

# Load data
setwd("H:/Katie/PhD/CEA/MH---CB-LTBI/Data")
data <- readRDS("cea.plane.2.rds")
data <- as.data.table(data)

off11.35 <- subset(data, other == "30yr horizon")
off11.35_80yrs <- subset(data, other == "80yr horizon")
noemigoff <- subset(data, other == "perm emigration")
ultbioff <- subset(data, other == "LTBI decrement")
inflow30 <- subset(data, other == "30 inflows")

dt<- copy(off11.35)

sortformatfunc <- function(dt){
  dt <- as.data.table(dt)
  dt <- dt[, c('strategy', 'total.additional.cost', 'Incremental.QALYS')]
  setnames(dt, 'Incremental.QALYS', "incremental.qalys")
  setnames(dt, 'total.additional.cost', "incremental.cost")
  dt[1, strategy := "Baseline"]
  dt[, strategy := as.character(strategy)]
  dt[is.na(incremental.cost), incremental.cost := 0]
  dt[is.na(incremental.qalys), incremental.qalys := 0]
}

plot1 <- sortformatfunc(off11.35)
plot2 <- sortformatfunc(off11.35_80yrs)
plot3 <- sortformatfunc(noemigoff)
plot4 <- sortformatfunc(ultbioff)
plot5 <- sortformatfunc(inflow30)

# Get the colour palatte
# I need 4 fill colours
getPalette<-brewer.pal(4, "Spectral")
getPalette

ylimmin <- -2
ylimmax <- 7.5
xlimmin <- -4
xlimmax <- 64

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
  annotate("text", x = 20, y = 6.5, 
           label = "30 year time horizon") +
  scale_shape_manual(values = c(19,
                                24, 24, 24, 24,
                                21, 21, 21, 21,
                                22, 22, 22, 22)) +
  scale_fill_manual(values = c(19,
                               getPalette, 
                               getPalette,
                               getPalette)) +
scale_y_continuous(breaks = seq(-10, 250, 1)) +
  scale_x_continuous(breaks = seq(-10, 1000, 5)) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax)) +
  theme(text = element_text(size = textsize2),
        panel.border = element_blank(),
        legend.position = "none")
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
  annotate("text", x = 20, y = 6.5, 
           label = "80 year time horizon") +
  scale_shape_manual(values = c(19,
                                24, 24, 24, 24,
                                21, 21, 21, 21,
                                22, 22, 22, 22)) +
  scale_fill_manual(values = c(19,
                               getPalette, 
                               getPalette,
                               getPalette)) +
  scale_y_continuous(breaks = seq(-10, 250, 1)) +
  scale_x_continuous(breaks = seq(-10, 1000, 5)) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax)) +
  theme(text = element_text(size = textsize2),
        panel.border = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank())
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
  scale_shape_manual(values = c(19,
                                24, 24, 24, 24,
                                21, 21, 21, 21,
                                22, 22, 22, 22)) +
  scale_fill_manual(values = c(19,
                               getPalette, 
                               getPalette,
                               getPalette)) +
  annotate("text", x = 25, y = 6.5, 
           label = "Screening migrants applying for\npermanent visas only") +
  scale_y_continuous(breaks = seq(-10, 250, 1)) +
  scale_x_continuous(breaks = seq(-10, 1000, 5)) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax)) +
  theme(text = element_text(size = textsize2),
        panel.border = element_blank(),
        axis.title.y = element_blank(),
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
  # geom_text(aes(label="More costly\nLess effective", x = -Inf, y = Inf),
  #         hjust = -0.03, vjust = 1.2, size = textsize,
  #         colour = "black") +
  # geom_text(aes(label="More costly\nMore effective", x = Inf, y = Inf),
  #         hjust = 1, vjust = 1.2, size = textsize,
  #         colour = "black") +
  # geom_text(aes(label="Less costly\nLess effective", x = -Inf, y = -Inf),
  #         hjust = -0.03, vjust = -0.2, size = textsize,
  #         colour = "black") +
  # geom_text(aes(label="Less costly\nMore effective", x = Inf, y = -Inf),
  #         hjust = 1, vjust = -0.2, size = textsize,
  #         colour = "black") +
  annotate("text", x = -250, y = 6.5,
           label = "Applying health utility values for those on\nLTBI treatment from Bauer et al 2015") +
  scale_shape_manual(values = c(19,
                                24, 24, 24, 24,
                                21, 21, 21, 21,
                                22, 22, 22, 22)) +
  scale_fill_manual(values = c(19,
                               getPalette, 
                               getPalette,
                               getPalette)) +
scale_y_continuous(breaks = seq(-10, 250, 1)) +
  scale_x_continuous(breaks = seq(-500, 1000, 100)) +
  theme_bw() +
  coord_cartesian(xlim = c(-600, 100), ylim = c(ylimmin, ylimmax)) +
  theme(text = element_text(size = textsize2),
        panel.border = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank())
#legend.position = c(0.80, 0.8),
#axis.text.x = element_text(angle=45, hjust=1),

# setwd("H:/Katie/PhD/CEA/MH---CB-LTBI/Figures")
# tiff('ceaplaneultbi.tiff', units = "in", width = 8, height = 4,
#      res = 200)
# myplot4
# dev.off()


myplot5 <-
  ggplot(plot5, aes(x = incremental.qalys, y = incremental.cost/1000000,
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
  scale_shape_manual(values = c(19,
                                24, 24, 24, 24,
                                21, 21, 21, 21,
                                22, 22, 22, 22)) +
  scale_fill_manual(values = c(19,
                               getPalette, 
                               getPalette,
                               getPalette)) +
  scale_y_continuous(breaks = seq(-10, 250, 1)) +
  scale_x_continuous(breaks = seq(-500, 1000, 100)) +
  theme_bw() +
  coord_cartesian(xlim = c(-600, 5), ylim = c(ylimmin, ylimmax)) +
  theme(text = element_text(size = textsize2),
        panel.border = element_blank(),
        axis.title.y = element_blank())
# Extract legend
grobs <- ggplotGrob(myplot5)$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]

myplot5 <-
  ggplot(plot5, aes(x = incremental.qalys, y = incremental.cost/1000000,
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
  annotate("text", x = 300, y = 100, 
           label = "30 year migrant inflow") +
  scale_shape_manual(values = c(19,
                                24, 24, 24, 24,
                                21, 21, 21, 21,
                                22, 22, 22, 22)) +
  scale_fill_manual(values = c(19,
                               getPalette, 
                               getPalette,
                               getPalette)) +
  scale_y_continuous(breaks = seq(-20, 250, 20)) +
  scale_x_continuous(breaks = seq(0, 1000, 100)) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, 580), ylim = c(ylimmin, 130)) +
  theme(text = element_text(size = textsize2),
        panel.border = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")



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


plot_grid(myplot1, myplot2, myplot3, myplot4, myplot5, legend,
          nrow = 1, 
          rel_widths = c(1, 1, 1, 1, 1, 0.3),
          labels = c("a)", "b)", "c)", "d)", "e)", ""))

setwd("H:/Katie/PhD/CEA/MH---CB-LTBI/Figures")
tiff('ceaplane2.tiff', units = "in", width = 18, height = 4,
     res = 200)
plot_grid(myplot1, myplot2, myplot3, myplot4, myplot5, legend,
          nrow = 1, 
          rel_widths = c(1, 1, 1, 1, 1, 0.3),
          labels = c("a)", "b)", "c)", "d)", "e)", ""))
dev.off()

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
# colour = "black") +


# Sorting the values for the permanent migrant screening 

