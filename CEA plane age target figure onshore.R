
#'===========================================================================================================
#' This script creates a cost-effectivenes plane, showing different intervention strategies.
#' showing incremental cost and effectiveness (in QALYs) of different intervention strategies.
#' To create the rds files that this script can import you need to run to gene the "CEA plane age target" first
#' to generate the results.
#' 
#' Inputs:
#' Need to run the "CEA plane age target" first, which can be used to create four rds files (onshore, and offshore
#' with and wihout emigration) that this code then uses.
#' 
#' Output:
#' tiff
#' 
#' Coding style
#' https://google.github.io/styleguide/Rguide.xml

#' LOAD LIBRARIES ===========================================================================================
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

ylimupper <- 5300000/1000
ylimlower <- -500000/1000
xlimupper <- -5
xlimlower <- 46 
linewidth <- 1  

#' I need to use true negative signs and not the hyphens that R automatically uses,
#' therefore I need to create new labels for the x axis.
yaxislab  <- c("\u2013500", "0", "1000", "2000", "3000", 
               "4000", "5000")
xaxislab  <- c("\u20135", "0", "5",  "10", 
               "15",  "20", "25", 
               "30",  "35", "40", "45")
yaxisbreaks  <- c(-500, 0, 1000, 2000, 3000, 4000, 5000)
xaxisbreaks  <- c(-5, 0, 5, 10,
                  15, 20, 25,
                  30, 35,  40, 45)


# Reading in the data 
setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
# setwd("C:/Users/Robin/Documents/Katie/PhD/CEA/LTBI-Aust-CEA")
# data <- readRDS("Data/agetarget.rds")
data <- readRDS("Data/agetargetonshore.rds")
data <- as.data.table(data)


# Write the table to clipboard so I can paste it into Excel
# to create the data table associated with the figure.
write.table(data, "clipboard", sep = "\t", row.names = FALSE)

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



data$strategy <- factor(data$strategy,levels = c("11-19", "20-29", "30-39",
                                               "40-59", "60-69", 
                                               "11-35", "11-65", "36-65"))

data[strategy == "11-19", strategy := paste0("11", "\U2013", "19")]
data[strategy == "20-29", strategy := paste0("20", "\U2013", "29")]
data[strategy == "30-39", strategy := paste0("30", "\U2013", "39")]
data[strategy == "40-59", strategy := paste0("40", "\U2013", "59")]
data[strategy == "60-69", strategy := paste0("60", "\U2013", "69")]
data[strategy == "11-35", strategy := paste0("11", "\U2013", "35")]
data[strategy == "11-65", strategy := paste0("11", "\U2013", "65")]
data[strategy == "36-65", strategy := paste0("36", "\U2013", "65")]




# Get the colour palatte
# I need 4 fill colours
getPalette <- brewer.pal(5, "Spectral")
getPalette 
getPalette <- c(getPalette, "gray50")
getPalette

textsize <- 20
geomtextsize <- 5



align_legend <- function(p, hjust = 0.5)
{
  # extract legend
  g <- cowplot::plot_to_gtable(p)
  grobs <- g$grobs
  legend_index <- which(sapply(grobs, function(x) x$name) == "guide-box")
  legend <- grobs[[legend_index]]
  
  # extract guides table
  guides_index <- which(sapply(legend$grobs, function(x) x$name) == "layout")
  
  # there can be multiple guides within one legend box  
  for (gi in guides_index) {
    guides <- legend$grobs[[gi]]
    
    # add extra column for spacing
    # guides$width[5] is the extra spacing from the end of the legend text
    # to the end of the legend title. If we instead distribute it by `hjust:(1-hjust)` on
    # both sides, we get an aligned legend
    spacing <- guides$width[5]
    guides <- gtable::gtable_add_cols(guides, hjust*spacing, 1)
    guides$widths[6] <- (1-hjust)*spacing
    title_index <- guides$layout$name == "title"
    guides$layout$l[title_index] <- 2
    
    # reconstruct guides and write back
    legend$grobs[[gi]] <- guides
  }
  
  # reconstruct legend and write back
  g$grobs[[legend_index]] <- legend
  g
}


options(scipen = 5)
#dev.off()
myplot1 <-  
  ggplot(data, aes(x = incremental.qalys, y = incremental.cost/1000,
                   fill = strategy,
                   shape =  strategy)) +
  geom_point(size = 7, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (45000/1000)/1,
              colour = "gray65",
              size = linewidth, lty = 3) +
  geom_abline(intercept = 0, slope = (75000/1000)/1,
              colour = "gray65", 
              size = linewidth, lty = 2) +
  labs(x = "Incremental QALYs", 
       y = "Incremental Cost in Thousands, A$.",
       fill = expression(paste("", underline('Age Group, years'))),
       shape = expression(paste("", underline('Age Group, years'))),
       tag = "A)") +
  scale_shape_manual(values = c(1,
                                10, 13,
                                8, 11, 15,
                                17, 19)) +
  scale_fill_manual(values = c(getPalette, getPalette)) +
  geom_text_repel (aes(label = tb.prev.percent),
                   hjust = 0.5, vjust = -1,
                   segment.color = "transparent",
                   size = geomtextsize) +
  scale_y_continuous(breaks = yaxisbreaks,
                     labels = yaxislab) +
  scale_x_continuous(breaks = xaxisbreaks,
                     labels = xaxislab) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimlower, xlimupper), 
                  ylim = c(ylimlower, ylimupper), 
                  expand = F) +
  theme(text = element_text(size = textsize),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.box.background = element_rect(color = "black"),
        axis.text = element_text(colour="black"),
        legend.position = c(0.8, 0.6),
        panel.border = element_blank(),
        legend.title.align = 0.5)
#ggdraw(align_legend(myplot1))
# tiff('Figures/ceaplaneage.tiff', units = "in", width = 15, height = 7,
#      res = 200)
# myplot1
# dev.off()

# Reading in the data without emigration
# data <- readRDS("Data/agetargetnoemig.rds")
data <- readRDS("Data/agetargetonshorenoemig.rds")
data <- as.data.table(data)

# Write the table to clipboard so I can paste it into Excel
write.table(data, "clipboard", sep = "\t", row.names = FALSE)

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

data$strategy <- factor(data$strategy,levels = c("11-19", "20-29", "30-39",
                                                 "40-59", "60-69", 
                                                 "11-35", "11-65", "36-65"))

# Get the colour palatte
# I need 4 fill colours
getPalette <- brewer.pal(5, "Spectral")
getPalette 
getPalette <- c(getPalette, "gray50")
getPalette

options(scipen = 5)
#dev.off()
# myplot2 <-  
#   ggplot(data, aes(x = incremental.qalys, y = incremental.cost/1000,
#                    fill = strategy,
#                    shape =  strategy)) +
#   geom_point(size = 7, alpha = 1, na.rm = T) +
#   geom_vline(xintercept = 0, color = "black") +
#   geom_hline(yintercept = 0, color = "black") +
#   geom_abline(intercept = 0, slope = (45000/1000)/1,
#               colour = "gray65",
#               size = linewidth, lty = 3) +
#   geom_abline(intercept = 0, slope = (75000/1000)/1,
#               colour = "gray65", 
#               size = linewidth, lty = 2) +
#   labs(x = "Incremental QALYs", 
#        y = "Incremental Cost in Thousands, A$.",
#        fill = expression(paste("", underline('Age Group, years'))),
#        shape = expression(paste("", underline('Age Group, years'))),
#        tag =  "A)") +
#   scale_shape_manual(values = c(1,
#                                 10, 13,
#                                 8, 11, 15,
#                                 17, 19)) +
#   scale_fill_manual(values = c(getPalette, getPalette)) +
#   geom_text_repel (aes(label = tb.prev.percent),
#                    hjust = 0.5, vjust = -1,
#                    segment.color = "transparent",
#                    size = geomtextsize) +
#   scale_y_continuous(breaks = yaxisbreaks,
#                      labels = yaxislab) +
#   scale_x_continuous(breaks = xaxisbreaks,
#                      labels = xaxislab) +
#   guides(colour=guide_legend(title.position = "top", 
#                              title.hjust = 0.5)) +
#   theme_bw() +
#   coord_cartesian(xlim = c(xlimlower, xlimupper), 
#                   ylim = c(ylimlower, ylimupper), 
#                   expand = F) +
#   theme(text = element_text(size = textsize), 
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.text = element_text(colour="black"),
#         legend.position = c(0.8, 0.6),
#         panel.border = element_blank())
# 
# # Extract legend
# grobs <- ggplotGrob(myplot2)$grobs
# legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]

# Resave the plot without the legend
myplot2 <-  
  ggplot(data, aes(x = incremental.qalys, y = incremental.cost/1000,
                   fill = strategy,
                   shape =  strategy)) +
  geom_point(size = 7, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (45000/1000)/1,
              colour = "gray65",
              size = linewidth, lty = 3) +
  geom_abline(intercept = 0, slope = (75000/1000)/1,
              colour = "gray65", 
              size = linewidth, lty = 2) +
  labs(x = "Incremental QALYs", 
       y = "Incremental Cost in Thousands, A$.",
       fill = 'underline("Age Group, years")',
       shape = 'underline("Age Group, years")',
       tag =  "B)")  +
  scale_shape_manual(values = c(1,
                                10, 13,
                                8, 11, 15,
                                17, 19)) +
  scale_fill_manual(values = c(getPalette, getPalette)) +
  geom_text_repel (aes(label = tb.prev.percent),
                   hjust = 0.5, vjust = -1,
                   segment.color = "transparent",
                   size = geomtextsize) +
# scale_y_continuous(breaks = seq(-500000/1000, 5000000/1000, 500000/1000),
#                    label = comma) +
#   scale_x_continuous(breaks = seq(-10, 50, 5)) +
  scale_y_continuous(breaks = yaxisbreaks,
                     labels = yaxislab) +
  scale_x_continuous(breaks = xaxisbreaks,
                     labels = xaxislab) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimlower, xlimupper), 
                  ylim = c(ylimlower, ylimupper), 
                  expand = F) +
  theme(text = element_text(size = textsize),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(colour="black"),
        legend.position = "none",
        panel.border = element_blank())

plotty <- plot_grid(myplot1, myplot2, ncol = 2, 
                    rel_widths = c(1, 1))
# labels = c("A)", "B)", " "))
# labels = c("a)", "b)", " "))
# plotty <- plot_grid(myplot1, myplot2, legend, ncol = 3, 
#                     rel_widths = c(1, 1, .3))
#                     # labels = c("A)", "B)", " "))
#                     # labels = c("a)", "b)", " "))
tiff('Figures/ceaplaneageonshore.tiff', units = "in", width = 15, height = 5,
     res = 200)
plotty
dev.off()


#legend.position = c(0.80, 0.8),
#axis.text.x = element_text(angle=45, hjust=1),
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

