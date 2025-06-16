rm(list = ls(all = TRUE))
# Extra check that we deleted everything.
# 20 Digits Precision Representation.
# options(scipen=20)


# Import needed libraries.

library(devtools)
library(coda)

# install_github("laduplessis/bdskytools")
# install_github("laduplessis/beastio")


library(bdskytools)
library(beastio)


# Loading package required to read library(readxl)
library(readxl)




# Setting working directory.
# setwd("C:/Users/akirpich/Google Drive/2022 Kirpich-Skums-GSU-Belriem-HIV/R_Code")


# Reading data
# final_predicitons_path <- paste0("../Data/final_predictitons.xlsx")
final_predicitons_path <- paste0("../Data/Ne_relaxed.xlsx")
final_predicitons <- data.frame(read_xlsx(path = final_predicitons_path )) 

# Fixing dates
final_predicitons$date <- as.Date(final_predicitons$date)


# Creating the corresponding log analogs
final_predicitons$mean_log   <- log(final_predicitons$mean, base = 10)
final_predicitons$median_log <- log(final_predicitons$median, base = 10)
final_predicitons$lower_log  <- log(final_predicitons$lower, base = 10)
final_predicitons$upper_log  <- log(final_predicitons$upper, base = 10)




# Creating function for labels.
label_function <- function(label_text = "")
{
  
  par(xpd = NA )
  
  di <- dev.size("in")
  x <- grconvertX(c(0, di[1]), from="in", to="user")
  y <- grconvertY(c(0, di[2]), from="in", to="user")
  
  fig <- par("fig")
  x <- x[1] + (x[2] - x[1]) * fig[1:2]
  y <- y[1] + (y[2] - y[1]) * fig[3:4]
  
  txt <- label_text
  x <- x[1] + strwidth(txt, cex=4) * 6 / 5
  y <- y[2] - strheight(txt, cex=4) * 6 / 5
  text(x, y, txt, cex = 4)
  
# End of -> label_function <- function(label_text = "X")  
}  









# Raw version median counts
pdf(paste("../Plots/part01_04_ploting_graphs_for_population_size_counts_mean.pdf", sep = ""), height = 7, width = 16)
# par(par(mfrow = c(1, 2)), mar = c(16.5, 5.1, 6, 2.1))
# par(mfrow = c(1, 2))

par(mar=c(4,4,3.5,3.5)+0.5)

# First plot
plot(x = final_predicitons$date,
     y = final_predicitons$mean,
     col = "darkblue",
     lwd = 5,
     # pch = 19,
     lty = 5,
     type = "l",
     # main = "Estimated Mean HIV Effective Population Size Estimate and 95% HPD Credible Interval",
     ylim = c( min(final_predicitons$lower), 
               max(final_predicitons$upper) ),
     xlab = "",
     ylab = "Counts",     
     xaxt='n',
     yaxt='n',
     cex = 1,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)
lines(x = final_predicitons$date,
      y = final_predicitons$upper,
      col = "darkblue",
      lty = 5,
      lwd = 5,
      # pch = 15,
      type = "l",
      cex = 1.15
)
lines(x = final_predicitons$date,
      y = final_predicitons$lower,
      col = "darkblue",
      lty = 5,
      lwd = 5,
      # pch = 15,
      type = "l",
      cex = 1.15
)
legend( x = "topleft", 
        inset= c(0.04, 0.04), 
        legend = c("Effective Population Size Estimate and 95% HPD Credible Interval"), 
        col = "black", 
        fill = c("darkblue"),   
        pt.cex = 4,  
        cex = 0.97 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_value_counts <- as.integer( min(final_predicitons$date) )
final_value_counts   <- as.integer( max(final_predicitons$date) )
number_of_value_counts <- final_value_counts - initial_value_counts

x_tlab <- seq( from  = initial_value_counts, to  = final_value_counts,  by = trunc(number_of_value_counts/30) )   
x_lablist <- format( as.Date(x_tlab, origin = "1970-01-01"), "%Y-%m" )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex.axis = 5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value_counts <- round( min(final_predicitons$lower) )
y_max_value_counts <- round( max(final_predicitons$upper) )
y_tlab  <- round( seq( from = y_min_value_counts, to = y_max_value_counts, by = (y_max_value_counts-y_min_value_counts)/5 ) )
# y_lablist <- as.character( round(y_tlab,  digits = 4) )
y_lablist <- y_tlab
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.1)



# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# Label A
# label_function(label_value = "B", label_cex = 6)


dev.off()








# Raw version median counts
pdf(paste("../Plots/part01_04_ploting_graphs_for_population_size_counts_median.pdf", sep = ""), height = 7, width = 16)
# par(par(mfrow = c(1, 2)), mar = c(16.5, 5.1, 6, 2.1))
# par(mfrow = c(1, 2))

par(mar=c(4,4,3.5,3.5)+0.5)

# First plot
plot(x = final_predicitons$date,
     y = final_predicitons$median,
     col = "darkblue",
     lwd = 5,
     # pch = 19,
     lty = 1,
     type = "l",
    # main = "Estimated Median HIV Effective Population Size Estimate and 95% HPD Credible Interval",
     ylim = c( min(final_predicitons$lower), 
               max(final_predicitons$upper) ),
     xlab = "",
     ylab = "Counts",     
     xaxt='n',
     yaxt='n',
     cex = 1,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)
lines(x = final_predicitons$date,
      y = final_predicitons$upper,
      col = "darkblue",
      lty = 5,
      lwd = 5,
      # pch = 15,
      type = "l",
      cex = 1.15
)
lines(x = final_predicitons$date,
      y = final_predicitons$lower,
      col = "darkblue",
      lty = 5,
      lwd = 5,
      # pch = 15,
      type = "l",
      cex = 1.15
)
legend( x = "topleft", 
        inset= c(0.04, 0.04), 
        legend = c("Effective Population Size Estimate and 95% HPD Credible Interval"), 
        col = "black", 
        fill = c("darkblue"),   
        pt.cex = 4,  
        cex = 0.97 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_value_counts <- as.integer( min(final_predicitons$date) )
final_value_counts   <- as.integer( max(final_predicitons$date) )
number_of_value_counts <- final_value_counts - initial_value_counts

x_tlab <- seq( from  = initial_value_counts, to  = final_value_counts,  by = trunc(number_of_value_counts/30) )   
x_lablist <- format( as.Date(x_tlab, origin = "1970-01-01"), "%Y-%m" )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex.axis = 5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value_counts <- round( min(final_predicitons$lower) )
y_max_value_counts <- round( max(final_predicitons$upper) )
y_tlab  <- round( seq( from = y_min_value_counts, to = y_max_value_counts, by = (y_max_value_counts-y_min_value_counts)/5 ) )
# y_lablist <- as.character( round(y_tlab,  digits = 4) )
y_lablist <- y_tlab
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.1)



# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# Label A
# label_function(label_value = "B", label_cex = 6)


dev.off()




























# Raw version median counts
pdf(paste("../Plots/part01_04_ploting_graphs_for_population_size_counts_mean_log.pdf", sep = ""), height = 7, width = 16)
# par(par(mfrow = c(1, 2)), mar = c(16.5, 5.1, 6, 2.1))
# par(mfrow = c(1, 2))

par(mar=c(4,4,3.5,3.5)+0.5)

# First plot
plot(x = final_predicitons$date,
     y = final_predicitons$mean_log,
     col = "darkblue",
     lwd = 5,
     # pch = 19,
     lty = 1,
     type = "l",
    # main = "Estimated Mean HIV Effective Population Size Estimate and 95% HPD Credible Interval",
     ylim = c( min(final_predicitons$lower_log), 
               max(final_predicitons$upper_log) ),
     xlab = "",
     # ylab = "Log Counts",     
     ylab = bquote(Log[10] ~ "Counts"),     
     xaxt='n',
     yaxt='n',
     cex = 1,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)
lines(x = final_predicitons$date,
      y = final_predicitons$upper_log,
      col = "darkblue",
      lty = 5,
      lwd = 5,
      # pch = 15,
      type = "l",
      cex = 1.15
)
lines(x = final_predicitons$date,
      y = final_predicitons$lower_log,
      col = "darkblue",
      lty = 5,
      lwd = 5,
      # pch = 15,
      type = "l",
      cex = 1.15
)
legend( x = "topleft", 
        inset= c(0.04, 0.04), 
        legend = c("Effective Population Size Estimate and 95% HPD Credible Interval"), 
        col = "black", 
        fill = c("darkblue"),   
        pt.cex = 4,  
        cex = 0.97 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_value_counts <- as.integer( min(final_predicitons$date) )
final_value_counts   <- as.integer( max(final_predicitons$date) )
number_of_value_counts <- final_value_counts - initial_value_counts

x_tlab <- seq( from  = initial_value_counts, to  = final_value_counts,  by = trunc(number_of_value_counts/30) )   
x_lablist <- format( as.Date(x_tlab, origin = "1970-01-01"), "%Y-%m" )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex.axis = 5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value_counts <- round( min(final_predicitons$lower_log) )
y_max_value_counts <- round( max(final_predicitons$upper_log) )
y_tlab  <- round( seq( from = y_min_value_counts, to = y_max_value_counts, by = (y_max_value_counts-y_min_value_counts)/5 ) )
# y_lablist <- as.character( round(y_tlab,  digits = 4) )
y_lablist <- y_tlab
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.1)



# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# Label A
# label_function(label_value = "B", label_cex = 6)


dev.off()








# Raw version median counts
pdf(paste("../Plots/part01_04_ploting_graphs_for_population_size_counts_median_log.pdf", sep = ""), height = 7, width = 16)
# par(par(mfrow = c(1, 2)), mar = c(16.5, 5.1, 6, 2.1))
# par(mfrow = c(1, 2))

par(mar=c(4,4,3.5,3.5)+0.5)

# First plot
plot(x = final_predicitons$date,
     y = final_predicitons$median_log,
     col = "darkblue",
     lwd = 5,
     # pch = 19,
     lty = 1,
     type = "l",
    # main = "Estimated Median HIV Effective Population Size Estimate and 95% HPD Credible Interval",
     ylim = c( min(final_predicitons$lower_log), 
               max(final_predicitons$upper_log) ),
     xlab = "",
     # ylab = "Log Counts",     
     ylab = bquote(Log[10] ~ "Counts"),     
     xaxt='n',
     yaxt='n',
     cex = 1,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 2
)
lines(x = final_predicitons$date,
      y = final_predicitons$upper_log,
      col = "darkblue",
      lty = 5,
      lwd = 5,
      # pch = 15,
      type = "l",
      cex = 1.15
)
lines(x = final_predicitons$date,
      y = final_predicitons$lower_log,
      col = "darkblue",
      lty = 5,
      lwd = 5,
      # pch = 15,
      type = "l",
      cex = 1.15
)
legend( x = "topleft", 
        inset= c(0.04, 0.24), 
        legend = c("Effective Population Size Estimate and 95% HPD Credible Interval"), 
        col = "black", 
        fill = c("darkblue"),   
        pt.cex = 4,  
        cex = 0.97 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.
initial_value_counts <- as.integer( min(final_predicitons$date) )
final_value_counts   <- as.integer( max(final_predicitons$date) )
number_of_value_counts <- final_value_counts - initial_value_counts

x_tlab <- seq( from  = initial_value_counts, to  = final_value_counts,  by = trunc(number_of_value_counts/30) )   
x_lablist <- format( as.Date(x_tlab, origin = "1970-01-01"), "%Y-%m" )
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex.axis = 5)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value_counts <- round( min(final_predicitons$lower_log) )
y_max_value_counts <- round( max(final_predicitons$upper_log) )
y_tlab  <- round( seq( from = y_min_value_counts, to = y_max_value_counts, by = (y_max_value_counts-y_min_value_counts)/5 ) )
# y_lablist <- as.character( round(y_tlab,  digits = 4) )
y_lablist <- y_tlab
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.1)



# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


# Label A
# label_function(label_value = "B", label_cex = 6)


dev.off()


















































