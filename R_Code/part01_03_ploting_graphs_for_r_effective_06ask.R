rm(list = ls(all = TRUE))
# Extra check that we deleted everything.
# 20 Digits Precision Representation.
# options(scipen=20)


# Import needed libraries.

library(devtools)
library(coda)

# install_github("laduplessis/bdskytools")
# install_github("laduplessis/beastio")
# FAQ if something does not install
# https://stackoverflow.com/questions/70908295/failed-to-install-unknown-package-from-github

library(bdskytools)
library(beastio)



params <- list(logfile = "../results/BDSKY/bdsky_ukr_relc_400mil_500_5.log")
bdsky_trace   <- beastio::readLog(params$logfile, burnin=0.1)


# With the trace loaded as an mcmc object from the coda package we can use coda functions to investigate the trace and check convergence. For details on how to use coda see the package on CRAN.
# Next, we can extract the HPDs of R~e~ and the becoming uninfectious rate:


Re_sky    <- beastio::getLogFileSubset(bdsky_trace, "reproductiveNumber_BDSKY_Serial")
Re_hpd    <- t(beastio::getHPDMedian(Re_sky))
bdsky_trace_df <- as.data.frame(bdsky_trace)
bUR <- bdsky_trace[, grep("^becomeUninfectiousRate_BDSKY_Serial", names(bdsky_trace_df), value=TRUE)]
delta_hpd <- t(beastio::getHPDMedian(bUR))



bdskytools::plotSkyline(1:10, Re_hpd, type='step', ylab="R")



tmrca_med  <- median(bdsky_trace[, "TreeHeight"])
gridTimes  <- seq(0, median(tmrca_med), length.out=10)

Re_gridded <- mcmc(bdskytools::gridSkyline(Re_sky, bdsky_trace[, "origin_BDSKY_Serial"], gridTimes))
Re_gridded_hpd <- t(getHPDMedian(Re_gridded))



times <- 2022 - gridTimes
plotSkyline(times, Re_gridded_hpd, xlab="Date", ylab="Re", type="smooth") 


plotSkyline(times, Re_gridded, type='steplines', traces=1, col=pal.dark(cblue,1),ylims=c(0,5), xlab="Time", ylab="R", main="1 random sample")
plotSkyline(times, Re_gridded, type='steplines', traces=10, col=pal.dark(cblue,0.5),ylims=c(0,5), xlab="Time", ylab="R", main="10 random samples")
plotSkyline(times, Re_gridded, type='steplines', traces=100, col=pal.dark(cblue,0.5),ylims=c(0,5), xlab="Time", ylab="R", main="100 random samples")
plotSkyline(times, Re_gridded, type='steplines', traces=1000, col=pal.dark(cblue,0.1),ylims=c(0,5), xlab="Time", ylab="R", main="1000 random samples")


par(mar=c(5,4,4,4)+0.1)

plotSkylinePretty(times, delta_hpd, type='step', axispadding=0.0, 
                  col=pal.dark(cblue), fill=pal.dark(cblue, 0.5), col.axis=pal.dark(cblue), 
                  ylab=expression(delta), side=4, yline=2, ylims=c(0,2), xaxis=FALSE)

plotSkylinePretty(times, Re_gridded_hpd, type='smooth', axispadding=0.0, 
                  col=pal.dark(corange), fill=pal.dark(corange, 0.5), col.axis=pal.dark(corange), 
                  xlab="Time", ylab=expression("R"[e]), side=2, yline=2.5, xline=2, xgrid=TRUE, 
                  ygrid=TRUE, gridcol=pal.dark(cgray), ylims=c(0,8), new=TRUE, add=TRUE)





# Versions on a single plot

# Raw version
pdf(paste("../Plots/part01_03_r_effective_raw.pdf", sep = ""), height = 7, width = 9)
# par(par(mfrow = c(1, 2)), mar = c(16.5, 5.1, 6, 2.1))
# par(mfrow = c(1, 2))

par(mar=c(4,4,3.5,3.5)+0.1)

plotSkylinePretty(times, delta_hpd, type='step', axispadding=0.0, 
                  col=pal.dark(cblue), fill=pal.dark(cblue, 0.60), col.axis=pal.dark(cblue), 
                  ylab=expression(delta), side=4, yline=2, ylims=c(0,6), xaxis=FALSE, xgrid = FALSE, ygrid = FALSE)

plotSkylinePretty(times, Re_gridded_hpd, type='step', axispadding=0.0, 
                  col=pal.dark(corange), fill=pal.dark(corange, 0.60), col.axis=pal.dark(corange), 
                  xlab="Time", ylab=expression("R"[e]), side=2, yline=2.5, xline=2, xgrid=FALSE, 
                  ygrid=FALSE, gridcol=pal.dark(cgray), ylims=c(0,6), new=TRUE, add=TRUE)

lines(x = c( min(times)-4.50, max(times) ) , 
      y = c(1,  1),
      col="darkblue", 
      lwd = 1.75, 
      lty = 2)


legend( x = "topleft", 
        inset= c(0.05, 0.05), 
        legend = c("Effective Reproduction Number Median Estimate", "95% HPD Interval", "Becoming Non-Infectious Rate Median Estimate", "95% HPD Interval", "1 Cutoff"), 
        col = "black", 
        fill = c(pal.dark(corange, 0.60), pal.dark(cblue, 0.60), pal.dark(corange, 1), pal.dark(corange, 0.60), "darkblue"),   
        pt.cex = 4,  
        cex = 0.80 ) 


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.



dev.off()




# Smoothed version
pdf(paste("../Plots/part01_03_r_effective_smoothed.pdf", sep = ""), height = 7, width = 9)
# par(par(mfrow = c(1, 2)), mar = c(16.5, 5.1, 6, 2.1))
# par(mfrow = c(1, 2))

par(mar=c(4,4,3.5,3.5)+0.1)

plotSkylinePretty(times, delta_hpd, type='smooth', axispadding=0.0, 
                  col=pal.dark(cblue), fill=pal.dark(cblue, 0.60), col.axis=pal.dark(cblue), 
                  ylab=expression(delta), side=4, yline=2, ylims=c(0,6), xaxis=FALSE, xgrid = FALSE, ygrid = FALSE)

plotSkylinePretty(times, Re_gridded_hpd, type='smooth', axispadding=0.0, 
                  col=pal.dark(corange), fill=pal.dark(corange, 0.60), col.axis=pal.dark(corange), 
                  xlab="Time", ylab=expression("R"[e]), side=2, yline=2.5, xline=2, xgrid=FALSE, 
                  ygrid=FALSE, gridcol=pal.dark(cgray), ylims=c(0,6), new=TRUE, add=TRUE)

lines(x = c( min(times)-4.50, max(times) ) , 
      y = c(1,  1),
      col="darkblue", 
      lwd = 1.75, 
      lty = 2)


legend( x = "topleft", 
        inset= c(0.05, 0.05), 
        legend = c("Effective Reproduction Number Median Estimate", "95% HPD Interval", "Becoming Non-Infectious Rate Median Estimate", "95% HPD Interval", "1 Cutoff"), 
        col = "black", 
        fill = c(pal.dark(corange, 0.60), pal.dark(cblue, 0.60), pal.dark(corange, 1), pal.dark(corange, 0.60), "darkblue"),   
        pt.cex = 4,  
        cex = 0.80 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.



dev.off()




# Two plots version




# Creating labeling functions
# We need two since we they use different coorinates

label_function_a <- function(label_value = "A", label_cex = 4) {
  
  par(xpd = NA )
  
  di <- dev.size("in")
  xx <- grconvertX(c(0, di[1]), from="in", to="user")
  yy <- grconvertY(c(0, di[2]), from="in", to="user")
  
  fig <- par("fig")
  xx <- xx[1] + (xx[2] - xx[1]) * fig[1:2]
  yx <- yy[1] + (yy[2] - yy[1]) * fig[3:4]
  
  txt <- label_value
  xx <- xx[1] + strwidth(txt, cex=4) * 6 / 5
  yy <- yy[2] - strheight(txt, cex=4) * 6 / 5
  text(xx, yy, txt, cex = label_cex )
  
}


label_function_b <- function(label_value = "A", label_cex = 4) {
  
  par(xpd = NA )
  
  di <- dev.size("in")
  xx <- grconvertX(c(0, di[1]), from="in", to="user")
  yy <- grconvertY(c(0, di[2]), from="in", to="user")
  
  fig <- par("fig")
  xx <- xx[1] + (xx[2] - xx[1]) * fig[1:2]
  yy <- yy[1]/2 + (yy[2] - yy[1]) * fig[3:4]
  
  txt <- label_value
  xx <- xx[1] + strwidth(txt, cex=4) * 6 / 5
  yy <- yy[2] - strheight(txt, cex=4) * 6 / 5
  text(xx, yy, txt, cex = label_cex )
  
}




# Raw version
pdf(paste("../Plots/part01_03_r_effective_raw_two_plots.pdf", sep = ""), height = 11, width = 9)
# par(par(mfrow = c(1, 2)), mar = c(16.5, 5.1, 6, 2.1))
par(mfrow = c(2, 1))

par(mar=c(4,4,2.5,3.5)+0.1)


plotSkylinePretty(times, Re_gridded_hpd, type='step', axispadding=0.0, 
                  col=pal.dark(corange), fill=pal.dark(corange, 0.60), # col.axis=pal.dark(corange), 
                  xlab="Time", ylab=expression("R"[e]), side=2, yline=2, xline=2, ylims=c(0,5.75), 
                  xgrid = FALSE, ygrid = FALSE)

lines(x = c( min(times)-4.50, max(times) ) , 
      y = c(1,  1),
      col="darkblue", 
      lwd = 1.75, 
      lty = 2)


legend( x = "topleft", 
        inset= c(0.05, 0.025), 
        legend = c("Median Estimate", "95% HPD Interval", "1 Cutoff"), 
        col = "black", 
        fill = c(pal.dark(corange, 1), pal.dark(corange, 0.60), "darkblue"),   
        pt.cex = 4,  
        cex = 1 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.




plotSkylinePretty(times, delta_hpd, type='step', axispadding=0.0, 
                  col=pal.dark(cblue), fill=pal.dark(cblue, 0.60), # col.axis=pal.dark(cblue), 
                  xlab="Time", ylab=expression(delta), side=2, yline=2, xline=2,  ylims=c(0,1.75), 
                  xgrid = FALSE, ygrid = FALSE)

# lines(x = c( min(times)-4.50, max(times) ) , 
#       y = c(1,  1),
#       col="darkblue", 
#       lwd = 1.75, 
#       lty = 2)


legend( x = "topleft", 
        inset= c(0.05, 0.025), 
        legend = c("Median Estimate", "95% HPD Interval", "1 Cutoff"), 
        col = "black", 
        fill = c(pal.dark(cblue, 1), pal.dark(cblue, 0.60), "darkblue"),   
        pt.cex = 4,  
        cex = 1 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.




# Label A
label_function_a(label_value = "A", label_cex = 4)

# Label B
label_function_b(label_value = "B", label_cex = 4)


dev.off()




# smoothed version
pdf(paste("../Plots/part01_03_r_effective_smoothed_two_plots.pdf", sep = ""), height = 11, width = 9)
# par(par(mfrow = c(1, 2)), mar = c(16.5, 5.1, 6, 2.1))
par(mfrow = c(2, 1))

par(mar=c(4,4,2.5,3.5)+0.1)


plotSkylinePretty(times, Re_gridded_hpd, type='smooth', axispadding=0.0, 
                  col=pal.dark(corange), fill=pal.dark(corange, 0.60), # col.axis=pal.dark(corange), 
                  xlab="Time", ylab=expression("R"[e]), side=2, yline=2, xline=2, ylims=c(0,5.75), 
                  xgrid = FALSE, ygrid = FALSE)

lines(x = c( min(times)-4.50, max(times) ) , 
      y = c(1,  1),
      col="darkblue", 
      lwd = 1.75, 
      lty = 2)


legend( x = "topleft", 
        inset= c(0.05, 0.025), 
        legend = c("Median Estimate", "95% HPD Interval", "1 Cutoff"), 
        col = "black", 
        fill = c(pal.dark(corange, 1), pal.dark(corange, 0.60), "darkblue"),   
        pt.cex = 4,  
        cex = 1 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.




plotSkylinePretty(times, delta_hpd, type='smooth', axispadding=0.0, 
                  col=pal.dark(cblue), fill=pal.dark(cblue, 0.60), # col.axis=pal.dark(cblue), 
                  xlab="Time", ylab=expression(delta), side=2, yline=2, xline=2,  ylims=c(0,1.75), 
                  xgrid = FALSE, ygrid = FALSE)

# lines(x = c( min(times)-4.50, max(times) ) , 
#       y = c(1,  1),
#       col="darkblue", 
#       lwd = 1.75, 
#       lty = 2)


legend( x = "topleft", 
        inset= c(0.05, 0.025), 
        legend = c("Median Estimate", "95% HPD Interval", "1 Cutoff"), 
        col = "black", 
        fill = c(pal.dark(cblue, 1), pal.dark(cblue, 0.60), "darkblue"),   
        pt.cex = 4,  
        cex = 1 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.




# Label A
label_function_a(label_value = "A", label_cex = 4)

# Label B
label_function_b(label_value = "B", label_cex = 4)


dev.off()



















# Fix 2024.09.24.
# TREUNCATED versions
# Creating list of truncated times
x_limits_to_plot <- c(1990,max(times))




# Versions on a single plot

# Raw version
pdf(paste("../Plots/part01_03_r_effective_raw_truncated.pdf", sep = ""), height = 7, width = 9)
# par(par(mfrow = c(1, 2)), mar = c(16.5, 5.1, 6, 2.1))
# par(mfrow = c(1, 2))

par(mar=c(4,4,3.5,3.5)+0.1)



plotSkylinePretty(times, delta_hpd, type='step', axispadding=0.0, 
                  col=pal.dark(cblue), fill=pal.dark(cblue, 0.60), col.axis=pal.dark(cblue), 
                  ylab=expression(delta), side=4, yline=2, ylims=c(0,6), xaxis=FALSE, xgrid = FALSE, ygrid = FALSE,
                  xlim = x_limits_to_plot )

plotSkylinePretty(times, Re_gridded_hpd, type='step', axispadding=0.0, 
                  col=pal.dark(corange), fill=pal.dark(corange, 0.60), col.axis=pal.dark(corange), 
                  xlab="Time", ylab=expression("R"[e]), side=2, yline=2.5, xline=2, xgrid=FALSE, 
                  ygrid=FALSE, gridcol=pal.dark(cgray), ylims=c(0,6), new=TRUE, add=TRUE, 
                  xlim = x_limits_to_plot )

lines(x = c( min(times)-4.50, max(times) ), 
      y = c(1,  1),
      col="darkblue", 
      lwd = 1.75, 
      lty = 2)


legend( x = "topleft", 
        inset= c(0.05, 0.05), 
        legend = c("Effective Reproduction Number Median Estimate", "95% HPD Interval", "Becoming Non-Infectious Rate Median Estimate", "95% HPD Interval", "1 Cutoff"), 
        col = "black", 
        fill = c(pal.dark(corange, 0.60), pal.dark(cblue, 0.60), pal.dark(corange, 1), pal.dark(corange, 0.60), "darkblue"),   
        pt.cex = 4,  
        cex = 0.80 ) 


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.



dev.off()




# Smoothed version
pdf(paste("../Plots/part01_03_r_effective_smoothed_truncated.pdf", sep = ""), height = 7, width = 9)
# par(par(mfrow = c(1, 2)), mar = c(16.5, 5.1, 6, 2.1))
# par(mfrow = c(1, 2))

par(mar=c(4,4,3.5,3.5)+0.1)

plotSkylinePretty(times, delta_hpd, type='smooth', axispadding=0.0, 
                  col=pal.dark(cblue), fill=pal.dark(cblue, 0.60), col.axis=pal.dark(cblue), 
                  ylab=expression(delta), side=4, yline=2, ylims=c(0,6), xaxis=FALSE, xgrid = FALSE, ygrid = FALSE,
                  xlim = x_limits_to_plot )

plotSkylinePretty(times, Re_gridded_hpd, type='smooth', axispadding=0.0, 
                  col=pal.dark(corange), fill=pal.dark(corange, 0.60), col.axis=pal.dark(corange), 
                  xlab="Time", ylab=expression("R"[e]), side=2, yline=2.5, xline=2, xgrid=FALSE, 
                  ygrid=FALSE, gridcol=pal.dark(cgray), ylims=c(0,6), new=TRUE, add=TRUE,
                  xlim = x_limits_to_plot )

lines(x = c( min(times)-4.50, max(times) ) , 
      y = c(1,  1),
      col="darkblue", 
      lwd = 1.75, 
      lty = 2)


legend( x = "topleft", 
        inset= c(0.05, 0.05), 
        legend = c("Effective Reproduction Number Median Estimate", "95% HPD Interval", "Becoming Non-Infectious Rate Median Estimate", "95% HPD Interval", "1 Cutoff"), 
        col = "black", 
        fill = c(pal.dark(corange, 0.60), pal.dark(cblue, 0.60), pal.dark(corange, 1), pal.dark(corange, 0.60), "darkblue"),   
        pt.cex = 4,  
        cex = 0.80 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.



dev.off()




# Two plots version




# Creating labeling functions
# We need two since we they use different coorinates

label_function_a <- function(label_value = "A", label_cex = 4) {
  
  par(xpd = NA )
  
  di <- dev.size("in")
  xx <- grconvertX(c(0, di[1]), from="in", to="user")
  yy <- grconvertY(c(0, di[2]), from="in", to="user")
  
  fig <- par("fig")
  xx <- xx[1] + (xx[2] - xx[1]) * fig[1:2]
  yx <- yy[1] + (yy[2] - yy[1]) * fig[3:4]
  
  txt <- label_value
  xx <- xx[1] + strwidth(txt, cex=4) * 6 / 5
  yy <- yy[2] - strheight(txt, cex=4) * 6 / 5
  text(xx, yy, txt, cex = label_cex )
  
}


label_function_b <- function(label_value = "A", label_cex = 4) {
  
  par(xpd = NA )
  
  di <- dev.size("in")
  xx <- grconvertX(c(0, di[1]), from="in", to="user")
  yy <- grconvertY(c(0, di[2]), from="in", to="user")
  
  fig <- par("fig")
  xx <- xx[1] + (xx[2] - xx[1]) * fig[1:2]
  yy <- yy[1]/2 + (yy[2] - yy[1]) * fig[3:4]
  
  txt <- label_value
  xx <- xx[1] + strwidth(txt, cex=4) * 6 / 5
  yy <- yy[2] - strheight(txt, cex=4) * 6 / 5
  text(xx, yy, txt, cex = label_cex )
  
}




# Raw version
pdf(paste("../Plots/part01_03_r_effective_raw_two_plots_truncated.pdf", sep = ""), height = 11, width = 9)
# par(par(mfrow = c(1, 2)), mar = c(16.5, 5.1, 6, 2.1))
par(mfrow = c(2, 1))

par(mar=c(4,4,2.5,3.5)+0.1)


plotSkylinePretty(times, Re_gridded_hpd, type='step', axispadding=0.0, 
                  col=pal.dark(corange), fill=pal.dark(corange, 0.60), # col.axis=pal.dark(corange), 
                  xlab="Time", ylab=expression("R"[e]), side=2, yline=2, xline=2, ylims=c(0,5.75), 
                  xgrid = FALSE, ygrid = FALSE, 
                  xlim = x_limits_to_plot )

lines(x = c( min(times)-4.50, max(times) ) , 
      y = c(1,  1),
      col="darkblue", 
      lwd = 1.75, 
      lty = 2)


legend( x = "topleft", 
        inset= c(0.05, 0.025), 
        legend = c("Median Estimate", "95% HPD Interval", "1 Cutoff"), 
        col = "black", 
        fill = c(pal.dark(corange, 1), pal.dark(corange, 0.60), "darkblue"),   
        pt.cex = 4,  
        cex = 1 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.




plotSkylinePretty(times, delta_hpd, type='step', axispadding=0.0, 
                  col=pal.dark(cblue), fill=pal.dark(cblue, 0.60), # col.axis=pal.dark(cblue), 
                  xlab="Time", ylab=expression(delta), side=2, yline=2, xline=2,  ylims=c(0,1.75), 
                  xgrid = FALSE, ygrid = FALSE, 
                  xlim = x_limits_to_plot )

# lines(x = c( min(times)-4.50, max(times) ) , 
#       y = c(1,  1),
#       col="darkblue", 
#       lwd = 1.75, 
#       lty = 2)


legend( x = "topleft", 
        inset= c(0.05, 0.025), 
        legend = c("Median Estimate", "95% HPD Interval", "1 Cutoff"), 
        col = "black", 
        fill = c(pal.dark(cblue, 1), pal.dark(cblue, 0.60), "darkblue"),   
        pt.cex = 4,  
        cex = 1 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.




# Label A
label_function_a(label_value = "A", label_cex = 4)

# Label B
label_function_b(label_value = "B", label_cex = 4)


dev.off()




# smoothed version
pdf(paste("../Plots/part01_03_r_effective_smoothed_two_plots_truncated.pdf", sep = ""), height = 11, width = 9)
# par(par(mfrow = c(1, 2)), mar = c(16.5, 5.1, 6, 2.1))
par(mfrow = c(2, 1))

par(mar=c(4,4,2.5,3.5)+0.1)


plotSkylinePretty(times, Re_gridded_hpd, type='smooth', axispadding=0.0, 
                  col=pal.dark(corange), fill=pal.dark(corange, 0.60), # col.axis=pal.dark(corange), 
                  xlab="Time", ylab=expression("R"[e]), side=2, yline=2, xline=2, ylims=c(0,5.75), 
                  xgrid = FALSE, ygrid = FALSE, 
                  xlim = x_limits_to_plot )

lines(x = c( min(times)-4.50, max(times) ) , 
      y = c(1,  1),
      col="darkblue", 
      lwd = 1.75, 
      lty = 2)


legend( x = "topleft", 
        inset= c(0.05, 0.025), 
        legend = c("Median Estimate", "95% HPD Interval", "Epidemic Threshold (1)"), 
        col = "black", 
        fill = c(pal.dark(corange, 1), pal.dark(corange, 0.60), "darkblue"),   
        pt.cex = 4,  
        cex = 1 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.




plotSkylinePretty(times, delta_hpd, type='smooth', axispadding=0.0, 
                  col=pal.dark(cblue), fill=pal.dark(cblue, 0.60), # col.axis=pal.dark(cblue), 
                  xlab="Time", ylab=expression(delta), side=2, yline=2, xline=2,  ylims=c(0,1.75), 
                  xgrid = FALSE, ygrid = FALSE, 
                  xlim = x_limits_to_plot )

# lines(x = c( min(times)-4.50, max(times) ) , 
#       y = c(1,  1),
#       col="darkblue", 
#       lwd = 1.75, 
#       lty = 2)


legend( x = "topleft", 
        inset= c(0.05, 0.025), 
        legend = c("Median Estimate", "95% HPD Interval"), 
        col = "black", 
        fill = c(pal.dark(cblue, 1), pal.dark(cblue, 0.60)),   
        pt.cex = 4,  
        cex = 1 ) 


# legend( x = "topleft", 
#         inset= c(0.05, 0.025), 
#         legend = c("Median Estimate", "95% HPD Interval", "1 Cutoff"), 
#         col = "black", 
#         fill = c(pal.dark(cblue, 1), pal.dark(cblue, 0.60), "darkblue"),   
#         pt.cex = 4,  
#         cex = 1 ) 
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.




# Label A
label_function_a(label_value = "A", label_cex = 4)

# Label B
label_function_b(label_value = "B", label_cex = 4)


dev.off()
























