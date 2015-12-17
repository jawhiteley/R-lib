####################################################
# Making wave functions
# Adding wave functions to simulate cyclical patterns
# e.g. Temperature
# Jonathan Whiteley		2011-02-20		R v2.12
####################################################
# TO DO
# - make the amplitude fluctuate through time
# - multi-year cycles?
# - compare with / try to simulate TRH data from HOBO dataloggers
####################################################
## INITIALISE
####################################################
rm(list=ls())	# clear memory
getwd()	# check that we're in the right place.

## LOAD PACKAGES


####################################################
## Define Parameters
####################################################

yr     <- 365  # days
hs_d   <- 24   # hours per day
pts_h  <- 1    # points per hour
pts_d  <- hs_d * pts_h
xrange <- yr * 1
days   <- seq( 0, xrange, by=(1/pts_d) )

period.yr <- ( yr/(pi*2) )  #  sin wave with a period of 365 days
phase_shift.yr <- -yr/4	# start period @ -1 (winter)
amp.yr    <- 25  # amplitude of yearly fluctuations
mean.yr   <- 5   # long-term average
yr_wave <- mean.yr + sin( (days + phase_shift.yr) / period.yr ) * amp.yr

period.wk <- ( 14/(pi*2) )  #  sin wave with a period of 2 weeks (?)
phase_shift.wk <- -14/4	# start period @ -1 (?)
amp.wk    <- 1  # amplitude of weekly fluctuations
wk_wave <- sin( (days + phase_shift.wk) / period.wk ) * amp.wk

period.d <- ( 1/(pi*2) )  #  sin wave with a period of 1 day
phase_shift.d <- -1/4	# start period @ -1 (midnight)
amp.d    <- 10  # amplitude of daily fluctuations
d_wave <- sin( (days + phase_shift.d) / period.d ) * amp.d

noise <- 5  # amplitude of noise
white_noise <- runif(days, min=-noise, max=noise)
white_noise <- rnorm(days, mean=0, sd=noise/2)

max_range <- max(amp.yr, amp.d, amp.wk, noise)
yrange <- c(-max_range, max_range) + mean.yr

debug <- FALSE  # set to true to see more verbose output for debugging:
  #  plots of individual waves
####################################################
## Plots
####################################################

sin_wave <- sin(days/10)


if (debug == TRUE) {
  plot( sin_wave ~ days, type="l", xlim=c(0,pi*40) )
  plot( yr_wave ~ days, type="l", ylim = yrange )
  plot( wk_wave ~ days, type="l", ylim = yrange )
  plot( d_wave ~ days, type="l", xlim=c(0,4), ylim = yrange )
  plot( white_noise ~ days, type="l", ylim = yrange )
}

##==================================================
## Adding waves together

sum_wave <- yr_wave + wk_wave + d_wave
noisy_wave <- sum_wave + white_noise


plot( sum_wave ~ days, type="l" )
plot( noisy_wave ~ days, type="l" )
plot( noisy_wave ~ days, type="p", pch="." )


