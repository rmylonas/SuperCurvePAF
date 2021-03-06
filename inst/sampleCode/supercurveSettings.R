###
### $Id: supercurveSettings.R 840 2014-06-27 16:50:25Z proebuck $
###

library("LysateArray")
tumor.home <- 'LysateArray/slides'
outputfile <- 'supercurve.csv'
ndilut <- 7
nsample <- 96
AME <- FALSE
retitle <- TRUE
normalize <- FALSE
median.normalize <- FALSE
control.spots <- TRUE
block.unique <- TRUE
control.samples <- NA
diagnostics <- TRUE
addclinical <- FALSE
medsub <- FALSE
spot.intensity <- 'mean.net'

