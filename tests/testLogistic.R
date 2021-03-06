###
### $Id: testLogistic.R 945 2015-01-21 00:06:51Z proebuck $
###


if (!nzchar(Sys.getenv("SUPERCURVE_FULL_TEST"))) {
    cat(">>>>                <<<<", "\n")
    cat(">>>>  Test skipped  <<<<", "\n")
    cat(">>>>                <<<<", "\n")
    message(sprintf("To run all package tests, define %s environment variable",
                    dQuote("SUPERCURVE_FULL_TEST")))
    q("no")
}
options(warn=1)
library(SuperCurve)
library(robustbase)
library(boot)

extdata.dir <- system.file("extdata", package="SuperCurveSampleData")

######################################
## load the data from the 40 cell lines
home <- file.path(extdata.dir, "rppaCellData")

## first locate the list of assays
## the name 'proteins' is required
## must include two columns named 'Antibody' and 'Filename'.
proteins <- read.delim(file.path(home, "proteinAssay.tsv"), as.is=TRUE)
rownames(proteins) <- as.character(proteins$Antibody)

for (i in seq_len(nrow(proteins))) {
    temp <- RPPA(proteins$Filename[i],
                 path=home,
                 antibody=proteins$Antibody[i])
    assign(proteins$Antibody[i], temp, 1)
}
remove(i, temp)

######################################
## work out the appropriate design layout

steps <- rep(c(rep(8:5, 2), rep(4:1, 2)), 40) - 4.5
rep.temp <- factor(paste("Rep", rep(rep(1:2, each=4), 80), sep=""))
series <- factor(paste(as.character(AKT@data$Sample),
                       as.character(rep.temp),
                       sep="."))
## the name 'design' is required'
design <- RPPADesign(AKT, steps=steps, series=series)
remove(steps, rep.temp, series)

######################################
## must define the 'model' to use
model <- "logistic"

######################################
## must define the 'measure' to use
measure <- "Mean.Net"

######################################
## must define the 'method' to use
method <- "nlrq"
source("testRblock", echo=TRUE, max.deparse.len=1024)

method <- "nlrob"
source("testRblock", echo=TRUE, max.deparse.len=1024)

method <- "nls"
source("testRblock", echo=TRUE, max.deparse.len=1024)

######################################
## print the concentrations from the last fit. These will
## automatically be compared with the saved output when we
## run 'R CMD check'
round(temp@concentrations, digits=4)

######################################
x <- temp@concentrations
rep1 <- which(regexpr("Rep1", names(x)) > 0)
rep2 <- which(regexpr("Rep2", names(x)) > 0)
cat("Difference between replicates", "\n")
summary(x[rep1]-x[rep2])

