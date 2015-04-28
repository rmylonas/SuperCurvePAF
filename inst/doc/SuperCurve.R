### R code from vignette source 'SuperCurve.Rnw'

###################################################
### code chunk number 1: getStarted01
###################################################
library(SuperCurve)
data(rppaCell)
ls()


###################################################
### code chunk number 2: getStarted02
###################################################
extdata.dir <- system.file("extdata", package="SuperCurveSampleData");
rawdata.dir <- file.path(extdata.dir, "rppaCellData");
filename <- "Akt.txt";
aktTake2 <- RPPA(file=filename, path=rawdata.dir);
class(aktTake2);
isTRUE(all.equal(akt, aktTake2, check.attributes=FALSE));


###################################################
### code chunk number 3: getStarted03
###################################################
slotNames(aktTake2);
aktTake2@file;


###################################################
### code chunk number 4: getStarted04
###################################################
summary(aktTake2)
names(aktTake2@data)


###################################################
### code chunk number 5: getStarted05
###################################################
read.user <- function(file) {
    df <- read.delim(file, skip=4);
    names(df)[5] <- "Sample";
    names(df)[6] <- "Mean.Net";
    return(df);
}
filename <- "Akt.txt";
aktTake3 <- RPPA(file=filename, path=rawdata.dir, software="user");
isTRUE(all.equal(akt, aktTake3, check.attributes=FALSE));


###################################################
### code chunk number 6: getStarted06
###################################################
image(akt);


###################################################
### code chunk number 7: getStarted07
###################################################
image(akt, "Mean.Net", colorbar=TRUE);


###################################################
### code chunk number 8: getStarted08
###################################################
image(akt, "Vol.Bkg", colorbar=TRUE);


###################################################
### code chunk number 9: getStarted09 (eval = FALSE)
###################################################
## with(akt@data,
##      plot(Vol.Bkg / (Mean.Total - Mean.Net)));


###################################################
### code chunk number 10: getStarted10
###################################################
image(ctnnb1, "Vol.Bkg", colorbar=TRUE);


###################################################
### code chunk number 11: getStarted11
###################################################
class(design40);
slotNames(design40);
class(design40@layout);


###################################################
### code chunk number 12: getStarted12
###################################################
names(design40@layout);


###################################################
### code chunk number 13: getStarted13
###################################################
design40@layout[1:17, ];


###################################################
### code chunk number 14: getStarted14
###################################################
steps <- rep(c(rep(8:5, 2), rep(4:1, 2)), 40) - 4.5;
rep.temp <- factor(paste('Rep', rep(rep(1:2, each=4), 80), sep=''));
series <- factor(paste(as.character(akt@data$Sample),
                       as.character(rep.temp),
                       sep='.'));
design40Take2 <- RPPADesign(akt, steps=steps, series=series);


###################################################
### code chunk number 15: getStarted15
###################################################
filename <- "Akt.txt";
aktTemp <- read.delim(file=file.path(rawdata.dir, filename), skip=4);
names(aktTemp)[5] <- "Sample";
designColHdrs <- c("Main.Row", "Main.Col", "Sub.Row", "Sub.Col", "Sample");
aktTemp <- aktTemp[, names(aktTemp) %in% designColHdrs];
steps <- rep(c(rep(8:5, 2), rep(4:1, 2)), 40) - 4.5;
rep.temp <- factor(paste('Rep', rep(rep(1:2, each=4), 80), sep=''));
series <- factor(paste(as.character(aktTemp$Sample),
                       as.character(rep.temp),
                       sep='.'));
aktLayout <- data.frame(aktTemp, Steps=steps, Series=series);
aktNames <- levels(aktLayout$Sample);
aktAlias <- list(Alias=aktNames, Sample=aktNames);
aktSampleMap <- as.vector(tapply(as.character(aktLayout$Sample),
                                 list(series),
                                 function(x) x[[1]]));
names(aktSampleMap) <- levels(aktLayout$Series);
design40Take3 <- new("RPPADesign", layout=aktLayout,
                     alias=aktAlias, sampleMap=aktSampleMap);


###################################################
### code chunk number 16: getStarted20
###################################################
aktFit <- RPPAFit(akt, design40, "Mean.Net");
class(aktFit);
slotNames(aktFit);
aktFit@call;
aktFit@version;


###################################################
### code chunk number 17: getStarted21
###################################################
plot(aktFit); # basic cloud plot


###################################################
### code chunk number 18: getStarted22
###################################################
plot(fitted(aktFit, 'X'), fitted(aktFit)); # the main curve
plot(fitted(aktFit, 'X'), resid(aktFit)); # residuals


###################################################
### code chunk number 19: getStarted23
###################################################
image(aktFit);


###################################################
### code chunk number 20: getStarted24 (eval = FALSE)
###################################################
## oldAsk <- par(ask=TRUE);
## plot(aktFit, type="individual");
## par(oldAsk);


###################################################
### code chunk number 21: getStarted25
###################################################
aktFit@concentrations[1];
aktFit@concentrations["sample1.Rep1"];


###################################################
### code chunk number 22: getStarted26 (eval = FALSE)
###################################################
## coefficients(aktFit);
## coef(aktFit);
## aktFit@coefficients;


###################################################
### code chunk number 23: getStarted27
###################################################
M1 <- (aktFit@concentrations[seq(2, 80, 2)] -
       aktFit@concentrations[seq(1, 80, 2)]);
A1 <- (aktFit@concentrations[seq(2, 80, 2)] +
       aktFit@concentrations[seq(1, 80, 2)]) / 2;
plot(A1, M1);


###################################################
### code chunk number 24: getStarted28
###################################################
steps <- rep(c(rep(8:5, 2), rep(4:1, 2)), 40) - 4.5;
rep.temp <- factor(paste('Rep', rep(rep(1:2, each=4), 80), sep=''));
series <- akt@data$Sample;
design40Sample <- RPPADesign(akt, steps=steps, series=series);


###################################################
### code chunk number 25: getStarted29
###################################################
aktFitSample <- RPPAFit(akt, design40Sample, "Mean.Net");


