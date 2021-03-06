###
### $Id: sc5-rppaSet.R 927 2014-11-14 17:02:52Z proebuck $
### Fit a set of slides with a common layout
###


##=============================================================================
setClassUnion("OptionalRPPASpatialParams", c("RPPASpatialParams", "NULL"))

setClass("RPPASet",
         representation(call="call",                 ## function invocation
                        design="RPPADesign",         ## common for all slides
                        rppas="array",               ## vector of RPPAs
                        spatialparams="OptionalRPPASpatialParams",
                        prefitqcs="array",           ## vector of QC values
                        fitparams="RPPAFitParams",
                        fits="array",                ## set of fits
                        completed="matrix",          ## what worked/failed
                        normparams="RPPANormalizationParams",
                        version="character"))        ## package version
## :KRC: Why is "rppas" an array or vector instead of a list (or environment)?
## :PLR: Because Corwin? wrote it this way...


##-----------------------------------------------------------------------------
is.RPPASet <- function(x) {
    is(x, "RPPASet")
}


##-----------------------------------------------------------------------------
## Create the fit graphs and save them as PNG files
.createFitGraphs <- function(rppaset,
                             path,
                             prefix) {
    ## Check arguments
    stopifnot(is.RPPASet(rppaset))
    stopifnot(is.character(path)   && length(path) == 1)
    stopifnot(is.character(prefix) && length(prefix) == 1)

    ## Begin processing
    saved.par <- par(no.readonly=TRUE)
    on.exit(par(saved.par))
    fitdev <- dev.cur()

    ## Use red/yellow/green palette for residual plots.
    ## From RColorBrewer palette RdYlGn
    RYG <- c("#A50026",
             "#D73027",
             "#F46D43",
             "#FDAE61",
             "#FEE08B",
             "#FFFFBF",
             "#D9EF8B",
             "#A6D96A",
             "#66BD63",
             "#1A9850",
             "#006837")

    fitxform <- rppaset@fitparams@xform

    antibodies <- rownames(rppaset@fits)
    for (i in seq_along(antibodies)) {
        antibody <- antibodies[i]
        rppafit <- rppaset@fits[[i]]

        if (!is.RPPAFit(rppafit)) {
            warning(paste("cannot create fit graphs for", antibody))
            next
        }

        main <- .mkPlotTitle(rppafit@measure, antibody)

        ##
        ## First pair of plots
        ##
        par(bg="white",
            mfrow=c(2, 1))

        ## Plot sigmoid curve graph
        tryCatch(plot(rppafit,
                      main=main,
                      xform=fitxform,
                      xlim=c(-15, 15)),
                 error=function(e) {
                     message(sprintf("cannot plot sigmoid curve for %s",
                                     antibody))
                     warning(conditionMessage(e), immediate.=TRUE)
                 })

        ## Image of RSS
        ## Mark R^2 <= 0.4 as red.
        imageRPPAFit <- getMethod("image", class(rppafit))
        tryCatch(imageRPPAFit(rppafit,
                              col=RYG,
                              measure="ResidualsR2",
                              zlim=c(0.4, 1)),
                 error=function(e) {
                     message(sprintf("cannot produce RSS image for %s",
                                     antibody))
                     warning(conditionMessage(e), immediate.=TRUE)
                 })

        filename <- sprintf("%s_%s_1.png", prefix, antibody)
        dev.copy(png,
                 file.path(path, .portableFilename(filename)),
                 width=640,
                 height=640)
        dev.off()
        dev.set(fitdev)

        ##
        ## Second pair of plots
        ##
        par(bg="white",
            mfrow=c(2, 1))

        ## Plot residuals graph
        tryCatch(plot(rppafit,
                      main=main,
                      type="resid",
                      xform=fitxform,
                      xlim=c(-15, 15)),
                 error=function(e) {
                     message(sprintf("cannot plot residuals for %s",
                                     antibody))
                     warning(conditionMessage(e), immediate.=TRUE)
                 })

        ## Plot steps graph
        tryCatch(plot(rppafit,
                      main=main,
                      type="steps",
                      xform=fitxform,
                      xlim=c(-15, 15)),
                 error=function(e) {
                     message(sprintf("cannot plot steps for %s",
                                     antibody))
                     warning(conditionMessage(e), immediate.=TRUE)
                 })

        filename <- sprintf("%s_%s_2.png", prefix, antibody)
        dev.copy(png,
                 file.path(path, .portableFilename(filename)),
                 width=640,
                 height=640)
        dev.off()
        dev.set(fitdev)
    }
}


##-----------------------------------------------------------------------------
## Examine system() return code to determine if execution of the shell failed.
.execShellFailed <- if (getRversion() <= "2.12") {
                        function(rc) { rc == 32512 }
                    } else {
                        function(rc) { rc == 127 }
                    }


##-----------------------------------------------------------------------------
## Merge output graphs with source tiff file, save it as JPG file
.mergeGraphsAndImage <- function(antibody,
                                 prefix,
                                 outputdir,
                                 tiff) {
    ## Check arguments
    stopifnot(is.character(antibody)  && length(antibody) == 1)
    stopifnot(is.character(prefix)    && length(prefix) == 1)
    stopifnot(is.character(outputdir) && length(outputdir) == 1)
    stopifnot(is.character(tiff)      && length(tiff) == 1)

    ## Begin processing
    filename <- sprintf("%s_%s_1.png", prefix, antibody)
    pg1 <- file.path(outputdir, .portableFilename(filename))

    filename <- sprintf("%s_%s_2.png", prefix, antibody)
    pg2 <- file.path(outputdir, .portableFilename(filename))

    filename <- sprintf("%s.jpg", antibody)
    output <- file.path(outputdir, .portableFilename(filename))

    ## Use ImageMagick 'convert' binary to perform merge
    command <- paste("convert",
                     shQuote(pg1),
                     shQuote(pg2),
                     "+append",
                     shQuote(tiff),
                     "-append",
                     "-quality 100",
                     shQuote(output))
    rc <- switch(EXPR=.Platform$OS.type,
                 unix=system(command),
                 windows=shell(command),
                 stop(sprintf("unrecognized operating system family %s",
                              sQuote(.Platform$OS.type))))
    #cat("rc =", rc, ", command:", command, "\n")

    return(rc)
}


##-----------------------------------------------------------------------------
.plotProbabilityOfGoodSlide <- function(qcprobs,
                                        good.cutoff=0.8) {
    stopifnot(is.numeric(qcprobs)     && length(qcprobs) >= 1)
    stopifnot(is.numeric(good.cutoff) && length(good.cutoff) == 1)
    stopifnot(.isProbability(qcprobs))
    stopifnot(.isProbability(good.cutoff))

    nslides <- length(qcprobs)
    rating <- rep("poor", len=nslides)
    rating[which(qcprobs >= good.cutoff)] <- "good"

    rating.fac <- ordered(rating, levels=c("poor", "good"))
    col.qcprobs <- c("red", "green")
    stopifnot(nlevels(rating.fac) == length(col.qcprobs))

    plot(qcprobs,
         las=1,
         main="Predicted Slide Quality",
         sub=sprintf("#Good = %d, #Poor = %d",
                     ngood <- sum(rating == "good"),
                     npoor <- nslides - ngood),
         type='n',
         xaxt='n',
         xlim=c(1, nslides),
         yaxt='n',
         ylab="Probability of Good Slide",
         ylim=0:1)
    mtext(side=3, paste("cutoff =", good.cutoff))
    axis(1, at=seq_len(nslides))
    axis(2, at=seq(0, 1, by=0.1), las=1)
    rect(xleft=1,
         ybottom=c(0, good.cutoff),
         xright=nslides,
         ytop=c(good.cutoff, 1),
         col=c('lightpink', 'lightgreen'),
         border=NA)
    text(x=nslides/2,
         y=c(good.cutoff/2, 1-((1-good.cutoff)/2)),
         labels=toupper(levels(rating.fac)),
         cex=2)
    abline(h=good.cutoff)
    points(qcprobs,
           bg=col.qcprobs[rating.fac],
           col='black',
           pch=21)
}


##-----------------------------------------------------------------------------
## Return TRUE if PreFit QC was performed.
ran.prefitqc <- function(rppaset) {
    ## Check arguments
    stopifnot(is.RPPASet(rppaset))

    ## Begin processing
    prefitqcs.tf <- rppaset@completed[, "prefitqc"]
    return(!all(is.na(prefitqcs.tf)))
}


##-----------------------------------------------------------------------------
setMethod("normalize", signature(object="RPPASet"),
          function(object, ...) {
    concs <- .fitSlot(object, "concentrations")
    normparams <- object@normparams
    arglist <- c(list(concs,
                      method=normparams@method),
                 normparams@arglist,
                 ...)

    ## Assemble matrix of concentrations from all fits in object
    do.call(callGeneric, arglist)
})


##-----------------------------------------------------------------------------
## See R FAQ (8.1 How should I write summary methods?)
setMethod("summary", signature(object="RPPASet"),
          function(object,
                   onlynormqcgood=ran.prefitqc(object),
                   ...) {
    dots <- list(...)
    monitor <- if ("monitor" %in% names(dots)) {
                   dots[["monitor"]]
               }
    RPPASetSummary(object,
                   onlynormqcgood,
                   monitor)
})


##-----------------------------------------------------------------------------
## Provide a convenience function to save fit results to disk
setMethod("write.summary", signature(object="RPPASet"),
          function(object,
                   path,
                   prefix="supercurve",
                   graphs=TRUE,
                   tiffdir=NULL,
                   onlynormqcgood=ran.prefitqc(object),
                   monitor=NULL,
                   ...) {
    ## Check arguments
    if (!is.character(path)) {
        stop(sprintf("argument %s must be character",
                     sQuote("path")))
    } else if (!(length(path) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("path")))
    } else if (!dir.exists(path)) {
        stop(sprintf("directory %s does not exist",
                     dQuote(path)))
    } else if (!dir.writable(path)) {
        stop(sprintf("directory %s is not writable",
                     dQuote(path)))
    }

    if (!is.character(prefix)) {
        stop(sprintf("argument %s must be character",
                     sQuote("prefix")))
    } else if (!(length(prefix) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("prefix")))
    }

    if (is.numeric(graphs)) {
        graphs <- as.logical(graphs)
    }

    if (!is.logical(graphs)) {
        stop(sprintf("argument %s must be logical",
                     sQuote("graphs")))
    } else if (!(length(graphs) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("graphs")))
    }

    if (is.numeric(onlynormqcgood)) {
        onlynormqcgood <- as.logical(onlynormqcgood)
    }

    if (!is.logical(onlynormqcgood)) {
        stop(sprintf("argument %s must be logical",
                     sQuote("onlynormqcgood")))
    } else if (!(length(onlynormqcgood) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("onlynormqcgood")))
    }

    if (!is.null(monitor)) {
        if (!is.SCProgressMonitor(monitor)) {
            stop(sprintf("argument %s must be object of class %s",
                         sQuote("monitor"), "SCProgressMonitor"))
        }
    } else {
        ## Create one, if necessary
        monitor <- SCProgressMonitor()
    }

    ## Begin processing

    ## Make sure at least one fit exists
    rppafits <- object@fits
    if (all(sapply(rppafits, is.null))) {
        stop("cannot summarize as no fits were stored")
    }

    ## Graph fits, if requested
    if (graphs) {
        pkgimgdir <- system.file("images", package="SuperCurve")

        if (is.null(tiffdir)) {
            ## Assume the tif images are in a sibling directory named "tif"
            tiffdir <- normalizePath(file.path(dirname(path), "tif"))
            if (!dir.exists(tiffdir)) {
                ## As last resort, use package directory for missing image
                message(sprintf("image directory unspecified and sibling directory %s does not exist",
                                dQuote(tiffdir)))
                tiffdir <- pkgimgdir
            }
        }

        if (!is.character(tiffdir)) {
            stop(sprintf("argument %s must be character",
                         sQuote("tiffdir")))
        } else if (!(length(tiffdir) == 1)) {
            stop(sprintf("argument %s must be of length 1",
                         sQuote("tiffdir")))
        } else if (!dir.exists(tiffdir)) {
            stop(sprintf("directory %s does not exist",
                         dQuote(tiffdir)))
        }

        ## Save fit graphs
        dev.new(title="Fit Graphs")
        progressMarquee(monitor) <- "Creating Fit Graphs"
        .createFitGraphs(object, path, prefix)

        ## Merge output graphs with source tiff file for each antibody
        imgfiles <- {
                        txtfiles <- sapply(rppafits,
                                           function(fit) {
                                               if (is.RPPAFit(fit)) {
                                                   fit@rppa@file
                                               } else {
                                                   NULL
                                               }
                                           })
                        txt.re <- "\\.[tT][xX][tT]$"
                        sub(txt.re, ".tif", txtfiles)
                    }

        ## For each antibody...
        progressMarquee(monitor) <- "Merging Fit Graphs With Images"
        progressValue(monitor) <- 0
        antibodies <- names(rppafits)
        progressMaximum(monitor) <- length(antibodies)
        for (i in seq_along(antibodies)) {
            antibody <- antibodies[i]
            progressLabel(monitor) <- antibody

            rppafit <- rppafits[[i]]
            if (is.RPPAFit(rppafit)) {
                message(paste("merging graphs and image for", antibody))
                flush.console()

                ## If no corresponding image exists, substitute "missing" image
                imgfile <- file.path(tiffdir, imgfiles[antibody])
                if (!file.exists(imgfile)) {
                    imgfile <- file.path(pkgimgdir, "missing_slide.tif")
                }

                ## Create merged image
                rc <- .mergeGraphsAndImage(antibody,
                                           prefix,
                                           path,
                                           imgfile)
                if (.execShellFailed(rc)) {
                    warning(sprintf("ImageMagick executable %s not installed or unavailable via PATH",
                                    sQuote("convert")),
                            call.=FALSE)
                    message("===> unable to create merged image files...")
                    flush.console()
                    break
                }
            }
            progressValue(monitor) <- i
        }
    }

    ## Write CSV files
    callGeneric(summary(object,
                        onlynormqcgood=onlynormqcgood,
                        monitor=monitor),
                path,
                prefix,
                monitor=monitor)
})


##-----------------------------------------------------------------------------
.loadAntibodyInfo <- function(antibodyfile,
                              slidefiles) {
    ## Check arguments
    stopifnot(is.character(antibodyfile) && length(antibodyfile) == 1)
    stopifnot(is.character(slidefiles) && length(slidefiles) >= 1)

    ## Begin processing
    tryCatch({
            stopifnot(file.exists(antibodyfile))
            if (file.info(antibodyfile)$isdir) {
                stop("argument is not a file")
            }

            ## Read datafile
            proteinassay.df <- read.delim(antibodyfile,
                                          as.is=TRUE,
                                          quote="",
                                          row.names=NULL)

            reqdColnames <- c("Antibody",
                              "Filename")

            ## Ensure required columns exist
            found <- reqdColnames %in% colnames(proteinassay.df)
            if (!(all(found))) {
                missingColumns <- reqdColnames[!found]
                stop(sprintf(ngettext(length(missingColumns),
                                      "missing required column: %s",
                                      "missing required columns: %s"),
                             paste(dQuote(missingColumns), collapse=", ")))
            }
        },
        error=function(cond) {
            stop(sprintf("cannot load antibody data from file %s - %s",
                         dQuote(antibodyfile),
                         conditionMessage(cond)))
        })

    ## Extract information from data.frame
    antibodies <- vector("list", length(slidefiles))
    names(antibodies) <- slidefiles

    for (filename in slidefiles) {
        x.antibody <- match(filename, proteinassay.df$Filename)[1]
        antibody <- proteinassay.df$Antibody[x.antibody]
        if (!is.na(antibody)) {
            x.slidefiles <- match(filename, slidefiles)
            antibodies[[x.slidefiles]] <- antibody
        }
    }

    return(antibodies)
}


##-----------------------------------------------------------------------------
## Create an RPPA set from a directory of slides.
RPPASet <- function(path,
                    designparams,
                    fitparams,
                    spatialparams=NULL,
                    normparams,
                    doprefitqc=FALSE,
                    monitor=SCProgressMonitor(),
                    antibodyfile=NULL,
                    software="microvigene",
                    alt.layout=NULL) {
    ## Check arguments
    if (!is.character(path)) {
        stop(sprintf("argument %s must be character",
                     sQuote("path")))
    } else if (!(length(path) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("path")))
    } else if (!dir.exists(path)) {
        stop(sprintf("directory %s does not exist",
                     dQuote(path)))
    }

    if (!is.RPPADesignParams(designparams)) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("designparams"), "RPPADesignParams"))
    }

    if (!is.RPPAFitParams(fitparams)) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("fitparams"), "RPPAFitParams"))
    }

    if (!is.null(spatialparams)) {
        if (!is.RPPASpatialParams(spatialparams)) {
            stop(sprintf("argument %s must be object of class %s",
                         sQuote("spatialparams"), "RPPASpatialParams"))
        }
    }

    if (!is.RPPANormalizationParams(normparams)) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("normparams"), "RPPANormalizationParams"))
    }

    if (!is.SCProgressMonitor(monitor)) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("monitor"), "SCProgressMonitor"))
    }

    if (!is.null(antibodyfile)) {
        if (!is.character(antibodyfile)) {
            stop(sprintf("argument %s must be character",
                         sQuote("antibodyfile")))
        } else if (!(length(antibodyfile) == 1)) {
            stop(sprintf("argument %s must be of length 1",
                         sQuote("antibodyfile")))
        } else if (!nzchar(antibodyfile)) {
            stop(sprintf("argument %s must not be empty string",
                         sQuote("antibodyfile")))
        }

        if (!.isAbsolutePathname(antibodyfile)) {
            antibodyfile <- file.path(path, antibodyfile)
        }
    }


    ##-------------------------------------------------------------------------
    ## Returns the names of all TXT files in directory argument.
    ## :TBD: Should this get the list of slides from a file ('proteinAssay.tsv'
    ## or 'targets.txt') instead of assuming all .txt files are slides?
    getQuantificationFilenames <- function(path) {
        stopifnot(is.character(path) && length(path) == 1)

        ## Assumes all .txt files in the directory are slides
        txt.re <- "\\.[tT][xX][tT]$"
        txtfiles <- list.files(path=path, pattern=txt.re)
        ## If SuperCurveGUI's input and output directories refer to the same
        ## path, then its settings file in TEXT format could be present...
        settingsfile.tf <- txtfiles %in% "sc-settings.txt"
        txtfiles[!settingsfile.tf]
    }


    ## Begin processing

    ## Record the call made to this method, but replace 'monitor'.
    ## Allows 'sc-rppaset.RData' to be rerun without needing GUI components
    ## if initially created using scui() method from 'SuperCurveGUI' package.
    call <- match.call()
    call$monitor <- quote(SCProgressMonitor())

    ## Get filenames of slides to process
    progressStage(monitor) <- "Data Input"
    slideFilenames <- getQuantificationFilenames(path)
    if (length(slideFilenames) == 0) {
        stop(sprintf("no quantification files found in directory %s",
                     dQuote(path)))
    }

    ## Load antibody information, if provided
    ab.list <- if (!is.null(antibodyfile)) {
                   progressMarquee(monitor) <- "Loading Antibody File"
                   .loadAntibodyInfo(antibodyfile, slideFilenames)
               } else {
                   vector("list", length(slideFilenames))
               }

    ## Fill in missing values with generated defaults
    x.which <- which(sapply(ab.list, is.null))
    txt.re <- "\\.[tT][xX][tT]$"
    for (x in x.which) {
        ab.list[[x]] <- sub(txt.re, "", slideFilenames[x])
    }

    ## Ensure antibody names are unique
    antibodies <- make.unique(abnames <- unlist(ab.list, use.names=FALSE))
    if (!identical(antibodies, abnames)) {
        warning("adjusting antibody names to be unique")
    }
    remove(abnames)

    ## Tracking success/failure of each step
    input.tf    <- logical(length(slideFilenames))
    spatial.tf  <- logical(length(slideFilenames))
    prefitqc.tf <- logical(length(slideFilenames))
    fits.tf     <- logical(length(slideFilenames))

    ## Load slides to process
    progressMarquee(monitor) <- "Reading slides"
    progressMaximum(monitor) <- length(slideFilenames)

    design <- NULL
    rppas <- array(list(), length(slideFilenames), list(antibodies))
    for (i in seq_along(slideFilenames)) {

        slideFilename <- slideFilenames[i]
        antibody <- antibodies[i]

        progressLabel(monitor) <- slideFilename
        message(paste("reading", slideFilename))
        flush.console()

        rppa <- tryCatch({
                        RPPA(slideFilename,
                             path=path,
                             antibody=antibody,
                             software=software,
                             alt.layout=alt.layout)
                    },
                    error=function(e) {
                        message(conditionMessage(e))
                        NULL
                    })

        if (is.RPPA(rppa)) {
            ## Update only on success
            rppas[[i]] <- rppa
            input.tf[i] <- TRUE

            ## If design has not been created...
            if (is.null(design)) {
                ## Create design
                design <- RPPADesignFromParams(rppa, designparams)

                ## Plot the first possible slide as a quick design check
                dev.new(title="Design Check")
                plot(rppa,
                     design,
                     fitparams@measure)

                ## :TODO: Need method to force R graphics system to update the
                ## plot window on OS X, which otherwise doesn't display until
                ## the computation ends (defeating its purpose).
            }
        }

        progressValue(monitor) <- i
    }

    ## This will trigger if no RPPAs exist.
    if (!is.RPPADesign(design)) {
        stop("no slides can be processed")
    }

    ##-------------------------------------------------------------------------
    ## Determines if spatial adjustment processing is warranted
    shouldPerformSpatialAdj <- function(spatialparams, fitparams) {
        stopifnot(is.RPPAFitParams(fitparams))

        measures <- eval(formals(spatialCorrection)$measure)
        is.RPPASpatialParams(spatialparams) && (fitparams@measure %in% measures)
    }


    ## Perform spatial adjustment, if enabled
    doSpatialAdj <- shouldPerformSpatialAdj(spatialparams, fitparams)
    if (doSpatialAdj) {
        if (spatialparams@plotSurface) {
            ## Open new device if surface plots were requested
            dev.new(title="Surface Plots")
            message("*** watch for prompts to plot on R console ***")
        }
        progressStage(monitor) <- "Spatial Adj"
        progressMarquee(monitor) <- "Performing spatial adjustment on slides"
        progressValue(monitor) <- 0

        for (i in seq_along(slideFilenames)) {
            antibody <- antibodies[i]

            progressLabel(monitor) <- antibody
            message(paste("spatially adjusting slide",
                          antibody, "-", "please wait."))
            flush.console()

            rppa <- rppas[[i]]
            if (!is.null(rppa)) {
                rppa <- tryCatch({
                                spatialAdjustmentFromParams(rppa,
                                                            design,
                                                            spatialparams)
                            },
                            error=function(e) {
                                traceback()
                                message(conditionMessage(e))
                                rppa@data$Adj.Mean.Net <- rppa@data$Mean.Net
                                rppa@data$Adj.Mean.Total <- rppa@data$Mean.Total
                                rppa
                            })
                if (is.RPPA(rppa)) {
                    ncols.list <- lapply(c(rppa, rppas[[i]]),
                                         function(x) {
                                             ncol(df <- slot(x, "data"))
                                         })
                    if (!do.call("identical", ncols.list)) {
                        ## Update only on modification
                        rppas[[i]] <- rppa
                        spatial.tf[i] <- TRUE
                    }
                    remove(ncols.list)
                }
            } else {
                warning(paste("no slide to adjust for", antibody))
            }
            progressValue(monitor) <- i
        }
    } else {
        spatial.tf <- rep(NA, length(spatial.tf))
    }

    ## Perform pre-fit QC, if enabled
    prefitqcs <- array(list(), length(slideFilenames), list(antibodies))
    if (doprefitqc) {
        progressStage(monitor) <- "Pre-Fit QC"
        progressMarquee(monitor) <- "Performing quality checks on slides"
        progressValue(monitor) <- 0

        for (i in seq_along(slideFilenames)) {
            antibody <- antibodies[i]

            progressLabel(monitor) <- antibody
            message(paste("quality checking slide",
                          antibody, "-", "please wait."))
            flush.console()

            rppa <- rppas[[i]]
            if (!is.null(rppa)) {
                prefitqc <- tryCatch({
                                RPPAPreFitQC(rppa,
                                             design,
                                             doSpatialAdj)
                            },
                            error=function(e) {
                                traceback()
                                message(conditionMessage(e))
                                NULL
                            })
                ## Update only on success
                if (is.RPPAPreFitQC(prefitqc)) {
                    prefitqcs[[i]] <- prefitqc
                    prefitqc.tf[i] <- TRUE
                }
            } else {
                warning(paste("no slide to quality check for", antibody))
            }
            progressValue(monitor) <- i
        }

        ## Plot 'goodness of slide' values
        dev.new(title="Predicted Slide Quality Plot")
        tryCatch({
                     qcprobs <- sapply(prefitqcs, qcprob)
                     qcprobs[is.na(qcprobs)] <- 0
                     .plotProbabilityOfGoodSlide(qcprobs)
                 },
                 error=function(e) {
                     message("cannot plot slide quality probabilities")
                     warning(conditionMessage(e), immediate.=TRUE)
                 })
    } else {
        prefitqc.tf <- rep(NA, length(prefitqc.tf))
    }

    ##-------------------------------------------------------------------------
    ## Reporting of progress through fitting is unbearably slow. Modify the
    ## label only so the program still looks alive...
    updateLabelWhileFitting <- function(phase) {
        progressLabel(monitor) <- sprintf("%s [%s]", antibody, phase)
    }


    ## Create fits
    progressStage(monitor) <- "Curve Fitting"
    progressMarquee(monitor) <- "Fitting slides"
    progressValue(monitor) <- 0

    adj.fitparams <- fitparams
    if (doSpatialAdj) {
        message("fits will be performed using spatially adjusted measure")
        adjMeasure <- paste("Adj", fitparams@measure, sep=".")
        adj.fitparams@measure <- adjMeasure
    }
    fits <- array(list(), length(slideFilenames), list(antibodies))
    for (i in seq_along(slideFilenames)) {
        antibody <- antibodies[i]

        progressLabel(monitor) <- antibody
        message(paste("fitting", antibody, "-", "please wait."))
        flush.console()

        rppa <- rppas[[i]]
        if (!is.null(rppa)) {
            fit <- tryCatch({
                            RPPAFitFromParams(rppa,
                                              design=design,
                                              fitparams=adj.fitparams,
                                              updateLabelWhileFitting)
                        },
                        error=function(e) {
                            message(conditionMessage(e))
                            NULL
                        })

            ## Update only on success
            if (is.RPPAFit(fit)) {
                fits[[i]] <- fit
                fits.tf[i] <- TRUE
            }
        } else {
            warning(paste("no slide to fit for", antibody))
        }
        progressValue(monitor) <- i
    }

    ## Create matrix for tracking what succeeded/failed
    completed <- cbind(input.tf,
                       spatial.tf,
                       prefitqc.tf,
                       fits.tf)
    rownames(completed) <- slideFilenames
    colnames(completed) <- names(getStages()[1:4])

    ## Create new class
    new("RPPASet",
        call=call,
        rppas=rppas,
        spatialparams=spatialparams,
        design=design,
        prefitqcs=prefitqcs,
        fits=fits,
        fitparams=fitparams,
        normparams=normparams,
        completed=completed,
        version=packageDescription("SuperCurve", fields="Version"))
}

