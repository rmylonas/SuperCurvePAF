###
### $Id: readQuantification.R 859 2014-07-06 19:25:22Z proebuck $
###

##
## Routines defined within are not really intended for direct consumption
## by user. RPPA generator should be considered the public interface.
##

##-----------------------------------------------------------------------------
.getReadMethod <- function(software) {
    ## Check arguments
    stopifnot(is.character(software) && length(software) == 1)

    ## Begin processing
    methodName <- paste("read", software, sep=".")
    if (exists(methodName,
               mode="function",
               globalenv())) {
        get(methodName,
            mode="function",
            globalenv())
    } else if (exists(methodName,
                      mode="function",
                      asNamespace("SuperCurve"))) {
        get(methodName,
            mode="function",
            asNamespace("SuperCurve"))
    } else {
        warning(sprintf("no user-provided method named %s found",
                        sQuote(methodName)))
        NULL
    }
}


##-----------------------------------------------------------------------------
.getLayoutMethod <- function(layout) {
    ## Check arguments
    stopifnot(is.character(layout) && length(layout) == 1)

    ## Begin processing
    methodName <- paste("layout", "as", layout, sep=".")
    if (exists(methodName,
               mode="function",
               globalenv())) {
        get(methodName,
            mode="function",
            globalenv())
    } else if (exists(methodName,
                      mode="function",
                      asNamespace("SuperCurve"))) {
        get(methodName,
            mode="function",
            asNamespace("SuperCurve"))
    } else {
        warning(sprintf("no user-provided method named %s found",
                        sQuote(methodName)))
        NULL
    }
}


##-----------------------------------------------------------------------------
## Reads quantification datafiles and returns data frame containing
## the desired information
readQuantification <- function(conn,
                               software,
                               alt.layout) {
    ## Check arguments
    if (!inherits(conn, "connection")) {
        stop(sprintf("argument %s must be connection",
                     sQuote("conn")))
    } else if (!isOpen(conn, "r")) {
        stop(sprintf("connection %s not open for read",
                     dQuote(summary(conn)$description)))
    }

    if (!is.character(software)) {
      #:KRC: should try(as.character...) instead
        stop(sprintf("argument %s must be character",
                     sQuote("software")))
    } else if (!(length(software) == 1)) {
      #:KRC: warning, not error unless length=0
        stop(sprintf("argument %s must be of length 1",
                     sQuote("software")))
    } else if (!nzchar(software)) {
        stop(sprintf("argument %s must not be empty string",
                     sQuote("software")))
    }

    if (!is.null(alt.layout)) {
        if (!is.character(alt.layout)) {
            stop(sprintf("argument %s must be character",
                         sQuote("alt.layout")))
        } else if (!(length(alt.layout) == 1)) {
            stop(sprintf("argument %s must be of length 1",
                         sQuote("alt.layout")))
        } else if (!nzchar(alt.layout)) {
            stop(sprintf("argument %s must not be empty string",
                         sQuote("alt.layout")))
        }
    }

    ## Begin processing
    pathname <- summary(conn)$description

    ## The datafile format for each row must contain the four values
    ## needed to specify the logical location of a spot on an array,
    ## the unique identifier of the sample at that location, and an
    ## unspecified numeric measurement. Additional columns may be present.

    readMethod <- .getReadMethod(software)
    quant.df <- if (is.function(readMethod)) {
                    readMethod(conn)
                }

    if (is.null(quant.df)) {
        stop(sprintf("cannot import data from file %s",
                     dQuote(pathname)))
    }

    ## Does layout need to change from original?
    if (!is.null(alt.layout)) {
        layoutMethod <- .getLayoutMethod(alt.layout)
        quant.df <- if (is.function(layoutMethod)) {
                        layoutMethod(quant.df, pathname)
                    }

        if (is.null(quant.df)) {
            stop(sprintf("cannot modify layout of data from file %s",
                         dQuote(pathname)))
        }
    }

    reqdColnames <- c("Main.Row",
                      "Main.Col",
                      "Sub.Row",
                      "Sub.Col",
                      "Sample")

    ## Ensure required columns exist
    found <- reqdColnames %in% colnames(quant.df)
    if (!all(found)) {
        missingColumns <- reqdColnames[!found]
        stop(sprintf(ngettext(length(missingColumns),
                              "missing required column %s",
                              "missing required columns %s"),
                     paste(dQuote(missingColumns), collapse = ", ")))
    }

    ## Ensure number of rows matches slide dimensions
    nexpectedRows <- prod(.dimOfLayout(quant.df))
    if (nrow(quant.df) != nexpectedRows) {
        stop(sprintf("number of rows (%d) differs from expected (%d)",
                     nrow(quant.df),
                     nexpectedRows))
    }

    ## Specify datatype of location columns
    storage.mode(quant.df$Main.Row) <- "integer"
    storage.mode(quant.df$Main.Col) <- "integer"
    storage.mode(quant.df$Sub.Row)  <- "integer"
    storage.mode(quant.df$Sub.Col)  <- "integer"

    return(quant.df)
}


##-----------------------------------------------------------------------------
## Reads MicroVigene text datafile
read.microvigene <- function(conn) {
    ## Check arguments
    stopifnot(inherits(conn, "connection"))

    ##-------------------------------------------------------------------------
    isMicroVigene <- function(pathname) {
        line <- readLines(pathname, n=1)
        isTRUE(grep("MicroVigene", line, fixed=TRUE) == 1)
    }


    ##-------------------------------------------------------------------------
    getMicroVigeneVersion <- function(pathname) {
        line <- readLines(pathname, n=1, ok=FALSE)
        mv.version <- as.numeric(strsplit(line, "[:blank:]")[[1]][3])
    }


    ##-------------------------------------------------------------------------
    getTimestamp <- function(pathname) {
        line <- readLines(pathname, n=3, ok=FALSE)[3]
        timestamp <- as.POSIXct(line, format="%m/%d/%Y %I:%M:%S %p")
    }


    ##-------------------------------------------------------------------------
    getNumHeaderLines <- function(mv.version) {
        ## Vendor introduced extra header line in later versions of file format
        numHeaderLines <- if (mv.version < 2900) 4 else 5
    }


    ##-------------------------------------------------------------------------
    ## Some quantification files from the labs contain CRCRLF at the end
    ## of each line, which messes up read.table() method on non-Windows OS.
    ## Returns the non-blank content lines if the column headers are found;
    ## otherwise, an empty string.
    extractContent <- function(conn) {
        firstcol <- "Main Row"
        firstcol.nchar <- nchar(firstcol)

        nmaxlines <- 15   # Column headers must be within first 15 lines
        for (lineno in seq_len(nmaxlines)) {
            ## Is this the line with column headers?
            line <- readLines(conn, n=1)
            if (firstcol == substr(line, 1, firstcol.nchar)) {
                ## Put it back
                pushBack(line, conn)
                ## Read everything
                content <- readLines(conn)
                blanklines.tf <- nchar(content) == 0
                ## Filter out blank lines
                return(content[!blanklines.tf])
            }
        }

        return("")
    }


    ## Begin processing
    pathname <- summary(conn)$description

    ## Check if this is really a MicroVigene datafile
    if (!isMicroVigene(pathname)) {
        stop(sprintf("file %s is not a MicroVigene datafile",
                     dQuote(pathname)))
    }

    ## Read data from file
    mvvers <- getMicroVigeneVersion(pathname)
    mvdata.df <- tryCatch(read.delim(conn,
                                     quote="",
                                     row.names=NULL,
                                     skip=getNumHeaderLines(mvvers)),
                          error=function(e) {
                              badformat <- "more columns than column names"
                              if (conditionMessage(e) == badformat) {
                                  message('retrying using alternative read...')
                                  content <- extractContent(conn)
                                  tconn <- textConnection(content, "r")
                                  on.exit(close(tconn))
                                  df <- try(read.delim(tconn,
                                                       quote="",
                                                       row.names=NULL))
                                  if (!inherits(df, "try-error")) {
                                      df
                                  } else {
                                      stop(e)    # retry failed
                                  }
                              } else {
                                  stop(e)        # cannot recover
                              }
                          })

    ## Eliminate spurious column caused by too many tab characters
    if ("X" %in% colnames(mvdata.df)) {
        mvdata.df[["X"]] <- NULL
    }

    ## Standardize column names
    colnames(mvdata.df)[colnames(mvdata.df) %in% "GeneID"] <- "Sample"
    colnames(mvdata.df) <- .capwords(make.names(colnames(mvdata.df),
                                                allow_=FALSE))

    ## Annotate data frame with metadata
    attr(mvdata.df, "software")  <- "microvigene"
    attr(mvdata.df, "version")   <- mvvers
    attr(mvdata.df, "timestamp") <- getTimestamp(pathname)

    return(mvdata.df)
}


##-----------------------------------------------------------------------------
## Reads ArrayPro text datafile. The file itself is little more than a TSV,
## which leaves no metadata either. This converts to use MV-style column names.
read.arraypro <- function(conn) {
    ## Check arguments
    stopifnot(inherits(conn, "connection"))

    ## Begin processing

    ## Read data from file
    apdata.df <- read.delim(conn,
                            quote="")

    ## Eliminate unneeded columns
    if ("X" %in% colnames(apdata.df)) {
        apdata.df[["X"]] <- NULL
    }
    if ("Slide" %in% colnames(apdata.df)) {
        apdata.df[["Slide"]] <- NULL
    }
    if ("Control" %in% colnames(apdata.df)) {
        apdata.df[["Control"]] <- NULL
    }
    if ("Dilution" %in% colnames(apdata.df)) {
        apdata.df[["Dilution"]] <- NULL
    }
    if ("Ignored.cells" %in% colnames(apdata.df)) {
        apdata.df[["Ignored.cells"]] <- NULL
    }

    ## Standardize column names
    colnames(apdata.df)[colnames(apdata.df) %in% "Gene.ID"] <- "Sample"

    ## Generic cleanup (Strip off tail portion of most column headers)
    colnames(apdata.df) <- sapply(colnames(apdata.df),
                                  function(s) {
                                      sub("..A.$", "", s)
                                  },
                                  USE.NAMES=FALSE)

    colname_for <- c("Net.intensity..mean." = "Mean.Net",
                     "Raw.intensity..mean." = "Mean.Total",
                     "Net.intensity..med."  = "Median.Net",
                     "Raw.intensity..med."  = "Median.Total",
                     "Background..mean."    = "Mean.Bkg",
                     "Background..med."     = "Median.Bkg",
                     "Background..cv."      = "Cv.Bkg",
                     "Net.intensity..sum."  = "Vol.Net",
                     "Raw.intensity..sum."  = "Vol.Total",
                     "Net.intensity..np."   = "NumPixels.Net",
                     "Background..np."      = "NumPixels.Bkg",
                     ## Not sure... (No real matches AFAICT)
                     "Raw.intensity..cv."   = "Cv.Total",
                     "Background..sum."     = "Bkg.Sum",
                     "Area..sum."           = "Area.Sum")

    colnames(apdata.df) <- sapply(colnames(apdata.df),
                                  function(cn, cnhash) {
                                      if (cn %in% names(cnhash)) {
                                          cnhash[cn]
                                      } else {
                                          cn
                                      }
                                  },
                                  cnhash=colname_for,
                                  USE.NAMES=FALSE)
    colnames(apdata.df) <- .capwords(make.names(colnames(apdata.df),
                                                allow_=FALSE))

    ## Add this one
    apdata.df[, "Vol.Bkg"] <- apdata.df[, "Vol.Total"] - apdata.df[, "Vol.Net"]


    ## Annotate data frame with metadata
    attr(apdata.df, "software")  <- "arraypro"

    return(apdata.df)
}


##-----------------------------------------------------------------------------
## Converts single subgrid physical layout to SuperSlide logical layout.
layout.as.superslide <- function(data.df,
                                 pathname) {
    ## Check arguments
    stopifnot(is.data.frame(data.df))
    stopifnot(is.character(pathname) && length(pathname) == 1)

    ## Begin processing

    ## Logical dimensions of SuperSlide format
    nmainrow <- 4
    nmaincol <- 12
    nsubrow  <- 11
    nsubcol  <- 11

    nspot.mr <- nmaincol * nsubrow * nsubcol  # number of spots in main row

    ## Ensure file is actually SuperSlide single subgrid layout
    dim.singlesubgrid <- as.integer(c(1,
                                      1,
                                      (nmainrow*nsubrow),
                                      (nmaincol*nsubcol)))
    dim.data.df <- c(max(data.df$Main.Row),
                     max(data.df$Main.Col),
                     max(data.df$Sub.Row),
                     max(data.df$Sub.Col))
    if (!identical(dim.singlesubgrid, dim.data.df)) {
        stop(sprintf("dim of file %s (%s) does not match SuperSlide single subgrid (%s)",
                     dQuote(pathname),
                     paste(dim.data.df, collapse="x"),
                     paste(dim.singlesubgrid, collapse="x")))
    }

    ## Save original location order back
    for (colname in .locationColnames()) {
        orig.colname <- sprintf("%s.Real", colname)    ## same as SlideDesigner
        data.df[[orig.colname]] <- data.df[[colname]]
    }

    ## Convert from single subgrid format
    data.df$Main.Row <- rep(1:nmainrow, each=nspot.mr)
    data.df$Main.Col <- rep(rep(1:nmaincol, each=nsubcol), nmainrow*nsubrow)
    data.df$Sub.Row  <- rep(rep(1:nsubrow, each=nsubcol*nmaincol), nmainrow)
    data.df$Sub.Col  <- rep(rep(1:nsubcol, nmaincol), nmainrow*nsubrow)

    ## Reorder the rows so that the order look as if the quantification was
    ## done for the first subgrid then the next subgrid until the first main
    ## row is finished, and then the next main row, and so on.
    new.ind <- NULL
    for (i in 1:nmainrow) {
        tmpind <- ((i-1)*nspot.mr+1):(i*nspot.mr)
        tmpind <- matrix(tmpind, byrow=TRUE, nrow=nsubrow)
        tmpind2 <- NULL
        for (j in 1:nmaincol) {
            tmpind2 <- c(tmpind2, as.vector(t(tmpind[, ((j-1)*11+1):(j*11)])))
        }
        new.ind <- c(new.ind, tmpind2)
    }
    data.df <- data.df[new.ind, ]
    attr(data.df, "layout") <- "superslide"

    return(data.df)
}


##-----------------------------------------------------------------------------
## read MAPIX data files
## added by Roman Mylonas 2015
# parse mapix files
read.mapix <- function(conn) {
  ## Check arguments
  stopifnot(inherits(conn, "connection"))
  
  # we have currently 4 replicates and 5 dilutions
  nr.replicates <- 4
  nr.dil <- 5
  
  transform.row <- function(x){
    # get the values of interest and make sure they're numeric
    my.row <- as.numeric(x$Row)
    my.col <- as.numeric(x$Column)
        
    # get Main and Sub.Rows
    Main.Row <- 1
    Sub.Row <- my.row
    
    # get Main and Sub.cols
    Main.Col <- ceiling(my.col/nr.dil)
    Sub.Col <- my.col %% nr.dil
    if(Sub.Col == 0){
      Sub.Col <- nr.dil
      Main.Col <- floor(my.col/nr.dil)
    }
    
    # construct the sample name
    Sample <- paste("sample", formatC(Main.Col, format="fg", digits=1, flag="0"), formatC(my.row, format="fg", digits=1, flag="0"), sep=".")
    
    # parse values from Mapix file
    Mean.Net <- as.numeric(x$F785.Mean...B785)
    Mean.Total <- as.numeric(x$F785.Mean) # + x$B785.Mean
    Median.Net <- as.numeric(x$F785.Median...B785)
    Vol.Bkg <- as.numeric(x$B785)
    Vol.Dust <- 0
    
    # create a data.frame and return it
    data.frame(Main.Row, Main.Col, Sub.Row, Sub.Col, Sample, Mean.Net, Mean.Total, Median.Net, Vol.Bkg, Vol.Dust)
  }
  
  my.mxd <- read.table(file=conn, header=TRUE, sep="\t")
  
  # get the first row seperately
  myAkt <- transform.row(my.mxd[1,])
  
  # and fill in all the following rows
  for(i in 2:dim(my.mxd)[1]){
    myAkt <- rbind(myAkt, transform.row(my.mxd[i,]))
  }
  
      ## Annotate data frame with metadata
  attr(myAkt, "software")  <- "mapix"
  #attr(myAkt, "timestamp") <- getTimestamp(pathname)

  return(myAkt)
  
}

