###
### $Id: AllElapsedTimeGenerics.R 833 2014-06-27 16:17:07Z proebuck $
###


##
## Accessors
##
if (!isGeneric("elapsed")) {
    setGeneric("elapsed",
               function(object, ...) standardGeneric("elapsed"))
}

