### =========================================================================
### VEPFlags class methods 
### =========================================================================

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

VEPFlags <- function(version=max(unlist(currentVEP())),
                     scriptPath=character(),
                     flags=list(), ...){

    .version_error(version)
    s <- supportedVEP()
    v <- s[[length(s)]]
    if(any(version %in% v[1]:v[2])){
        new("VEPFlags", ..., flags=flags, version=version,
            scriptPath=scriptPath)
    }else{
        stop("undefined VEPFlags version. Try VEPParam()")
    }
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity 
###

setMethod(.validity, "VEPFlags",
    function(object)
{
    c(.valid.VEPParam.version(object),
      .valid.VEPParam.scriptname(object))
})

#
# Getters and Setters
#

setMethod("flags", "VEPFlags",
    function(x) slot(x, "flags"))
setMethod("flags<-", "VEPFlags",
    function(x, value) 
{
    value <- .formatList(value)
    slot(x, "flags")[names(value)] <- value
})

# version and scriptpath see methods-VEPParm-class.R

setMethod(show, "VEPFlags",
    function(object)
{
    .show(object)
})
