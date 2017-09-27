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
        flags_opts<- flagOpts(flags)
        new("VEPFlags", ..., flags=flags_opts, version=version,
            scriptPath=scriptPath)
    }else{
        stop("undefined VEPFlags version. Try VEPParam()")
    }
}

flagOpts <- function(flags){

    # set some defaults if not given
    nm<- names(flags)

    if (!("host" %in% nm))
        flags = c(flags, host="useastdb.ensembl.org")
    if (!("database" %in% nm))
        flags = c(flags, database=TRUE)
    if (!("vcf" %in% nm))
        flags = c(flags, vcf=FALSE)
    flags
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
    value <- flagOpts(value)
    slot(x, "flags")[names(value)] <- value
    x
})

# version and scriptpath see methods-VEPParm-class.R

setMethod(show, "VEPFlags",
    function(object)
{
    .show(object)
})
