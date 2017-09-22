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

    if (!("buffer_size" %in% nm))
        flags = c(flags, buffer_size=5000)
    if (!("species" %in% nm))
        flags = c(flags, species="homo_sapiens")
    if (!("dir" %in% nm))
        flags = c(flags, dir="$HOME/.vep")
    if (!("dir_cache" %in% nm))
        flags = c(flags, dir_cache="$HOME/.vep")
    if (!("dir_plugins" %in% nm))
        flags = c(flags, dir_plugins="$HOME/.vep")
    if (!("host" %in% nm))
        flags = c(flags, host="useastdb.ensembl.org")
    if (!("terms" %in% nm))
        flags = c(flags, terms="SO")
    if (!("database" %in% nm))
        flags = c(flags, database=TRUE)
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
})

# version and scriptpath see methods-VEPParm-class.R

setMethod(show, "VEPFlags",
    function(object)
{
    .show(object)
})
