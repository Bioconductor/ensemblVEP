### =========================================================================
### VEPParam73 class methods 
### =========================================================================

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

### See methods-VECParam-class.R

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity 
###

.valid.VEPParam73.identifier <- function(x)
{
    current <- identifier(x)
    target <- identifierOpts(version(x))
    c(.checkNames(current, target), .checkLogicals(current, target))
}

.valid.VEPParam73.colocated <- function(x)
{
    current <- colocatedVariants(x)
    target <- colocatedVariantsOpts(version(x))
    c(.checkNames(current, target), .checkLogicals(current, target))
}

.valid.VEPParam73.dataformat <- function(x)
{
    current <- dataformat(x)
    if (!is.character(current$convert))
        return(paste0("'convert' must be character() or 'ensembl', ",
               "'vcf' or 'pileup'"))

    target <- dataformatOpts(version(x))
    c(.checkNames(current, target), .checkLogicals(current, target))
}

setMethod(.validity, "VEPParam73",
    function(object)
{
    c(.valid.VEPParam.basic(object),
      .valid.VEPParam.database(object),
      .valid.VEPParam.advanced(object),
      .valid.VEPParam.input(object),
      .valid.VEPParam.cache(object),
      .valid.VEPParam.output(object),
      .valid.VEPParam.filterqc(object),
      .valid.VEPParam.version(object),
      .valid.VEPParam73.identifier(object),
      .valid.VEPParam73.colocated(object),
      .valid.VEPParam73.dataformat(object))
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Getters and Setters
###

setMethod("identifier", "VEPParam73",
    function(x) slot(x, "identifier"))
setMethod("identifier<-", "VEPParam73",
    function(x, value) 
{
    value <- .formatList(value)
    slot(x, "identifier")[names(value)] <- value
    msg <- .valid.VEPParam73.identifier(x)
    if (!is.null(msg))
        stop(msg) 
    x 
})

setMethod("colocatedVariants", "VEPParam73",
    function(x) slot(x, "colocatedVariants"))
setMethod("colocatedVariants<-", "VEPParam73",
    function(x, value) 
{
    value <- .formatList(value)
    slot(x, "colocatedVariants")[names(value)] <- value
    msg <- .valid.VEPParam73.colocated(x)
    if (!is.null(msg))
        stop(msg) 
    x 
})

setMethod("dataformat", "VEPParam73",
    function(x) slot(x, "dataformat"))
setMethod("dataformat<-", "VEPParam73",
    function(x, value) 
{
    value <- .formatList(value)
    slot(x, "dataformat")[names(value)] <- value
    msg <- .valid.VEPParam73.dataformat(x)
    if (!is.null(msg))
        stop(msg) 
    x 
})
