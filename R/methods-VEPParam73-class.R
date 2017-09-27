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
      .valid.VEPParam.identifier(object),
      .valid.VEPParam.colocated(object),
      .valid.VEPParam.dataformat(object))
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Getters and Setters
###

setMethod("identifier", "VEPParam73",
    function(x) slot(x, "identifier"))
setMethod("identifier<-", "VEPParam73",
    function(x, value) .identifier_setter(x, value)
)

.identifier_setter <- function(x, value)
{
    value <- .formatList(value)
    slot(x, "identifier")[names(value)] <- value
    msg <- .valid.VEPParam.identifier(x)
    if (!is.null(msg))
        stop(msg)
    x
}

setMethod("colocatedVariants", "VEPParam73",
    function(x) slot(x, "colocatedVariants"))
setMethod("colocatedVariants<-", "VEPParam73",
    function(x, value) .colocatedVariants_setter(x, value)
)

.colocatedVariants_setter <- function(x, value)
{
    value <- .formatList(value)
    slot(x, "colocatedVariants")[names(value)] <- value
    msg <- .valid.VEPParam.colocated(x)
    if (!is.null(msg))
        stop(msg)
    x
}

setMethod("dataformat", "VEPParam73",
    function(x) slot(x, "dataformat"))
setMethod("dataformat<-", "VEPParam73",
    function(x, value) .dataformat_setter(x, value)
)

.dataformat_setter <- function(x, value)
{
    value <- .formatList(value)
    slot(x, "dataformat")[names(value)] <- value
    msg <- .valid.VEPParam.dataformat(x)
    if (!is.null(msg))
        stop(msg)
    x
}
