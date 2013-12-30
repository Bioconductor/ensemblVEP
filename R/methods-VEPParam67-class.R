### =========================================================================
### VEPParam67 class methods 
### =========================================================================

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

### See methods-VECParam-class.R

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity 
###

.valid.VEPParam67.version <- function(x)
{
    v <- 67
    if (version(x) != v)
        paste0("version(x) must be ", v)
    else
        NULL 
}

setMethod(.validity, "VEPParam67",
    function(object)
{
    c(.valid.VEPParam.basic(object),
      .valid.VEPParam.database(object),
      .valid.VEPParam.advanced(object),
      .valid.VEPParam.input(object),
      .valid.VEPParam.cache(object),
      .valid.VEPParam.output(object),
      .valid.VEPParam.filterqc(object),
      .valid.VEPParam67.version(object))
})

