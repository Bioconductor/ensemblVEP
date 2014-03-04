### =========================================================================
### VEPParam75 class methods 
### =========================================================================

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

### See methods-VECParam-class.R

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity 
###

setMethod(.validity, "VEPParam75",
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

setMethod("identifier", "VEPParam75",
    function(x) slot(x, "identifier"))
setMethod("identifier<-", "VEPParam75",
    function(x, value) .identifier_setter(x, value)
) 

setMethod("colocatedVariants", "VEPParam75",
    function(x) slot(x, "colocatedVariants"))
setMethod("colocatedVariants<-", "VEPParam75",
    function(x, value) .colocatedVariants_setter(x, value)
) 

setMethod("dataformat", "VEPParam75",
    function(x) slot(x, "dataformat"))
setMethod("dataformat<-", "VEPParam75",
    function(x, value) .dataformat_setter(x, value)
) 
