### =========================================================================
### All Classes. 
### =========================================================================

### ------------------------------------------------------------------------- 
### VEPParam (VIRTUAL)
###

setGeneric(".validity", function(object) standardGeneric(".validity"))

setClass("VEPParam",
    representation("VIRTUAL",
        basic="list",
        input="list",
        cache="list",
        output="list",
        filterqc="list",
        database="list",
        advanced="list",
        version="numeric",
        scriptPath="character"),
    validity=.validity)

### ------------------------------------------------------------------------- 
### VEPParam67 and VEPParam73
###

setClass("VEPParam67", contains="VEPParam",
    representation(),
    prototype(
        version=67),
    validity=.validity)

setClass("VEPParam73", contains="VEPParam",
    representation(
        identifier="list",
        colocatedVariants="list",
        dataformat="list"),
    prototype(
        version=73),
    validity=.validity)
