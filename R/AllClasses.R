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
### VEPParam67, VEPParam73, VEPParam75, VEPParam77
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
        version=c(73, 74)),
    validity=.validity)

setClass("VEPParam75", contains="VEPParam",
    representation(
        identifier="list",
        colocatedVariants="list",
        dataformat="list"),
    prototype(
        version=75),
    validity=.validity)

setClass("VEPParam77", contains="VEPParam",
    representation(
        identifier="list",
        colocatedVariants="list",
        dataformat="list"),
    prototype(
        version=77),
    validity=.validity)
