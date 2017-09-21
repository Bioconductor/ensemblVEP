### =========================================================================
### All Classes.
### =========================================================================

### -------------------------------------------------------------------------
### VEPParam (VIRTUAL)
###

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

setClass("VEPFlags",
         representation(
             flags="list",
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

setClass("VEPParam78", contains="VEPParam",
    representation(
        identifier="list",
        colocatedVariants="list",
        dataformat="list"),
    prototype(
        version=c(78, 80, 81)),
    validity=.validity)

setClass("VEPParam82", contains="VEPParam",
    representation(
        identifier="list",
        colocatedVariants="list",
        dataformat="list"),
    prototype(
        version=c(82,83,84,85,86,87)),
    validity=.validity)

setClass("VEPParam88", contains="VEPParam",
    representation(
        identifier="list",
        colocatedVariants="list",
        dataformat="list"),
    prototype(
        version=c(88)),
    validity=.validity)
