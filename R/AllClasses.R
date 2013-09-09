### ------------------------------------------------------------------------- 
### VEPParam class 
###

setClass("VEPParam",
    representation(
        basic="list",
        input="list",
        cache="list",
        output="list",
        identifier="list",
        colocatedVariants="list",
        dataformat="list",
        filterqc="list",
        database="list",
        advanced="list")
)
