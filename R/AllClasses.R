### ------------------------------------------------------------------------- 
### VEPParam class 
###

setClass("VEPParam",
    representation(
        basic="list",
        input="list",
        database="list",
        output="list",
        filterqc="list",
        cache="list")
)
