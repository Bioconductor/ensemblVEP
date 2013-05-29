### ------------------------------------------------------------------------- 
### VEPParam class 
###

setClass("VEPParam",
    representation(
        basic="list",
        input="list",
        cache="list",
        output="list",
        filterqc="list",
        database="list",
        advanced="list")
)
