setGeneric("ensemblVEP", signature = "file",
           function(file, param=VEPParam(), ...) 
               standardGeneric("ensemblVEP")
)

setGeneric("parseCSQToGRanges", signature = "x",
           function(x, VCFRowID=TRUE, ...) 
               standardGeneric("parseCSQToGRanges")
)
