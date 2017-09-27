### =========================================================================
### Generics.
### =========================================================================

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### ensemblVEP
###

setGeneric("ensemblVEP", signature = "file",
           function(file, param=VEPParam(), ...)
               standardGeneric("ensemblVEP")
)

setGeneric(".runtimeOpts", signature = "param",
           function(param, ...)
           standardGeneric(".runtimeOpts"))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### parseCSQToGRanges
###

setGeneric("parseCSQToGRanges", signature = "x",
           function(x, VCFRowID=TRUE, ...)
               standardGeneric("parseCSQToGRanges")
)


setGeneric("parseCSQToGRanges", signature = "x",
           function(x, VCFRowID=TRUE, ...)
               standardGeneric("parseCSQToGRanges")
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### VEPParam
###
setGeneric("basic", function(x) standardGeneric("basic"))
setGeneric("basic<-", function(x, value) standardGeneric("basic<-"))
setGeneric("input", function(x) standardGeneric("input"))
setGeneric("input<-", function(x, value) standardGeneric("input<-"))
setGeneric("cache", function(x) standardGeneric("cache"))
setGeneric("cache<-", function(x, value) standardGeneric("cache<-"))
setGeneric("output", function(x) standardGeneric("output"))
setGeneric("output<-", function(x, value) standardGeneric("output<-"))
setGeneric("filterqc", function(x) standardGeneric("filterqc"))
setGeneric("filterqc<-", function(x, value) standardGeneric("filterqc<-"))
setGeneric("database", function(x) standardGeneric("database"))
setGeneric("database<-", function(x, value) standardGeneric("database<-"))
setGeneric("advanced", function(x) standardGeneric("advanced"))
setGeneric("advanced<-", function(x, value) standardGeneric("advanced<-"))
setGeneric("identifier", function(x) standardGeneric("identifier"))
setGeneric("identifier<-", function(x, value) standardGeneric("identifier<-"))
setGeneric("colocatedVariants",
    function(x) standardGeneric("colocatedVariants"))
setGeneric("colocatedVariants<-",
    function(x, value) standardGeneric("colocatedVariants<-"))
setGeneric("dataformat", function(x) standardGeneric("dataformat"))
setGeneric("dataformat<-", function(x, value) standardGeneric("dataformat<-"))
setGeneric("flags", function(x) standardGeneric("flags"))
setGeneric("flags<-", function(x, value) standardGeneric("flags<-"))
