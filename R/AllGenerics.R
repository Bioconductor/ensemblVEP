### =========================================================================
### Generics.
### =========================================================================

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### ensemblVEP
###

setGeneric("ensemblVEP", signature = "file",
           function(file, param=VEPParam(), ..., verbose=FALSE) {
               .Deprecated(new="vep_by_region", msg="This package now focuses on the ensemblVEP REST API, see, e.g., 'https://rest.ensembl.org/documentation/info/vep_region_post'")
               standardGeneric("ensemblVEP")
           }
)

setGeneric(".runtimeOpts", signature = "param",
           function(param, ...) {
               .Deprecated(new="vep_by_region", msg="This package now focuses on the ensemblVEP REST API, see, e.g., 'https://rest.ensembl.org/documentation/info/vep_region_post'")
           standardGeneric(".runtimeOpts")
           }
          )

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
setGeneric("basic", function(x) {
               .Deprecated(new="vep_by_region", msg="This package now focuses on the ensemblVEP REST API, see, e.g., 'https://rest.ensembl.org/documentation/info/vep_region_post'")
standardGeneric("basic")
})
setGeneric("basic<-", function(x, value) {
               .Deprecated(new="vep_by_region", msg="This package now focuses on the ensemblVEP REST API, see, e.g., 'https://rest.ensembl.org/documentation/info/vep_region_post'")
standardGeneric("basic<-")
})
setGeneric("input", function(x) {
               .Deprecated(new="vep_by_region", msg="This package now focuses on the ensemblVEP REST API, see, e.g., 'https://rest.ensembl.org/documentation/info/vep_region_post'")
standardGeneric("input")
})
setGeneric("input<-", function(x, value) {
               .Deprecated(new="vep_by_region", msg="This package now focuses on the ensemblVEP REST API, see, e.g., 'https://rest.ensembl.org/documentation/info/vep_region_post'")
standardGeneric("input<-")
})
setGeneric("cache", function(x) {
               .Deprecated(new="vep_by_region", msg="This package now focuses on the ensemblVEP REST API, see, e.g., 'https://rest.ensembl.org/documentation/info/vep_region_post'")
standardGeneric("cache")
})
setGeneric("cache<-", function(x, value) {
               .Deprecated(new="vep_by_region", msg="This package now focuses on the ensemblVEP REST API, see, e.g., 'https://rest.ensembl.org/documentation/info/vep_region_post'")
standardGeneric("cache<-")
})
setGeneric("output", function(x) {
               .Deprecated(new="vep_by_region", msg="This package now focuses on the ensemblVEP REST API, see, e.g., 'https://rest.ensembl.org/documentation/info/vep_region_post'")
standardGeneric("output")
})
setGeneric("output<-", function(x, value) {
               .Deprecated(new="vep_by_region", msg="This package now focuses on the ensemblVEP REST API, see, e.g., 'https://rest.ensembl.org/documentation/info/vep_region_post'")
standardGeneric("output<-")
})
setGeneric("filterqc", function(x) {
               .Deprecated(new="vep_by_region", msg="This package now focuses on the ensemblVEP REST API, see, e.g., 'https://rest.ensembl.org/documentation/info/vep_region_post'")
standardGeneric("filterqc")
})
setGeneric("filterqc<-", function(x, value) {
               .Deprecated(new="vep_by_region", msg="This package now focuses on the ensemblVEP REST API, see, e.g., 'https://rest.ensembl.org/documentation/info/vep_region_post'")
standardGeneric("filterqc<-")
})
setGeneric("database", function(x) {
               .Deprecated(new="vep_by_region", msg="This package now focuses on the ensemblVEP REST API, see, e.g., 'https://rest.ensembl.org/documentation/info/vep_region_post'")
standardGeneric("database")
})
setGeneric("database<-", function(x, value) {
               .Deprecated(new="vep_by_region", msg="This package now focuses on the ensemblVEP REST API, see, e.g., 'https://rest.ensembl.org/documentation/info/vep_region_post'")
standardGeneric("database<-")
})
setGeneric("advanced", function(x) {
               .Deprecated(new="vep_by_region", msg="This package now focuses on the ensemblVEP REST API, see, e.g., 'https://rest.ensembl.org/documentation/info/vep_region_post'")
standardGeneric("advanced")
})
setGeneric("advanced<-", function(x, value) {
               .Deprecated(new="vep_by_region", msg="This package now focuses on the ensemblVEP REST API, see, e.g., 'https://rest.ensembl.org/documentation/info/vep_region_post'")
standardGeneric("advanced<-")
})
setGeneric("identifier", function(x) {
               .Deprecated(new="vep_by_region", msg="This package now focuses on the ensemblVEP REST API, see, e.g., 'https://rest.ensembl.org/documentation/info/vep_region_post'")
standardGeneric("identifier")
})
setGeneric("identifier<-", function(x, value) {
               .Deprecated(new="vep_by_region", msg="This package now focuses on the ensemblVEP REST API, see, e.g., 'https://rest.ensembl.org/documentation/info/vep_region_post'")
standardGeneric("identifier<-")
})
setGeneric("colocatedVariants",
    function(x) {
               .Deprecated(new="vep_by_region", msg="This package now focuses on the ensemblVEP REST API, see, e.g., 'https://rest.ensembl.org/documentation/info/vep_region_post'")
standardGeneric("colocatedVariants")
})
setGeneric("colocatedVariants<-",
    function(x, value) {
               .Deprecated(new="vep_by_region", msg="This package now focuses on the ensemblVEP REST API, see, e.g., 'https://rest.ensembl.org/documentation/info/vep_region_post'")
standardGeneric("colocatedVariants<-")
})
setGeneric("dataformat", function(x) {
               .Deprecated(new="vep_by_region", msg="This package now focuses on the ensemblVEP REST API, see, e.g., 'https://rest.ensembl.org/documentation/info/vep_region_post'")
standardGeneric("dataformat")
})
setGeneric("dataformat<-", function(x, value) {
               .Deprecated(new="vep_by_region", msg="This package now focuses on the ensemblVEP REST API, see, e.g., 'https://rest.ensembl.org/documentation/info/vep_region_post'")
standardGeneric("dataformat<-")
})
setGeneric("flags", function(x) {
               .Deprecated(new="vep_by_region", msg="This package now focuses on the ensemblVEP REST API, see, e.g., 'https://rest.ensembl.org/documentation/info/vep_region_post'")
standardGeneric("flags")
})
setGeneric("flags<-", function(x, value) {
               .Deprecated(new="vep_by_region", msg="This package now focuses on the ensemblVEP REST API, see, e.g., 'https://rest.ensembl.org/documentation/info/vep_region_post'")
standardGeneric("flags<-")
})
