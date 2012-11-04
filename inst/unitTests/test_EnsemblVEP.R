

library(VariantAnnotation)
fl <- system.file("extdata", "ex2.vcf", package="VariantAnnotation")
vcf <- readVcf(fl, "hg19")

.test_EnsemblVEP_class <- function()
{
    empty <- .EnsemblVEP$new()
    target <- c("location", "transcript", "consequence")
    checkIdentical(names(empty), target) 
    checkTrue(class(empty) == "EnsemblVEP") 
}

.test_EnsemblVEP_accessors <- function()
{
    vep <- EnsemblVEP(vcf)
    ## getters
    checkIdentical(location(vep), vep[["location"]])
    checkIdentical(transcript(vep), vep[["transcript"]])
    checkIdentical(consequence(vep), vep[["consequence"]])

    ## empty-ish; no tx match
    vep <- EnsemblVEP(vcf[1:2])
    checkIdentical(DataFrame(), vep[["transcript"]])
    checkIdentical(DataFrame(), vep[["consequence"]])

    ## FIXME : single variant doesn't work
    # vep <- EnsemblVEP(vcf[1])
}


.test_EnsemblVEP_select <- function()
{
    ## empty-ish; no tx match
    vep <- EnsemblVEP(vcf[1:2])
    current <- cols(vep)
    target <- colnames(location(vep))
    checkIdentical(current, target)
 
    vep <- EnsemblVEP(vcf[3])
    ## keys no cols
    current <- select(vep, keys(vep)[1], "cds_start") 

    ## empty : cols no keys 
}
