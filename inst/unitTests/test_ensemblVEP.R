file <- system.file("extdata", "structural.vcf", 
                    package="VariantAnnotation")

test_ensemblVEP <- function()
{
    gr <- ensemblVEP(file, param=VEPParam(input=c(format="vcf")))
    checkIdentical(unique(nzchar(as.matrix(mcols(gr)))), TRUE)
    checkTrue(length(names(mcols(gr))) == 13)
}

