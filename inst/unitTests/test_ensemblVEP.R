file <- system.file("extdata", "structural.vcf", 
                    package="VariantAnnotation")

test_ensemblVEP <- function()
{
    gr <- ensemblVEP(file, param=VEPParam(input=c(format="vcf")))
    checkIdentical(unique(nzchar(as.matrix(mcols(gr)))), TRUE)
    target <- c("Allele", "Gene", "Feature", "Feature_type",
                "Consequence", "cDNA_position", 
                "CDS_position", "Protein_position", "Amino_acids",
                "Codons", "Existing_variation", "DISTANCE")
    checkTrue(all(names(mcols(gr)) %in% target))
}
