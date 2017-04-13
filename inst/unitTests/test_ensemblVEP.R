file <- system.file("extdata", "ex2.vcf", package="VariantAnnotation") 

test_ensemblVEP <- function()
{
    gr <- ensemblVEP(file)
    checkIdentical(unique(nzchar(as.matrix(mcols(gr)))), TRUE)
    target <- c("Allele", "Gene", "Feature", "Feature_type",
                "Consequence", "cDNA_position", "CDS_position", 
                "Protein_position", "Amino_acids", "Codons", 
                "Existing_variation", "DISTANCE", "STRAND")
    checkTrue(all(target %in% names(mcols(gr))))
}
