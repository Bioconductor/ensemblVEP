file <- system.file("extdata", "ex2.vcf", package="VariantAnnotation")

test_ensemblVEP <- function()
{
    myparam <- VEPFlags(flags=list(host="useastdb.ensembl.org"))
    gr <- ensemblVEP(file, param=myparam)
    checkIdentical(unique(nzchar(as.matrix(mcols(gr)))), TRUE)
    target <- c("Allele", "Gene", "Feature", "Feature_type",
                "Consequence", "cDNA_position", "CDS_position",
                "Protein_position", "Amino_acids", "Codons",
                "Existing_variation", "DISTANCE", "STRAND")
    checkTrue(all(target %in% names(mcols(gr))))
    # check bad arguments/flags
    checkException(ensemblVEP("ImNotAFile"))
    checkException(ensemblVEP(fl, VEPFlags(flags=list(nofound=20))))
    checkException(ensemblVEP(fl, VEPFlags(flags=list(sift=20))))
}
