
test_VEPToGRanges_vcf_input <- function()
{
    fl <- system.file("extdata", "ex2.vcf.txt", package="ensemblVEP")
    gr <- VEPToGRanges(fl)
    nms <- c("Allele", "Gene", "Feature", "Feature_type", "Consequence",
             "cDNA_position", "CDS_position", "Protein_position",
             "Amino_acids", "Codons", "Existing_variation", "Extra")
    checkIdentical(nms, names(mcols(gr)))
}
