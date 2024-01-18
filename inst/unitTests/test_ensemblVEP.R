
test_vep_by_region <- function() {
 fl <- system.file("extdata", "chr22.vcf.gz", package="VariantAnnotation")
 r22 = readVcf(fl)
 dr = which(width(rowRanges(r22))!=1)
 r22s = r22[-dr]
 res = vep_by_region(r22[1:100], snv_only=FALSE, chk_max=FALSE)
 ans = fromJSON(toJSON(content(res)))
 checkTrue(nrow(ans)==100)
 checkException(vep_by_region("X"))
}




#test_ensemblVEP <- function()
#{
#    myparam <- VEPFlags(flags=list(host="useastdb.ensembl.org"))
#    gr <- ensemblVEP(file, param=myparam)
#    checkIdentical(unique(nzchar(as.matrix(mcols(gr)))), TRUE)
#    target <- c("Allele", "Gene", "Feature", "Feature_type",
#                "Consequence", "cDNA_position", "CDS_position",
#                "Protein_position", "Amino_acids", "Codons",
#                "Existing_variation", "DISTANCE", "STRAND")
#    checkTrue(all(target %in% names(mcols(gr))))
#    # check bad arguments/flags
#    checkException(ensemblVEP("ImNotAFile"))
#    checkException(ensemblVEP(fl, VEPFlags(flags=list(nofound=20))))
#    checkException(ensemblVEP(fl, VEPFlags(flags=list(sift=20))))
#}
