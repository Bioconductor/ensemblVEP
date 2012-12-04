### =========================================================================
### parseCSQToGRanges methods 
### =========================================================================

setMethod("parseCSQToGRanges", "character", 
    function(x, ...)
    {
        vcf <- readVcf(x, "", 
            param=ScanVcfParam(info="CSQ", geno=NA_character_))
        callGeneric(vcf, ...)
    }
)

setMethod("parseCSQToGRanges", "VCF", 
    function(x, ...)
    {
        hd <- info(header(x))["CSQ", "Description"]
        flds <- strsplit(hd, "Format: ")[[1]][2]
        csq <- read.csv(textConnection(unlist(info(x)$CSQ)), 
                        sep="|", header=FALSE)
        names(csq) <- unlist(strsplit(flds, "\\|"))

        rd <- rowData(x)
        gr <- rd[rep(seq_len(length(rd)), elementLengths(info(x)$CSQ))]
        mcols(gr) <- csq 
        genome(gr) <- genome(x)
        gr 
    }
)
