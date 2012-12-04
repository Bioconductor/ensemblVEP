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
        raw <- sub("\\|$", "| ", unlist(info(x)$CSQ, use.names=FALSE))
        raw <- strsplit(raw, "\\|")
        mat <- matrix(unlist(raw), nrow=length(raw), byrow=TRUE) 
        mat[!nzchar(mat)] <- NA_character_
        csq <- DataFrame(mat)
        hd <- info(header(x))["CSQ", "Description"]
        flds <- strsplit(hd, "Format: ")[[1]][2]
        names(csq) <- unlist(strsplit(flds, "\\|"))

        rd <- rowData(x)
        gr <- rd[rep(seq_len(length(rd)), elementLengths(info(x)$CSQ))]
        mcols(gr) <- csq 
        genome(gr) <- genome(x)
        gr 
    }
)
