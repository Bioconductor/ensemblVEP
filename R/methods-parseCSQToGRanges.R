### =========================================================================
### parseCSQToGRanges methods 
### =========================================================================

setMethod("parseCSQToGRanges", "character", 
    function(x, VCFRowID=TRUE, ...)
    {
        vcf <- readVcf(x, "", 
            param=ScanVcfParam(info="CSQ", geno=NA_character_))
        callGeneric(vcf, FALSE, ...)
    }
)

setMethod("parseCSQToGRanges", "VCF", 
    function(x, VCFRowID=TRUE, ...)
    {
        ulst <- unlist(info(x)$CSQ, use.names=FALSE)
        if (all(is.na(ulst)))
            return(rowData(x))

        elt <- elementLengths(info(x)$CSQ)
        hdr <- info(header(x))["CSQ", "Description"]
        nms <- unlist(strsplit(strsplit(hdr, "Format: ")[[1]][2], "\\|"))
        raw <- strsplit(ulst, "\\|")
        csq <- matrix(nrow=length(ulst), ncol=length(nms))
        for (i in 1:nrow(csq))
            csq[i, 1:length(raw[[i]])] <- raw[[i]]
        csq[!nzchar(csq)] <- NA
        colnames(csq) <- nms
 
        if (VCFRowID) {
            VCFRowID <- rep(seq_len(nrow(x)), elt)
            csq <- cbind(VCFRowID, csq)
        }
        rd <- rowData(x)
        gr <- rd[rep(seq_len(length(rd)), elt)]
        mcols(gr) <- DataFrame(csq) 
        genome(gr) <- genome(x)
        gr 
    }
)
