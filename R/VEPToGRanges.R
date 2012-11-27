VEPToGRanges <- function(x, ..., genome=NULL)
{
    ## FIXME : handle different headers
    txt <- readLines(x, n=200)
    hdr <- txt[grepl("^#", txt)]
    tbl <- read.delim(x, comment.char="#", stringsAsFactors=FALSE)
    cnms <- strsplit(sub("#", "", hdr[length(hdr)]), "\t", fixed=TRUE)
    colnames(tbl) <- unlist(cnms, use.names=FALSE) 

    splitloc <- do.call(rbind, strsplit(tbl$Location, split=":"))
    ss <- strsplit(splitloc[,2], "-")
    start <- sapply(ss, "[", 1)
    end <- sapply(ss, "[", 2)
    len <- sapply(ss, length)
    end[len == 1] <- start[len == 1]
    gr <- GRanges(Rle(splitloc[,1]), 
                  IRanges(as.numeric(start), as.numeric(end),
                          names=tbl$Uploaded_variation))
    idx <- !colnames(tbl) %in% c("Uploaded_variation", "Location", "Allele") 
    Allele <- tryCatch(DNAStringSet(tbl$Allele), 
                       error=function(e)
                                 CharacterList(strsplit(tbl$Allele, " "))) 
    mcols(gr) <- DataFrame(Allele=Allele, tbl[idx])
    if (!is.null(genome))
        genome(gr) <- genome
    gr
}
