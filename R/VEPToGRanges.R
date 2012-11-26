VEPToGRanges <- function(x, ..., genome=NULL)
{
    ## FIXME : appropriate subset for header
    ## FIXME : Allele not robust to structural 
    txt <- readLines(x, n=200)
    hdr <- txt[grepl("^#", txt)]
    tbl <- read.delim(x, comment.char="#", stringsAsFactors=FALSE)
    colnms <- unlist(strsplit(sub("#", "", hdr[length(hdr)]), 
        "\t", fixed=TRUE), use.names=FALSE)
    colnames(tbl) <- colnms

    allele <- DNAStringSet(tbl$Allele)
    loc <- do.call(rbind, strsplit(tbl$Location, split=":"))
    ss <- strsplit(loc[2], "-")
    len <- sapply(ss, length)
    start <- sapply(ss, "[", 1)
    end <- sapply(ss, "[", 2)
    end[len == 1] <- start[len == 1]
    gr <- GRanges(Rle(loc[,1]), 
                  IRanges(as.numeric(start), as.numeric(end)), 
                  Allele=allele)
    nms <- c("Uploaded_variation", "Location", "Allele")
    mcols(gr) <- DataFrame(tbl[!colnames(tbl) %in% nms]) 
    names(gr) <- tbl$Uploaded_variation
    if (!is.null(genome))
        genome(gr) <- genome
    gr
}
