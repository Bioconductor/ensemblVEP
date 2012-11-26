### =========================================================================
### ensemblVEP methods 
### =========================================================================

setMethod("ensemblVEP", "character", 
    function(file, ..., param=VEPParam(), genome="GRCh37")
    {
        ## FIXME : check for ensemblVEP installation
        if (!is.logical(param$vcf))
            fun <- readVcf
        else
            fun <- VEPToGRanges

        call <- paste0("variant_effect_predictor.pl ", 
                       "--input ", file, .createOpts(param)) 
        system(call)
        fun(input(param)$output_file, ..., genome=genome)
    }
)

.createOpts <- function(param, ...)
{
    
    ops <- c(basic(param), input(param), database(param), 
             output(param), filterqc(param))
    drop <- which(ops == FALSE)
    ops[which(ops == TRUE)] <- character(1) 
    paste0(" --", names(ops)[-drop], " ", ops[-drop], collapse=" ")
}

parseCSQ <- function(vcf)
{
    hdr <- info(exptData(vcf)$header)["CSQ", "Description"]
    nms <- unlist(strsplit(unlist(strsplit(hdr, "Format: ", fixed=TRUE))[2],
                  "\\|"), use.names=FALSE) 
    csq <- info(vcf)$CSQ
    lst <- lapply(csq, function(elt, nms) {
        ## FIXME : catch trailing empty value
        str <- gsub("\\|", "\t", unlist(elt, use.names=FALSE)) 
        mat <- matrix(unlist(strsplit(str, "\t"), use.names=FALSE), 
                      length(elt), byrow=TRUE)
           #    dimnames=list(NULL, nms))}, nms
        colnames(mat) <- nms[1:ncol(mat)]
        mat}, nms
    )
    names(lst) <- rownames(vcf)
    lst
}
