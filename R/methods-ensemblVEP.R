### =========================================================================
### ensemblVEP methods 
### =========================================================================

.getVepPath <- function()
{
    ## only needed on the BioC build system
    if (nchar(Sys.getenv("VEP_PATH")))
    {
        return(Sys.getenv("VEP_PATH"))
    }
    loc <- unname(Sys.which("variant_effect_predictor.pl"))
    if (!grepl("variant_effect_predictor", loc))
        stop()
    loc
}

setMethod("ensemblVEP", "character", 
    function(file, param=VEPParam(), ...)
    {
        call <- paste0(.getVepPath(), " ",
                       "-i ", file, .runtimeOpts(param)) 
        if (identical(character(), input(param)$output_file)) {
        ## return R object
            dest <- file.path(tempfile())
            if (output(param)$vcf) {
                fun <- readVcf 
                call <- paste0(call, " --output_file ", dest)
            } else {
                fun <- parseCSQToGRanges 
                call <- paste0(call, " --vcf --output_file ", dest)
            }
            system2("perl", call)
            fun(dest, genome="")
        } else {
        ## write to file or STDOUT
            system2("perl", call)
        }
    }
)

.runtimeOpts <- function(param, ...)
{
    ops <- c(basic(param), input(param), database(param), 
             output(param), filterqc(param))
    keep <- sapply(ops, function(x) 
        ifelse (is.logical(x), x == TRUE, length(x) > 0)) 
    paste0(" --", names(ops)[keep], " ", ops[keep], collapse=" ")
}
