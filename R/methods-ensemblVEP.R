### =========================================================================
### ensemblVEP methods 
### =========================================================================


.getVepPath <- function()
{
    ## this should only be turned on/needed on the BioC build system
    if (nchar(Sys.getenv("VEP_PATH")))
    {
        return(Sys.getenv("VEP_PATH"))
    }
    unname(Sys.which("variant_effect_predictor.pl"))
}

setMethod("ensemblVEP", "character", 
    function(file, param=VEPParam(), ...)
    {
        call <- paste0(.getVepPath(), " ",
                       "-i ", file, .runtimeOpts(param)) 
        warning(sprintf("whoa, call is %s", call))
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
    drop <- c(which(ops == FALSE), which(elementLengths(ops) == 0L)) 
    ops[which(ops == TRUE)] <- character(1) 
    paste0(" --", names(ops)[-drop], " ", ops[-drop], collapse=" ")
}
