### =========================================================================
### ensemblVEP methods 
### =========================================================================

setMethod("ensemblVEP", "character", 
    function(file, param=VEPParam(), ...)
    {
        call <- paste0(Sys.which("variant_effect_predictor.pl"), " ",
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
    drop <- c(which(ops == FALSE), which(elementLengths(ops) == 0L)) 
    ops[which(ops == TRUE)] <- character(1) 
    paste0(" --", names(ops)[-drop], " ", ops[-drop], collapse=" ")
}
