### =========================================================================
### ensemblVEP methods 
### =========================================================================

.getVepPath <- function()
{
    ## only needed on the BioC build system
    if (nchar(Sys.getenv("VEP_PATH")))
        return(Sys.getenv("VEP_PATH"))
    loc <- unname(Sys.which("variant_effect_predictor.pl"))
    if (!grepl("varian", loc, ignore.case=TRUE))
        stop("Couldn't find variant_effect_predictor.pl in your PATH.")
    loc
}

setMethod("ensemblVEP", "character", 
    function(file, param=VEPParam(), ...)
    {
        if (!length(path <- scriptPath(param)))
            path <- .getVepPath()
        old.quotes <- getOption("useFancyQuotes")
        on.exit(options(useFancyQuotes=old.quotes))
        options(useFancyQuotes=FALSE)
        call <- paste0(path, " -i ", dQuote(file), .runtimeOpts(param)) 
        if (identical(character(), input(param)$output_file)) {
        ## return R object
            dest <- file.path(tempfile())
            if (is(param, "VEPParam67"))
                vcfout <- output(param)$vcf
            else
                vcfout <- dataformat(param)$vcf
            if (vcfout) {
                fun <- readVcf 
                call <- paste0(call, " --output_file ", dQuote(dest))
            } else {
                fun <- parseCSQToGRanges 
                call <- paste0(call, " --vcf --output_file ", dQuote(dest))
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
    ops <- c(basic(param), input(param), cache(param),
             output(param), filterqc(param), 
             database(param), advanced(param))
    if (!is(param, "VEPParam67"))
        ops <- c(ops, identifier(param), colocatedVariants(param), 
                 dataformat(param))
    keep <- sapply(ops, function(x) 
                   ifelse (is.logical(x), x == TRUE, length(x) > 0)) 
    paste0(" --", names(ops)[keep], " ", ops[keep], collapse=" ")
}
