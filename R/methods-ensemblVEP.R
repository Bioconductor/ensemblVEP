### =========================================================================
### ensemblVEP methods
### =========================================================================

.getVepPath <- function()
{
    ## only needed on the BioC build system
    if (nchar(Sys.getenv("VEP_PATH")))
        return(Sys.getenv("VEP_PATH"))
    loc0 <- unname(Sys.which("variant_effect_predictor.pl"))
    loc1 <- unname(Sys.which("vep"))
    if (!grepl("variant_effect_predictor.pl", loc0, ignore.case=TRUE) &&
        !grepl("vep", loc1, ignore.case=TRUE))
        stop("Couldn't find variant_effect_predictor.pl or vep in your PATH.")
    ifelse(nchar(loc0) > 0L, loc0, loc1)
}

setMethod("ensemblVEP", "character", function(file, param=VEPFlags(), ...){

    if (!length(path <- scriptPath(param)))
        path <- .getVepPath()
    old.quotes <- getOption("useFancyQuotes")
    on.exit(options(useFancyQuotes=old.quotes))
    options(useFancyQuotes=FALSE)
    call <- paste0(path, " -i ", dQuote(file), .runtimeOpts(param))

    .outputFile <- function(param){
        if(is(param, "VEPFlags")){
            is.null(flags(param)$output_file)
        }else{
            identical(character(), input(param)$output_file)
        }
    }
    if (.outputFile(param)) {
        ## return R object
        dest <- file.path(tempfile())
        if (is(param, "VEPParam67")){
            vcfout <- output(param)$vcf
        } else if (is(param, "VEPFlags")){
            vcfout <- flags(param)$vcf
        } else {
            vcfout <- dataformat(param)$vcf
        }

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

})

setMethod(.runtimeOpts, "VEPParam", function(param, ...){
    ops <- c(basic(param), input(param), cache(param),
             output(param), filterqc(param),
             database(param), advanced(param))
    if (!is(param, "VEPParam67"))
        ops <- c(ops, identifier(param), colocatedVariants(param),
                 dataformat(param))
    keep <- sapply(ops, function(x)
                   ifelse (is.logical(x), x == TRUE, length(x) > 0))
    paste0(" --", names(ops)[keep], " ", ops[keep], collapse=" ")
})


setMethod(.runtimeOpts, "VEPFlags", function(param, ...){

    ops = flags(param)
    keep <- sapply(ops, function(x)
                   ifelse (is.logical(x), x == TRUE, length(x) > 0))
    if(length(keep)==0L)
        ""
    else
        paste0(" --", names(ops)[keep], " ", ops[keep], collapse=" ")
})
