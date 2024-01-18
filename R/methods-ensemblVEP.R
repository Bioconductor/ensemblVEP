### =========================================================================
### ensemblVEP methods
### =========================================================================

.getVepPath <- function()
{
    ## only needed on the BioC build system
    if (nchar(Sys.getenv("VEP_PATH")))
        return(Sys.getenv("VEP_PATH"))
    loc1 <- unname(Sys.which("vep"))
    if (!grepl("vep", loc1, ignore.case=TRUE))
        stop("Couldn't find vep in your PATH.")
    loc1
}

#' basic method
#' @param file A \code{character} specifying the full path to the file,
#' including the file name.
#' @param param An instance of \code{VEPFlags} specifying runtime options.
#' @param \dots Additional arguments passed to methods.
#' @param verbose logical(1) should system call to vep be printed
#' @export
setMethod("ensemblVEP", "character", function(file, param=VEPFlags(),
                                              ..., verbose=FALSE){

.Deprecated(new="vep_by_region", msg="This package now focuses on the ensemblVEP REST API, see, e.g., 'https://rest.ensembl.org/documentation/info/vep_region_post'")
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
        if (verbose) message("Running:\n", call, "\n")
        system(call)
        fun(dest, genome="")
    } else {
        ## write to file or STDOUT
        if (verbose) message("Running:\n", call, "\n")
        system(call)
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
        ans = ""
    else
        ans = paste0(" --", names(ops)[keep], " ", ops[keep], collapse=" ")
    ans = gsub("TRUE", "", ans) # VJC Nov 28 2022 hack ... 'vep' script doesn't want logical values
    ans = gsub("FALSE", "", ans) 
    ans
})
