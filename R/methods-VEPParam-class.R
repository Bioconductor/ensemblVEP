### =========================================================================
### VEPParam class methods 
### =========================================================================

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

## Force to list, reset logical data type
.formatList <- function(x)
{
    if (is.list(x))
        return(x)
    x <- as.list(x)
    idx <- x %in% c("TRUE", "FALSE")
    x[idx] <- as.logical(x[idx]) 
    x
} 

VEPParam  <- function(version=max(unlist(currentVEP())), basic=list(), 
                      input=list(), cache=list(), output=list(), 
                      filterqc=list(), database=list(), advanced=list(), 
                      identifier=list(), colocatedVariants=list(), 
                      dataformat=list(), scriptPath=character(), ...)
{
    .version_error(version)
    basic_opts <- basicOpts(version)
    basic_opts[names(basic)] <- .formatList(basic)

    input_opts <- inputOpts(version)
    input_opts[names(input)] <- .formatList(input)

    cache_opts <- cacheOpts(version)
    cache_opts[names(cache)] <- .formatList(cache)

    output_opts <- outputOpts(version)
    output_opts[names(output)] <- .formatList(output)

    filterqc_opts <- filterqcOpts(version)
    filterqc_opts[names(filterqc)] <- .formatList(filterqc) 

    database_opts <- databaseOpts(version)
    database_opts[names(database)] <- .formatList(database)

    advanced_opts <- advancedOpts(version)
    advanced_opts[names(advanced)] <- .formatList(advanced)

    if (version > 67) {
        identifier_opts <- identifierOpts(version)
        identifier_opts[names(identifier)] <- .formatList(identifier)

        colocated_opts <- colocatedVariantsOpts(version)
        colocated_opts[names(colocatedVariants)] <- 
            .formatList(colocatedVariants)

        dataformat_opts <- dataformatOpts(version)
        dataformat_opts[names(dataformat)] <- .formatList(dataformat)

        if (version %in% c(73, 74))
            VEP_class <- "VEPParam73"
        else if (version == 75)
            VEP_class <- "VEPParam75"
        else if (version == 77)
            VEP_class <- "VEPParam77"
        else if (version == 78)
            VEP_class <- "VEPParam78"
        else
            stop("undefined VEP version")

        new(VEP_class, ..., basic=basic_opts, 
            database=database_opts, advanced=advanced_opts,
            input=input_opts, cache=cache_opts, 
            output=output_opts, filterqc=filterqc_opts, 
            identifier=identifier_opts, colocatedVariants=colocated_opts, 
            dataformat=dataformat_opts, scriptPath=scriptPath)
    } else {
        new("VEPParam67", ..., basic=basic_opts, input=input_opts,
            cache=cache_opts, output=output_opts,
            filterqc=filterqc_opts, database=database_opts,
            advanced=advanced_opts, scriptPath=scriptPath)
    }
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity 
###

.checkNames <- function(current, target)
{
    invalid <- !names(current) %in% names(target)
    if (any(invalid))
        return(paste0("invalid runtime options '", 
               paste(names(current)[invalid], "'", sep=",")))
    NULL
}

.checkLogicals <- function(current, target)
{
    logic <- current[names(target)[target %in% c(TRUE, FALSE)]]
    invalid <- !logic %in% c(TRUE, FALSE)
    if (any(invalid))
        return(paste0("runtime options '", 
                      paste(names(logic)[invalid], sep=","), 
                      "' must be TRUE or FALSE"))
    NULL 
}

.valid.VEPParam.basic <- function(x)
{
    current <- basic(x)
    if (!is.numeric(current$fork))
        return("'fork' must be numeric")
    if (!is.character(current$config))
        return("'config' must be character() or a file name")

    target <- basicOpts(version(x))
    c(.checkNames(current, target), .checkLogicals(current, target))
} 

.valid.VEPParam.input <- function(x)
{
    current <- input(x)
    if (!identical(character(), current$format))
        if (!current$format %in% 
            c("ensembl", "vcf", "pileup", "hgvs", "id", "vep"))
            return(paste0("'format' must be one of 'ensembl', ",
                          "'vcf', 'pileup', 'hgvs', 'id' or 'vep'."))
    target <- inputOpts(version(x))
    c(.checkNames(current, target), .checkLogicals(current, target))
} 

.valid.VEPParam.cache <- function(x)
{
    current <- cache(x)
    target <- cacheOpts(version(x))
    c(.checkNames(current, target), .checkLogicals(current, target))
}

.valid.VEPParam.database <- function(x)
{
    current <- database(x)
    if (!is.numeric(current$port))
        return("'port' must be numeric")
    if (!is.numeric(current$db_version))
        return("'db_version' must be numeric")

    target <- databaseOpts(version(x))
    c(.checkNames(current, target), .checkLogicals(current, target))
}

.valid.VEPParam.output <- function(x)
{
    current <- output(x)
    if (!is.character(current$cell_type))
        return("'cell_type' must be a character'")
    if (!is.character(current$terms))
        return("'terms' must be character() or 'all' or 'so'")
    if (!is.character(current$sift))
        return("'sift' must be character() or 'p', 's' or 'b'")
    if (!is.character(current$polyphen))
        return("'polyphen' must be character() or 'p', 's' or 'b'")
    if (!is.character(current$custom))
        return("'custom' must be a character'")
    if (!is.character(current$plugin))
        return("'plugin' must be a character'")

}

.valid.VEPParam.filterqc <- function(x)
{
    current <- filterqc(x)
    target <- filterqcOpts(version(x))
    c(.checkNames(current, target), .checkLogicals(current, target))
}

.valid.VEPParam.advanced <- function(x)
{
    current <- advanced(x)
    target <- advancedOpts(version(x))
    c(.checkNames(current, target), .checkLogicals(current, target))
}

.valid.VEPParam.version <- function(x)
{
    s <- supportedVEP()
    v <- unname(unlist(s[names(s) == class(x)]))
    if (any(version(x) %in% v))
        NULL 
    else
        paste0("for class ", class(x), " version(x) must be one of ", 
               paste(v, collapse=","))
}

.valid.VEPParam.identifier <- function(x)
{
    current <- identifier(x)
    target <- identifierOpts(version(x))
    c(.checkNames(current, target), .checkLogicals(current, target))
}

.valid.VEPParam.colocated <- function(x)
{
    current <- colocatedVariants(x)
    target <- colocatedVariantsOpts(version(x))
    c(.checkNames(current, target), .checkLogicals(current, target))
}

.valid.VEPParam.dataformat <- function(x)
{
    current <- dataformat(x)
    if (!is.character(current$convert))
        return(paste0("'convert' must be character() or 'ensembl', ",
               "'vcf' or 'pileup'"))

    target <- dataformatOpts(version(x))
    c(.checkNames(current, target), .checkLogicals(current, target))
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Getters and Setters
###

setMethod("basic", "VEPParam",
    function(x) slot(x, "basic")) 
setMethod("basic<-", "VEPParam",
    function(x, value) 
{
    value <- .formatList(value)
    slot(x, "basic")[names(value)] <- value
    msg <- .valid.VEPParam.basic(x)
    if (!is.null(msg))
        stop(msg) 
    x
})

setMethod("input", "VEPParam",
    function(x) slot(x, "input")) 
setMethod("input<-", "VEPParam",
    function(x, value) 
{
    value <- .formatList(value)
    slot(x, "input")[names(value)] <- value
    msg <- .valid.VEPParam.input(x)
    if (!is.null(msg))
        stop(msg) 
    x 
})

setMethod("cache", "VEPParam",
    function(x) slot(x, "cache")) 
setMethod("cache<-", "VEPParam",
    function(x, value) 
{
    value <- .formatList(value)
    slot(x, "cache")[names(value)] <- value
    msg <- .valid.VEPParam.cache(x)
    if (!is.null(msg))
        stop(msg) 
    x 
})

setMethod("output", "VEPParam",
    function(x) slot(x, "output"))
setMethod("output<-", "VEPParam",
    function(x, value) 
{
    value <- .formatList(value)
    slot(x, "output")[names(value)] <- value
    msg <- .valid.VEPParam.output(x)
    if (!is.null(msg))
        stop(msg) 
    x 
})

setMethod("filterqc", "VEPParam",
    function(x) slot(x, "filterqc"))
setMethod("filterqc<-", "VEPParam",
    function(x, value) 
{
    value <- .formatList(value)
    slot(x, "filterqc")[names(value)] <- value
    msg <- .valid.VEPParam.filterqc(x)
    if (!is.null(msg))
        stop(msg) 
    x 
})

setMethod("database", "VEPParam",
    function(x) slot(x, "database"))
setMethod("database<-", "VEPParam",
    function(x, value) 
{
    value <- .formatList(value)
    slot(x, "database")[names(value)] <- value
    msg <- .valid.VEPParam.database(x)
    if (!is.null(msg))
        stop(msg) 
    x 
})

setMethod("advanced", "VEPParam",
    function(x) slot(x, "advanced"))
setMethod("advanced<-", "VEPParam",
    function(x, value) 
{
    value <- .formatList(value)
    slot(x, "advanced")[names(value)] <- value
    msg <- .valid.VEPParam.advanced(x)
    if (!is.null(msg))
        stop(msg) 
    x 
})

version <- function(x)
    slot(x, "version")
`version<-` <- function(x, value) 
{
    if (!is.numeric(value))
        stop("'value' must be numeric") 
        initialize(x, version=as.numeric(value))
}

scriptPath <- function(x)
    slot(x, "scriptPath")

`scriptPath<-` <- function(x, value)
{
    slot(x, "scriptPath") <- value
    x
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### helpers / utils 
###

supportedVEP <- function() list("VEPParam67"=67, "VEPParam73"=c(73, 74), 
                                "VEPParam75"=75, "VEPParam77"=77, 
                                "VEPParam78"=78)
currentVEP <- function() tail(supportedVEP(), 1) 

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### show 
###

setMethod(show, "VEPParam",
    function(object)
{
    scat <- function(fmt, vals=character(), exdent=2, ...)
    {
        vals <- ifelse(nzchar(vals), vals, "''")
        lbls <- paste(BiocGenerics:::selectSome(vals), collapse=", ")
        txt <- sprintf(fmt, length(vals), lbls)
        cat(strwrap(txt, exdent=exdent, ...), sep="\n")
    }
    cat("class:", class(object), "\n")
    nms <- slotNames(class(object))
    for (i in nms[!nms %in% c("version", "scriptPath")]) {
        elt <- slot(object, i)
        drop <- elt == FALSE | elementLengths(elt) == 0L
        drop[is.na(drop)] <- FALSE
        if (is.null(nms <- names(elt)[!drop]))
            nms <- character()
        scat(paste0(i, "(%d): %s\n"), nms)
    }
    cat(paste0("version: ", paste(version(object), collapse=","), "\n"))
    cat(paste0("scriptPath: ", scriptPath(object)), "\n")
})
