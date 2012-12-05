.onLoad <-
    function(libname, pkgname)
{
    msg <- paste0("variant_effect_predictor.pl not found. ",
                  "Ensembl VEP is not installed in your path.")
    tryCatch(check <- system2("perl", Sys.which("variant_effect_predictor.pl"), 
        stdout=TRUE, stderr=TRUE), 
        error=function(e) packageStartupMessage(msg))
}
