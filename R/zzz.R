.onAttach <-
    function(libname, pkgname)
{
    msg <- paste0("variant_effect_predictor.pl or vep script not found. ",
                  "Ensembl VEP is not installed in your path.")
    tryCatch(check <- system2("perl", .getVepPath(), 
        stdout=TRUE, stderr=TRUE), 
        error=function(e) packageStartupMessage(msg))
}
