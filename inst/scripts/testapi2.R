library(httr)

#' helper function to construct inputs for VEP REST API
#' @param chr character(1)
variant_body = function(chr, pos, id, ref, alt) {
  sprintf("%s  %d  %s %s %s . . .", chr, pos, id, ref, alt)
}
#' elementary vep/homo_sapiens/region call to ensembl VEP REST API
#' @param chr character(1) ensembl chromosome identifier (e.g., "7")
#' @param pos numeric(1) 1-based chromosome position
#' @param id character(1) arbitrary identifier
#' @param ref character(1) reference allele
#' @param alt character(1) alternative allele
#' @examples
#' post_Hs_region("7", 155800001, "chk", "A", "T")
#' @export
post_Hs_region = function(chr, pos, id, ref, alt) {
 server <- "https://rest.ensembl.org"
 ext <- "/vep/homo_sapiens/region"
 tj = jsonlite::toJSON(list(variants=variant_body(chr, pos, id, ref, alt)))
 ans = httr::POST(paste(server, ext, sep = ""), 
   httr::content_type("application/json"), 
   httr::accept("application/json"), 
   body = as.character(tj)
   )
 httr::stop_for_status(ans)
 ans
}


if (FALSE) {
basic_chr = c(21,21)
basic_pos = c(26960070, 26965148)
basic_id = c("rs116645811", "rs116645811")
basic_ref = c("G", "G")
basic_alt = c("A", "A")

z = post_Hs_region(basic_chr, basic_pos, basic_id, basic_ref, basic_alt) 

file <- system.file("extdata", "ex2.vcf", package="VariantAnnotation")
library(VariantAnnotation)
rv = readVcf(file)
erv = expand(rv)
#vr = as(rv, "VRanges")
rr = rowRanges(erv)
rrc = as.character(seqnames(rr))
rrp = start(rr)
rr_id = rep("X", length(rrp))
rr_ref = as.character(rr$REF)
rr_alt = as.character(rr$ALT)

zz = post_Hs_region(rrc, rrp, rr_id, rr_ref, rr_alt)
}

post_Hs_region(7, 155800001, "chk", "A", "T")

fl <- system.file("extdata", "chr22.vcf.gz", package="VariantAnnotation")
r22 = readVcf(fl)
dr = which(width(rowRanges(r22))!=1)
r22s = r22[-dr]

vep_by_region = function(vcfobj, snv_only=TRUE, chk_max=TRUE) {
 if (snv_only) {
  dr = which(width(rowRanges(vcfobj))!=1)
  if (length(dr)>0) vcfobj = vcfobj[-dr]
 }
 if (chk_max) {
  if (nrow(vcfobj) > 200) stop("VEP API is limited to 200 positions")
  }
 rr = rowRanges(vcfobj)
 post_Hs_region( chr = as.character(seqnames(rr)),
                 pos = start(rr),
                 id = names(rr),
                 ref = as.character(rr$REF),
                 alt = as.character(unlist(rr$ALT)) )
}

ans = vep_by_region(r22[1:100], snv_only=FALSE, chk_max=FALSE)
     
