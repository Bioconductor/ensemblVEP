variant_body = function(chr, pos, id, ref, alt) {
  sprintf("%s  %d  %s %s %s . . .", chr, pos, id, ref, alt)
}

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

basic_chr = c(21,21)
basic_pos = c(26960070, 26965148)
basic_id = c("rs116645811", "rs116645811")
basic_ref = c("G", "G")
basic_alt = c("A", "A")

library(httr)
z = post_Hs_region(basic_chr, basic_pos, basic_id, basic_ref, basic_alt) 
