VEPToGRanges <- function(VEP_tab){
  locs <- do.call(rbind, strsplit(VEP_tab$Location, split = ':'))
  gr <- GRanges(seqnames <- locs[,1], IRanges(start = as.numeric(locs[,2]), width = 1))
  values(gr)$Allele <- VEP_tab$Allele
  values(gr)$Gene <- VEP_tab$Gene
  values(gr)$TXID <- VEP_tab$Feature
  values(gr)$Feature_type <- VEP_tab$Feature_type
  values(gr)$Consequence <- VEP_tab$Consequence
  values(gr)$Existing_variation <- VEP_tab$Existing_variation
  return(gr)
}



