hts.normalize <- function(plates){
  #apply standard methods
  plates.norm <- ddply(plates, .(Plate, Replicate), transform, 
        poc=(CTB / mean(CTB[Control=="NEG"], na.rm=T)), 
        npi=(mean(CTB[Control=="NEG"], na.rm=T) - CTB) / (mean(CTB[Control=="NEG"], na.rm=T) - mean(CTB[Control=="POS"], na.rm=T)), 
        rzscore=(CTB - median(CTB))/mad(CTB),
        zscore=(CTB - mean(CTB))/sd(CTB), 
        centered=CTB/mean(CTB),
        rcentered=CTB/median(CTB))
  
  #apply bscore
  plates.norm <- ddply(plates.norm, .(Plate, Replicate), hts.Bscore)
  
  #modify data
  plates.norm$wellCount <- as.integer(row.names(plates.norm))
  plates.norm$Replicate <- as.factor(plates.norm$Replicate)
  plates.norm$Plate <- as.factor(plates.norm$Plate)
  
  return(plates.norm)
}