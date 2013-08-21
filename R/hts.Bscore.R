hts.Bscore <-function(plate, value.var="CTB"){
  
  require(reshape2)
  require(plyr)
  #casting the signal to a matrix
  values <- acast(plate, row~column, value.var=value.var)
  #Tukey's two way median polish
  med.pol <- medpolish(values, trace.iter=F)
  
  #extract values
  estPlateAverage <- med.pol$overall
  rowEffects <- med.pol$row
  colEffects <- med.pol$col
  
  #computation function
  calcB <- function(x, mu_p, rE, cE){ return(x[[value.var]] - (mu_p + rE[x$row] + cE[x$column]))}
  #calculate Bscore for each well
  result <- adply(plate,1, calcB, mu_p=estPlateAverage, rE=rowEffects, cE=colEffects)
  colnames(result)[ncol(result)] <- "Bscore"      
  result$Bscore <- result$Bscore / mad(result[[value.var]])
  return(result)
}