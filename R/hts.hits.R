#hit detection
hts.hits <- function(plates, signalType, method, margin=2, withControls=F, control.columns=12)
{ 
  plates$signal <- plates[[signalType]]
  
  upper_margin <- margin
  
  if(withControls)
  {
    data <- plates
  }
  else
  {
    data <- plates[,-control.columns]
  }
  
  if(method=="SD")
  {
    kIn1.mean <- mean(plates$signal, na.rm=T)
    kIn1.sd <- sd(plates$signal, na.rm=T)
    kIn1.outliers <-  subset(data,((signal > (kIn1.mean + (upper_margin * kIn1.sd))) 
                                   | (signal < (kIn1.mean - (margin * kIn1.sd))))) 
  }
  else if(method=="MAD")
  {
    kIn1.median <- median(plates$signal, na.rm=T)
    kIn1.mad <- mad(plates$signal, na.rm=T)
    kIn1.outliers <- subset(data,((signal > (kIn1.median + (upper_margin * kIn1.mad)))
                                  | (signal < (kIn1.median - (margin * kIn1.mad)))))
  }
  
  else if(method=="quartile")
  {
    kIn1.quantiles <- quantile(plates$signal, na.rm=T)
    kIn1.IQR <- kIn1.quantiles[4] - kIn1.quantiles[2]
    kIn1.outliers <- subset(data, ((signal > (kIn1.quantiles[4] + (upper_margin * kIn1.IQR))) 
                                   | (signal < (kIn1.quantiles[2] - (margin * kIn1.IQR)))))
  }
  
  else kIn1.outliers <- plates
  
  return(kIn1.outliers)
}
