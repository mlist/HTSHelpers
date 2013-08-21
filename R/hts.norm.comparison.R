hts.norm.comparison<- function(data, plate=1){
  require(reshape)
  p <- qplot(x=wellCount, y=value, data=melt(subset(data, Plate==plate), id.vars=c("Plate", "wellCount", "Replicate", "Sample", "Control", "row", "column")), color=Replicate, main="Comparison of different normalization methods") + facet_wrap(~variable, scales="free", ncol=3) + geom_smooth(method="loess")
  print(p)
}
