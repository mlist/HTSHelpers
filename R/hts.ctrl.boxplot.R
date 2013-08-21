hts.ctrl.boxplot <- function(plates.norm)
{
  qplot(x=Sample, y=CTB, data=subset(plates.norm, !is.na(Control)), geom="boxplot", color=Plate, shape=Replicate)
}
