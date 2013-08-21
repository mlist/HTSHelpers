hts.scatter <- function(plates.norm)
{
  q <- qplot(x=wellCount, y=CTB, data=plates.norm, color=Plate, shape=Replicate) + geom_line(stat="hline", yintercept="mean", color="black", aes(group=interaction(Plate, Replicate)))
  print(q)
}