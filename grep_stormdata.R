#This code was useful from the console when tidying the data

grep_stormdata <- function(compstr,levels=TRUE,col="EVTYPENEW")
{
  gd <- grep(compstr,toupper(stormdata[,col]))
  if(levels)
    levels(factor(stormdata[gd,col]))
  else
    stormdata[gd,]
}