## define_multi_events.R
## source to tidy storm data that contains two or more event types in a list
## it returns a string of the first item in the evtypes, if no elements in the
## list match it returns a join str with the sep default = ":"

define_multi_events <- function(multi_event_list,evtypes,sep=":")
{
  len <- length(multi_event_list)
  result <- vector(mode="character",length=len)
  for(i in 1:len)
  {
    result[i] <- NA
    for(j in 1:length(multi_event_list[[i]]))
    {
      if(multi_event_list[[i]][j] %in% toupper(evtypes))
      {
        result[i] <- multi_event_list[[i]][j]
        break
      }
    }
    if(is.na(result[i]))
    {
      joinstr <- NA
      for(j in 1:length(multi_event_list[[i]]))
      {
        if(j == 1)
          joinstr <- multi_event_list[[i]][j]
        else
          joinstr <- paste(joinstr,multi_event_list[[i]][j],sep)
      }
      result[i] <- joinstr
    }
  }
  result
}