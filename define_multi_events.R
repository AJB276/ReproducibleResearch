define_multi_events <- function(multi_event_list)
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
  }
  result
}