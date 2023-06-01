#' @export

list_get_monthly_listerns <- function(dataframe){
  #wrapper for the monthly stream function to more
  #easily use sapply to vectorize the function

  #Select only the second column of the datafarme
  codes <- dataframe[,2]

  #Apply monthly listens to each row and remove the name
  listens <- unname(sapply(codes , get_monthly_listeners))

  return(listens)

}


