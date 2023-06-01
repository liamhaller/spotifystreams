#' list get montly listeners

#' @param dataframe A dataframe that contains artist stream
#' @param artist_code_column the column in which the artist codes are stored
#' @return An vector represetning total monthly streams of an artists


list_get_monthly_listerns <- function(dataframe, artist_code_column = 2){
  #wrapper for the monthly stream function to more
  #easily use sapply to vectorize the function

  #Select only the second column of the datafarme
  codes <- dataframe[,artist_code_column]

  #Apply monthly listens to each row and remove the name
  listens <- unname(sapply(codes , get_monthly_listeners))

  return(listens)

}


