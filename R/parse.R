#Parse
#helper functions

#' Parse Spotify Links
#'
#' @param link
#' @param link A link to an artists page from spotify web player
#' @return A character of an artist's spotify code
#' @export
#'
parse_spotify_link <- function(link){

  artist_code <- strsplit(link, split =  "/")[[1]][5]
  return(artist_code)

}

