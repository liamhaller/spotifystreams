#get artist name


#' Get Monthly Listeners
#' @export get_artist_name
#' @inheritParams get_monthly_listeners
#' @return A character of the artist's name
#' @importFrom rvest read_html html_elements html_text
#' @importFrom dplyr %>%
#' @importFrom stringr str_remove

get_artist_name <- function(artist_code){

  #Spotify link to artists page
  artist_url <- paste0("https://open.spotify.com/artist/", artist_code)

  #Load html data from spotify page in enviroment
  web <- rvest::read_html(artist_url)

  #select all text within div content
  div_content <- web %>% rvest::html_elements("div") %>% rvest::html_text()

  #7th item in the vector contains artistname info
  artist_name <- div_content[7]

  #name info is cluttered so we need to remove a few strings, first the number of plays

  total_plays <- div_content[10] %>% strsplit(split = " ")
  total_plays <- total_plays[[1]][1]

  #remove strings
  artist_name <-
    stringr::str_remove(artist_name, c('monthly')) %>%
    stringr::str_remove('listenersFollow') %>%
    stringr::str_remove(total_plays)

  #remove white spaces
  artist_name <- trimws(artist_name, which = c("right"))

  return(artist_name)
}






