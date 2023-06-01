#' @importFrom rvest read_html %>% html_elements html_text



get_monthly_listeners <- function(artist_code){

  #Spotify link to artists page
  artist_url <- paste0("https://open.spotify.com/artist/", artist_code)

  #Load html data from spotify page in enviroment
  web <- rvest::read_html(artist_url)

  #select all text within div content
  div_content <- web %>% rvest::html_elements("div") %>% rvest::html_text()

  #10th item in the vector contains stream info
  monthly_streams <- div_content[10]

  #seperate text from "monthly listerers"
  monthly_streams <- strsplit(monthly_streams, split = " ")
  #subset list, and then vector to get first element
  monthly_streams <- monthly_streams[[1]][1]
  #remove the comma so we can convert to numeric
  monthly_streams <- as.numeric(gsub(",", "", monthly_streams))

  return(monthly_streams)
}

