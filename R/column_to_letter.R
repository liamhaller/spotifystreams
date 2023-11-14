#' Column to letter
#'
#' @param column_number Given a number, this function will tell you the corresponding alphabatized column
#'
#' @return A sequence of letters corresponding to the column number e.g., 1 = A, 26 = Z, 27 = AA
#'
#'
#'
colnum_to_letter <- function(column_number){
  if(column_number <= 26){
    letter <- LETTERS[column_number]
  } else {
    #Fist letter rotates every 26, AB...AZ...BA
    letter1 <- LETTERS[round(26/column_number, 0)]
    #Second letter is remainder after 26
    letter2 <- LETTERS[column_number%%26]

    letter <- paste0(letter1,letter2)
  }
  return(letter)
}
