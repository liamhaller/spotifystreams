#' @export

update_streams <- function(list){

  #Get current date for column title
  col_title <- format(Sys.Date(), "%D")

  #need number of columsn to change the column name after
  n_col <- ncol(league[[1]]) + 1 #total columns post update

  #Get updated value of streams for all league members
  update <- map(list, list_get_monthly_listerns)

  #update values in list & change name of column
  for(i in seq_along(league)){
    #create a new column in the league member database
    list[[i]]$placeholder <- update[[i]]

    #set column title to the date (you would not belive how complicated this was)
    names <- colnames(list[[i]])
    names[n_col] <-  enexpr(col_title)
    colnames(list[[i]]) <- names

  }
  return(list)
}




