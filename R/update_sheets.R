#' Update Sheets
#'
#' @param list Enter a list of dataframes that contain updated streaming metrics
#' @param updated_coulmn which column contains the updated metrics
#' @param sheet_id code that identifys google sheet
#' @param auth_token saved auth token to connnect to googlesheets
#'
#' @return nothing is returned, but external sheet is updated
#' @export
#'
update_sheets <- function(list, updated_coulmn, sheet_id, auth_token){

  #loop within list for each member
  for(i in seq_along(league)){

    #name within the list should match name of sheet in googlesheets
    sheet_name <- names(list)[i]

    #Deauth from google sheets, since not necessary to read
    gs4_deauth()

    #Read the data from google sheets to determin where to write
    member_sheet <- read_sheet(sheet_id, sheet = sheet_name)

    #number of active columsn currently being used in googlesheet
    last_used_col <- ncol(member_sheet)
    new_col <-  last_used_col + 1 #we want to write to the next open column

    ##Convert column number to googlesheets letter value ##
    ##                                                   ##
    #The name of the column in which we will write new data (the first non-used column)
    col_name <- colnum_to_letter(new_col)
    #the full range where we will past information
    writing_range <- paste0(col_name, "3",":",col_name, "8")

    new_streaming_data <- list[[i]][updated_coulmn] #fourht column is the updated values

    ##Writing ##
    gs4_auth(token = auth_token)

    do_we_have_a_token <- gs4_has_token()

    if(do_we_have_a_token == TRUE){

      range_write(
        sheet_id,
        new_streaming_data,
        sheet = sheet_name,
        range = writing_range,
        col_names = TRUE,
        reformat = TRUE
      )
    } else {

      stop("No googlesheets token provided cannot write")

    }

  }
}
