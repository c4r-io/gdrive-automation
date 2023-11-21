#' Authorize API access to Google Drive and Google Sheets
#'
#' @return NULL
#' @export
do_auth <- function()
{
    my_token <- decrypt_gdrive_token()
    googledrive::drive_auth(token = my_token)
    googlesheets4::gs4_auth(token = my_token)
    invisible()
}


#' Get a Token for access to google drive
#'
#' This function expects that there is an encrypted file in RDS format with an
#' OAuth token, and an environmental variable with the key to decrypt the token.
#'
#' @param token_file filepath to the encrypted token file
#' @param decrypt_env_var name of the environmental variable that is the decryption key
#'
#' @return an OAuth token
#' @export
decrypt_gdrive_token <- function(token_file = ".secrets/gdrive-token.rds",
                                 decrypt_env_var = "GDRIVE_KEY")
{
    token <- NULL
    if (gargle::secret_has_key(decrypt_env_var))
    {
        token <- gargle::secret_read_rds(token_file,
                                key = decrypt_env_var)
    }

    invisible(token)
}

#' Default URL for the googlesheet with the master list of links for all units
#'
#' @return string with the URL for the
#' @export
URL_db_units <- function()
{
   "https://docs.google.com/spreadsheets/d/1cdEwmaUIXPvEIdVOow8tQCPjsz7hseO4h7aaS8ylImA/edit#gid=472058338"
}
