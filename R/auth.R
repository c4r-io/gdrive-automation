#' Authorize API access to Google Drive and Google Sheets
#'
#' @return NULL
#' @export
gdrv_auth <- function()
{
    my_token <- decrypt_gdrive_token()
    googledrive::drive_auth(token = my_token)
    googlesheets4::gs4_auth(token = my_token)
    invisible()
}


#' Get a token for access to google drive
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

#' Get a token for access to Monday
#'
#' @param token_env_var name of the environmental variable that is the token
#'
#' @return a character
#' @export
get_monday_token <- function(token_env_var = "monday_token")
{
    token <- Sys.getenv(token_env_var, unset = NA)
    invisible(token)
}
