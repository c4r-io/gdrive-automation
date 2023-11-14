decrypt_gdrive_token <- function()
{
    token <- NULL
    if (gargle:::secret_has_key("GDRIVE_KEY"))
    {
        token <- gargle::secret_read_rds(".secrets/gdrive-token.rds", 
                                key = "GDRIVE_KEY")
    }
    
    invisible(token)
}