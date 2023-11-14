library(googledrive)

# set up a local file cache for a new token
options(gargle_oauth_cache = ".secrets")
gargle::gargle_oauth_cache()

# create a new token
drive_auth(email = TRUE, scopes = "https://www.googleapis.com/auth/drive")
my_token <- drive_token()

# setup encryption key
key <- gargle::secret_make_key()
cat(paste0("GDRIVE_KEY=", key)) # copy the result of this string
usethis::edit_r_environ() # run this to open your .Renviron file
# paste the result into that file
# restart your R session

# encrypt a new token and save it in inst/secrets/gs4-auth.token
gargle::secret_write_rds(
    x = my_token, 
    path = ".secrets/gdrive-token.rds", 
    key = "GDRIVE_KEY"
)

# check that the token can be decrypted
stopifnot(!is.null(decrypt_gdrive_token()))
