library("googledrive")

source("api-functions.R")

drive_auth(token = decrypt_gdrive_token())

c4r_drive <- "(C4R) Community for Rigor"

# get some info about working with google drive
drive_find("Unit Roadmap", shared_drive = c4r_drive)

drive_about()$exportFormats

# do some stuff
roadmap_file <- drive_find("Unit Roadmap - Sample Size and Power Calculation", 
                           shared_drive = c4r_drive)
downloaded_file <- drive_download(roadmap_file, type = "rtf")


