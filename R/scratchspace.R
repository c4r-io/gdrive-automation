run = FALSE
if (run)
{
    do_auth()

    c4r_drive <- "(C4R) Community for Rigor"

    # get some info about working with google drive
    drive_find("Unit Roadmap", shared_drive = c4r_drive)

    drive_about()$exportFormats

    # do some stuff
    roadmap_file <- drive_find("Unit Roadmap - Sample Size and Power Calculation",
                               shared_drive = c4r_drive)
    downloaded_file <- drive_download(roadmap_file, type = "rtf")

    # access info on all units
    db_units <- read_db_units()

    test_unit_url <- db_units$`Tasks URL`[1]
    test_unit_sheet <- db_units$`Task Sheet Name`[1]

    unit_tasks <- read_unit_tasks(test_unit_url, test_unit_sheet)




}
