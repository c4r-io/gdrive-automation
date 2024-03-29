update_parsing_dat <- FALSE
run_sync <- FALSE
check_roadmap_files <- FALSE
test_roadmap <- FALSE
generate_new_oauth_token <- FALSE

if (update_parsing_dat)
{
    parsing_dat <- read.csv("inst/parsing_dat.csv")
    save("parsing_dat", file = "R/sysdata.rda")
}

if (check_roadmap_files)
{
    gdrv_auth()

    # access info on all units
    db_units <- read_db_units()
    roadmap_files <- find_files_roadmaps()
    check_files_roadmap(db_units, roadmap_files)
}

if (test_roadmap)
{
    gdrv_auth()

    # access roadmap docs from db
    db_units <- read_db_units()
    roadmap_urls <- db_units$`roadmap URL`
    tracker_urls <- db_units$`unit tasks URL`
    tracker_sheets <- db_units$`Sheet Name`

    idx <- which(db_units$Unit == "TESTING")

    roadmap_url <- roadmap_urls[idx]
    tracker_url <- tracker_urls[idx]
    tracker_sheet <- tracker_sheets[idx]
    unit_id <- db_units[[idx, "unit-id"]]
    sync_statuses(roadmap_url, tracker_url, tracker_sheet, unit_id)
}

if (run_sync)
{
    process_roadmaps()
}

if (generate_new_oauth_token)
{
    googledrive::drive_auth_configure(path = ".secrets/gdrive-oauth.json")
    googledrive::drive_oauth_client()
}



