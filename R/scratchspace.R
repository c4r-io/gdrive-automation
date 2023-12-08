run = FALSE
if (run)
{
    parsing_dat <- read.csv("inst/parsing_dat.csv")
    use_data(parsing_dat, overwrite = TRUE)

    do_auth()

    # access info on all units
    db_units <- read_db_units()
    roadmap_files <- find_files_roadmaps()
    check_files_roadmap(db_units, roadmap_files)


}

if (run)
{
    # start of processing loop
    loop_num <- get_last_loop_num() + 1
    log_action("Starting Processing Loop", loop_num = loop_num)

    # access roadmap docs from db
    tryCatch({
        do_auth()
        db_units <- read_db_units()
        roadmap_urls <- db_units$`Roadmap URL`
        roadmap_ids <- googledrive::as_id(roadmap_urls)
        tracker_urls <- db_units$`Tracker URL`
        tracker_sheets <- db_units$`Tracker Sheet Name`
    }, error = function(e) {
        log_action(e$message, type = "ERROR", loop_num = loop_num)
    })

    tryCatch({
        idx <- 1

        # get statuses from unit roadmap
        roadmap_id <- roadmap_ids[idx]
        roadmap_dat <- read_roadmap_statuses(roadmap_id)

        # get statuses from task spreadsheet
        tracker_url <- tracker_urls[idx]
        tracker_sheet <- tracker_sheets[idx]
        tracker_dat <- read_tracker_statuses(tracker_url, tracker_sheet)

        # check for same number of rows
        stopifnot(NROW(roadmap_dat) == NROW(tracker_dat))

        # check for updated unit title
        if (roadmap_dat$Unit[1] != tracker_dat$Unit[1])
        {
            log_action(paste0("Updating title to '", roadmap_dat$Unit[1], "'"),
                       paste0("URL = ", tracker_url),
                       "ACTION TAKEN",
                       loop_num = loop_num)
            tracker_dat$Unit <- roadmap_dat$Unit[1]
        }

        # check for updated mini-unit names
        mini_unit_idx <- 7:38
        if (!identical(roadmap_dat$`Mini-Unit`[mini_unit_idx],
                       tracker_dat$`Mini-Unit`[mini_unit_idx]))
        {
            log_action(paste0("Updating mini-unit titles to {'",
                              paste0(unique(roadmap_dat$`Mini-Unit`[mini_unit_idx]), collapse = "', '"), "'}"),
                       paste0("URL = ", tracker_url),
                       "ACTION TAKEN",
                       loop_num = loop_num)
            tracker_dat$`Mini-Unit`[mini_unit_idx] <-
                roadmap_dat$`Mini-Unit`[mini_unit_idx]
        }

        # check for updated statuses
        if (!identical(roadmap_dat$Status,
                       tracker_dat$Status))
        {
            # go through each status
            # if roadmap is submitted and tracker is not started
            # take action for submission
            # if roadmap is approved and tracker is not approved
            # log action needed (check if tracker needs updating)
            # if tracker is approved and roadmap is not approved
            # log action needed (update roadmap)
            # if any other discrepancy
            # log action needed
        }

    }, error = function(e) {
        log_action(e$message, type = "ERROR", loop_num = loop_num)
    })


    log_action("Ending Processing Loop", note = "Success", loop_num = loop_num)

}
