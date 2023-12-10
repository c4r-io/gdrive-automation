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
    do_auth()
    set_loop_num_var()
    log_action("Starting Processing Loop")

    # access roadmap docs from db
    tryCatch({
        db_units <- read_db_units()
        roadmap_urls <- db_units$`Roadmap URL`
        tracker_urls <- db_units$`Tracker URL`
        tracker_sheets <- db_units$`Tracker Sheet Name`
    }, error = function(e) {
        log_action(e$message, type = "ERROR")
    })

    tryCatch({
        idx <- 1

        # get statuses from unit roadmap
        roadmap_url <- roadmap_urls[idx]
        roadmap_dat <- read_roadmap_statuses(roadmap_url)

        # get statuses from task spreadsheet
        tracker_url <- tracker_urls[idx]
        tracker_sheet <- tracker_sheets[idx]
        tracker_dat <- read_tracker_statuses(tracker_url, tracker_sheet)

        # check for same number of rows
        stopifnot(NROW(roadmap_dat) == NROW(tracker_dat))

        # check for updated unit title
        if (title(roadmap_dat) != title(tracker_dat))
        {
            title(tracker_dat) <- title(roadmap_dat)
            log_action(paste0("Updating title to '", title(tracker_dat), "'"),
                       url = tracker_url,
                       "ACTION TAKEN")
        }

        # check for updated mini-unit names
        if (!identical(mini_units(roadmap_dat),
                       mini_units(tracker_dat)))
        {
            mini_units(tracker_dat) <- mini_units(roadmap_dat)
            log_action(paste0("Updating mini-unit titles to {'",
                              paste0(mini_units(tracker_dat), collapse = "', '"),
                              "'}"),
                       url = tracker_url,
                       "ACTION TAKEN")
        }

        # check for differences in statuses
        tracker_dat <- handle_diff_statuses(roadmap_dat, roadmap_url,
                                            tracker_dat, tracker_url)

        # CLEANUP: merge staged todos and updated tracker data
        merge_todo()
        update_tracker_data(tracker_dat, tracker_url, tracker_sheet)

    }, error = function(e) {
        log_action(e$message, type = "ERROR")
    })

    log_action("Ending Processing Loop", note = "Success")
}
