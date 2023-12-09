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
        roadmap_ids <- googledrive::as_id(roadmap_urls)
        tracker_urls <- db_units$`Tracker URL`
        tracker_sheets <- db_units$`Tracker Sheet Name`
    }, error = function(e) {
        log_action(e$message, type = "ERROR")
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
        if (title(roadmap_dat) != title(tracker_dat))
        {
            title(tracker_dat) <- title(roadmap_dat)
            log_action(paste0("Updating title to '", title(tracker_dat), "'"),
                       note = tracker_url,
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
                       note = tracker_url,
                       "ACTION TAKEN")
        }

        # check for updated statuses
        if (!identical(roadmap_dat$Status,
                       tracker_dat$Status))
        {
            diff_statuses <- which(roadmap_dat$Status != tracker_dat$Status)

            # go through each status
            for(i in diff_statuses)
            {
                roadmap_status <- roadmap_dat$Status[i]
                tracker_status <- tracker_dat$Status[i]
                # if roadmap is submitted and tracker is not started
                # take action for submission
                if (roadmap_status == "Submitted" &&
                    tracker_status == "Not started")
                {
                    tracker_dat$Status[i] <- "Submitted"
                    log_action(paste0("Updating status: {",
                                      format_status_msg(tracker_status, sep = ", "),
                                      "}"),
                               note = tracker_URL,
                               "ACTION TAKEN")
                    notify(to = "CENTER",
                           dat = roadmap_status)
                }

                # if roadmap is approved and tracker is not approved
                # log action needed (check if tracker needs updating)
                # if tracker is approved and roadmap is not approved
                # log action needed (update roadmap)
                # if any other discrepancy
                # log action needed



            }
        }

    }, error = function(e) {
        log_action(e$message, type = "ERROR")
    })


    log_action("Ending Processing Loop", note = "Success")

}
