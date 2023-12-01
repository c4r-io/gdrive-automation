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

    # test unit
    idx <- 1

    # get tasks for one unit
    test_unit_sheet <- db_units$`Task Sheet Name`[idx]
    test_unit_url <- db_units$`Tasks URL`[idx]
    unit_tasks <- read_unit_tasks(test_unit_url, test_unit_sheet)

    # get roadmap for one unit
    roadmap_url <- db_units$`Roadmap URL`[idx]
    roadmap_id <- googledrive::as_id(roadmap_url)

    statuses <- get_roadmap_statuses(roadmap_id)

    all.equal(unit_tasks, statuses, check.attributes = FALSE)

    # get statuses from all roadmaps
    #   log errors
    # compare against stored data
    #   log any changes
    # store new data (list with roadmap_id column, and statuses column)



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
    }, error = function(e) {
        log_action(e$message, type = "ERROR", loop_num = loop_num)
    })

    tryCatch({
        idx <- 1
        roadmap_id <- roadmap_ids[idx]
        statuses <- get_roadmap_statuses(roadmap_id)


    }, error = function(e) {
        log_action(e$message, type = "ERROR", loop_num = loop_num)
    })


    log_action("Ending Processing Loop", note = "Success", loop_num = loop_num)

}
