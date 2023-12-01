run = FALSE
if (run)
{
    parsing_dat <- read.csv("inst/signoff_parsing.csv")
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


