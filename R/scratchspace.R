run = FALSE
if (run)
{
    do_auth()

    # access info on all units
    db_units <- read_db_units()

    # test unit
    idx <- 1

    # get tasks for one unit
    test_unit_sheet <- db_units$`Task Sheet Name`[idx]
    test_unit_url <- db_units$`Tasks URL`[idx]
    unit_tasks <- read_unit_tasks(test_unit_url, test_unit_sheet)

    # get roadmap for one unit
    roadmap_url <- db_units$`Roadmap URL`[idx]
    roadmap_id <- googledrive::as_id(roadmap_url)

    ## download roadmap as docx
    dl_path <- tempfile(fileext = ".docx")
    googledrive::drive_download(roadmap_id, dl_path)

    ## read in docx
    roadmap <- officer::read_docx(dl_path)
    content <- officer::docx_summary(roadmap)

    ## find phase label and signoff labels
    phase_labels <- find_roadmap_phases(content)
    signoff_labels <- find_roadmap_signoffs(content)

    ## extract signoff statuses
    signoff_df <- data.frame(task = character(), signoff = character(), status = character())
    table_cells <- subset(content, content_type %in% "table cell")

    ### signoffs for phase 1
    find_status_phase_1(table_cells, phase_labels, signoff_labels)
    find_status_phase_2(table_cells, phase_labels, signoff_labels)
    find_status_phase_3(table_cells, phase_labels, signoff_labels)



}




