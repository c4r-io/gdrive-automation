run = FALSE
if (run)
{
    do_auth()

    # access info on all units
    db_units <- read_db_units()

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



    tapply(content$doc_index,
           content$content_type,
           function(x) length(unique(x)))
    table_cells <- subset(content, content_type %in% "table cell")
    print(head( table_cells) )

    ## identify dropdowns and map to signoffs
    signoff_df <- data.frame(task = character(), signoff = character(), status = character())

    instructions_box_id <- which(grepl("Instructions will be provided", table_cells$text))
    phase_1_box_id <- instructions_box_id + 5
    phase_1_approvals <- table_cells[phase_1_box_id,]
    signoff_df %>%
        add_status(phase_1_approvals$text, "(\\w+)\\s*Review by CENTER",
                   task = "Phase 1", signoff = "CENTER") %>%
        add_status(phase_1_approvals$text, "(\\w+)\\s*Topic Approval by NIH/NINDS",
                   task = "Phase 1", signoff = "NIH/NINDS PO") %>%
        add_status(phase_1_approvals$text, "(\\w+)\\s*Review by SC",
                   task = "Phase 1", signoff = "SC")







}




