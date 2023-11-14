log_action <- function(action = "", note = "")
{
    my_log <- access_log()
    log_row <- data.frame(datetime = format(Sys.time(), "%Y-%M-%d %X %Z"),
                          action = action,
                          note = note)
    googlesheets4::sheet_append(my_log, log_row)
}

access_log <- function()
{
    do_auth()
    googledrive::drive_get("https://docs.google.com/spreadsheets/d/1n3rmcM94r_RL-QPTEuxP2D2g2BlrB_Hhdw1Ag_FkUsA/edit?usp=sharing")
}


