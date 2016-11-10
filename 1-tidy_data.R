
library(tidyverse)
library(readxl)

# manual fixes
# - removed linebreaks from all column names
# - converted text to number; column BG

data_file <- "2016-10-28_data.xlsx"

demograph <- read_excel(data_file, "Demog")

nm <- c("patient", "category", "med", "dose_route", "freq", "location",
        "admin_datetime", "stop_datetime", "num_meds", "before_proc",
        "after_proc", "before_nv", "anti_nv_hrs", "anti_nv_or", "num_anti_nv",
        "rr")
types <- c(rep("text", 6), rep("date", 2), "numeric", rep("text", 5), "numeric", "text")
meds <- read_excel(data_file, "Medications", col_names = nm, col_types = types, skip = 1)
    # separate("dose_route", c("dose", "units", "route"), sep = " ")

# - convert text to number; vital signs

nm <- c("patient", "vital_datetime", "vital", "vital_result", "amt_time",
            "location", "num_uncontrolled", "num_bps")
types <- c("text", "date", "text", "numeric", "text", "text", "numeric", "numeric")
vitals <- read_excel(data_file, "Vitals", col_names = nm, col_types = types, skip = 1)
