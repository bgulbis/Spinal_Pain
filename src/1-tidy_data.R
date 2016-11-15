
library(tidyverse)
library(readxl)
library(stringr)

# manual fixes for Demog tab
# - removed linebreaks from all column names
# - converted text to number; column BG
# - fix duplicate column names: X, Y, Z, AE, AH, AI, AJ, AL, AN, AO, AP, AQ, AU, AV, AW, AY, AZ, BA, BB, BH, BJ, BK
#   * Note, need to remove data validation: AI
# - remove 25 null columns

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

data_tidy <- demograph %>%
    select(1:9, 11:14, 16:21)

names(data_tidy) <- str_to_lower(names(data_tidy))
names(data_tidy) <- str_replace_all(names(data_tidy), " \\(.*\\)", "")
names(data_tidy) <- str_replace_all(names(data_tidy), "date/time", "datetime")
names(data_tidy) <- str_replace_all(names(data_tidy), "date/", "")
names(data_tidy) <- str_replace_all(names(data_tidy), " ", "_")

data_tidy <- data_tidy %>%
    rename(num_pain_meds = number__of_pain_meds_used,
           weight = wt,
           height = ht)
