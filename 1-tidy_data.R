
library(tidyverse)
library(readxl)

# manual fixes
# - removed linebreaks from all column names
# - converted text to number; column BG

data_file <- "2016-10-28_data.xlsx"

demograph <- read_excel(data_file, "Demog")

meds <- read_excel(data_file, "Medications")

nm <- c("patient", "vital_datetime", "vital", "vital_result", "amt_time",
            "location", "num_uncontrolled", "num_bps")
types <- c("text", "date", "text", "numeric", "text", "text", "numeric", "numeric")
vitals <- read_excel(data_file, "Vitals", col_names = nm, col_types = types, skip = 1)
