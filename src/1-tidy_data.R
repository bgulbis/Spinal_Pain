
library(tidyverse)
library(readxl)
library(stringr)

# manual fixes for Demog tab
# - removed linebreaks from all column names
# - converted text to number; column BG
# - fix duplicate column names: X, Y, Z, AE, AH, AI, AJ, AL, AN, AO, AP, AQ, AU, AV, AW, AY, AZ, BA, BB, BH, BJ, BK
#   * Note, need to remove data validation: AI
# - remove 25 null columns

data_file <- "data/raw/2016-10-28_data.xlsx"

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
    select(1:9, 11:14, 16:21, 64)

names(data_tidy) <- str_to_lower(names(data_tidy))
names(data_tidy) <- str_replace_all(names(data_tidy), " \\(.*\\)", "")
names(data_tidy) <- str_replace_all(names(data_tidy), "date/time", "datetime")
names(data_tidy) <- str_replace_all(names(data_tidy), "date/", "")
names(data_tidy) <- str_replace_all(names(data_tidy), " ", "_")

data_tidy <- data_tidy %>%
    rename(num_pain_meds = number__of_pain_meds_used,
           weight = wt,
           height = ht,
           med_allergies = `medication_allergies_to_meds_currently_taking?`) %>%
    mutate(length_stay = as.numeric(difftime(discharge_datetime, admission_datetime, units = "days")),
           surgery_duration = as.numeric(difftime(surgery_stop, surgery_start, units = "hours")),
           pacu_duration = as.numeric(difftime(pacu_end, pacu_start, units = "hours")))

data_pmh <- demograph %>%
    select(patient = Patient, starts_with("PMH-")) %>%
    dmap_if(is.character, ~ .x == "Yes")

names(data_pmh) <- str_replace_all(names(data_pmh), "PMH-|\\(.*\\)", "")
names(data_pmh) <- str_replace_all(names(data_pmh), " |/", "_")
names(data_pmh) <- str_to_lower(names(data_pmh))

data_tidy <- left_join(data_tidy, data_pmh, by = "patient")

data_home_med <- demograph %>%
    select(patient = Patient, starts_with("Home med ")) %>%
    mutate(home_gabapentin = `Home med Gabapentin/Pregabalin` == "Gabapentin",
           home_pregabalin = `Home med Gabapentin/Pregabalin` == "Pregabalin") %>%
    select(-`Home med Gabapentin/Pregabalin`) %>%
    dmap_if(is.character, ~ .x == "Yes")

names(data_home_med) <- str_replace_all(names(data_home_med), " med| \\+", "")
names(data_home_med) <- str_replace_all(names(data_home_med), " ", "_")
names(data_home_med) <- str_to_lower(names(data_home_med))

data_tidy <- left_join(data_tidy, data_home_med, by = "patient")

data_adverse <- demograph %>%
    select(patient = Patient, starts_with("AE")) %>%
    dmap_if(is.character, ~ .x == "Yes") %>%
    rename(adv_eff_nausea_vomiting = `AE N/V requiring anti-emetic`)

names(data_adverse) <- str_replace_all(names(data_adverse), "AE", "adv_eff")
names(data_adverse) <- str_replace_all(names(data_adverse), " ", "_")
names(data_adverse) <- str_to_lower(names(data_adverse))

data_tidy <- left_join(data_tidy, data_adverse, by = "patient")

# there was no cardiac arrest data
data_arrest <- demograph %>%
    select(patient = Patient, 24:31)

# there was minimal respiratory distress data
data_distress <- demograph %>%
    select(patient = Patient, 33:43)

# there was no sedation data
data_sedation <- demograph %>%
    select(patient = Patient, 45:55)

# there was minimal hypotension data
data_hypotension <- demograph %>%
    select(patient = Patient, 59:63)
