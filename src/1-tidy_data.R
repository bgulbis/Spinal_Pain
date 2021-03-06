
library(tidyverse)
library(readxl)
library(stringr)
library(lubridate)
library(MESS)

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
types <- c("numeric", rep("text", 5), rep("date", 2), "numeric", rep("text", 5), "numeric", "text")
meds <- read_excel(data_file, "Medications", col_names = nm, col_types = types, skip = 1) %>%
    dmap_at(c("category", "med", "location"), str_trim, side = "both") %>%
    dmap_at("med", str_to_lower) %>%
    dmap_at("med", str_replace_all, pattern = "ondandetron", replacement = "ondansetron") %>%
    dmap_at("med", str_replace_all, pattern = "faotidine", replacement = "famotidine") %>%
    dmap_at("before_nv", str_replace_all, pattern = "N/V", replacement = "Nausea")

    # separate("dose_route", c("dose", "units", "route"), sep = " ")

# - convert text to number; vital signs

nm <- c("patient", "vital_datetime", "vital", "vital_result", "amt_time",
            "location", "num_uncontrolled", "num_bps")
types <- c("numeric", "date", "text", "numeric", "text", "text", "numeric", "numeric")
vitals <- read_excel(data_file, "Vitals", col_names = nm, col_types = types, skip = 1) %>%
    filter(!is.na(vital)) %>%
    dmap_at("vital", str_trim, side = "both")

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

data_uncontrolled_postop <- vitals %>%
    left_join(data_tidy[c("patient", "surgery_start", "surgery_stop")], by = "patient") %>%
    filter(vital == "BPS",
           vital_datetime >= surgery_stop,
           vital_datetime <= surgery_stop + hours(24)) %>%
    arrange(patient, vital_datetime) %>%
    group_by(patient) %>%
    mutate(uncontrolled = vital_result > 6 & (lead(vital_result) > 6 | lag(vital_result > 6)),
           duration = as.numeric(difftime(lead(vital_datetime), vital_datetime, units = "hours")),
           duration = coalesce(duration, 0.1),
           diff_pain = uncontrolled & (uncontrolled != lag(uncontrolled) | is.na(lag(uncontrolled))),
           pain_num = cumsum(diff_pain))

data_uncontrolled_postop_sum <- data_uncontrolled %>%
    filter(uncontrolled) %>%
    group_by(patient, pain_num) %>%
    summarize(uncontrol_duration = sum(duration, na.rm = TRUE),
              location = first(location)) %>%
    group_by(patient) %>%
    summarize(num_uncontrolled_postop = n(),
              uncontrolled_location_postop = first(location),
              uncontrolled_duration_postop = sum(uncontrol_duration))

data_bps <- vitals %>%
    left_join(data_tidy[c("patient", "surgery_stop")], by = "patient") %>%
    filter(vital == "BPS") %>%
    arrange(patient, vital_datetime) %>%
    group_by(patient) %>%
    mutate(pain_duration = as.numeric(difftime(vital_datetime, first(vital_datetime), units = "hours"))) %>%
    summarize(bps_num = n(),
              bps_auc = auc(pain_duration, vital_result),
              bps_duration = last(pain_duration)) %>%
    mutate(bps_wt_avg_all = bps_auc / bps_duration)

data_bps_postop <- vitals %>%
    left_join(data_tidy[c("patient", "surgery_stop")], by = "patient") %>%
    filter(vital == "BPS",
           vital_datetime > surgery_stop,
           vital_datetime <= surgery_stop + hours(24)) %>%
    arrange(patient, vital_datetime) %>%
    group_by(patient) %>%
    mutate(pain_duration = as.numeric(difftime(vital_datetime, first(vital_datetime), units = "hours"))) %>%
    summarize(pain_auc = auc(pain_duration, vital_result),
              pain_duration = last(pain_duration)) %>%
    mutate(pain_wt_avg_postop_24h = pain_auc / pain_duration)

data_painmeds <- meds %>%
    left_join(data_tidy[c("patient", "surgery_stop")], by = "patient") %>%
    filter(category %in% c("APAP", "Opioid", "Combo product", "NSAID"),
           admin_datetime >= surgery_stop,
           admin_datetime <= surgery_stop + hours(24)) %>%
    group_by(patient) %>%
    summarize(num_painmeds_postop_24h = n())

data_antinv_postop <- meds %>%
    left_join(data_tidy[c("patient", "surgery_stop")], by = "patient") %>%
    filter(med %in% c("ondansetron", "promethazine"),
           admin_datetime >= surgery_stop,
           admin_datetime <= surgery_stop + hours(24)) %>%
    group_by(patient) %>%
    summarize(num_anti_emetics_postop_24h = n())

data_antinv_intraop <- meds %>%
    left_join(data_tidy[c("patient", "surgery_start", "surgery_stop")], by = "patient") %>%
    filter(med %in% c("ondansetron", "promethazine"),
           admin_datetime >= surgery_start,
           admin_datetime <= surgery_stop) %>%
    group_by(patient) %>%
    summarize(num_anti_emetics_intraop = n())

data_painmeds_nv <- meds %>%
    filter(category %in% c("APAP", "Opioid", "Combo product", "NSAID")) %>%
    dmap_at("before_nv", ~ .x == "Nausea") %>%
    dmap_at("before_nv", ~ coalesce(.x, FALSE)) %>%
    group_by(med) %>%
    summarize(total = n(),
              nausea = sum(before_nv),
              nausea_prct = sum(before_nv) / n())

data_painmeds_pacu <- meds %>%
    filter(category %in% c("APAP", "Opioid", "Combo product", "NSAID"),
           location == "PACU") %>%
    group_by(med) %>%
    summarize(painmed_pacu = n())

data_score <- vitals %>%
    left_join(data_tidy[c("patient", "surgery_stop")], by = "patient") %>%
    filter(vital == "BPS",
           vital_datetime > surgery_stop,
           vital_datetime <= surgery_stop + hours(24)) %>%
    arrange(patient, vital_datetime) %>%
    select(patient, datetime = vital_datetime, event = vital, result = vital_result)

data_med <- meds %>%
    left_join(data_tidy[c("patient", "surgery_stop")], by = "patient") %>%
    filter(category %in% c("APAP", "Opioid", "Combo product", "NSAID"),
           admin_datetime > surgery_stop,
           admin_datetime <= surgery_stop + hours(24)) %>%
    arrange(patient, admin_datetime) %>%
    select(patient, datetime = admin_datetime, event = med, result = NA_integer_)

data_meds_scores <- bind_rows(data_score, data_med) %>%
    arrange(patient, datetime) %>%
    mutate(med = if_else(result >= 8, lead(event), if_else(result <= 3, lead(event), NA_character_))) %>%
    dmap_at("med", str_replace_all, pattern = "BPS", replacement = "none")

data_med_pain_high <- data_meds_scores %>%
    filter(result >= 8) %>%
    group_by(med) %>%
    summarize(num = n())

data_med_pain_low <- data_meds_scores %>%
    filter(result <= 3) %>%
    group_by(med) %>%
    summarize(num = n())

data_tidy <- data_tidy %>%
    left_join(data_bps_postop, by = "patient") %>%
    left_join(data_bps, by = "patient") %>%
    left_join(data_uncontrolled_postop_sum, by = "patient") %>%
    dmap_at("num_uncontrolled_postop", ~ coalesce(.x, 0L)) %>%
    mutate(home_opioid_uncontrolled_postop = if_else(home_opioid, num_uncontrolled_postop > 0, NA),
           uncontrolled_postop_prct = uncontrolled_duration_postop / pain_duration) %>%
    left_join(data_painmeds, by = "patient") %>%
    left_join(data_antinv_intraop, by = "patient") %>%
    left_join(data_antinv_postop, by = "patient") %>%
    mutate(anti_emetic_intraop_postop = if_else(!is.na(num_anti_emetics_intraop), !is.na(num_anti_emetics_postop_24h), NA))

write_csv(data_tidy, "data/tidy/main_analysis.csv")
write_csv(data_painmeds_nv, "data/tidy/pain_meds_nausea.csv")
write_csv(data_med_pain_high, "data/tidy/meds_high_pain.csv")
write_csv(data_med_pain_low, "data/tidy/meds_low_pain.csv")
