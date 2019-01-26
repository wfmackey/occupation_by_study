
# This script builds the dat.rds data file which is called by app.R
# It only needs to be run (once) when changes are required for the dat.rds file.


library(tidyverse)
library(data.table)
library(grattantheme)

# Rename ABS function
source("R/rename_abs.R")

# Load convert_foe_census function
source("R/convert_foe_census.R")

# Simplify qualifications
source("R/convert_quals.R")


# Qualification adjustments (after conver_foe_census)
# Grad dip/cert for law/psych counts as bachelor
adjust_quals <- function(simple, q, f) {
  case_when(
    f == "Law" &        q == "Graduate Diploma Level"      ~ "Bachelor",
    f == "Law" &        q == "Graduate Certificate Level"  ~ "Bachelor",
    f == "Psychology" & q == "Graduate Diploma Level"      ~ "Bachelor",
    f == "Psychology" & q == "Graduate Certificate Level"  ~ "Bachelor",
    TRUE ~ simple
  )
}


# Load abs function:
load_abs <- function(levelFOE, levelOCC, 
                     saveObject = FALSE,
                     toSkip = 10) {
  
  filename <- paste0("foe", levelFOE, "_occ", levelOCC)
  csv <- paste0("foe", levelFOE, "_occ", levelOCC, ".csv")
  
  unzip(paste0("data/", filename, ".zip"))

  data <- fread(csv, skip = toSkip) %>% as_tibble()

  # Clean up!
  file.remove(csv)
  
  names(data) <- rename_abs(data)
  
 if (sum(grepl("empty", names(data))) > 0) data <- select(data, -empty)
  
  data <- data %>%
          select(-person_place, -citizen, -age) %>% 
          filter(!is.na(n)) %>%  # remove empty rows at bottom
          mutate(foe_census = convert_foe_census(foe),
                 qualsimple = convert_quals(qual),
                 # qualsimple = adjust_quals(qualsimple, qual, foe_census),
                 level_foe = levelFOE,
                 level_occ = levelOCC)
  
  message("Your data looks like:")
  if (saveObject) {
    assign(filename, data, envir = .GlobalEnv)
    print(data)
  } 
  
  data
  
}


# Load and combine files by FOE census
all <- bind_rows(
         load_abs(2, 2),
         load_abs(2, 3),
         load_abs(2, 4),
         load_abs(4, 2),
         load_abs(4, 3),
         load_abs(4, 4)
       ) %>% 
      filter(occ != "Not stated",
             occ != "Not applicable",
             qualsimple != "Not applicable",
             !is.na(qualsimple),
             sex != "Total")

dat <- all %>% 
  group_by(sex, qualsimple, foe_census, level_occ, occ) %>% 
  summarise(n = sum(n)) %>% 
  mutate(pc = 100 * n / sum(n))
  

# Loading done
# Save .Rdata file
saveRDS(dat, "data/dat")

