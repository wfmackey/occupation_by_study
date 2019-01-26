# A function to rename space-laden ABS variables


rename_abs <- function(data) {
  
  new_names <- 
    names(data) %>% 
    tibble(names = .) %>% 
    mutate(new_names = 
             case_when(
               names == "Counting"                                ~ "person_place",
               names == "CITP Australian Citizenship"             ~ "citizen",
               names == "HSCP Highest Year of School Completed"   ~ "highschool",
               names == "STUP Full-Time/Part-Time Student Status" ~ "student_status",
               names == "SEXP Sex"                                ~ "sex",
               names == "QALLP - 1 Digit Level"                   ~ "qual",
               names == "QALLP - 2 Digit Level"                   ~ "qual",
               names == "QALLP - 3 Digit Level"                   ~ "qual",
               names == "QALFP - 2 Digit Level"                   ~ "foe",
               names == "QALFP - 4 Digit Level"                   ~ "foe",
               names == "QALFP - 6 Digit Level"                   ~ "foe",
               names == "OCCP - 1 Digit Level"                    ~ "occ",
               names == "OCCP - 2 Digit Level"                    ~ "occ",
               names == "OCCP - 3 Digit Level"                    ~ "occ",
               names == "OCCP - 4 Digit Level"                    ~ "occ",
               names == "INCP Total Personal Income (weekly)"     ~ "income",
               names == "AGEP Age"                                ~ "age",
               names == "Count"                                   ~ "n",
               grepl("(X|V)[0-9]", names(data))                      ~ "empty")
    ) %>% 
    pull(new_names)
  
  if (sum(is.na(new_names)) > 0) {
    warning("There are undefined variables names, which will show up as empty varnames! Check your variable names and add to `rename_abs`.")
  }
  
  message("Your new variable names are:")
  print(new_names)
  new_names
  
  
}
