
# Create title function

get_title <- function(gender,
                      qual,
                      foe,
                      occ_level,
                      topn,
                      compareGender = FALSE,
                      compareQual = FALSE) {
  

  if (gender == "Compare genders")        compareGender = TRUE
  if (qual   == "Compare qualifications") compareQual   = TRUE
  
  # Title of the chart and topn
  if (!compareGender & !compareQual) {
    title = paste0("Occupations of ", tolower(gender), " workers with a ", qual, 
                   " qualification in ", foe)
  }
  
  if ( compareGender &  compareQual) {
    title = paste0("Occupations of women and men with a ", foe, " qualification")
  }
  
  if ( compareGender & !compareQual) {
    title = paste0("Occupations of women and men with a ", qual, 
                   " qualification in ", foe)  
  }
  
  if (!compareGender &  compareQual) {
    title = paste0("Occupations of ", tolower(gender), 
                   "s with a qualification in ", foe)  
  }
  
  title
  
}

