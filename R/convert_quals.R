# A function to convert 2- and 4-digit FOE (text) to custom foe_census codes


simpleQualLevel <- c(
  "Postgrad",
  "Bachelor",
  "Diploma/Advanced",
  "Cert III/IV",
  "Cert I/IV",
  "Not applicable"
)

convert_quals <- function(x) {
  factor(
  case_when(
    x == "Postgraduate Degree Level, nfd"                       ~ "Postgrad",
    x == "Doctoral Degree Level"                                ~ "Postgrad",
    x == "Master Degree Level"                                  ~ "Postgrad",
    x == "Graduate Diploma and Graduate Certificate Level, nfd" ~ "Postgrad",
    x == "Graduate Diploma Level"                               ~ "Postgrad",
    x == "Graduate Certificate Level"                           ~ "Postgrad",
    x == "Bachelor Degree Level"                                ~ "Bachelor",
    x == "Advanced Diploma and Diploma Level, nfd"              ~ "Diploma",
    x == "Advanced Diploma and Associate Degree Level"          ~ "Diploma",
    x == "Diploma Level"                                        ~ "Diploma",
    x == "Certificate Level, nfd"                               ~ "Not applicable",
    x == "Certificate III & IV Level"                           ~ "Cert III/IV",
    x == "Certificate I & II Level"                             ~ "Cert I/II",
    x == "Level of education inadequately described"            ~ "Not applicable",
    x == "Level of education not stated"                        ~ "Not applicable",
    x == "Not applicable"                                       ~ "Not applicable",
    TRUE                                                        ~ ""
    ),
  levels = simpleQualLevel)
}
