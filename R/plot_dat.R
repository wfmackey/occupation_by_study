
# Plot dat function

plot_dat <- function(gender,
                     qual,
                     foe,
                     occ_level,
                     topn = 20,
                     compareGender = FALSE,
                     compareQual = FALSE,
                     getData = TRUE,
                     useTitle = TRUE) {
  
  if (getData) dat <- read_rds("data/dat.rds")
  source("R/get_title.R")
  
  if (gender == "Compare genders")        compareGender = TRUE
  if (qual   == "Compare qualifications") compareQual   = TRUE
  
  title <- get_title(gender, qual, foe, occ_level, topn, compareGender, compareQual)
  if (!useTitle) title <- ""
  
  maxLabelSize = 18
  if (compareGender & compareQual) maxLabelSize = 12
  
  occLabelSize <- maxLabelSize/(topn^(1/5))
  
  if (!compareGender) dat <- filter(dat, sex == gender)
  if (!compareQual)   dat <- filter(dat, qualsimple == qual)
  
  if ( compareGender &  compareQual) dat <- group_by(dat, sex, qualsimple)
  if ( compareGender & !compareQual) dat <- group_by(dat, sex)
  if (!compareGender &  compareQual) dat <- group_by(dat, qualsimple)
  
  # Build data
  dat <- 
    dat %>% 
    filter(foe_census == foe,
           level_occ == occ_level) %>% 
    # Generate top 15 order
    arrange(-pc) %>% 
    mutate(one = 1,
           rank = cumsum(one),
           top = rank <= topn)
  
  # Group to arrange percentages
  if (!compareGender & !compareQual) dat <- group_by(dat, top)
  if ( compareGender &  compareQual) dat <- group_by(dat, top, sex, qualsimple)
  if ( compareGender & !compareQual) dat <- group_by(dat, top, sex)
  if (!compareGender &  compareQual) dat <- group_by(dat, top, qualsimple)
  
  # Edit data and start building plot
  p <-
    dat %>% 
    mutate(pc_sum = sum(pc),
           pc =  if_else(rank == topn + 1, pc_sum, pc),
           occ = if_else(rank == topn + 1, "Other occupations", occ)) %>% 
    filter(rank <= topn + 1) %>% 
    # filter(occ != "Other occupations") %>%
    
    ggplot(aes(x = reorder(occ, -rank),
               y = pc,
               alpha = (if_else(occ != "Other occupations", 1, 0.5))))
  
  if (!compareGender & !compareQual) p <- p + geom_bar(fill = grattan_lightorange, stat = "identity")
  if ( compareGender &  compareQual) p <- p + geom_bar(aes(fill = qualsimple, linetype = sex), stat = "identity")
  if ( compareGender & !compareQual) p <- p + geom_bar(aes(fill = sex), stat = "identity")
  if (!compareGender &  compareQual) p <- p + geom_bar(aes(fill = qualsimple), stat = "identity")
  
  p <- p + 
    geom_hline(yintercept = 0) +
    scale_y_continuous(expand = c(0, 0), position = "left") +
    scale_x_discrete(position = "bottom") +
    scale_alpha_continuous(range = c(0.5, 1)) +
    coord_flip() +
    theme_minimal() +
    theme(plot.title = element_text(),
          axis.title = element_text(size = 18, hjust = 1),
          axis.text.x =  element_text(size = 18),
          axis.text.y = element_text(size = occLabelSize),
          strip.text = element_text(size = 18),
          panel.spacing = unit(1, "lines"),
          legend.position = "off") +
    labs(title = title,
         x = "",
         y = "Per cent of workers")
  
  
  if (!compareGender & !compareQual) p
  if ( compareGender &  compareQual) p <- p + facet_grid(sex ~ qualsimple)
  if ( compareGender & !compareQual) p <- p + facet_grid(. ~ sex)
  if (!compareGender &  compareQual) p <- p + facet_grid(.   ~ qualsimple)
  
  
  p
  
}



