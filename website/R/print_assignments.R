print_readings <- function(reading_df){
  reading_list <- c()
  
  reading_keys <- reading_df %>% 
    select(reading, chapter) %>%
    filter(!is.na(reading)) %>%
    mutate(chapter = ifelse(is.na(chapter), "", paste(chapter, ",")))
  
  if(nrow(reading_keys > 0)) {
    reading_list <- sapply(reading_keys$reading, function(x) {
      
      paste0(
        capture.output(print(bib[x], .opts = list(style = "markdown"))), 
        collapse = " ")
    })
    
    cat(paste("", fa(name ="book"), "   ", reading_keys$chapter, reading_list), sep = '  \n')
  }
  else{
    cat(paste("", fa(name ="book"), "   ", "No Readings"))
    cat("\n")
  }
}

print_writing_assignments <- function(assignment_df){
  assignment_names <- assignment_df %>% 
    select(writing_assignment) %>% 
    drop_na() %>%
    pull()
  
  if(length(assignment_names > 0)) {
    cat("\n")
    cat(paste("", fa(name ="file-alt"), "   ", assignment_names), sep = "  \n")
  }
}

print_project_assignments <- function(assignment_df){
  assignment_names <- assignment_df %>% 
    select(project_assignment) %>% 
    drop_na() %>%
    pull()
  
  if(length(assignment_names > 0)) {
    cat("\n")
    cat(paste("", fa(name ="user-friends"), "   ", assignment_names), sep = "  \n")
  }
}

print_announcements <- function(announcement_df){
  announcement_names <- announcement_df %>% 
    select(announcement) %>% 
    drop_na() %>%
    pull()
  
  if(length(announcement_names > 0)) {
    cat("\n")
    cat(paste("", fa(name ="bullhorn"), "   ", announcement_names), sep = "  \n")
  }
}

print_due_today <- function(topic){
  this_week_required_reading <- required_reading %>% filter(topic_abbr == topic) 
  this_week_project_assignments <- project_assignments %>% filter(topic_abbr == topic)
  this_week_writing_assignments <- writing_assignments %>% filter(topic_abbr == topic)
  print_readings(this_week_required_reading)
  print_project_assignments(this_week_project_assignments)
  print_writing_assignments(this_week_writing_assignments)
}

print_further_reading <- function(topic){
  this_week_optional_reading <- optional_reading %>% filter(topic_abbr == topic) 
  print_readings(this_week_optional_reading)
}

print_today_announcements <- function(topic){
  this_week_announcement <- announcements %>% filter(topic_abbr == topic) 
  print_announcements(this_week_announcement)
}
