##bar chart of treatments given for a chosen cancer type

bar_treatment <- function(data, selected_type) {
  summary <- data %>% filter(Cancer_Type == selected_type) %>% count(Treatment_Type)
  
  ggplot(summary, aes(x = reorder(Treatment_Type, n)), y = n, fill = Treatment_Type) + 
    geom_col() +
    coord_flip() + 
    labs(
      title = "Treatments for ", 
      x = "Treatment")
          
}