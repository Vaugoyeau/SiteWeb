######### Script pour faire mon logo avec la méthode Antonio S. Chinchón’s traveling salesperson #####

library(imager)
library(dplyr)
library(ggplot2)
library(scales)
library(TSP)

# Selon la fonction faite par Evan Tachovsky

tspDraw <- function(raw_img, point_sample_size, line_color, back_color) {
  
  # load the image and get started
  # more background on how imager works here https://dahtah.github.io/imager/imager.html
  raw_img <- load.image(raw_img)
  
  # get the sample points
  data <- raw_img %>%
    grayscale() %>%
    as.cimg() %>%
    as.data.frame() %>%
    # adjust the point_sample_size variable to adjust the texture of the tsp image
    # smaller sample looks jagged, large sample looks smooth
    sample_n(size = point_sample_size, weight = (1 - value)) %>%
    select(x, y)
  
  # solve the tsp problem and return a data.frame with the values
  solution <- as.TSP(dist(data)) %>% 
    solve_TSP(method = "arbitrary_insertion") %>%
    as.integer()
  
  order <- data.frame(id = solution) %>%
    mutate(order = row_number())
  
  # join the tsp solution 
  data_to_plot <- data %>%
    mutate(id = row_number()) %>%
    inner_join(order, by = "id") %>%
    arrange(order) %>%
    select(x, y)
  
  
  p <- ggplot(data_to_plot, aes(x, y)) +
    geom_path(color = line_color) +
    scale_y_continuous(trans=reverse_trans())+
    coord_fixed() +
    theme_void() +
    theme(plot.background = element_rect(fill = back_color))
  
  
  return(p)
  
}

## test avec mon logo
marie <- tspDraw(raw_img = "C:/Users/Nous Deux/Pictures/MesSeb.jpg",
                 point_sample_size = 4000,
                 line_color = c(1:4000), 
                 back_color = "white")
plot(marie)

ggsave("logo.png", marie)
