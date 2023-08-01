library(dplyr)
library(ggplot2)

iterate <- function(df) {
  n = nrow(df)
  
  theta =  2 * pi * 1/(df$id/max(df$id)) * runif(1, -1, 1)
  
  x <- df$x
  y <- df$y
  
  df$x <- x * cos(theta) - y * sin(theta)
  df$y <- x * sin(theta) + y * cos(theta)
  
  
  return(df)
}


generate_data <- function(iterate_function, n_iteration) {
  n = 500
  
  df <- (
    expand.grid(angle = 2 * pi * 1/(1:n))
    %>% mutate(x = cos(angle), y = sin(angle))
    %>% mutate(id = 1:n(), iteration = 0)
    )
  
  for (i in 1:n_iteration) {
    df <- bind_rows(
      df,
      df %>% filter(iteration == max(iteration)) %>% mutate(lag_x = x, lag_y = y) %>% iterate_function() %>% mutate(iteration = iteration + 1)
      )
    }
  
  # df <- df %>% filter(iteration > 10)
  return(df)
}


for (i in 1:10) {
  for (n in 1:10) {
    graphe <- (
      ggplot(generate_data(iterate, 2*n))
      + aes(
        y = lag_y,
        x = lag_x,
        yend = y,
        xend = x,
        group = id,
      )
      + geom_segment(size = .02, alpha = .95)
      + theme_void()
      + theme(legend.position = "none")
    )

    ggsave(
      graphe,
      filename = paste0("output/", i, "_", n, ".svg"),
      width = 10,
      height = 10
    )
  }  
}


