library(data.table)
library(ggplot2)

source("get_maze.R")
source("update_maze.R")
source("maze_to_path.R")

get_set <- function(size) {
  set <- get_maze(size)
  set <- update_maze(set)
  set <- maze_to_path(set)
  
  set[, ":="(x = x - (size + .5),
             y = y - (size + .5))]
  
  set <- set[order(order), ]
  set[, ':=' (xend = shift(x, type = "lead", fill = 1 - (size + .5) ),
              yend = shift(y, type = "lead", fill = 1 - (size + .5) ))]
}

# Get color scheme by finding a random circle in HCL color space
get_colors <- function() {
  
  # two random vectors
  v1 <- runif(3, -1, 1)
  v2 <- runif(3, -1, 1)
  # orthogonal
  v2 <- v2 - c(v1 %*% v2 / v1 %*% v1 ) * v1
  # unit
  v1 <- v1 / sqrt(c(v1 %*% v1))
  v2 <- v2 / sqrt(c(v2 %*% v2))
  
  # random point
  p <- runif(3, -30, 30) + c(0, 0, 50)
  
  # random radius
  r <- runif(1, 10, 30)
  
  # ten points around the circle
  # need to keep both end points even though they're the same value
  # for scale_color_gradientn to loop back around
  t <- seq(0, 2 * pi, length.out = 11)
  
  colors <- data.table(p1 = p[1],
                       p2 = p[2],
                       p3 = p[3],
                       r = r,
                       t = t,
                       v11 = v1[1],
                       v12 = v1[2],
                       v13 = v1[3],
                       v21 = v2[1],
                       v22 = v2[2],
                       v23 = v2[3])
  
  colors[, ':=' (x = p1 + r * cos(t) * v11 + r * sin(t) * v21,
                 y = p2 + r * cos(t) * v12 + r * sin(t) * v22,
                 z = p3 + r * cos(t) * v13 + r * sin(t) * v23)]
  colors[, ":=" (H = (atan2(y, x) * 180/pi) %% 360,
                 C = sqrt(x^2 + y^2),
                 L = z)]
  
  
  colors[, ':=' (hex_value = ifelse(L < 0 | L > 100, NA_character_, 
                                    hcl(H, C, L, fixup = FALSE))), 
         by = seq_len(nrow(colors))]

  return(colors)
}

# Sometimes the circle will be out of range,
# so try again until all the colors are valid
get_color_scheme <- function() {
  colors <- get_colors()

  while(any(is.na(colors$hex_value))) {
    colors <- get_colors()
  }
  
  return(colors)
}

# Create 10 outputs
set.seed(10101010)
for(i in 1:10) {
  size <- 10
  set <- get_set(size)
  
  colors <- get_color_scheme()
  
  # Save center of the circle
  color_center <- hcl(h = (atan2(colors$p2[1], colors$p1[1]) * 180/pi) %% 360,
                      c = sqrt(colors$p1[1]^2 + colors$p2[1]^2),
                      l = colors$p3[1])
  
  color_scheme <- colors[["hex_value"]]
  
  # Get boundaries of white background square
  boundaries <- data.table(x = c(min(set$x), max(set$x), max(set$x), min(set$x)),
                           y = c(min(set$y), min(set$y), max(set$y), max(set$y)))
  boundaries[, ':=' (x = x * 1.1,
                     y = y * 1.1)]
  
  # Set up connections size
  # order goes from 1 to 400
  # 400 / waves = num segments per wave
  waves <- round(runif(1, 70, 130))
  offset <- runif(1, 0, 2*pi)
  set[, size := order / max(order) * 2*pi]
  set[, size := (sin(waves * size + offset) + 1) * 2.5 ]
  
  ggplot() +
    geom_polygon(data = boundaries,
                 aes(x, y),
                 color = "white",
                 fill = "white") +
    geom_segment(data = set,
                 aes(x, y,
                     xend = xend, yend = yend, 
                     color = order, size = size),
                 alpha = .5,
                 lineend = "round") +
    geom_segment(data = set,
                 aes(x, y,
                     xend = xend, yend = yend, 
                     color = order, size = size / 5),
                 alpha = 1,
                 lineend = "round") +
    geom_point(data = set,
               aes(x, y), 
               color = color_center,
               size = .5) +
    scale_color_gradientn(colours = color_scheme) +
    theme_void() +
    theme(legend.position = "none") +
    coord_equal()
  ggsave(paste0("output/output_", i, ".jpeg"), height = 5, width = 5, bg = color_center)
}
