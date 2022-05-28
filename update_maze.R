update_maze <- function(edges) {
  
  # list out all possible edges
  # (basically same code as setting up the maze)
  # plus adds edges that stick out on the outside
  size <- max(edges$x1) + 1
  all_possible_edges <- CJ(
    x1 = rep(seq(1, size), 2) - 1,
    y1 = seq(1, size) - 1
  )
  all_possible_edges[, ":="(x2 = ifelse(.I %% 2 == 0, x1 + 1, x1),
                            y2 = ifelse(.I %% 2 == 1, y1 + 1, y1))]
  all_possible_edges <- all_possible_edges[(x1 != 0 | x2 != 0) &
                                             (y1 != 0 | y2 != 0), ]
  
  edges <- edges[, .(x1, x2, y1, y2, id)]
  
  # merge maze and all possible edges to see which ones weren't used
  all_possible_edges <- merge(all_possible_edges, edges,
                              by = c("x1", "y1", "x2", "y2"),
                              all.x = TRUE
  )
  
  # This function subs in the new edges appropriately
  # basically, any path edge needs to be updated to two edges so the maze
  # starts at the bottom middle, travels through the maze, and back to the start
  create_new_edges <- function(x1, y1, x2, y2, id) {
    # if no edges, add block
    if (is.na(id)) {
      if (y1 == y2) { # horizontal edge
        list(
          x1_1 = 2 * x1,
          y1_1 = 2 * y1 - 1,
          x2_1 = 2 * x1,
          y2_1 = 2 * y1,
          x1_2 = 2 * x2 - 1,
          y1_2 = 2 * y2 - 1,
          x2_2 = 2 * x2 - 1,
          y2_2 = 2 * y2
        )
      } else { # vertical edge
        list(
          x1_1 = 2 * x1 - 1,
          y1_1 = 2 * y1,
          x2_1 = 2 * x1,
          y2_1 = 2 * y1,
          x1_2 = 2 * x2 - 1,
          y1_2 = 2 * y2 - 1,
          x2_2 = 2 * x2,
          y2_2 = 2 * y2 - 1
        )
      }
    } else { # has edge, add connections
      if (y1 == y2) { # horizontal edge
        list(
          x1_1 = 2 * x1,
          y1_1 = 2 * y1 - 1,
          x2_1 = 2 * x2 - 1,
          y2_1 = 2 * y2 - 1,
          x1_2 = 2 * x1,
          y1_2 = 2 * y1,
          x2_2 = 2 * x2 - 1,
          y2_2 = 2 * y2
        )
      } else { # vertical edge
        list(
          x1_1 = 2 * x1 - 1,
          y1_1 = 2 * y1,
          x2_1 = 2 * x2 - 1,
          y2_1 = 2 * y2 - 1,
          x1_2 = 2 * x1,
          y1_2 = 2 * y1,
          x2_2 = 2 * x2,
          y2_2 = 2 * y2 - 1
        )
      }
    }
  }
  
  # fill in blocks and paths
  all_possible_edges[, c(
    "x1_1", "y1_1", "x2_1", "y2_1",
    "x1_2", "y1_2", "x2_2", "y2_2"
  ) := create_new_edges(x1, y1, x2, y2, id),
  by = seq_len(nrow(all_possible_edges))
  ]
  
  # clean everything up
  all_possible_edges[, ":="(x1 = NULL,
                            y1 = NULL,
                            x2 = NULL,
                            y2 = NULL,
                            id = NULL)]
  
  all_possible_edges <- melt(all_possible_edges,
                             measure.vars = patterns("x1", "y1", "x2", "y2"),
                             value.name = c("x1", "y1", "x2", "y2")
  )[, variable := NULL]
  
  all_possible_edges <- all_possible_edges[(x1 > 0 &
                                              y1 > 0 &
                                              x2 < (2 * size - 1) &
                                              y2 < (2 * size - 1)), ]
}
