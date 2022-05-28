get_maze <- function(size) {
  
  # Sets up base data set of potential edges for the maze
  # size is always 5 for right now
  edges <- CJ(
    x1 = rep(seq(1, size), 2),
    y1 = seq(1, size)
  )
  edges[, ":="(x2 = ifelse(.I %% 2 == 0, x1 + 1, x1),
               y2 = ifelse(.I %% 2 == 1, y1 + 1, y1))]
  edges <- edges[x2 <= size & y2 <= size, ]
  edges[, id := seq(1, nrow(edges))]
  edges[, ":="(node1 = (x1 - 1) * size + (y1 - 1) + 1,
               node2 = (x2 - 1) * size + (y2 - 1) + 1)]
  setkey(edges, id)
  
  # data set of nodes
  nodes <- unique(rbind(edges[, .(id = node1)], edges[, .(id = node2)]))
  nodes[, connected := 0]
  setkey(nodes, id)
  
  # data set of node id to edge ids
  nodes_edges <- unique(rbind(edges[, .(id = node1, edge = id)][], 
                              edges[, .(id = node2, edge = id)]))
  setkey(nodes_edges, id)
  
  # location : 1 for maze, 0 for frontier, -1 for uncharted, -2 for discarded
  # starting point : bottom middle
  # include bottom middle then either off to the sides or up
  starting_edge <- edges[(x1 == 3 & y1 == 1) |
                           (x1 == 2 & y1 == 1 & x2 == 3 & y2 == 1), ][sample(.N, 1), ]
  
  # Set up base columns
  edges[, ":="(location = -1,
               probability = 0)]
  edges[.(starting_edge$id), ":="(location = 1,
                                  probability = 0)]
  nodes[.(c(starting_edge$node1, starting_edge$node2)), connected := 1]
  edges[.(nodes_edges[.(c(starting_edge$node1, starting_edge$node2)), "edge"]), ":="
        (location = fifelse(location == -1, 0, location),
          probability = fifelse(location == -1, 1, probability))]
  
  #### Loop through maze generation ----
  num_edges <- 1
  while (num_edges < (size^2 - 1)) {
    
    # select next edge
    selected_edge <- edges[sample(.N, 1, prob = probability), ]
    
    ## if it's good, then
    # add it to the maze
    # add connecting edges to the frontier
    # else add it to discard
    if (any(nodes[.(c(selected_edge$node1, selected_edge$node2))
                  , connected] == 0)) {
      
      # add to maze
      edges[.(selected_edge$id), ":="(location = 1,
                                      probability = 0)]
      
      # update nodes
      nodes[.(c(selected_edge$node1, selected_edge$node2)), connected := 1]
      
      # update frontier
      edges[.(nodes_edges[.(c(selected_edge$node1, selected_edge$node2))
                          , "edge"]), ":="
            (location = fifelse(location == -1, 0, location),
              probability = fifelse(location == -1, 1, probability))]
      
      num_edges <- num_edges + 1
    } else {
      # drop from frontier
      edges[.(selected_edge$id), ":="(location = -2,
                                      probability = 0)]
    }
  }
  
  return(edges[location == 1, ])
}
