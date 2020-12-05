library(tidyverse)

get_grid <- function(input_file) {
  map <- read.delim(input_file, sep = "", header = FALSE)
  map_length <- str_length(map$V1[1])
  grid <- map %>%
    separate(V1, into=as.character(seq(1,map_length+1)), sep="") %>%
    select(-1)

  names(grid) <- as.character(seq(1,map_length))
  return(grid)
}

count_trees <- function(grid, column, start_row, increment_right, increment_down) {
  locations = c()
  start_process = FALSE
  for(row in seq(1,nrow(grid),by=increment_down)) {
    if(row == start_row) {
      start_process = TRUE
    }
    if(start_process) {
      column = column + increment_right
      if(column > ncol(grid)) {
        column = column - ncol(grid)
      }
      locations = c(locations, as.character(grid[row,][column]))
      # print(glue::glue("{row} x {column} = {as.character(grid[row,][column])}"))
    }
  }

  c = data.frame(loc = locations) %>%
    filter(loc == "#") %>%
    count()
  c$n[1]
}


count_trees(get_grid("day3_input_test.txt"), 1, 2, 1, 1)
count_trees(get_grid("day3_input_test.txt"), 1, 2, 3, 1)
count_trees(get_grid("day3_input_test.txt"), 1, 2, 5, 1)
count_trees(get_grid("day3_input_test.txt"), 1, 2, 7, 1)
count_trees(get_grid("day3_input_test.txt"), 1, 3, 1, 2)

grid <- get_grid("day3_input.txt")

# sol1
count_trees(grid, 1, 2, 3, 1)

# sol2
prod(
  count_trees(grid, 1, 2, 1, 1),
  count_trees(grid, 1, 2, 3, 1),
  count_trees(grid, 1, 2, 5, 1),
  count_trees(grid, 1, 2, 7, 1),
  count_trees(grid, 1, 3, 1, 2))
