library(tidyverse)

get_next_range <- function(front_back, range) {
  if(front_back %in% c("F","L")) {
    return(c(range[1],range[1]+(range[2]-range[1]-1)/2))
  }
  if(front_back %in% c("B","R")) {
    return(c(range[1]+(range[2]-range[1]+1)/2, range[2]))
  }
}

get_row <- function(seat) {
  range <- c(0,127)
  for(seat_char in str_split(seat, "")[[1]]) {
    range <- get_next_range(seat_char, range)
  }
  return(range[1])
}

get_column <- function(seat) {
  range <- c(0,7)
  for(seat_char in str_split(seat, "")[[1]]) {
    range <- get_next_range(seat_char, range)
  }
  return(range[1])
}

get_seat_id <- function(seat) {
  return(get_row(substr(seat,0,7))*8 + get_column(substr(seat,8,11)))
}

assertthat::are_equal(get_seat_id("BBFFBBFRLL"), 820)
assertthat::are_equal(get_seat_id("FFFBBBFRRR"), 119)
assertthat::are_equal(get_seat_id("BFFFBBFRRR"), 567)
assertthat::are_equal(get_next_range("F",c(0,127)), c(0,63))
assertthat::are_equal(get_next_range("F",c(44,45)), c(44,44))
assertthat::are_equal(get_row("FBFBBFF"), 44)
assertthat::are_equal(get_column("RLR"), 5)

boarding_passes <- read.csv("day5_input.txt", header = FALSE)

boarding_passes <- boarding_passes %>%
  rowwise() %>%
  mutate(seat_id = get_seat_id(V1))

# sol1
max(boarding_passes$seat_id)

# sol2
setdiff(seq(min(boarding_passes$seat_id), max(boarding_passes$seat_id)), boarding_passes$seat_id)
