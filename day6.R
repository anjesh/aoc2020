library(tidyverse)

get_grouped_answers <- function(input_file) {
  con <- file(input_file, "r")
  group_df <- data.frame(answer = character(0), people = double(0))
  line_count <- 0
  group <- ""
  while(TRUE) {
    line = readLines(con, n = 1)
    if(length(line) ==0) {
      break
    }
    line_count = line_count + 1
    group <- paste(group, line, sep="")
    if(trimws(line) == "") {
      group_df <- add_row(group_df, answer = group, people = line_count-1)
      group <- ""
      line_count <- 0
    }
  }
  close(con)
  group_df <- add_row(group_df, answer = group, people = line_count)
  group_df
}

# df <- get_grouped_answers("day6_input_test.txt")
df <- get_grouped_answers("day6_input.txt")

# soln1
df %>%
  mutate(unique_answers = map_dbl(answer, function(g) {
    return(length((unique(strsplit(g,"")[[1]]))))
  })) %>%
  summarise(sum(unique_answers))

# soln2
df %>%
  mutate(sn = seq(1:nrow(df))) %>%
  mutate(char = strsplit(answer,"")) %>%
  unnest(char) %>%
  select(sn, people, char) %>%
  group_by(sn, char, people) %>%
  count() %>%
  filter(people == n) %>%
  ungroup() %>%
  summarise(n())
