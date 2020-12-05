library(tidyverse)

get_row_lines <- function(input_file) {
  lines <- readLines(input_file)
  prev_line = ""
  rows <- c()
  for(line in lines) {
    if(line == "") {
      rows = c(rows, trimws(prev_line))
      prev_line = ""
    }
    prev_line = paste(prev_line, line, sep = " ")
  }
  rows = c(rows, trimws(prev_line))
  rows
}

get_df <- function(rows) {
  df <- data.frame(row = rows)
  df$rowid <- seq(1:nrow(df))
  df
}

# sol1
get_df(get_row_lines("day4_input.txt")) %>%
  separate(row, into = as.character(seq(1:8)), sep = " " ) %>%
  pivot_longer(cols = -rowid, names_to = "var", values_to = "keyval") %>%
  select(-var) %>%
  separate(keyval, into = c("key", "val"), sep = ":") %>%
  filter(!is.na(key)) %>%
  group_by(rowid, key) %>%
  count() %>%
  spread(key, n) %>%
  mutate(total = sum(byr, ecl, eyr, hcl, hgt, iyr, pid, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(total == 7) %>%
  summarise(n = n())

# sol2
get_df(get_row_lines("day4_input.txt")) %>%
  separate(row, into = as.character(seq(1:8)), sep = " " ) %>%
  pivot_longer(cols = -rowid, names_to = "var", values_to = "keyval") %>%
  select(-var) %>%
  separate(keyval, into = c("key", "val"), sep = ":") %>%
  filter(!is.na(key)) %>%
  pivot_wider(names_from = "key", values_from = "val") %>%
  mutate(byr = as.integer(byr),
         iyr = as.integer(iyr),
         eyp = as.integer(eyr)) %>%
  mutate(hgt_unit = gsub("[0-9]*([cm|in]*)","\\1", hgt),
         hgt_val = gsub("([0-9]*)([cm|in]*)","\\1", hgt)) %>%
  filter(between(byr, 1920, 2002),
         between(iyr, 2010, 2020),
         between(eyr, 2020, 2030),
         (between(hgt_val, 150, 193) & hgt_unit == "cm") | (between(hgt_val, 59, 76) & hgt_unit == "in"),
         grepl("#[0-9a-f]{6}", hcl),
         ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth"),
         grepl("^[0-9]{9}$", pid)) %>%
  count()
