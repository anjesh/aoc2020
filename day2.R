library(tidyverse)
passwords <- read.csv("day2_input.txt", sep = " ", header = FALSE)
names(passwords) <- c("range","char","pwd")

# sol1
passwords %>%
  separate(range, into = c("min","max"), sep ="-", convert = TRUE) %>%
  mutate(char = gsub(":","", char)) %>%
  mutate(count = str_count(pwd, char)) %>%
  mutate(correct_pwd = if_else(count >= min & count <= max , 1, 0)) %>%
  summarise(n = sum(correct_pwd))

# sol2
passwords %>%
  separate(range, into = c("min","max"), sep ="-", convert = TRUE) %>%
  mutate(char = gsub(":","", char)) %>%
  mutate(pos = str_locate_all(pwd, char)) %>%
  mutate(pos = map(pos, function(x) {as.data.frame(x)$start})) %>%
  mutate(min_found = map2_dbl(pos, min, function(x, min) { ifelse(min %in% x,return(1),return(0)) }),
         max_found = map2_dbl(pos, max, function(x, max) { ifelse(max %in% x,return(1),return(0)) })) %>%
  mutate(correct_pwd = if_else(min_found + max_found == 1, 1, 0)) %>%
  summarise(n = sum(correct_pwd))

