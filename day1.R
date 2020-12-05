library(tidyverse)
expenses <- read.csv("day1_input.txt", header = FALSE)
names(expenses) <- "val"

# sol1
purrr::walk(expenses$val, function(val1) {
  val = val1 + expenses$val
  if(2020 %in% val) {
    # print(val)
    val2 <- expenses[match(2020,val),]
    print(glue::glue("{val1} + {val2} = {val1 + val2}"))
    print(glue::glue("{val1} * {val2} = {val1 * val2}"))
    break
  }
})

# sol2
purrr::walk(expenses$val, function(val1) {
  purrr::walk(expenses$val, function(val2) {
    val = val1 + val2 + expenses$val
    if(2020 %in% val) {
      val3 <- expenses[match(2020,val),]
      print(glue::glue("{val1} + {val2} + {val3} = {val1 + val2 + val3}"))
      print(glue::glue("{val1} * {val2} * {val3} = {val1 * val2 * val3}"))
      break
    }
  })
})

