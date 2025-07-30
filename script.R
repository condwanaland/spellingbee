library(words)
library(dplyr)
library(stringr)

key_letter <- "c"
extra_letters <- c("e", "g", "o", "a", "t", "n")

data("words") 

allowed <- unique(c(key_letter, extra_letters))

forbid_rx <- paste0("[^", paste0(allowed, collapse=""), "]")

wordlist <- words %>%
  filter(word_length >= 4,         
         str_detect(word, key_letter),
         !str_detect(word, forbid_rx)) |> 
  arrange(desc(word_length))
