

## spelling bee app

library(tidyverse)
library(kbutils)
library(purrr)
library(glue)
library(lubridate)


# 
# words <- c("secret", "pond", "shake", "proof", "gorp", "bingo", "film", 
#            "soda", "seal", "admit", "brass", "jolly", "cabin", "broth", 
#            "ash", "flame", "grits", "cliff", "hem", "brim", "plot", "desk", 
#            "bobcat", "polo", "drum", "giant", "mouth", "cone", "never", 
#            "silly", "drool", "sound", "still", "pie", "size", "hall", "chain", 
#            "finish", "chips", "scoop", "shoo", "beam", "crew", "crate", 
#            "stray", "Monday", "grub", "hook", "neon", "mix")



words <- 'career athlete sighed duo either else afraid billow bison squid rallies clatter firefly launch mention pinpoint countless glazed drowsy window whimper eddy cautioned imposing blissfully cocoa lairs schedule grateful snorkels dappled whales retreat haggis indecipherable expression patent ventured strait laboratory convulsive detergent cladding garish president copious outlandish ineffective trough corporate' %>% str_split(' ') %>% flatten_chr()


# words <- 'forever motor couch chapter squeak movie double dream child piper awe sweat bottle plopped sidekick little carefully
# freeze talking nibble chess friend known watch whine space hear afar cost smart darted stuff felt would
# scorch
# money strands slimy laptop suffer ahoy beans princess sprint bowl sleek stared angry least summer'  %>% str_split(' ') %>% flatten_chr()
# 


walk(words, ~system2('say', .))

system2('say', 'hello')

#answer = readline(prompt = 'Spell the word')


say_letters <- function(word) {
  letters <- str_split(word, '') %>% flatten_chr()
  system2('say', glue('The word {word} is spelled') %>% as.character())
  walk(letters, ~system2('say', .))
}



test_word <- function(word) {
  answer <- ''
  while (answer == '') {
    system2('say', word)
    answer <- readline('Please spell the word you hear: ')
    
  }
  if (answer == word) {
    system2('say', sample(c('nailed it!', 'correct!', 'you are right!', 'yes!!!', 'cha-ching!', 'woohoo!'), 1))
    return('correct')
  } else {
    system2('say', 'That is incorrect.')
    message(word)
    say_letters(word)
    return('incorrect')
  }
  
  
}





#test_word('cabin')

spelling_bee_data_path <- '~/spelling_bee_data'
play_game <- function(words, player) {
  
  player_file <- glue::glue('{spelling_bee_data_path}/player_data/{tolower(player)}_data.rds')
  if (!file.exists(player_file)) {
    message(glue::glue('Hello {player}! This must be your first time here! Welcome!'))
    player_dat <- list()
    player_dat['name'] = player
    word_dat <- data.frame(word = c(), correct = c(), incorrect = c(), seen = c())
    
  } else {
    player_dat <- readRDS(player_file)
    word_dat <- player_dat[['word_data']]
  }
  
  results <- c()
  total_words <- length(words)
  message(glue::glue('There are {total_words} in this test. Good luck!'))
  for (word in words) {
    result <- test_word(word)
    results[word] <- result
    
  }
  
  ## update word data and save the results
  new_results <- results %>% enframe() %>% 
    set_names('word', 'result')

  
  if (nrow(word_dat) == 0) {
    # case for a brand new player
    word_dat <- new_results %>% mutate(correct = as.integer(result == 'correct'),
                                       incorrect = as.integer(results == 'incorrect'),
                                       seen = correct + incorrect)
  } else {
    # player with preexisting data
  word_dat <- word_dat %>% full_join(new_results, by = 'word') %>% 
    mutate(correct = replace_na(correct, 0),
           incorrect = replace_na(incorrect, 0)
           ) %>% 
    mutate(correct = case_when(is.na(result) ~ correct,
                               result == 'incorrect' ~ correct,
                               result == 'correct' ~ correct + 1L),
           incorrect = case_when(is.na(result) ~ incorrect,
                                 result == 'correct' ~ incorrect,
                                 result == 'incorrect' ~ incorrect + 1L),
           seen = correct + incorrect)
  }
  
  player_dat[['word_data']] <- word_dat %>% select(word, correct, incorrect, seen)
  
  saveRDS(player_dat, player_file)
  
  return(list(results = results, word_dat = word_dat))
  

}



results <- play_game(words = c('animal', 'horse', 'fine'), player = 'kevin')
results$word_dat
walk(words, test_word)


show_player_results <- function(player) {
  
}




pd <- read_rds('~/spelling_bee_data/player_data/kevin_data.rds')
pd
pd
pd <- pd %>% set_names('name', 'word_data')
pd
saveRDS(pd, '~/spelling_bee_data/player_data/kevin_data.rds')
