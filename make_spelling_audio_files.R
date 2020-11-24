install.packages('googleLanguageR')
library(googleLanguageR)
gl_talk_languages()

library(tidyverse)
library(glue)
tl <- gl_talk_languages()
en <- tl %>% filter(languageCodes == 'en-US')


## for a given list of words I would like to make audio files in several voices of the word and of the word being spelled

words <- c("secret", "pond", "shake", "proof", "gorp", "bingo", "film", 
           "soda", "seal", "admit", "brass", "jolly", "cabin", "broth", 
           "ash", "flame", "grits", "cliff", "hem", "brim", "plot", "desk", 
           "bobcat", "polo", "drum", "giant", "mouth", "cone", "never", 
           "silly", "drool", "sound", "still", "pie", "size", "hall", "chain", 
           "finish", "chips", "scoop", "shoo", "beam", "crew", "crate", 
           "stray", "Monday", "grub", "hook", "neon", "mix")


rootdir <- '~/spelling_words'

word <- words[1]
gl_talk(word, name = en$name[1], output = file.path(rootdir, glue('{word}.wav')))
voice <- 
spelled_word <- word %>% str_split('') %>% flatten_chr() %>% glue_collapse(' ')
gl_talk(spelled_word, name = en$name[1], output = file.path(rootdir, glue('{word}_spelled.wav')))



make_spell_files <- function(word, name = 'en-US-Wavenet-A') {
  spelled_word <- word %>% str_split('') %>% flatten_chr() %>% glue_collapse(' ')
  gl_talk(word, name = en$name[1], output = file.path(rootdir, glue('{word}_{name}.wav')))
  gl_talk(spelled_word, name = en$name[1], output = file.path(rootdir, glue('{word}_{name}_spelled.wav')))
  invisible(NULL)
}


walk(words, make_spell_files)





