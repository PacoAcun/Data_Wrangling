library(readr)
library(tidyverse)
library(stringr)
library(tidytext)
library(lubridate)
library(stopwords)
library(wordcloud)
library(highcharter)

df <- read_csv("Health_and_Personal_Care.csv")
df %>% head()

meta <- read_csv("Health_and_Personal_Care_metadata.csv")
meta %>% head()

## Limpiando el texto - vamos a remover el " de cada elemento.
df$text <- str_replace_all(df$text, pattern = '\\"', replacement = '')
df %>% head()


## Extract
str_extract(
  string = df$text[1:5]
  ,pattern = "pain"
)

str_extract_all(
  string = df$text[1:5]
  ,pattern = "pain"
)
### puedo acceder a los distintos elementos dentro de esta lista.


## Detect
str_detect(
  string = df$text[1:5]
  ,pattern = "pain"
)

## Locate
str_locate(
  string = df$text[1:5]
  ,pattern = "pain"
)

str_locate_all(
  string = df$text[1:5]
  ,pattern = "pain"
)

## remove
df$text[2]
str_remove(
  string = df$text[2]
  ,pattern = "Beats carrying home a big old jug of bleach.Walmark has been out of them since pademic hit and was so glad to find hear."
)

## replace
df$text[2]
str_replace(
  string = df$text[2]
  ,pattern = "Beats carrying home a big old jug of bleach.Walmark has been out of them since pademic hit and was so glad to find hear."
  ,replacement = "REPLACEMENT!"
)

## Split
df$text[2]
str_split(
  string = df$text[2]
  ,pattern = " "
)

## Boundaries - tokenization
df$text[2]
vec_words <- str_split(df$text[2], boundary("word"))
vec_words
### la diferencia es que "word" no contiene el ultimo "hear." con el punto. Dado que no es una palabra.



## 0. Cuántos productos contienen reviews con palabras positivas? - a partir de 2023

p_words <- c(
  "love"
  #,"recommend"
  #,"like"
  #,"work"
  #,"works"
  #,"enjoy"
  #,"impressed"
)

# expresion regular
p_words_regex <- paste(p_words, collapse = '|')
str_detect(
  string = df$text
  ,pattern = p_words_regex
) %>% 
  table()


df_2 <- df %>% 
  mutate(
    p_flag = str_detect(text, pattern = p_words_regex)
    ,review_dt = as_datetime(timestamp/1000) %>% date()
  ) %>% 
  filter(review_dt >= '2023-01-01')

df_2 %>% 
  filter(
    p_flag = TRUE
  ) %>% 
  group_by(review_dt) %>% 
  summarise(n = n_distinct(parent_id)) %>% 
  arrange(review_dt) %>%
  hchart(
    type = "line"
    ,hcaes(x = review_dt, y = n)
    ,name = 'ParentIDs'
    ,color = 'green'
  )




## EJERCICIO
## 1. Remover los stopwords de la columna de texto
## 2. Consolidar la columna de texto para una sola
## 3. Generar un wordcloud


stop_words %>% head()
stop_words %>% group_by(lexicon) %>% summarise(n())

stop_vec <- c(stopwords(language = "en"), stopwords(language = "es"))

vec_words <- str_split(df$text[1:50], boundary("word")) %>% unlist() # utilizando únicamente los primeros 50 reviews para evitar problemas con la memoria.

no_stopwords <- vec_words %>% 
  as_tibble() %>% 
  filter(!(value %in% stop_vec)) %>% 
  group_by(value) %>% 
  summarise(freq = n())

wordcloud(no_stopwords$value, no_stopwords$freq)

