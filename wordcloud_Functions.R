library(tidyverse)
library(tidytext)

# set up
sjj_token <- "TvlJ1cbNUCTNGvwE2YRyiDTQ_UywG0hojfXhHJQR969xb2RfxhdLhmwLr4BaAo-d"
nrc_data <- readRDS("data/nrc.rds")
negative_words <- nrc_data %>%
  filter(sentiment %in% c("negative","fear","sadness","anger",
                          "disgust")) %>% 
  .$word

positive_words <- nrc_data %>%
  filter(sentiment %in% c("positive", "trust", "surprise",
                          "joy", "anticipation")) %>% 
  .$word

genius_get_artists <- function(artist_name, n_results = 10) {
  baseURL <- 'https://api.genius.com/search?q=' 
  requestURL <- paste0(baseURL, gsub(' ', '%20', artist_name),
                       '&per_page=', n_results,
                       '&access_token=', sjj_token)
  
  res <- GET(requestURL) %>% content %>% .$response %>% .$hits
  
  map_df(1:length(res), function(x) {
    tmp <- res[[x]]$result$primary_artist
    list(
      artist_id = tmp$id,
      artist_name = tmp$name
    )
  }) %>% unique
}

genius_artist_image <- function(artist_id) {
  baseurl <- "https://api.genius.com/artists/"
  requesturl <- paste0(baseurl, artist_id)
  
  imageurl <- GET(requesturl, query = list(access_token = sjj_token)) %>%
    content %>% .$response %>% .$artist %>% .$image_url
  
  return (imageurl)
}

lyric_scraper <- function(url) {
  read_html(url) %>% 
    html_node('.lyrics p') %>% 
    html_text
}

top10tracks <- function(artist_id) {
  baseURL <- 'https://api.genius.com/artists/' 
  requestURL <- paste0(baseURL, artist_id, '/songs')
  
  top10track <- GET(requestURL,
                    query = list(access_token = sjj_token, sort = "popularity",
                                 per_page = 10, page = 1)) %>%
    content %>% .$response %>% .$songs
  
  genius_df <- map_df(1:length(top10track), function(x) {
    lyrics <- try(lyric_scraper(top10track[[x]]$url))
    if (class(lyrics) != 'try-error') {
      lyrics <- gsub("\\[[^\\]]*\\]", "", lyrics, perl=TRUE) %>%
        str_replace_all("[\\.!?\\(\\)\\[\\],]|[:punct:]", "") %>%
        str_replace_all('\\n', ' ') %>%
        str_replace_all('([A-Z])', ' \\1') %>%
        str_replace_all(' {2,}', ' ')
      lyrics <- tolower(str_trim(lyrics))
    } else {
      lyrics <- NA
    }
    
    tots <- list(
      track_name = top10track[[x]]$title,
      lyrics = lyrics
    )
    
    return(tots)
  })
}

get_wordfreq <- function(tracks) {
  words <- unlist(unique(tracks$lyrics)) %>%
    paste0(collapse = " ") %>%
    strsplit(split = " ") %>% .[[1]]
  
  wordfreq <- data.frame(table(words)) %>%
    as_tibble() %>%
    filter(!words %in% tm::stopwords("english")) %>%
    filter(!words %in% c("im","youre","oh","ohh","ooh",
                         "na")) %>%
    arrange(-Freq) %>%
    top_frac(0.25)
  
  return (wordfreq)
}


