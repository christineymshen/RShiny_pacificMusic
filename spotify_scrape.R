# libraries
library(rvest)
library(jsonlite)
library(httr)
library(magrittr)
library(tidyverse)
library(dplyr)
library(plyr)
library(stringr)
library(data.table)
library(purrr)
library(httr)
library(assertthat)

#######################

# get access token
# code taken from https://rayheberer.ai/archive/spotifyapi/
get_token <- function(){
  clientID = "3a9bb802ae9d464a8eccacbfa65e7dd8"
  secret = "75abf174c2f44280b067403e885dff5f"
  
  response = POST(
    'https://accounts.spotify.com/api/token',
    accept_json(),
    authenticate(clientID, secret),
    body = list(grant_type = 'client_credentials'),
    encode = 'form'
  )
  
  token = content(response)$access_token
  authorization.header = paste0("Bearer ", token)
}

authorization.header = get_token()

#######################

# get categories
cats <- GET(url = "https://api.spotify.com/v1/browse/categories?country=US&limit=50",
            config = add_headers(authorization = authorization.header))
cats <- content(cats)

# clean categories
cats <- map(cats$categories$items, function(x){
  c(x$id, x$name)
})
cats <- as.data.frame( matrix(unlist(cats), ncol = 2, byrow = T ) )
colnames(cats) <- c("category_id", "category")

# select categories(remove Latin?? too few songs)
select_categories <- c("Pop", "Hip-Hop", "Country", "Rock", "Latin", "Jazz", "Dance/Electronic", "R&B",
                       "Indie", "Folk & Acoustic", "Party", "Chill", "Classical", "Soul", "Metal", "Reggae",
                       "Blues", "Punk", "Funk", "Anime", "Christian", "Romance", "K-Pop", "Arab", "Desi", "Afro")
#select_categories <- c("Rock", "Jazz")
select_cats <- cats %>%
  filter(category %in% select_categories)

#######################

authorization.header = get_token()

# get playlist for each selected category
playlists_by_cats <- map(select_cats$category_id, function(cat){
  content(GET(url = sprintf("https://api.spotify.com/v1/browse/categories/%s/playlists?country=US&limit=50", cat),
              config = add_headers(authorization = authorization.header)))
})

# clean playlists(list of dataframes, playlists for each category)
plays <- map(playlists_by_cats, function(pc){
  pls <- map(pc$playlists$items, function(x){
    c(x$name, x$href, x$id)
  })
  pls <- as.data.frame( matrix(unlist(pls), ncol = 3, byrow = T ) , stringsAsFactors = F)
  colnames(pls) <- c("name", "href", "id")
  return(pls)
})

#######################

authorization.header = get_token()

# get tracks from the playlists
tracks_all <- map(plays, function(play){
  trks <- map(play$id, function(pid){
    content(GET(url = sprintf("https://api.spotify.com/v1/playlists/%s/tracks?market=US&fields=items(track(name%%2Cid%%2Cpopularity))", pid), 
                config = add_headers(authorization = authorization.header)))
  })
})

# cleaning 
tracks_all <- map(tracks_all, function(trk){
  t1 <- map(trk, function(t){
    rt1 <- map(t$items, function(tf){
      l <- tf$track
      l <- c(l$name, l$id, l$popularity)
    })
  })
  t1 <- as.data.frame( matrix(unlist(t1), ncol = 3, byrow = T ) , stringsAsFactors = F)
  colnames(t1) <- c("name", "id", "popularity")
  return(t1)
})

tracks_all <- map(tracks_all, distinct)   # remove duplicate rows
tracks_all <- map(1:length(select_cats$category), function(i){
  tracks_all[[i]] %>%
    mutate(category = select_cats$category[i])
})

tracks_all <- data.table::rbindlist(tracks_all)   # join the dataframes
tracks_all$popularity <- as.numeric(tracks_all$popularity)    # convert popularity to numeric

#######################

# traverse 100 at a time
inds <- seq(from = 1, to = nrow(tracks_all), by = 100)
inds <- c(inds,nrow(tracks_all)+1)

# get track features(returns which tracks didn't have features as indices)
trk_feats <- map(1:(length(inds)-1), function(i){
  j <- inds[i]:(inds[i+1]-1)
  ids <- tracks_all$id[j]
  ids <- paste(ids, collapse = ",")
  trk_f <- content(GET(url = sprintf("https://api.spotify.com/v1/audio-features/?ids=%s", ids), 
                       config = add_headers(authorization = get_token())))
  trk_f <- compact(trk_f$audio_features)
  nms <- names(trk_f[[1]])
  trk_f <- as.data.frame( matrix(unlist(trk_f), ncol = length(trk_f[[1]]), byrow = T ) , stringsAsFactors = F)
  colnames(trk_f) <- nms
  
  # check missing observations(remove later)
  mis <- j[!(tracks_all$id[j] %in% trk_f$id)]
  
  # return
  return( list(trk_f,mis) )
} )

# split the list
misses <- unlist(map(trk_feats, function(t) t[[2]]))
trks <- map(trk_feats, function(t) t[[1]])
trk_feats <- data.table::rbindlist(trks)   # join the dataframes

# remove missing rows from tracks_all
tracks_all <- tracks_all[-misses,]

# merge data, as the order is the same, no need to merge by checking columns
tracks_all <- cbind(tracks_all, trk_feats %>% select(-id))
tracks_all <- as.data.frame(tracks_all)

#######################

# convert to numeric data
numeric_nms <- c("popularity", "danceability", "energy", "loudness", "speechiness", "acousticness",
                 "instrumentalness", "liveness", "valence", "tempo", "duration_ms")
tracks_all[,numeric_nms] <- map(tracks_all[,numeric_nms], as.numeric)

# clean, keep adding lines as needed
# tempo > 0
tracks_all <- tracks_all %>%
  filter(tempo > 0)

# new vars
tracks_all <- tracks_all %>%
  mutate(tempo_lvl = cut(tempo, breaks = c(0,80,120,160, max(tempo)), labels = c("1", "2", "3", "4")))

#######################

save(tracks_all, file="data/tracks.Rdata")





