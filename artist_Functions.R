# currently in use
library(tidyverse)
library(httr)
library(assertthat)

token <- ""
token_time <- NULL

clientID <- "1633de1c011d40c6aab5ff5ade327489"
secret <- "53b461f05f5b45968cccfd1ce15c1681"
base_url <- "https://api.spotify.com/v1/"
max_limit <- 50
max_album <- 20 # to get full album info
max_track <- 100 # to get features
num_hist <- 7 # number of histograms to show

get_names <- function(df){
  df %>% 
    sapply(attributes) %>% 
    unlist() %>% 
    unique
}

get_token <- function(){
  
  is_refresh <- F
  
  if (is.null(token_time) | token == "") {
    is_refresh <- T
  } else if (difftime(Sys.time(), token_time, units = "secs") >= 3590){
    is_refresh <- T
  }
  
  if (is_refresh){
    response <- POST(
      'https://accounts.spotify.com/api/token',
      accept_json(),
      authenticate(clientID, secret),
      body = list(grant_type = 'client_credentials'),
      encode = 'form',
      verbose()
    )
    
    token_time <<- Sys.time()
    token <<- content(response)$access_token
  }
}

remove_duplicates <- function(df){
  
  assert_that(is.data.frame(df))
  
  a <- df %>% 
    mutate(name_lower = tolower(name)) %>%
    add_count(name_lower) %>%
    group_by(name_lower) %>%
    dplyr::mutate(rank = rank(-popularity)) %>%
    ungroup() %>%
    filter(rank == 1) %>%
    select(-c(name_lower, rank, n))
}

get_artist <- function(name){
  
  
  assert_that(is.string(name))
  
  get_token()
  url <- str_c(base_url, "search")
  query <- list(q = name, type = "artist", access_token = token, limit = max_limit)
  
  res <- GET(url = url, query = query) %>% content %>% .$artists
  
  if (is.null(res)) {
    return(NULL)
  } else{
    
    num_artists <- length(res$items)
    
    if (num_artists == 0){
      return(NULL)
    } else{
      
      res <- res$items
      names <- get_names(res)
      
      res %>%
        purrr::flatten() %>%
        matrix(., nrow = min(num_artists, max_limit), byrow = T) %>%
        data.frame() %>%
        `colnames<-`(names) %>%
        mutate(external_urls = map(external_urls, "spotify")) %>%
        mutate(followers = map(followers, "total")) %>%
        mutate(images = map(images, 1)) %>%
        mutate(images = map(images, "url")) %>%
        # mutate(images = ifelse(images == "NULL", default_image, images)) %>%
        mutate(genres = map(genres, as.character)) %>%
        mutate_all(as.character) %>%
        mutate_at(c("followers", "popularity"), as.numeric) %>%
        remove_duplicates() %>%
        select(-c(type, uri, href))
      
      # checking
      # data.frame(sapply(a,typeof))    
    }
  }
}

get_related_artists <- function(artist_id){
  
  assert_that(is.string(artist_id))
  
  get_token()
  url <- str_c(base_url, "artists/", artist_id, "/related-artists")
  query <- list(access_token = token)
  
  GET(url = url, query = query) %>% content %>% .$artists %>%
    map(`[[`, "name") %>% unlist()
  
}

get_album <- function(artist_id, offset = ""){
  
  assert_that(is.string(artist_id),
              offset == "" | is.count(offset))
  
  get_token()
  url <- str_c(base_url, "artists/", artist_id, "/albums")
  query <- list(access_token = token, limit = max_limit, 
                offset = ifelse(offset == "", 0, offset), album_type = "album")
  
  res <- GET(url = url, query = query) %>% content
  
  if (offset != ""){
    
    # return album ids for page 2 onwards
    res <- res$items
    names <- get_names(res)
    
    return(get_album_ids(res, names))
    
  } else {
    
    num_albums <- res$total
    
    if (num_albums == 0) {
      
      # if no album, return NULL
      return(NULL)
      
    } else {
      
      # get album ids for page 1
      res <- res$items
      names <- get_names(res)
      album_ids <- get_album_ids(res, names)
      
      # get album ids for other pages if needed
      num_page <- num_albums %/% max_limit + 1
      
      if (num_page > 1){
        for (i in 2:num_page){
          album_ids <- c(album_ids, get_album(artist_id, offset = (i - 1) * max_limit))
        }        
      }
      
      # extract full album info
      res <- NULL
      for (i in 1:(num_albums %/% max_album + 1)){
        
        album_ids_i <- paste(album_ids[((i - 1) * max_album + 1):(min(i * max_album, num_albums))], 
                             collapse = ",")
        res <- c(res, get_multi_albums(album_ids_i)$albums)
      }
      
      names <- get_names(res)
      res <- res %>%
        purrr::flatten() %>%
        matrix(., nrow = num_albums, byrow = T) %>%
        data.frame() %>%
        `colnames<-`(names) %>%
        mutate(external_urls = map(external_urls, "spotify")) %>%
        mutate(images = map(images, 1)) %>%
        mutate(images = map(images, "url")) %>%
        mutate(genres = map(genres, as.character)) %>%
        mutate_at(vars(-c(tracks)), as.character) %>%
        mutate_at(c("total_tracks", "popularity"), as.numeric) %>%
        remove_duplicates() %>%
        select(-c(album_type, artists, available_markets, copyrights, external_ids, 
                  href, label, release_date_precision, type, uri))
      
      return(res)
      
      # checking
      # data.frame(sapply(a,typeof)) 
    }
  }
}

get_album_ids <- function(df, names){
  
  assert_that(is.character(names),
              !is.null(df))
  
  num <- length(df)
  
  df %>%
    purrr::flatten() %>%
    matrix(., nrow = num, byrow = T) %>%
    data.frame() %>%
    `colnames<-`(names) %>%
    select(id) %>% unlist()
}

get_multi_albums <- function(album_ids){
  
  assert_that(is.string(album_ids))
  
  get_token()
  url <- str_c(base_url, "albums")
  query <- list(access_token = token, ids = album_ids)
  
  GET(url = url, query = query) %>% content
}

get_track <- function(df, album_name){
  
  assert_that(is.string(album_name),
              is.data.frame(df))
  
  album_num <- which(df$name == album_name)
  res <- df$tracks[album_num][[1]]
  num_tracks <- res$total
  # assert_that(res$total <= max_limit)
  
  if (num_tracks == 0) {
    return(NULL)
  } else {
    res <- res$items
    names <- get_names(res)
    
    res <- res %>%
      purrr::flatten() %>%
      matrix(., nrow = min(max_limit,num_tracks), byrow = T) %>%
      data.frame() %>%
      `colnames<-`(names) %>%
      mutate(external_urls = map(external_urls, "spotify")) %>%
      mutate_all(as.character) %>%
      mutate_at(c("duration_ms", "track_number"), as.numeric) %>%
      select(-c(artists, available_markets, disc_number, explicit, href, is_local,
                type, uri))
    return(res)
    
    # checking
    # data.frame(sapply(a,typeof)) 
  }
}

get_multi_features <- function(track_ids){
  
  assert_that(is.character(track_ids))
  
  num_tracks <- length(track_ids)
  url <- str_c(base_url, "audio-features")
  
  if (num_tracks == 0){
    return(NULL)
  } else {
    
    get_token()
    
    res <- NULL
    for (i in 1:(num_tracks %/% max_track + 1)){
      
      l_track <- (i - 1) * max_track + 1
      u_track <- min(num_tracks, i * max_track)
      
      track_ids_i <- paste(track_ids[l_track:u_track], collapse = ",")
      query <- list(access_token = token, ids = track_ids_i)
      
      res_i <- GET(url = url, query = query) %>% content %>% .$audio_features
      names <- get_names(res_i)
      num_features <- length(res_i)
      
      res_i <- res_i %>%
        purrr::flatten() %>%
        matrix(., nrow = num_features, byrow = T) %>%
        data.frame() %>%
        `colnames<-`(names) %>%
        select(-c(key, mode, type, id, uri, track_href, analysis_url, time_signature)) %>%
        mutate_all(as.numeric)
      
      res <- rbind(res, res_i)
      
      # checking
      # data.frame(sapply(a,typeof))       
      
    }
    return(res)
  }
}

get_all_tracks <- function(artist_name, offset = ""){
  
  assert_that(is.string(artist_name))
  
  get_token()
  url <- str_c(base_url, "search?")
  query <- list(access_token = token, q = str_c("artist:", artist_name), type = "track", limit = max_limit, offset = ifelse(offset == "", 0, offset))
  
  res <- GET(url = url, query = query) %>% content %>% .$tracks
  
  if (offset != ""){
    
    # return list for page 2 onwards
    return(res$items)
    
  } else {
    
    num_tracks <- res$total
    
    if (num_tracks == 0){
      return(NULL)
    } else {
      
      res <- res$items
      num_page <- num_tracks %/% max_limit + 1
      
      if (num_page > 1){
        for (i in 2:num_page){
          res <- c(res, get_all_tracks(artist_name, offset = (i - 1) * max_limit))
        }
      }
      
      names <- get_names(res)
      
      res <- res %>%
        purrr::flatten() %>%
        matrix(., nrow = num_tracks, byrow = T) %>%
        data.frame() %>%
        `colnames<-`(names) %>% 
        mutate(album = map(album, "name")) %>%
        mutate(external_urls = map(external_urls, "spotify")) %>%
        mutate_all(as.character) %>%
        mutate_at(c("duration_ms", "popularity"), as.numeric) %>%
        remove_duplicates() %>%
        dplyr::rename(Track = name, Album = album, Popularity = popularity) %>%
        select(-c(artists, available_markets, disc_number, explicit, external_ids,
                  href, is_local, preview_url, type, uri, track_number)) %>%
        select(Track, Album, Popularity, duration_ms, external_urls, id)
      
      return(res)
      
      # checking
      # data.frame(sapply(a,typeof))
    }
  }
}
