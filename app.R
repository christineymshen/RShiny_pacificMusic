library(plotly)
library(tidytext)
library(wordcloud2)
library(spotifyr)
library(tidyverse)
library(httr)
library(rvest)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyalert)
library(jsonlite)
library(DT)

source("wordcloud_Functions.R")
source("artist_Functions.R")
source("get_rec.R")
source("Cor_test_genre.R")
source("song_search_functions.R")

get_token()

ui <- dashboardPage(
  dashboardHeader(title = "Pacific Music App"),
  dashboardSidebar(
    
    useShinyjs(),
    useShinyalert(),
    
    tags$head(tags$style(HTML(".small-box {height: 70px;}"))),
    
    sidebarMenu(id = "sidebarmenu",

      menuItem("User Manual", tabName = "usermanual", icon = icon("book-open")),
      menuItem("Musician", tabName = "cs_artist", icon = icon("headphones")),
      conditionalPanel("input.sidebarmenu === 'cs_artist'",
                       
         # Input for Artist
         textInput("cs_artist_name", label = "Musician", value = "Enter musician name"),
         
         # Action button
         actionButton("cs_get_artist", label = "Search", icon = icon("search")),
         
         conditionalPanel("input.cs_get_artist > 0", 
                          # Select box Artist(s)
                          uiOutput("cs_select_artist"),
                          uiOutput("cs_artist_img"),
                          br(),
                          uiOutput("cs_artist_info"),
                          uiOutput("cs_related_artists")
                       )
      ),
      
      menuItem("Song Search", tabName = "vr_tracks", icon = icon("search")),
      conditionalPanel("input.sidebarmenu === 'vr_tracks'",
                       # Categories
                       selectizeInput(
                         'vr_cats', 'Choose Categories to search from!', choices = select_categories, multiple = TRUE
                       )
      ),
      
      menuItem("Lyrics", tabName = "sjj_lyrics", icon = icon("music")),
      conditionalPanel("input.sidebarmenu === 'sjj_lyrics'",
                       textInput("sjj_artist", label = "Search artist", value = "The Beatles"),
                       actionButton("sjj_search", label = "Search"),
                       conditionalPanel(condition = "input.sjj_search > 0",
                                        selectInput("sjj_select_artist", label = "Choose your artist", choices = c("The Beatles")),
                                        actionButton("sjj_plot", label = "Explore!")),
                       br(),
                       conditionalPanel(condition = "input.sjj_search > 0",
                                        htmlOutput("sjj_artist_image"))
      ),
      
      menuItem("Big Five Personality Test", tabName = "ym_Dashboard", icon = icon("user-secret"))

    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "usermanual",

              h1("Welcome to the Pacific Music App", align = "center"),
              HTML('<p>Pacific Music App provides music lovers unique gadgets to search for musicians and their musics, analyze lyrics and get personal music recommendations</p>'),
              
              h2("How to use the App", align = "center"),
              
              fluidRow(
                # Musician tab
                column(6,
                       h3("Musician", align = "center"),
                       p("Users can search for artists and their musics."),
                       tags$ol(
                         tags$li("Go to 'Musician' tab, input an musician name and click `Search`"),
                         tags$li("Select a musician if more than one are relevant"),
                         tags$li("Select from the list of albums"),
                         tags$li("Select from the list of tracks of the selected album"),
                         tags$li("We provide 30 seconds preview for some songs. If it's available, music preview panel will appear at the bottom")),
                       p("Users can search from all the tracks of the selected musician under `Tracks` section."),
                       HTML('<p>We also provide an analysis of the features of the selected album or tracks. Refer to <a href="https://developer.spotify.com/documentation/web-api/reference/tracks/get-several-audio-features/">here</a> for details of each metrics.</p>'),
                       
                ),
                
                # Song Search tab
                column(6,
                       h3("Song Search", align = "center"),
                       p("Users can search for songs by defining the features they are looking for. You can look for songs you can dance to, or songs with acoustic instruments, or happy feeling high energy songs, or a mix of everything! The app looks for songs which can best satisfy your needs."),
                       tags$ol(
                         tags$li("In the song search tab, choose your categories/genres you want to look for(default is everything)."),
                         tags$li("Select the song features you want."),
                         tags$li("The app displays 10 songs that it thinks are the best for you."),
                         tags$li("Clicking on the link will take you to the spotify page of the song. You need a spotify account to play it."))
                )),
              
              fluidRow(
                # Lyrics tab
                column(6,
                       h3("Lyrics", align = "center"),
                       p("Users can search for artists and their lyrics analysis."),
                       tags$ol(
                         tags$li("Go to 'Lyrics' tab, input an artist name and click `Search`"),
                         tags$li("Select your interested musician with the help of singer image"),
                         tags$li("Wordcloud tab shows the wordcloud and word count table for top 10 hits of the artist."),
                         tags$li("Sentiment analysis tab shows the word counting plot for positive and negative words."))
                ),
                
                # Personality test tab
                column(6,
                       h3("Big Five Personality Test", align = "center"),
                       p("Users can take the Big Five Personality Test and get recommended genres and songs based on the test results."),
                       tags$ol(
                         tags$li("Go to 'Big Five Personality Test' tab."),
                         tags$li("Choose age group first."),
                         tags$li("Answer all the questions. (Higher number means the stronger level of agreement.)"),
                         tags$li("Click the `SEE RESULTS` button."),
                         tags$li("We analyze the best match genres and then recommend songs based on the top three genres.")),
                       p('The Big Five Personality Test is by far the most scientifically validated and reliable psychological model to measure personality.'),
                       p("We use Spearman’s correlation data between music genres and personality traits over age groups (Ferwerda, 2017) to 
                                    find the best match genres and then use Spotify's API to get recommended songs.")
                ))
              
              ),
      
      tabItem(tabName = "cs_artist",
              
              fluidRow(id = "cs_fluidrow",
                 tabBox(id = "cs_albums_tracks", title = tagList(icon("guitar"), ""), 
                        width = 7,
                        tabPanel(#title = tagList(icon("compact-disc"), "Albums"), 
                          title = "Albums",
                          uiOutput("cs_select_album"),
                          uiOutput("cs_album_img"),
                          br(),
                          uiOutput("cs_album_info"),
                          uiOutput("cs_tracks"),
                          uiOutput("cs_track_info"),
                          uiOutput("cs_track_preview")),
                        tabPanel(#title = tagList(icon("book"), "Tracks"),
                          title = "Tracks",
                          DT::dataTableOutput("cs_tracks_table"))
                 ),
                 
                 box(id = "cs_box", width = 5,
                     lapply(1:num_hist, function(i) {
                       plotlyOutput(paste0('h', i), width = "100%", height = "150px")
                     })
                 )
              ) 

      ),
      
      tabItem(tabName = "sjj_lyrics",
              tabsetPanel(
                tabPanel("WORD CLOUD",
                         br(),
                         box(width = 8,
                             wordcloud2Output("sjj_plot")
                         ),
                         
                         box(width = 4,
                             DT::dataTableOutput("sjj_my")
                         )
                ),
                tabPanel("SENTIMENT ANALYSIS",
                         br(),
                         box(
                           width = 8,
                           plotOutput("sjj_barplot")
                         ),
                         
                         box(
                           width = 4,
                           DT::dataTableOutput("sjj_sent_table")
                         )
                )
              )
      ),
      
      tabItem(tabName = "ym_Dashboard",
              fluidRow( 
                box(
                  width = 3, solidHeader = TRUE,
                  div(
                    h5('Please select your age first: ',
                       style = "font-style:italic;text-align:center;font-size:120%;"),
                    radioButtons('ym_age', "Is in this age group", 
                                 choices = c('12-19','20-39','40-65'), 
                                 selected = '20-39'),
                    style = "text-align:center;"
                  ),
                  br(),
                  tabBox(
                    title = tagList(shiny::icon("question-circle"), "Questions"),
                    width = '300px',
                    tabPanel(
                      tags$head(
                        tags$style(
                          ".irs-bar {",
                          "  border-color: #3CB371;",
                          "  background-color: #3CB371;",
                          "  div-align:center;",
                          "}",
                          ".irs-bar-edge {",
                          "  border-color: #3CB371;",
                          "  background-color: #3CB371;",
                          "}",
                          ".irs-single {",
                          "  border-color: #3CB371;",
                          "  background-color: #3CB371;",
                          "}"
                        )
                      ),
                      
                      
                      title = 'Q01-11', icon = icon("check-circle"),
                      sliderInput('ym_q1', HTML("Is talkative"), 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q2', "Tends to find fault with others", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q3', "Does a thorough job", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q4', "Is depressed, blue", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q5', "Is original, comes up with new ideas", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q6', "Is reserved", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q7', "Is helpful and unselfish with others", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q8', "Can be somewhat careless", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q9', "Is relaxed, handles stress well", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q10', "Is curious about many different things", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q11', "Is full of energy", 
                                  1,5,3,width = '200px')
                    ),
                    tabPanel(
                      'Q12-22', icon = icon("check-circle"),
                      sliderInput('ym_q12', "Starts quarrels with others", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q13', "Is a reliable worker", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q14', "Can be tense", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q15', "Is ingenious, a deep thinker", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q16', "Generates a lot of enthusiasm", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q17', "Has a forgiving nature", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q18', "Tends to be disorganized", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q19', "Worries a lot", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q20', "Has an active imagination", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q21', "Tends to be quiet", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q22', "Is generally trusting", 
                                  1,5,3,width = '200px')       
                    ),
                    tabPanel(
                      'Q23-33', icon = icon("check-circle"),
                      sliderInput('ym_q23', "Tends to be lazy", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q24', "Is emotionally stable", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q25', "Is inventive", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q26', "Has an assertive personality", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q27', "Can be cold and aloof", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q28', "Perseveres until the task is finished", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q29', "Can be moody", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q30', "Values artistic, aesthetic experiences", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q31', "Is sometimes shy, inhibited", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q32', "Is considerate and kind to almost everyone", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q33', "Does things efficiently", 
                                  1,5,3,width = '200px')       
                    ),
                    tabPanel(
                      'Q34-44', icon = icon("check-circle"),
                      sliderInput('ym_q34', "Remains calm in tense situations", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q35', "Prefers work that is routine", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q36', "Is outgoing, sociable", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q37', "Is sometimes rude to others", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q38', "Makes plans and follows through with them", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q39', "Gets nervous easily", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q40', "Likes to reflect, play with ideas", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q41', "Has few artistic interests", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q42', "Likes to cooperate with others", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q43', "Is easily distracted", 
                                  1,5,3,width = '200px'),
                      sliderInput('ym_q44', "Is sophisticated in art, music, or literature", 
                                  1,5,3,width = '200px')       
                    )
                  ) 
                ),
                box(width = 9,
                    solidHeader = TRUE,
                    br(),
                    div(
                      actionButton(inputId = 'ym_click', 
                                   label = 'SEE RESULTS',
                                   icon = icon('spotify')),
                      style = "text-align:center;"),
                    br(),
                    box(
                      width = 15,
                      solidHeader = TRUE,
                      h4("Based on your test result, ", 
                         style = "font-style:italic;text-align:center;"),
                      h4('You may enjoy these three genres: ', 
                         style = "font-style:italic;text-align:center;"),
                      h3(textOutput("ym_genreshow"),
                         style = "font-style:italic;color:#3CB371;text-align:center;font-size:200%;")
                    ),
                    
                    box(
                      width = 15,
                      status = "primary",
                      h4("Recommendations based on genre: ", 
                         style = "font-style:italic;text-align:center;"),
                      h1(textOutput('ym_showrec1_title'),
                         style = "font-style:italic;color:#3CB371;text-align:center;font-size:200%;"),
                      dataTableOutput('ym_showrec1') 
                    ),
                    box(
                      width = 15,
                      status = "primary",
                      h4("Recommendations based on genre: ", 
                         style = "font-style:italic;text-align:center;"),
                      h1(textOutput('ym_showrec2_title'),
                         style = "font-style:italic;color:#3CB371;text-align:center;font-size:200%;"),
                      dataTableOutput('ym_showrec2') 
                    ),
                    box(
                      width = 15,
                      status = "primary",
                      h4("Recommendations based on genre: ", 
                         style = "font-style:italic;text-align:center;"),
                      h1(textOutput('ym_showrec3_title'),
                         style = "font-style:italic;color:#3CB371;text-align:center;font-size:200%;"),
                      dataTableOutput('ym_showrec3') 
                    ) 
                )
              )
      ),
      
      # display suggested tracks
      
      tabItem(tabName = "vr_tracks",
              fluidRow(
                box(width = 3,
                    h4("Select the song features you want!", style = "font-weight:bold;color:#3289b3;text-align:center;font-size:110%;"),
                    
                    # Dance checkbox
                    checkboxInput("vr_dance", label = tags$b("Are you looking for a song to dance to?"), value = FALSE),
                    # Energy checkbox and slider
                    checkboxInput("vr_energy", label = tags$b("Do you want a certain amount of energy?"), value = FALSE),
                    conditionalPanel("input.vr_energy > 0", 
                                     sliderInput("vr_energy_slid", label = "How much energy?", min = 0, max = 1, value = .5)
                    ),
                    # tempo levels
                    selectInput('vr_tempo', label = "How fast do you want the song to be?", 
                                choices = list("Any" = 0, "Slow" = 1, "Medium" = 2, "Fast" = 3, "Super-fast" = 4), 
                                selected = "Any", selectize = TRUE),
                    # instrumental
                    selectInput("vr_instrument", label = "Are you looking for Instrumental or vocal centric songs?", 
                                choices = list("No Preference" = 2, "Instrumental" = 1, "Vocal Centric" = 0),
                                selected = "No Preference", selectize = TRUE),
                    # live
                    selectInput("vr_live", label = "Are you looking for songs performend live?", 
                                choices = list("No Preference" = 2, "Yes" = 1, "No, studio only" = 0),
                                selected = "No Preference", selectize = TRUE),
                    # acoustic checkbox and slider
                    checkboxInput("vr_acoustic", label = tags$b("Do you want a certain amount of acousticness?"), value = FALSE),
                    conditionalPanel("input.vr_acoustic > 0", 
                                     sliderInput("vr_acoustic_slid", label = "How much acousticness?", min = 0, max = 1, value = .5)
                    ),
                    # valence checkbox and slider
                    checkboxInput("vr_valence", label = tags$b("Are you looking for happy or sad songs?"), value = FALSE),
                    conditionalPanel("input.vr_valence > 0", 
                                     sliderInput("vr_valence_slid", label = "Choose level of happiness(0 is sad, 1 is happy)", min = 0, max = 1, value = .5)
                    ),
                    # Action button
                    actionButton("vr_get_tracks", label = "Search", icon = icon("search"))
                ),
                box(width = 9,
                    h4("10 songs that you might like!", style = "font-weight:bold;color:#3289b3;text-align:center;font-size:110%;"),
                    DT::dataTableOutput("vr_tracks_df")
                ))
      )
    )
  )
)

server <- function(input, output, session) {
  
  genius_artists <- reactive({
    input$sjj_search
    
    isolate({
      genius_get_artists(input$sjj_artist)
    })
  })
  
  artist_id <- reactive({
    genius_artists() %>%
      filter(artist_name == input$sjj_select_artist) %>%
      select(artist_id) %>% as.numeric()
  })
  
  lyrics_df <- eventReactive(input$sjj_plot, {
    withProgress({
      setProgress(message = "Generating hit songs...")
      tracks = top10tracks(artist_id())$track_name
      cbind(1:length(tracks), tracks)
    })
  })
  
  sent_df <- eventReactive(input$sjj_plot, {
    withProgress({
      setProgress(message = "Generating sentiment data...")
      tracks_df <- top10tracks(artist_id())
      tracks_df %>% 
        unnest_tokens(word, lyrics) %>%
        anti_join(stop_words, by = 'word') %>%
        mutate(positive = word %in% positive_words,
               negative = word %in% negative_words) %>%
        group_by(track_name) %>% 
        dplyr::summarise(negative = sum(negative, na.rm = T),
                         positive = sum(positive, na.rm = T),
                         word_count = n()) %>%
        ungroup
    })
  })
  
  wordfreq <- eventReactive(input$sjj_plot, {
    withProgress({
      setProgress(message = "Generating wordcloud...")
      get_wordfreq(top10tracks(artist_id()))
    })
  })
  
  observe({
    artist <- input$sjj_artist
    
    updateSelectInput(session, "sjj_select_artist",
                      choices = genius_artists()$artist_name
    )
  })
  
  output$sjj_plot <- renderWordcloud2({
    wordcloud2(wordfreq(), color = "random-light", backgroundColor = "grey", fontFamily = "Courier")
  })
  
  output$sjj_sent_table <- renderDataTable({
    DT::datatable(sent_df()[, 1:3], selection = "single")
  })
  
  output$sjj_barplot <- renderPlot({
    sent_df() %>%
      gather(key = "sentiment", value = "count", negative:positive) %>%
      ggplot() +
      geom_bar(aes(x = track_name, y = count, fill = sentiment),
               stat = "identity") +
      labs(x = "Track", y = "Word Count") +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
  })
  
  output$sjj_my <- DT::renderDataTable({
    df <- lyrics_df()
    colnames(df) <- c("Index", "Track")
    DT::datatable(df, selection = "single")
  })
  
  output$sjj_artist_image <- renderText({
    src <- genius_artist_image(artist_id())
    c('<img src="',src,'"height="200">')
  })
  
  shinyjs::hide(id = "cs_fluidrow")
  
  # get artists data from user input
  cs_artists <- reactive({
    
    input$cs_get_artist
    
    isolate({
      get_artist(input$cs_artist_name)
    })
    
  })
  
  # get related artists when an artist is selected
  cs_related_artists <- reactive({
    
    input$cs_select_artist
    
    isolate({
      id <- cs_artists()$id[cs_artists()$name == input$cs_select_artist]
      req(id)
      get_related_artists(id)      
    })
  })
  
  # get albums when an artist is selected
  cs_albums <- reactive({
    
    input$cs_select_artist
    
    isolate({
      id <- cs_artists()$id[cs_artists()$name == input$cs_select_artist]
      req(id)
      get_album(id)      
      
    })
  })
  
  # get tracks when an album is selected
  cs_tracks <- reactive({
    
    input$cs_select_album
    
    isolate({
      req(input$cs_select_album, cs_albums())
      get_track(cs_albums(), input$cs_select_album)
    })
    
  })
  
  # get all tracks for "Tracks" tab
  cs_all_tracks <- reactive({
    
    req(input$cs_select_artist)
    get_all_tracks(input$cs_select_artist)
    
  })
  
  # get features
  cs_track_features <- reactive({
    
    input$cs_select_album
    
    isolate({
      
      req(input$cs_select_album, cs_tracks())
      get_multi_features(cs_tracks()$id)
    })
    
  })
  
  cs_all_track_features <- reactive({
    
    req(input$cs_select_artist, cs_all_tracks())
    get_multi_features(cs_all_tracks()$id)
    
  })
  
  # data table on "Tracks" tab
  output$cs_tracks_table <- DT::renderDataTable(DT::datatable({
    data <- cs_all_tracks()[,1:3]
  }))
  
  # after clicking search button
  observeEvent(input$cs_get_artist, {
    
    if (is.null(cs_artists())) {
      shinyalert("", "Cannot find any musicians. Please start a new search", type = "info")
    } else {
      
      # select artist
      output$cs_select_artist <- renderUI({
        selectInput("cs_select_artist", label = "Select a Musician", choices = cs_artists()$name)
      })
      
    }
  })
  
  # after selecting artist
  observeEvent(input$cs_select_artist, {
    
    shinyjs::show(id = "cs_fluidrow")    
    # artist image
    # already pre-processed all records without image to default image
    artist_img <- cs_artists()$images[cs_artists()$name == input$cs_select_artist]
    req(artist_img)
    if (artist_img != "NULL"){
      output$cs_artist_img <- renderUI({
        tags$img(src = artist_img, 
                 width = "80%", style = "display:block; margin-left:auto; margin-right:auto;")
      })        
    } else {
      output$cs_artist_img <- renderUI({
      })   
    }
    
    output$cs_artist_info <- renderValueBox({
      valueBox(value = tags$p(prettyNum(cs_artists()$followers[cs_artists()$name == input$cs_select_artist], big.mark = ","), style = "font-size: 60%;"),
               subtitle = tags$p("Followers", style = "font-size: 90%; "),  
               icon = tags$i(class="fa fa-heart fa-fw", style="font-size: 30px;"),
               color = "red"
      )      
    })
    
    # related artists
    output$cs_related_artists <- renderUI({
      selectInput("cs_related_artists", label = "Other Musicians You May Like", choices =
                    cs_related_artists(), multiple = T, selectize = F)
    })
    
    # select albums
    if (is.null(cs_albums())) {
      output$cs_select_album <- renderUI({
        h3("No available album information")
      })
      output$cs_album_img <- renderUI({})
      output$cs_album_info <- renderUI({})
      output$cs_tracks <- renderUI({})
      lapply(1:num_hist, function(i) {
        output[[paste0('h', i)]] <- renderPlotly({})
      })
      
    } else {
      output$cs_select_album <- renderUI({
        selectInput("cs_select_album", label = "Select an Album", choices = cs_albums()$name)
      })
      
    }
    
  })
  
  # after selecting album
  observeEvent(input$cs_select_album, {
    
    # album img
    album_img <- cs_albums()$images[cs_albums()$name == input$cs_select_album]
    req(album_img)
    if (album_img != "NULL"){
      output$cs_album_img <- renderUI({
        tags$img(src = album_img, 
                 width = "80%", style = "display:block; margin-left:auto; margin-right:auto;")
      })        
    } else{
      output$cs_album_img <- renderUI({
      })        
    }
    
    # album info
    output$cs_album_info <- renderValueBox({
      valueBox(value = tags$p(cs_albums()$popularity[cs_albums()$name == input$cs_select_album], style = "font-size: 60%;"),
               subtitle = tags$p("Album Popularity", style = "font-size: 90%; "),  
               icon = tags$i(class="fa fa-heart fa-fw", style="font-size: 30px;"),
               color = "red"
      )   
    })
    
    # tracks
    if (is.null(cs_tracks())) {
      output$cs_tracks <- renderUI({
        h3("No available tracks information")
      })
    } else {
      
      withProgress(message = "updating plots ...", value = 0,{
        req(cs_tracks(), input$cs_select_album)
        output$cs_tracks <- renderUI({
          selectInput("cs_select_track", label = str_c("Tracks from Album ", input$cs_select_album), 
                      choices = cs_tracks()$name, multiple = T, selectize = F)
        })
        
        # histograms, same codes as those below, not ideal
        shinyjs::show(id = "cs_box")
        req(cs_track_features())
        lapply(1:num_hist, function(i) {
          output[[paste0('h', i)]] <- renderPlotly({
            feature_name <- colnames(cs_track_features())[i]
            
            ggplot(cs_track_features(), aes_string(x = feature_name)) +
              geom_histogram(bins = 8, fill="#F08080", color="#e9ecef", alpha=0.9) + 
              labs(x = "", y = "", title = feature_name) +
              theme_bw()
            
          })
          incProgress(1/num_hist, detail = paste("loading", i))
        })        
      })
      
    }
  })
  
  # after selecting track
  observeEvent(input$cs_select_track, {
    
    # track info
    track_popularity <- cs_all_tracks()$Popularity[cs_all_tracks()$Track == input$cs_select_track]
    output$cs_track_info <- renderValueBox({
      valueBox(value = tags$p(cs_all_tracks()$Popularity[cs_all_tracks()$Track == input$cs_select_track], style = "font-size: 60%;"),
               subtitle = tags$p("Track Popularity", style = "font-size: 90%; "),  
               icon = tags$i(class="fa fa-heart fa-fw", style="font-size: 30px;"),
               color = "red"
      )   
    })
    
    # preview_url
    preview_url <- cs_tracks()$preview_url[cs_tracks()$name == input$cs_select_track]
    req(preview_url)
    if (preview_url != "NULL"){
      output$cs_track_preview <- renderUI({
        h2("Track Preview")
        tags$audio(src =cs_tracks()$preview_url[cs_tracks()$name == input$cs_select_track], type = "audio/mp3", controls = NA)
      })  
    } else {
      output$cs_track_preview <- renderUI({
      })  
    }
    
  })
  
  # after clicking between tabs
  observeEvent(input$cs_albums_tracks, {
    
    
    if (input$cs_albums_tracks == "Tracks"){
      
      withProgress(message = "updating plots ...", value = 0, {
        req(cs_all_track_features())
        lapply(1:num_hist, function(i) {
          output[[paste0('h', i)]] <- renderPlotly({
            feature_name <- colnames(cs_all_track_features())[i]
            
            ggplot(cs_all_track_features(), aes_string(x = feature_name)) +
              geom_histogram(bins = 8, fill="#F08080", color="#e9ecef", alpha=0.9) + 
              labs(x = "", y = "", title = feature_name)
            
          })
          incProgress(1/num_hist, detail = paste("loading", i))
        })        
        
      })
      
    } else if(input$cs_albums_tracks == "Albums"){
      
      
      if (is.null(cs_albums())) {
        lapply(1:num_hist, function(i) {
          output[[paste0('h', i)]] <- renderPlotly({})
        })
      } else {
        withProgress(message = "updating plots ...", value = 0, {
          req(cs_track_features(), cs_albums())
          lapply(1:num_hist, function(i) {
            output[[paste0('h', i)]] <- renderPlotly({
              feature_name <- colnames(cs_track_features())[i]
              
              ggplot(cs_track_features(), aes_string(x = feature_name)) +
                geom_histogram(bins = 8, fill="#F08080", color="#e9ecef", alpha=0.9) + 
                labs(x = "", y = "", title = feature_name)
              
            })
            incProgress(1/num_hist, detail = paste("loading", i))
          })
        })
      }
    }
    
  })
  
  
  df = eventReactive(input$ym_click,{
    ### compute test scores
    openness_p = input$ym_q5 + input$ym_q10 + input$ym_q15 + input$ym_q20 + input$ym_q25 + input$ym_q30 + 
      input$ym_q40 + input$ym_q44
    openness_r = 12 - (input$ym_q35 + input$ym_q41)
    openness = (openness_p + openness_r) / 10
    openness = (openness - 1) / 4 
    
    conscientiousness_p = input$ym_q3 + input$ym_q13 + input$ym_q28 + input$ym_q33 + input$ym_q38
    conscientiousness_r = 24 - (input$ym_q8 + input$ym_q18 + input$ym_q23 + input$ym_q43)
    conscientiousness = (conscientiousness_p + conscientiousness_r) / 9
    conscientiousness = (conscientiousness - 1) / 4
    
    extraversion_p = input$ym_q1 + input$ym_q11 + input$ym_q16 + input$ym_q26 + input$ym_q36
    extraversion_r = 18 - (input$ym_q6 + input$ym_q21 + input$ym_q31)
    extraversion = (extraversion_p + extraversion_r) / 8
    extraversion = (extraversion - 1) / 4
    
    agreeableness_p = input$ym_q7 + input$ym_q17 + input$ym_q22 + input$ym_q32 + input$ym_q42
    agreeableness_r = 24 - (input$ym_q2 + input$ym_q12 + input$ym_q27 + input$ym_q37)
    agreeableness = (agreeableness_p + agreeableness_r) / 9
    agreeableness = (agreeableness - 1) / 4
    
    neuroticism_p = input$ym_q4 + input$ym_q14 + input$ym_q19 + input$ym_q29 + input$ym_q39
    neuroticism_r = 18 - (input$ym_q9 + input$ym_q24 + input$ym_q34)
    neuroticism = (neuroticism_p + neuroticism_r) / 8
    neuroticism = (neuroticism - 1) / 4     
    
    ym_score.test = c(openness, conscientiousness, extraversion, agreeableness, neuroticism)
    
    ### compute genre scores
    if(input$ym_age=='12-19'){
      ym_score.genre = cor.age.12_19%*%ym_score.test
    }
    if(input$ym_age=='20-39'){
      ym_score.genre = cor.age.20_39%*%ym_score.test
    }
    if(input$ym_age=='40-65'){
      ym_score.genre = cor.age.40_65%*%ym_score.test
    }
    ### find genres recommended
    ym_rec.genre = ym_score.genre %>% 
      data.frame(.) %>% 
      `colnames<-`('score') %>% 
      bind_cols(genre = c('r-n-b', 'hip-hop', 'electronic', 'rock', 'new-age',	
                          'classical', 'reggae', 'blues',	'country', 'world-music',
                          'folk',	'soundtracks', 'jazz', 'opera', 'punk',	
                          'alternative', 'pop', 'metal')) %>% 
      arrange(desc(score)) %>% 
      slice(1:3) 
  })
  
  ### get dataframe of three genres
  rec1 = reactive({
    get_rec(df()[1,2])
  })
  rec2 = reactive({
    get_rec(df()[2,2])
  })
  rec3 = reactive({
    get_rec(df()[3,2])
  })
  
  ### show genre recommended
  output$ym_genreshow = renderText({
    g = df()$genre
    toupper(str_c(g[1],g[2],g[3],sep=', '))
  })
  
  ### show FIRST SONG
  output$ym_showrec1_title = renderText({
    toupper(df()$genre[1])
  })
  #
  output$ym_showrec1 = renderDataTable({
    DT::datatable(rec1() %>%
                    select(image.url,track,album,artist) %>% 
                    mutate(image.url = str_c('<img src="',image.url,'" height="130"></img>')) %>% 
                    `colnames<-`(c('Cover','Song','Album','Artist')) %>% 
                    t(.) %>% 
                    as.data.frame() %>%
                    `colnames<-`(c('First','Second','Third')),
                  escape = FALSE,
                  options = list(
                    searching = FALSE,lengthChange = FALSE, paging = FALSE,
                    info = FALSE, ordering = FALSE,pageLength = 5,
                    lengthMenu = c(5, 10, 15, 20), autoWidth = TRUE,
                    columnDefs = list(list(width = '230px',targets = c(1,2,3)),
                                      list(className = 'dt-center',targets = '_all'))
                  )
    )
  })
  
  ### show SECOND SONG   
  output$ym_showrec2_title = renderText({
    toupper(df()$genre[2])
  })
  #
  output$ym_showrec2 = renderDataTable({
    DT::datatable(rec2() %>%
                    select(image.url,track,album,artist) %>% 
                    mutate(image.url = str_c('<img src="',image.url,'" height="130"></img>')) %>% 
                    `colnames<-`(c('Cover','Song','Album','Artist')) %>% 
                    t(.) %>% 
                    as.data.frame() %>%
                    `colnames<-`(c('First','Second','Third')),
                  escape = FALSE,
                  options = list(
                    searching = FALSE,lengthChange = FALSE, paging = FALSE,
                    info = FALSE, ordering = FALSE,pageLength = 5,
                    lengthMenu = c(5, 10, 15, 20), autoWidth = TRUE,
                    columnDefs = list(list(width = '230px',targets = c(1,2,3)),
                                      list(className = 'dt-center',targets = '_all'))
                  )
    )
  })
  
  ### show THIRD SONG
  output$ym_showrec3_title = renderText({
    toupper(df()$genre[3])
  })
  #
  output$ym_showrec3 = renderDataTable({
    DT::datatable(rec3() %>%
                    select(image.url,track,album,artist) %>% 
                    mutate(image.url = str_c('<img src="',image.url,'" height="130"></img>')) %>% 
                    `colnames<-`(c('Cover','Song','Album','Artist')) %>% 
                    t(.) %>% 
                    as.data.frame() %>%
                    `colnames<-`(c('First','Second','Third')),
                  escape = FALSE,
                  options = list(
                    searching = FALSE,lengthChange = FALSE, paging = FALSE,
                    info = FALSE, ordering = FALSE,pageLength = 5,
                    lengthMenu = c(5, 10, 15, 20), autoWidth = TRUE,
                    columnDefs = list(list(width = '230px',targets = c(1,2,3)),
                                      list(className = 'dt-center',targets = '_all'))
                  )
    )
  })
  
  # get nearest tracks
  observeEvent(input$vr_get_tracks, {
    
    output$vr_tracks_df <- DT::renderDataTable({
      
      input$vr_get_tracks
      
      isolate({
        # test input(requires tempo mapping)
        inps <- input_creator(list(100,                     # popularity(always 100, pop_flag sets weigths in distance calculators)
                                   unlist( ifelse(input$vr_dance, 1, list(NULL)) ),   # dance 1(want) or NULL(don't care)
                                   unlist( ifelse(input$vr_energy, input$vr_energy_slid, list(NULL)) ),   # energy - something or NULL
                                   unlist( ifelse(input$vr_acoustic, input$vr_acoustic_slid, list(NULL)) ),   # same as energy
                                   unlist( ifelse(input$vr_instrument == 2, list(NULL), input$vr_instrument )),   # return NULL or 0,1
                                   unlist( ifelse(input$vr_live == 2, list(NULL), input$vr_live )),     # same as instrument
                                   unlist( ifelse(input$vr_valence, input$vr_valence_slid, list(NULL)) ),    # same as energy
                                   unlist( ifelse(input$vr_tempo == 0, list(NULL), input$vr_tempo ))
        ))
        # split input and weights
        input_vals <- inps$input_val
        input_wts <- inps$weights
        # get results!
        df <- get_results(inp = input_vals, wt = input_wts, categories = input$vr_cats)
        df$Link <- paste0("<a href='",df$Link,"'target='_blank'>", "Link", "</a>")
        DT::datatable(df, rownames = F,
                      escape = FALSE,
                      options = list(
                        searching = FALSE, paging = FALSE,
                        info = FALSE, ordering = FALSE,pageLength = 5,
                        lengthMenu = c(5, 10, 15, 20)
                      )
        )
      })
    })
  })
  
}

shinyApp(ui = ui, server = server)

