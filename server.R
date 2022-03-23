server <- function(input, output, session) {
  
  # reactive variable----
  v <- reactiveValues(
    i = NULL,   # video info
    l = NULL    # video like status
  )
  
  # click recommend button----
  observeEvent(
    eventExpr = input$rec,
    if (input$rec > 0) { # must be clicked at least once
      # _randomly select a video, need a better method----
      v$i <- dbGetQuery(
        conn = con,
        statement = 'SELECT * FROM vids ORDER BY random() LIMIT 1;'
      )
      # _show video info----
      output$inf <- renderText(
        paste0(
          '<h5 style = "text-align:center;">', 
          v$i$artist, ' &mdash; ',
          v$i$song_name, 
          ' [', v$i$genre, ']',
          '</h5>'
        )
      )
      # _show embedded video----
      output$vid <- renderUI(
        tags$iframe(
          height = '400',
          width = '100%',
          src = paste0('https://www.youtube.com/embed/', v$i$ytv),
          frameborder = '3',
          autoplay = 1,
          allow = 'accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope;'
        )
      )
      # _show dislike/like buttons----
      output$like <- renderUI(
        fluidRow(
          column(
            width = 6,
            actionBttn(
              inputId = 'opt0',
              label = NULL,
              style = 'gradient',
              color = 'danger',
              block = TRUE,
              icon = icon(
                'thumbs-down',
                lib = 'glyphicon'
              )
            )
          ),
          column(
            width = 6,
            actionBttn(
              inputId = 'opt1',
              label = NULL,
              style = 'gradient',
              color = 'success',
              block = TRUE,
              icon = icon(
                'thumbs-up',
                lib = 'glyphicon'
              )
            )
          )
        )
      )
    } 
  )

  # click dislike button----
  observeEvent(
    eventExpr = input$opt0,
    if (input$opt0 > 0) {
      # _randomly select a video, need a better method----
      v$i <- dbGetQuery(
        conn = con,
        statement = 'SELECT * FROM vids 
                     ORDER BY random() LIMIT 1;'
      )
    }
  )
  
  # click like button----
  observeEvent(
    eventExpr = input$opt1,
    # _randomly select a video, need a better method----
    if (input$opt1 > 0) {
      v$i <- dbGetQuery(
        conn = con,
        statement = 'SELECT * FROM vids ORDER BY random() LIMIT 1;'
      )
    }
  )
  
  # right panel----
  output$plots <- renderUI(
    if (input$rec > 0) {
      tabsetPanel(
        tabPanel(
          title = 'BPM Trend',
          plotlyOutput(
            outputId = 'plot1',
            height = '550px'
          )
        ),
        tabPanel(
          title = 'Hot Spots',
          leafletOutput(
            outputId = 'map1',
            height = '550px'
          )
        )
      )
    }
  )
  
  # _plot1----
  output$plot1 <- renderPlotly(
    {
      ytdat <- NULL
      gen <- c('Dance', 'K-Pop', 'Pop', 'Rock')
      for (i in 1:4) {
        y0 <- runif(1, 80, 145)
        ytdat <- ytdat %>% 
          bind_rows(
            tibble(
              t = sample(3:100, 40, replace = TRUE),
              y = rnorm(40, y0, 10),
              g = gen[i]
            )
          )
      }
      plot_ly(
        data = ytdat,
        x = ~t, 
        y = ~y, 
        color = ~g,
        size = 3,
        type = 'scatter',
        mode = 'markers'
      ) %>% 
        layout(
          paper_bgcolor = '#222222',
          plot_bgcolor='#222222',
          # title = list(
          #   text = 'BPM Trend',
          #   font = list(
          #     color = '#ffffff'
          #   )
          # ),
          xaxis = list(
            title = 'Time',
            color = '#ffffff',
            titlefont = list(size = 16)
          ),
          yaxis = list(
            title = 'BPM',
            color = '#ffffff',
            range = c(75, 150),
            titlefont = list(size = 16)
          ),
          # margin = list(
          #   l = 50,
          #   r = 50,
          #   b = 100,
          #   t = 100,
          #   pad = 20
          # ),
          legend = list(
            font = list(
              color = '#ffffff'
            ),
            bordercolor = '#e2e2e2',
            borderwidth = 2,
            x = 0.06,
            y = 0.99
          ),
          showlegend = TRUE
        )
    }
  )
  
  # _map1----
  output$map1 <- renderLeaflet(
    {
      u <- dbGetQuery(
        conn = con,
        statement = paste0(
          'SELECT * FROM users ',
          'WHERE st NOT IN (\'AK\', \'HI\') ',
          'ORDER BY random() LIMIT 500;'
        )
      )
      leaflet(
        data = u,
        options = leafletOptions(
          zoomControl = FALSE
        )
      ) %>%
        addTiles() %>% 
        addCircleMarkers(
          lng = ~lng,
          lat = ~lat,
          radius = 1,
          clusterOptions = markerClusterOptions()
        ) %>% 
        fitBounds(
          lng1 = ~min(lng), 
          lat1 = ~min(lat),
          lng2 = ~max(lng), 
          lat2 = ~max(lat)
        ) 
    }
  )
}

