server <- function(input, output, session) {
  
  # reactive variables----
  v <- reactiveValues(
    id = NULL,          # video id
    i = 1,              # selected video (1-5)
    l = c(rep(0, 5)),   # video like status (-1,0,1)
    ge = c(rep('', 5)), # genres of rec videos
    ar = c(rep('', 5)), # artists of rec videos
    sn = c(rep('', 5)), # song names of rec videos
    a = 0,              # activity 1-3
    g = c(rep(0, 5)),   # genre flags (0=off, 1=on)
    rec = 0,            # recommend status
    id2 = 0,            # id of top 16 video selected
    lb = c(16:13)       # labels of top 16 video buttons
  )
  
  p <- reactiveValues(
    w = 0    # window 0 for initial, 1 for choices
  )
  
  s <- reactiveValues(
    t1 = c(rep('white', 3)), # colors of activity borders
    t2 = c(rep('white', 5))  # colors of genre borders
  )
  
  # 1) ui preferences----
  output$prefs <- renderUI(
    if (p$w == 0) {
      div(
        style = 'padding-left:5%; padding-right:5%;',
        fullRow(
          column(
            width = 5,
            offset = 1,
            # _a) wide----
            fullButtonDown(
              actionBttn(
                inputId = 'prefWide',
                label = 'Wide',
                style = 'gradient',
                color = 'warning',
                icon = icon('random'),
                size = 'lg',
                block = TRUE
              )
            )
          ),
          column(
            width = 5,
            # _b) narrow----
            actionBttn(
              inputId = 'prefNarr',
              label = 'Narrow',
              style = 'gradient',
              color = 'primary',
              icon = icon('filter'),
              size = 'lg',
              block = TRUE
            )
          )
        )
      )
    } else {
      div(
        style = 'padding-left:5%; padding-right:5%;',
        
        # __i) activity selector----
        wellPanel(
          style = 'background-color:rgba(240,240,240,0.25);',
          
          # __act1----
          tags$button(
            id = 'btnAct1',
            class = 'btn action-button',
            style = paste0(
              'border:10px solid ', s$t1[1], '; ',
              'border-radius:10px; ',
              'background-color:rgba(200,200,200,0.25); ',
              'margin:0px 2% 2px 2%;'
            ),
            img(
              src = 'act1.png', 
              height = '60px',
              style = 'padding-left:30px; padding-right:30px;'
            )
          ),
          
          # __act2----
          tags$button(
            id = 'btnAct2',
            class = 'btn action-button',
            style = paste0(
              'border:10px solid ', s$t1[2], '; ',
              'border-radius:10px; ',
              'background-color:rgba(200,200,200,0.25); ',
              'margin:0px 2% 2px 2%;'
            ),
            img(
              src = 'act2.png', 
              height = '60px',
              style = 'padding-left:30px; padding-right:30px;'
            )
          ),
          
          # __act3----
          tags$button(
            id = 'btnAct3',
            class = 'btn action-button',
            style = paste0(
              'border:10px solid ', s$t1[3], '; ',
              'border-radius:10px; ',
              'background-color:rgba(200,200,200,0.25); ',
              'margin:0px 2% 2px 2%;'
            ),
            img(
              src = 'act3.png', 
              height = '60px',
              style = 'padding-left:30px; padding-right:30px;'
            )
          )
        ),
        
        # __ii) genre selector----
        wellPanel(
          style = 'background-color:rgba(240,240,240,0.25);',
          # ___dance----
          tags$button(
            id = 'btnDance',
            class = 'btn action-button',
            style = 'background-color:rgba(0,0,0,0);',
            img(
              src = 'genre_dance.png',
              width = '120px',
              style = paste0(
                'border:10px solid ', s$t2[1], '; ',
                'border-radius:70px;'
              )
            ),
            br(),
            h5('Dance')
          ),
          # ___pop----
          tags$button(
            id = 'btnPop',
            class = 'btn action-button',
            style = 'background-color:rgba(0,0,0,0);',
            img(
              src = 'genre_pop.png',
              width = '120px',
              style = paste0(
                'border:10px solid ', s$t2[2], '; ',
                'border-radius:70px;'
              )
            ),
            br(),
            h5('Pop')
          ),
          # ___kpop----
          tags$button(
            id = 'btnKpop',
            class = 'btn action-button',
            style = 'background-color:rgba(0,0,0,0);',
            img(
              src = 'genre_kpop.png',
              width = '120px',
              style = paste0(
                'border:10px solid ', s$t2[3], '; ',
                'border-radius:70px;'
              )
            ),
            br(),
            h5('K-Pop')
          ),
          # ___hiphop----
          tags$button(
            id = 'btnHiphop',
            class = 'btn action-button',
            style = 'background-color:rgba(0,0,0,0);',
            img(
              src = 'genre_hiphop.png',
              width = '120px',
              style = paste0(
                'border:10px solid ', s$t2[4], '; ',
                'border-radius:70px;'
              )
            ),
            br(),
            h5('Hip-Hop')
          ),
          # ___rock----
          tags$button(
            id = 'btnRock',
            class = 'btn action-button',
            style = 'background-color:rgba(0,0,0,0);',
            img(
              src = 'genre_rock.png',
              width = '120px',
              style = paste0(
                'border:10px solid ', s$t2[5], '; ',
                'border-radius:70px;'
              )
            ),
            br(),
            h5('Rock')
          )
        ),
        hr(),
        # __recommend button----
        fullButtonDown(
          actionBttn(
            inputId = 'goRec',
            label = 'View Recommended Videos',
            style = 'gradient',
            color = 'success',
            icon = icon('arrow-down'),
            size = 'md'
          )
        )
      )
    }
  )
  
  # _event wide----
  observeEvent(
    eventExpr = input$prefWide,
    {
      z <- dbGetQuery(
        conn = con,
        statement = 'SELECT * FROM vids 
                     ORDER BY random() LIMIT 5;'
      )
      v$id <- z$ytv_id
      v$ge <- z$genre
      v$ar <- z$artist
      v$sn <- z$song_name
      v$rec <- 1
    }
  )
  
  # _event narrow----
  observeEvent(
    eventExpr = input$prefNarr,
    {
      p$w <- 1
    }
  )
  
  # 2) ui recommendations----
  output$recs <- renderUI(
    if (v$rec == 1) {
      fullRow(
        fullColumn(
          width = 1
        ),
        fullColumn(
          width = 10,
          wellPanel(
            style = 'background-color:rgba(80,80,80,0.75);
                     border-width:0px;',
            # _a) title----
            h3(
              style = 'color:#ffffff;',
              'Recommended Videos'
            ),
            # hr(
            #   style = 'border-color:#141414;'
            # ),
            # _b) embedded videos----
            tags$iframe(
              height = '425px',
              width = '100%',
              src = paste0(
                'https://www.youtube.com/embed/', 
                v$id[v$i],
                '?rel=0'),
              allow = 'fullscreen;'
            ),
            # _c) video buttons----
            fullRow(
              style = 'padding-top:10px;',
              # __i) first column----
              fullColumn(
                width = 3,
                style = 'padding-top:10px;',
                # ___1. genre info----
                actionBttn(
                  inputId = 'vidG',
                  label = v$ge[v$i],
                  style = 'simple',
                  color = 'default',
                  block = FALSE,
                  size = 'sm'
                ),
                # ___2. reset button----
                wellPanel(
                  style = 'background-color:rgba(240,240,240,0.5);
                           margin-top:18px; margin-bottom:0px;',
                  fullButtonUp(
                    actionBttn(
                      inputId = 'btnReset',
                      label = 'Start Over',
                      style = 'gradient',
                      color = 'royal',
                      block = TRUE,
                      icon = icon(
                        name = 'undo'
                      )
                    )
                  )
                )
              ),
              # __ii) second column----
              fullColumn(
                width = 6,
                wellPanel(
                  style = 'background-color:rgba(240,240,240,0.5);
                           margin-bottom:0px;',
                  # ___1. dislike/like buttons----
                  fullRow(
                    style = 'padding:0 5px 20px 5px',
                    fullColumn(
                      width = 6,
                      style = 'padding:0 3px 0 3px;',
                      actionBttn(
                        inputId = 'opt0',
                        label = NULL,
                        style = 'gradient',
                        color = 'danger',
                        block = TRUE,
                        icon = icon(
                          name = 'thumbs-down',
                          lib = 'glyphicon'
                        )
                      )
                    ),
                    fullColumn(
                      width = 6,
                      style = 'padding:0 3px 0 3px;',
                      actionBttn(
                        inputId = 'opt1',
                        label = NULL,
                        style = 'gradient',
                        color = 'success',
                        block = TRUE,
                        icon = icon(
                          name = 'thumbs-up',
                          lib = 'glyphicon'
                        )
                      )
                    )
                  ),
                  # ___2. fwd/rev buttons----
                  fullRow(
                    style = 'padding:0 5px 0 5px',
                    fullColumn(
                      width = 3,
                      style = 'padding:0 3px 0 3px;',
                      actionBttn(
                        inputId = 'vidL',
                        label = NULL,
                        style = 'simple',
                        color = 'warning',
                        size = 'md',
                        block = TRUE,
                        icon = icon('chevron-left')
                      )
                    ),
                    fullColumn(
                      width = 6,
                      style = 'padding:0 3px 0 3px;',
                      actionBttn(
                        inputId = 'vidN',
                        label = paste0(
                          v$i, 
                          ' of 5'
                        ),
                        style = 'simple',
                        color = 'warning',
                        size = 'md',
                        block = TRUE
                      )
                    ),
                    fullColumn(
                      width = 3,
                      style = 'padding:0 3px 0 3px;',
                      actionBttn(
                        inputId = 'vidR',
                        label = NULL,
                        style = 'simple',
                        color = 'warning',
                        size = 'md',
                        block = TRUE,
                        icon = icon('chevron-right')
                      )
                    )
                  )
                )
              ),
              # __iii) third column----
              fullColumn(
                width = 3,
                style = 'padding-top:10px;',
                # ___1. like statuses----
                div(
                  actionBttn(
                    inputId = 'vLike1',
                    label = NULL,
                    style = 'simple',
                    color = ifelse(
                      v$l[1] == 1,
                      'success',
                      ifelse(
                        v$l[1] == -1, 
                        'danger',
                        'default'
                      )
                    ),
                    size = 'sm',
                    icon = icon(
                      name = ifelse(
                        v$l[1] == 1,
                        'thumbs-up',
                        ifelse(
                          v$l[1] == -1, 
                          'thumbs-down',
                          'minus'
                        )
                      ),
                      lib = 'glyphicon'
                    )
                  ),
                  actionBttn(
                    inputId = 'vLike2',
                    label = NULL,
                    style = 'simple',
                    color = ifelse(
                      v$l[2] == 1,
                      'success',
                      ifelse(
                        v$l[2] == -1, 
                        'danger',
                        'default'
                      )
                    ),
                    size = 'sm',
                    icon = icon(
                      name = ifelse(
                        v$l[2] == 1,
                        'thumbs-up',
                        ifelse(
                          v$l[2] == -1, 
                          'thumbs-down',
                          'minus'
                        )
                      ),
                      lib = 'glyphicon'
                    )
                  ),
                  actionBttn(
                    inputId = 'vLike3',
                    label = NULL,
                    style = 'simple',
                    color = ifelse(
                      v$l[3] == 1,
                      'success',
                      ifelse(
                        v$l[3] == -1, 
                        'danger',
                        'default'
                      )
                    ),
                    size = 'sm',
                    icon = icon(
                      name = ifelse(
                        v$l[3] == 1,
                        'thumbs-up',
                        ifelse(
                          v$l[3] == -1, 
                          'thumbs-down',
                          'minus'
                        )
                      ),
                      lib = 'glyphicon'
                    )
                  ),
                  actionBttn(
                    inputId = 'vLike4',
                    label = NULL,
                    style = 'simple',
                    color = ifelse(
                      v$l[4] == 1,
                      'success',
                      ifelse(
                        v$l[4] == -1, 
                        'danger',
                        'default'
                      )
                    ),
                    size = 'sm',
                    icon = icon(
                      name = ifelse(
                        v$l[4] == 1,
                        'thumbs-up',
                        ifelse(
                          v$l[4] == -1, 
                          'thumbs-down',
                          'minus'
                        )
                      ),
                      lib = 'glyphicon'
                    )
                  ),
                  actionBttn(
                    inputId = 'vLike5',
                    label = NULL,
                    style = 'simple',
                    color = ifelse(
                      v$l[5] == 1,
                      'success',
                      ifelse(
                        v$l[5] == -1, 
                        'danger',
                        'default'
                      )
                    ),
                    size = 'sm',
                    icon = icon(
                      name = ifelse(
                        v$l[5] == 1,
                        'thumbs-up',
                        ifelse(
                          v$l[5] == -1, 
                          'thumbs-down',
                          'minus'
                        )
                      ),
                      lib = 'glyphicon'
                    )
                  )
                ),
                # ___2. video analytics button----
                wellPanel(
                  style = 'background-color:rgba(240,240,240,0.5);
                           margin-top:18px; margin-bottom:0px;',
                  fullButtonRight(
                    actionBttn(
                      inputId = 'vidAna',
                      label = 'Video Analytics',
                      style = 'gradient',
                      color = 'primary',
                      block = TRUE,
                      icon = icon(
                        name = 'chart-line'
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    } else {
      fullRow(
        fullColumn(
          width = 1
        ),
        fullColumn(
          width = 10,
          wellPanel(
            style = 'background-color:rgba(80,80,80,0.75);
                     border-width:0px;',
            h3(
              style = 'color:#ffffff;',
              paste0(
                'Please select an option in the ',
                '\'Preferences\' section first.'
              )
            )
          )
        )
      )
    }
  )
  
  # _event vid left----
  observeEvent(
    eventExpr = input$vidL,
    if (v$i > 1) {
      v$i <- v$i - 1
    } else {
      v$i <- 5
    }
  )
  
  # _event vid right----
  observeEvent(
    eventExpr = input$vidR,
    if (v$i < 5) {
      v$i <- v$i + 1
    } else {
      v$i <- 1
    }
  )
  
  # _event vid list----
  observeEvent(
    eventExpr = input$vidN,
    {
      y <- 'https://www.youtube.com/watch?v='
      sendSweetAlert(
        session = session,
        title = 'Video List',
        text = HTML(
          paste0(
            '<style>table, td, th {border:2px solid white;} ',
            'th {background-color:#42594c; padding:5px 5px 5px 5px; color:#ffffff; text-align:center;} ',
            'td {background-color:#cee0d5; padding:5px 5px 5px 5px;}</style>',
            '<table style="width:100%; border:2px solid blue; padding:2px">',
            '<tr><th>#</th><th>Artist</th><th>Song Name</th></tr>',
            '<tr><td>1</td><td>', v$ar[1], '</td>',
            '<td><a href="', y, v$id[1], '" target = "_blank">', v$sn[1], '</a></td></tr>',
            '<tr><td>2</td><td>', v$ar[2], '</td>',
            '<td><a href="', y, v$id[2], '" target = "_blank">', v$sn[2], '</a></td></tr>',
            '<tr><td>3</td><td>', v$ar[3], '</td>',
            '<td><a href="', y, v$id[3], '" target = "_blank">', v$sn[3], '</a></td></tr>',
            '<tr><td>4</td><td>', v$ar[4], '</td>',
            '<td><a href="', y, v$id[4], '" target = "_blank">', v$sn[4], '</a></td></tr>',
            '<tr><td>5</td><td>', v$ar[5], '</td>',
            '<td><a href="', y, v$id[5], '" target = "_blank">', v$sn[5], '</a></td></tr>',
            '</table>'
          )
        ),
        type = 'success',
        html = TRUE,
        width = '900px'
      )
    }
  )
  
  # _event click btnRec----
  observeEvent(
    eventExpr = input$btnRec,
    {
      v$rec <- 1
      enerA <- ifelse(
        v$a == 3, 
        86, 
        ifelse(
          v$a == 2, 
          71, 
          0
        )
      )
      enerB <- ifelse(
        v$a == 1, 
        70, 
        ifelse(
          v$a == 2, 
          85, 
          99
        )
      )
      v$id <- dbGetQuery(
        conn = con,
        statement = paste0(
          'SELECT * FROM vids ',
          'WHERE energy BETWEEN ', enerA, ' AND ', enerB, ' ',
          'ORDER BY random() LIMIT 1;'
        )
      )$ytv_id
    }
  )
  
  # _event click btnReset----
  observeEvent(
    eventExpr = input$btnReset,
    {
      p$w <- 0
    }
  )
  
  # _event click activity----
  # __act1----
  observeEvent(
    eventExpr = input$btnAct1,
    if (v$a != 1) {
      s$t1 <- c('yellow', 'white', 'white')
      v$a <- 1
    } else {
      s$t1 <- c('white', 'white', 'white')
      v$a <- 0
    }
  )
  
  # __act2----
  observeEvent(
    eventExpr = input$btnAct2,
    if (v$a != 2) {
      s$t1 <- c('white', 'yellow', 'white')
      v$a <- 2
    } else {
      s$t1 <- c('white', 'white', 'white')
      v$a <- 0
    }
  )
  
  # __act3----
  observeEvent(
    eventExpr = input$btnAct3,
    if (v$a != 3) {
      s$t1 <- c('white', 'white', 'yellow')
      v$a <- 3
    } else {
      s$t1 <- c('white', 'white', 'white')
      v$a <- 0
    }
  )
  
  # _event click genre buttons----
  # __dance----
  observeEvent(
    eventExpr = input$btnDance,
    if (v$g[1] == 0) {
      s$t2[1] <- '#f000ff'
      v$g[1] <- 1
    } else {
      s$t2[1] <-'white'
      v$g[1] <- 0
    }
  )
  
  # __pop----
  observeEvent(
    eventExpr = input$btnPop,
    if (v$g[2] == 0) {
      s$t2[2] <- '#f000ff'
      v$g[2] <- 1
    } else {
      s$t2[2] <-'white'
      v$g[2] <- 0
    }
  )
  
  # __kpop----
  observeEvent(
    eventExpr = input$btnKpop,
    if (v$g[3] == 0) {
      s$t2[3] <- '#f000ff'
      v$g[3] <- 1
    } else {
      s$t2[3] <-'white'
      v$g[3] <- 0
    }
  )
  
  # __hiphop----
  observeEvent(
    eventExpr = input$btnHiphop,
    if (v$g[4] == 0) {
      s$t2[4] <- '#f000ff'
      v$g[4] <- 1
    } else {
      s$t2[4] <-'white'
      v$g[4] <- 0
    }
  )
  
  # __rock----
  observeEvent(
    eventExpr = input$btnRock,
    if (v$g[5] == 0) {
      s$t2[5] <- '#f000ff'
      v$g[5] <- 1
    } else {
      s$t2[5] <-'white'
      v$g[5] <- 0
    }
  )
  
  # event dislike button----
  observeEvent(
    eventExpr = input$opt0,
    {
      confirmSweetAlert(
        session = session,
        inputId = 'dislike',
        title = 'Why Don\'t You Like This Song?',
        text = tagList(
          checkboxGroupButtons(
            inputId = 'disArt',
            choices = c(
              'Dislike Artist', 
              'Dislike Genre'),
            label = '1',
            selected = character(0),
            status = 'default',
            checkIcon = list(
              yes = icon(
                name = 'ok',
                lib = 'glyphicon'
              )
            )
          ),
          radioGroupButtons(
            inputId = 'disTem',
            choices = c('Too Slow', 'Too Fast'),
            label = '2',
            selected = character(0),
            status = 'default',
            checkIcon = list(
              yes = icon(
                name = 'ok',
                lib = 'glyphicon'
              )
            )
          ),
          radioGroupButtons(
            inputId = 'disEra',
            choices = c('Too Old', 'Too New'),
            label = '3',
            selected = character(0),
            status = 'default',
            checkIcon = list(
              yes = icon(
                name = 'ok',
                lib = 'glyphicon'
              )
            )
          )
        ),
        type = 'question',
        btn_labels = c('Cancel', 'OK'),
        btn_colors = '#005500;'
      )
    }
  )
  
  # event dislike reasons---
  observeEvent(
    eventExpr = input$dislike,
    if (input$dislike) {
      z <- dbGetQuery(
        conn = con,
        statement = 'SELECT * FROM vids 
                     ORDER BY random() LIMIT 1;'
      )
      v$id[v$i] <- z$ytv_id
      v$ge[v$i] <- z$genre
      v$ar[v$i] <- z$artist
      v$sn[v$i] <- z$song_name
      v$l[v$i] <- -1
    }
  )
  
  # event like button----
  observeEvent(
    eventExpr = input$opt1,
    {
      v$l[v$i] <- 1
    }
  )
  
  # event analytics button----
  observeEvent(
    eventExpr = input$vidAna,
    if (input$opt1 > 0) {
      v$rec <- 2
    }
  )
  
  # recom panel----
  # output$recSlide <- renderUI(
  #   if (!is.null(input$intel)) {
  #     if (input$intel %% 2 == 0) {
  #       wellPanel(
  #         style = 'background-color:rgba(240,240,240,0.9)',
  #         h1('Recommended Video'),
  #         hr(),
  #         uiOutput('vid'),
  #         br(),
  #         uiOutput('like')
  #       ) 
  #     } else {
  #       wellPanel(
  #         style = 'background-color:rgba(240,240,240,0.9)',
  #         uiOutput('intelTitle'),
  #         hr(),
  #         uiOutput('intelViz'),
  #         # br(),
  #         radioGroupButtons(
  #           inputId = 'intelOpt',
  #           label = NULL,
  #           choices = c(
  #             `<i class = 'fa fa-user'></i>` = 1,
  #             `<i class = 'fa fa-globe'></i>` = 2,
  #             `<i class = 'fa fa-clock'></i>` = 3
  #           ),
  #           status = 'danger',
  #           size = 'lg',
  #           justified = TRUE
  #         )
  #       )
  #     }
  #   }
  # )
  
  # _intel title----
  output$intelTitle <- renderUI(
    if (input$intelOpt == 1) {
      h1('Who\'s Viewing the Video')
    } else if (input$intelOpt == 2) {
      h1('Where They\'re Viewing the Video')
    } else if (input$intelOpt == 3) {
      h1('When They\'re Viewing the Video')
    }
  )
  
  # _intel viz----
  output$intelViz <- renderUI(
    if (input$intelOpt == 1) {
      plotlyOutput(
        outputId = 'intelWho',
        height = '450px'
      )
    } else if (input$intelOpt == 2) {
      leafletOutput(
        outputId = 'intelWhere', 
        height = '450px'
      )
    } else if (input$intelOpt == 3) {
      plotlyOutput(
        outputId = 'intelWhen',
        height = '450px'
      )
    }
  )
  
  # __intel who----
  output$intelWho <- renderPlotly(
    {
      
      z <- tibble(
        'gender' = c('Male', 'Female'),
        'n' = runif(2) * 5000
      )
      plot_ly(
        data = z,
        labels = ~gender,
        values = ~n,
        marker = list(
          colors = c('#bae1ff', '#ffb3ba'),
          line = list(color = '#ffffff', width = 1)
        )
      ) %>% 
        add_pie(hole = 0.4) %>% 
        layout(
          legend = list(
            orientation = 'v',
            font = list(
              color = '#ffffff'
            )
          ),
          paper_bgcolor = '#444444',
          plot_bgcolor='#444444',
          margin = list(
            l = 10,
            r = 10,
            b = 30,
            t = 30,
            pad = 0
          )
        )
      
    }
  )
  
  # __intel where----
  output$intelWhere <- renderLeaflet(
    {
      z <- dbGetQuery(
        conn = con,
        statement = paste0(
          'SELECT * FROM users ',
          'WHERE st NOT IN (\'AK\', \'HI\') ',
          'ORDER BY random() LIMIT 2000;'
        )
      )
      leaflet(
        data = z,
        options = leafletOptions(
          zoomControl = FALSE
        )
      ) %>%
        # addProviderTiles(providers$Thunderforest.SpinalMap) %>%
        addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>% 
        addCircleMarkers(
          lng = ~lng,
          lat = ~lat,
          radius = 5,
          stroke = FALSE,
          color = ~ifelse(gender == 'M', '#15f4ee', '#ee15f4')
          # clusterOptions = markerClusterOptions()
        ) %>% 
        fitBounds(
          lng1 = ~min(lng), 
          lat1 = ~min(lat),
          lng2 = ~max(lng), 
          lat2 = ~max(lat)
        ) 
    }
  )
  
  # __intel when----
  output$intelWhen <- renderPlotly(
    {
      
    }
  )
  
  # 3) ui top 16----
  output$top16 <- renderUI(
    {
      wellPanel(
        style = 'background-color:rgba(240,240,240,0.9)',
        fullRow(
          column(
            width = 8,
            uiOutput('vid2')
          ),
          column(
            width = 4,
            uiOutput('top16up'),
            uiOutput('top16a'),
            uiOutput('top16b'),
            uiOutput('top16c'),
            uiOutput('top16d'),
            uiOutput('top16dn'),
            # br(),
            uiOutput('top16info')
          )
        )
      )
    }
  )
  
  # _top 16 up arrow----
  output$top16up <- renderUI(
    {
      actionBttn(
        inputId = 'arrowUp',
        label = NULL,
        style = 'fill',
        color = 'royal',
        block = TRUE,
        icon = icon('chevron-up')
      )
    }
  )
  
  # _top 16 dn arrow----
  output$top16dn <- renderUI(
    {
      actionBttn(
        inputId = 'arrowDn',
        label = NULL,
        style = 'fill',
        color = 'royal',
        block = TRUE,
        icon = icon('chevron-down')
      )
    }
  )
  
  # _top 16 button a----
  output$top16a <- renderUI(
    {
      actionBttn(
        inputId = 'top16a',
        label = v$lb[1],
        style = 'fill',
        color = 'danger',
        block = TRUE
      )
    }
  )
  
  # _top 16 button b----
  output$top16b <- renderUI(
    {
      actionBttn(
        inputId = 'top16b',
        label = v$lb[2],
        style = 'fill',
        color = 'danger',
        block = TRUE
      )
    }
  )
  
  # _top 16 button c----
  output$top16c <- renderUI(
    {
      actionBttn(
        inputId = 'top16c',
        label = v$lb[3],
        style = 'fill',
        color = 'danger',
        block = TRUE
      )
    }
  )
  
  # _top 16 button d----
  output$top16d <- renderUI(
    {
      actionBttn(
        inputId = 'top16d',
        label = v$lb[4],
        style = 'fill',
        color = 'danger',
        block = TRUE
      )
    }
  )
  
  # _top 16 info----
  output$top16info <- renderUI(
    if (v$id2 > 0) {
      z <- dbGetQuery(
        conn = con,
        statement = paste0(
          'SELECT * FROM vids ',
          'WHERE ytv_id = \'',
          tt[v$id2], '\''
        )
      )
      wellPanel(
        style = 'margin-top:10px; margin-bottom:0px;',
        h3(style = 'color:red;', v$id2),
        h4(z$artist),
        h5(paste0('"', z$song_name, '"'))
      )
    }
  )
  
  # _event arrow up----
  observeEvent(
    eventExpr = input$arrowUp,
    if (v$lb[1] < 16) {
      v$lb <- v$lb + 4
    }
  )
  
  # _event arrow down----
  observeEvent(
    eventExpr = input$arrowDn,
    if (v$lb[4] > 1) {
      v$lb <- v$lb - 4
    }
  )
  
  # _event top 16a----
  observeEvent(
    eventExpr = input$top16a,
    {
      v$id2 <- v$lb[1]
    }
  )
  
  # _event top 16b----
  observeEvent(
    eventExpr = input$top16b,
    {
      v$id2 <- v$lb[2]
    }
  )
  
  # _event top 16c----
  observeEvent(
    eventExpr = input$top16c,
    {
      v$id2 <- v$lb[3]
    }
  )
  
  # _event top 16d----
  observeEvent(
    eventExpr = input$top16d,
    {
      v$id2 <- v$lb[4]
    }
  )
  
  # _ui embedded video2----
  output$vid2 <- renderUI(
    if (v$id2 > 0) {
      tags$iframe(
        height = '400px',
        width = '100%',
        src = paste0(
          'https://www.youtube.com/embed/', 
          tt[v$id2],
          '?autoplay=1'),
        allow = 'fullscreen; autoplay;'
      )
    } else {
      wellPanel(
        style = 'padding-top:180px; padding-bottom:180px;
                 background-color:#000000;',
        h4(
          style = 'color:#ffffff;', 
          'Select a number'
        )
      )
    }
  )
  
}