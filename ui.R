ui <- fluidPage(
  
  theme = shinytheme('darkly'),
  
  titlePanel(
    title = NULL,
    windowTitle = 'Music'
  ),
  
  h3(
    style = 'text-align:center;',
    'YouTube Music Video Recommender'
  ),
  hr(),
  
  fluidRow(
    # left panel----
    column(
      width = 8,
      actionBttn(
        inputId = 'rec',
        label = 'Recommend a Video',
        style = 'gradient',
        color = 'royal',
        block = TRUE,
        icon = icon(
          'reset',
          lib = 'glyphicon'
        )
      ),
      br(),
      uiOutput('like'),
      br(),
      htmlOutput('inf'),
      uiOutput('vid')
    ),
    # right panel----
    column(
      width = 4,
      br(),
      uiOutput('plots')
    )
  )
  
)

