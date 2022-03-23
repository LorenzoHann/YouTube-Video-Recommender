ui <- fullPage(
  
  sections.color = c(rep('#000000', 4)),
  
  opts = list(
    controlArrows = FALSE,
    fadingEffect = TRUE,
    fitToSection = TRUE,
    loopBottom = FALSE,
    loopHorizontal = TRUE,
    navigation = TRUE,
    scrollBar = FALSE,
    scrollingSpeed = 1000,
    scrollOverflow = TRUE,
    scrollOverflowReset = TRUE,
    slidesNavigation = TRUE,
    verticalCentered = TRUE
  ),
  
  menu = c(
    'Home' = 'home',
    'Preferences' = 'pref',
    'Recommendations' = 'reco',
    'About Me' = 'about'
  ),
  
  # home section----
  fullSectionImage(
    menu = 'home',
    center = TRUE,
    img = 'home.jpg',
    div(
      style = 'padding-left:25%;
               padding-right:25%;',
      wellPanel(
        style = 'background-color:rgba(80,80,80,0.75);
                 border-width:0px;',
        fullButtonTo(
          section = 'about',
          img(
            src = 'dy.png', 
            height = '100px'
          )
        ),
        hr(
          style = 'border-color:#141414;'
        ),
        img(
          src = 'youtube.png',
          height = '40px'
        ),
        h2(
          style = 'color:#ffffff;',
          'Music Video Recommender'
        ),
        br(),
        fullButtonDown(
          actionBttn(
            inputId = 'goDown',
            label = NULL,
            style = 'gradient',
            color = 'success',
            icon = icon('arrow-down'),
            size = 'lg'
          )
        )
      )
    )
  ),
  
  # preferences section----
  fullSectionImage(
    menu = 'pref',
    center = TRUE,
    img = 'pref.jpg',
    fullRow(
      column(
        width = 10,
        offset = 1,
        wellPanel(
          style = 'background-color:rgba(80,80,80,0.75);
                   border-width:0px;',
          h2(
            style = 'color:#ffffff;',
            'Your Preferences'
          ),
          hr(
            style = 'border-color:#141414;'
          ),
          uiOutput('prefs')
        )
      )
    )
  ),
  
  # recommendations section----
  fullSectionImage(
    menu = 'reco',
    center = TRUE,
    img = 'recom.jpg',
    fullSlide(uiOutput('recs')),
    fullSlide(uiOutput('reca'))
  ),
  
  # about me section----
  fullSectionImage(
    menu = 'about',
    center = TRUE,
    img = 'columbia.jpg',
    fullSlide(
      fullRow(
        column(
          width = 8,
          offset = 2,
          wellPanel(
            align = 'left',
            style = 'background-color:rgba(240,240,240,0.75)',
            fullRow(
              column(
                width = 8,
                align = 'left',
                tags$a(
                  href = 'https://www.linkedin.com/in/dayhyi/',
                  h1('Day Yi'),
                  target = '_blank'
                ),
                hr(style = 'border-color:#000000;'),
                tags$a(
                  href = 'https://sps.columbia.edu/faculty/day-yi-1',
                  h3(
                    style = 'color:#366d96;',
                    'Columbia University - Applied Analytics Professor'
                  ),
                  target = '_blank'
                ),
                tags$ul(
                  tags$li(
                    h4('Instructor of SQL and Capstone Project courses')
                  ),
                  tags$li(
                    h4('Mentor to Applied Analytics graduate students')
                  ),
                  tags$li(
                    h4('Consultant to Fortune 500 companies')
                  )
                )
              ),
              column(
                width = 4,
                align = 'right',
                img(
                  src = 'day.png',
                  style = 'border:5px solid white; vertical-align:middle;',
                  width = '80%'
                )
              )
            ),
            div(
              align = 'right',
              style = 'padding-top:10px;',
              fullButtonRight(
                actionBttn(
                  inputId = 'view16',
                  label = 'View Day\'s Sweet 16 Videos >',
                  style = 'fill',
                  color = 'primary',
                  block = FALSE
                )
              )
            )
          )
        )
      )
    ),
    fullSlide(
      fullRow(
        column(
          width = 10,
          offset = 1,
          wellPanel(
            style = 'background-color:rgba(240,240,240,0.75)',
            h2('Day\'s Sweet 16'),
            uiOutput('top16'),
            div(
              align = 'left',
              fullButtonLeft(
                actionBttn(
                  inputId = 'viewAbout',
                  label = '< Return to About Me',
                  style = 'fill',
                  color = 'success',
                  block = FALSE
                )
              )
            )
          )
        )
      )
    )
  )
  
)
