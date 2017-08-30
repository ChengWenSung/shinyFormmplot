library(shiny)

#UI

ui <- shinyUI(fluidPage(
  titlePanel("mmplot shiny"),
  sidebarLayout(
    sidebarPanel(
      #a button indicate user to  upload file
      fileInput('file1', 'Choose file to upload',
                accept = c('text/csv',
                           'text/comma-separated-values',
                           'text/tab-separated-values',
                           'text/plain','.csv','.tsv')),
      #choose plot type
      checkboxGroupInput("type", label = h3("Choose Plot Type"), 
                         choices = list("dot" = 1,"dot with 95% cl" = 2, "bar" = 3, "bar with 95% cl" = 4,
                                        "boxplot" = 5)),
      #choose grouping number
      sliderInput('number','Choose grouping number',1,7,value = 7,step = 1),
      #allow user to specify head title (not necessary)
      textInput('header','Input plot header'),
      #allow user to specify x axis title (not necessary)
      textInput('xtitle','Input X axis title'),
      #let "go" button show
      uiOutput('buttonsUI')
    ),
    mainPanel(
      fluidRow(
        plotOutput("out.plot"),
        textOutput("warning"),
        tags$head(tags$style("#warning{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
        )
        )
      )
    )
  )
))

##

