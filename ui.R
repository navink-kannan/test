#---------------------------------------------------------------------#
#               UDPipe NLP App                               #
#---------------------------------------------------------------------#

library("shiny")
# Define ui function

ui <- shinyUI(
  fluidPage(
    
    titlePanel("UDPipe NLP"),
    
    sidebarLayout( 
      
      sidebarPanel(  
        
        fileInput("file", "Upload data (txt file) "),
        
        selectInput("lang", "Choose the language:",
                    list(`english`="english",
                         `french`= "french",
                         `dutch`="dutch"), selected="english"),
        
        checkboxGroupInput(inputId='checkopt',  "Choose POSTag:",
                           c("adjective (ADJ)" = "ADJ",
                             "noun(NOUN)" = "NOUN",
                             "proper noun (PROPN)" = "PROPN",
                             "adverb (ADV)" = "ADV",
                             "verb (VERB)" = "VERB"),selected = c("ADJ","NOUN","PROPN")
        ),  
        
        downloadButton("downloadData", "Download")),
      
      
      mainPanel(
        
        tabsetPanel(type = "tabs",
                    
                    tabPanel(value="hide","Overview",
                             h4(p("Data input")),
                             p("This app supports only text data file.",align="justify"),
                             p("Please refer to the link below for sample english file."),
                             a(href="https://github.com/navink-kannan/sample/blob/master/sample_dutch.txt","Sample english data input file:-"),   
                             br(),
                             br(),
                             p("Please refer to the link below for sample french file."),
                             a(href="https://github.com/navink-kannan/sample/blob/master/sample_french.txt","Sample french data input file:-"),   
                             br(),
                             br(),
                             p("Please refer to the link below for sample dutch file."),
                             a(href="https://github.com/navink-kannan/sample/blob/master/sample_dutch.txt","Sample dutch data input file:-"),   
                             br(),
                             br(),
                             h4('How to use this App'),
                             p('To use this app, click on', 
                               span(strong("Upload data ")),
                               'and upload the data file.')),
                    
                    tabPanel(value="show","Annotated doc", 
                             dataTableOutput('plot2')),
                    tabPanel(value="hide","WordCloud", 
                             fluidRow(
                               column(6,plotOutput('plot3_1')),
                               column(6,plotOutput('plot3_2'))
                             )),
                    tabPanel(value="hide","Cooccurence Analysis", 
                             plotOutput('plot4'))
        )))))

