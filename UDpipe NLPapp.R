#---------------------------------------------------------------------#
#               UDPipe NLP App                               #
#---------------------------------------------------------------------#


suppressPackageStartupMessages({
  if (!require(shiny)){install.packages("shiny")}
  if (!require(tidyverse)){install.packages("tidyverse")}
  if (!require(tidytext)){install.packages("tidytext")}
  if (!require(udpipe)){install.packages("udpipe")}
  if (!require(textrank)){install.packages("textrank")}
  if (!require(lattice)){install.packages("lattice")}
  if (!require(igraph)){install.packages("igraph")}
  if (!require(ggraph)){install.packages("ggraph")}
  if (!require(ggplot2)){install.packages("ggplot2")}
  if (!require(wordcloud)){install.packages("wordcloud")
  if (!require(stringr)){install.packages("stringr")}  }
  library("shiny")
  library(tidyverse)
  library(tidytext)
  library(udpipe)
  library(textrank)
  library(lattice)
  library(igraph)
  library(ggraph)
  library(ggplot2)
  library(wordcloud)
  library(stringr)
})
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
                             p("Please refer to the link below for sample file."),
                             a(href="https://github.com/navink-kannan/sample/blob/master/Sample.txt","Sample data input file"),   
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




# Define Server function
server <- shinyServer(function(input, output, session) {

  annotated_table <- function(data)
  {
    
    model <- udpipe_download_model(language = input$lang, overwrite=FALSE)
    english_model <- udpipe_load_model(model$file_model)
    
    x <- udpipe_annotate(english_model, x = data)
    x <- as.data.frame(x)
    return(x)
  }
  
  cooccurence_analysis <- function(data, sub_title)
  {
    
    word_cooc <- cooccurrence(     
      x = data, 
      term = "lemma", 
      group = c("doc_id", "paragraph_id", "sentence_id"))  
    
    
    wordnetwork <- head(word_cooc, 30)
    wordnetwork <- igraph::graph_from_data_frame(wordnetwork) # needs edgelist in first 2 colms.
    
    cooc_plot<- ggraph(wordnetwork, layout = "fr") +  
      
      geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "orange") +  
      geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
      
      theme_graph(base_family = "Arial Narrow") +  
      theme(legend.position = "none") +
      
      labs(title = "Cooccurrences within 3 words distance", subtitle = sub_title)
    return(cooc_plot)
  }
  
  
  Dataset <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      #readLines(input$file$datapath)
      Data <- readLines(input$file$datapath)
      Data <-  str_replace_all(Data, "<.*?>", "")
      upos_tab<- annotated_table(Data)
      
      all_adjs=all_nouns=all_propns=all_advs=all_verbs = upos_tab[FALSE,]
      
      if((input$checkopt %in% 'ADJ') == TRUE)
      all_adjs = upos_tab %>% subset(., upos %in% "ADJ")
      
      if((input$checkopt %in% 'NOUN')  == TRUE)
      all_nouns = upos_tab %>% subset(., upos %in% "NOUN")
       
      if((input$checkopt %in% 'PROPN')  == TRUE)
      all_propns = upos_tab %>% subset(., upos %in% "PROPN")
      
      if((input$checkopt %in% 'ADV')  == TRUE)
      all_advs = upos_tab %>% subset(., upos %in% "ADV")
      
      if((input$checkopt %in% 'VERB')  == TRUE)
      all_verbs = upos_tab %>% subset(., upos %in% "VERB")
      
      upos_data<-rbind(all_adjs, all_nouns, all_propns, all_advs, all_verbs)
      return(upos_data)
    }
  })
  
  
  WC_Dataset <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      Data <- readLines(input$file$datapath)
      Data <-  str_replace_all(Data, "<.*?>", "")
      Data<- annotated_table(Data)
      return(Data)
    }
  })
  

  output$plot2 = renderDataTable({
    tab<-Dataset()
    out <- subset(tab, select = -c(sentence))
    head(out,100)
  })
  
  output$plot3_1 = renderPlot({ 
    
    library(wordcloud)
    wc<- WC_Dataset()
    
    layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
    par(mar=rep(0, 4))
    plot.new()
    text(x=0.5, y=0.2, "Wordcloud for Nouns")
    
    all_nouns = wc %>% subset(., upos %in% "NOUN")
    
    top_nouns = txt_freq(all_nouns$lemma)
    
    nouns<- wordcloud(words = top_nouns$key, 
                  freq = top_nouns$freq, 
                  min.freq = 2, 
                  max.words = 100,
                  random.order = FALSE, 
                  colors = brewer.pal(6, "Dark2"), main="Wordcloud for Nouns", fixed.asp=TRUE)
    
    nouns
  })

  output$plot3_2 = renderPlot({ 
    
    library(wordcloud)
    wc<- WC_Dataset()
    
    layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
    par(mar=rep(0, 4))
    plot.new()
    text(x=0.5, y=0.2, "Wordcloud for Verbs")
    all_verbs = wc %>% subset(., upos %in% "VERB")
    
    top_verbs = txt_freq(all_verbs$lemma)
    
    verbs<- wordcloud(words = top_verbs$key, 
                      freq = top_verbs$freq, 
                      min.freq = 2, 
                      max.words = 100,
                      random.order = FALSE, 
                      colors = brewer.pal(6, "Dark2"), main="Wordcloud for Verbs",fixed.asp=TRUE)
    verbs
  })
  
  output$plot4 = renderPlot({ 
    subtitle=""
    if((input$checkopt %in% 'ADJ')  == TRUE)
    subtitle="Adjective"
    if((input$checkopt %in% 'NOUN')  == TRUE)
    if (subtitle=="") subtitle="Noun" else  subtitle=paste(subtitle,"Noun", sep=",")
    if((input$checkopt %in% 'PROPN')  == TRUE)
    if (subtitle=="") subtitle="Proper Noun" else  subtitle=paste(subtitle,"Proper Noun", sep=",")
    if((input$checkopt %in% 'ADV')  == TRUE)
    if (subtitle=="") subtitle="Adverb" else  subtitle=paste(subtitle,"Adverb", sep=",")
    if((input$checkopt %in% 'VERB')  == TRUE)
    if (subtitle=="") subtitle="Verb" else  subtitle=paste(subtitle,"Verb", sep=",")
    
    plot_cooc<-cooccurence_analysis(Dataset(), subtitle)
    plot_cooc
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Annotated_document", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(Dataset(), file, row.names = FALSE)
    }
  )
  
})


shinyApp(ui = ui, server = server)
