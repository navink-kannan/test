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
        checkboxInput("adj", "adjective (ADJ)", TRUE),
        checkboxInput("noun", "noun(NOUN)", TRUE),
        checkboxInput("propn", "proper noun (PROPN)", TRUE),
        checkboxInput("adv", "adverb (ADV)", FALSE),
        checkboxInput("verb", "verb (VERB)", FALSE),
        downloadButton("downloadData", "Download")),
      
      
      mainPanel(
        
        tabsetPanel(type = "tabs",
                    
                    tabPanel(value="hide","Overview",
                             h4(p("Data input")),
                             p("This app supports only text data file.",align="justify"),
                             p("Please refer to the link below for sample file."),
                             #a(href="","Sample data input file"),   
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


annotated_table <- function(data)
{
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

