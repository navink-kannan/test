#---------------------------------------------------------------------#
#               UDPipe NLP App                               #
#---------------------------------------------------------------------#


# Define ui function
english_model <- udpipe_load_model("./english-ewt-ud-2.4-190531.udpipe")

server <- shinyServer(function(input, output, session) {
  
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
  
  
  Dataset <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      Data <- readLines(input$file$datapath)
      Data <-  str_replace_all(Data, "<.*?>", "")
      upos_tab<- annotated_table(Data)
      
      all_adjs=all_nouns=all_propns=all_advs=all_verbs = upos_tab[FALSE,]
      
      if(input$adj == TRUE)
        all_adjs = upos_tab %>% subset(., upos %in% "ADJ")
      
      if(input$noun == TRUE)
        all_nouns = upos_tab %>% subset(., upos %in% "NOUN")
      
      if(input$propn == TRUE)
        all_propns = upos_tab %>% subset(., upos %in% "PROPN")
      
      if(input$adv == TRUE)
        all_advs = upos_tab %>% subset(., upos %in% "ADV")
      
      if(input$verb == TRUE)
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
    if(input$adj == TRUE)
      subtitle="Adjective"
    if(input$noun == TRUE)
      if (subtitle=="") subtitle="Noun" else  subtitle=paste(subtitle,"Noun", sep=",")
      if(input$propn == TRUE)
        if (subtitle=="") subtitle="Proper Noun" else  subtitle=paste(subtitle,"Proper Noun", sep=",")
        if(input$adv == TRUE)
          if (subtitle=="") subtitle="Adverb" else  subtitle=paste(subtitle,"Adverb", sep=",")
          if(input$verb == TRUE)
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