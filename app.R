library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggiraph)


#write.csv(tab, "koa_04_03.csv",  row.names = FALSE)


#UI
ui <- dashboardPage(
  skin = "purple",
  
  dashboardHeader(title = "KoA : Stats des dons des Be2 "),
  dashboardSidebar(disable = TRUE
    
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("meanbox"),
      valueBoxOutput("plusbox"),
      valueBoxOutput("moinsbox")
    ),
    fluidRow(
      box(
        width = 4,
        height = 820,
        title = "Tableau des dons d'alliance",
        solidHeader = TRUE,
        status = "info",
        fileInput("file", 
                  label = tags$a("Exemple de fichier CSV à importer", 
                                 href = "https://github.com/cecilesauder/koa_be2/blob/master/koa_04_03.csv",
                                 target="_blank" ),
                  buttonLabel = "Importer un fichier CSV..."),
        dataTableOutput("tableau")
      ),
      box(
        title = "Plus de 40k de dons",
        solidHeader = TRUE, 
        status = "info",
        width = 4,
        height = 820,
        ggiraphOutput("plot1")
        
      ),
      box(
        title = "Moins de 40k de dons",
        solidHeader = TRUE, 
        status = "info",
        width = 4,
        height = 820,
        ggiraphOutput("plot2")
        
      )
    )
    
  )
)


# Define server 
server <- function(input, output, session) {
  
  tab <- reactive({
    if(is.null(input$file)){
      tab <- read.csv("koa_be2_18_03.csv", sep = ",", header = TRUE )
    }else{
      tab <- read.csv(input$file$datapath, sep = ",", header = TRUE )
    }
    tab <- tab %>% 
      select(Participants, Dons, Rang) %>%
      arrange(-Dons) %>% 
      mutate(Dons = Dons/1000)
  })
  
  nb_mbr <- reactive( nrow(tab()))
  
  moy <- reactive ( round( mean(tab()$Dons), 3) ) 
  
  tab1 <- reactive({
    tab() %>% 
      filter(Dons > 40)
  }) 
  
  nb_plus <- reactive( nrow(tab1()))
  
  tab2 <- reactive({
    tab() %>% 
      filter(Dons < 40)
  }) 
  
  nb_moins <- reactive( nrow(tab2()) )
  
  
  output$meanbox <- renderValueBox({
    valueBox(
      value = paste0(moy(), " K"), "Moyenne des dons", icon = icon("dollar"),
      color = "maroon"
    )
  })
  
  output$plusbox <- renderValueBox({
    valueBox(
      value = paste0(nb_plus(), "/", nb_mbr(), " membres"), "Plus de 40k", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "maroon"
    )
  })
  
  output$moinsbox <- renderValueBox({
    valueBox(
      value = paste0(nb_moins(), "/", nb_mbr(), " membres"), "Moins de 40k", icon = icon("thumbs-down", lib = "glyphicon"),
      color = "maroon"
    )
  })
  
  
  output$tableau <- renderDataTable(
    tab(),
    options = list(
      pageLength = 15
    )
  )
  
  output$plot1 <- renderggiraph({
    
    gg <- tab1() %>% 
      ggplot(aes(x = reorder(Participants, Dons), y = Dons , fill = Rang)) + 
      theme_bw() +
      geom_bar_interactive(aes(tooltip = Dons, data_id = Participants),stat="identity") +
      coord_flip() +
      geom_hline(yintercept = 40, linetype = 2) +
      scale_y_continuous(name = "Dons d'alliance en K", breaks=seq(0,max(tab()$Dons),40)) +
      xlab("Participants")
    
    
    ggiraph(code = print(gg), hover_css = "fill-opacity:.6;cursor:pointer;",
            width = 0.8, width_svg = 6,
            height_svg = 8, selection_type = "none")
  })
  
  output$plot2 <- renderggiraph({
    
    gg <- tab2() %>% 
      ggplot(aes(x = reorder(Participants, Dons), y = Dons , fill = Rang)) + 
      theme_bw() +
      geom_bar_interactive(aes(tooltip = Dons, data_id = Participants),stat="identity") +
      coord_flip() +
      geom_hline(yintercept = 40, linetype = 2) +
      scale_y_continuous(name = "Dons d'alliance en K", breaks=seq(0,max(tab()$Dons),40)) +
      xlab("Participants")
    
    
    ggiraph(code = print(gg), hover_css = "fill-opacity:.6;cursor:pointer;",
            width = 0.8, width_svg = 6,
            height_svg = 8, selection_type = "none")
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


