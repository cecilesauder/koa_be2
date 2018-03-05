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
        title = "Tableau des dons d'alliance du 4 mars 2018",
        solidHeader = TRUE,
        status = "info",
        dataTableOutput("tableau")
      ),
      box(
        title = "Plus de 50k de dons",
        solidHeader = TRUE, 
        status = "info",
        width = 4,
        height = 820,
        ggiraphOutput("plot1")
        
      ),
      box(
        title = "Moins de 50k de dons",
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
  
  tab <- read.csv("koa_04_03.csv", sep = ",", header = TRUE )
  tab <- tab %>% 
    select(Participants, Dons, Rang) %>%
    filter(Dons != 0) %>%
    arrange(-Dons) %>% 
    mutate(Dons = Dons/1000)
  
  nb_mbr <- nrow(tab)
  
  moy <- round( mean(tab$Dons), 3)
  
  tab1 <- tab %>% 
    filter(Dons > 50)
  
  nb_plus <- nrow(tab1)
  
  tab2 <- tab %>% 
    filter(Dons < 50)
  
  nb_moins <- nrow(tab2)
  
  
  output$meanbox <- renderValueBox({
    valueBox(
      value = paste0(moy, " K"), "Moyenne des dons", icon = icon("dollar"),
      color = "maroon"
    )
  })
  
  output$plusbox <- renderValueBox({
    valueBox(
      value = paste0(nb_plus, "/", nb_mbr, " membres"), "Plus de 50k", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "maroon"
    )
  })
  
  output$moinsbox <- renderValueBox({
    valueBox(
      value = paste0(nb_moins, "/", nb_mbr, " membres"), "Moins de 50k", icon = icon("thumbs-down", lib = "glyphicon"),
      color = "maroon"
    )
  })
  
  
  output$tableau <- renderDataTable(
    tab,
    options = list(
      pageLength = 15
    )
  )
  
  output$plot1 <- renderggiraph({
    
    gg <- tab1 %>% 
      ggplot(aes(x = reorder(Participants, Dons), y = Dons , fill = Rang)) + 
      theme_bw() +
      geom_bar_interactive(aes(tooltip = Dons, data_id = Participants),stat="identity") +
      coord_flip() +
      geom_hline(yintercept = 50, linetype = 2) +
      scale_y_continuous(name = "Dons d'alliance en K", breaks=seq(0,max(tab$Dons),50)) +
      xlab("Participants")
    
    
    ggiraph(code = print(gg), hover_css = "fill-opacity:.6;cursor:pointer;",
            width = 0.8, width_svg = 6,
            height_svg = 8, selection_type = "none")
  })
  
  output$plot2 <- renderggiraph({
    
    gg <- tab2 %>% 
      ggplot(aes(x = reorder(Participants, Dons), y = Dons , fill = Rang)) + 
      theme_bw() +
      geom_bar_interactive(aes(tooltip = Dons, data_id = Participants),stat="identity") +
      coord_flip() +
      geom_hline(yintercept = 50, linetype = 2) +
      scale_y_continuous(name = "Dons d'alliance en K", breaks=seq(0,max(tab$Dons),50)) +
      xlab("Participants")
    
    
    ggiraph(code = print(gg), hover_css = "fill-opacity:.6;cursor:pointer;",
            width = 0.8, width_svg = 6,
            height_svg = 8, selection_type = "none")
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


