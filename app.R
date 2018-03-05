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
    box(
      title = "Tableau des dons d'alliance du 4 mars 2018",
      status = "info",
      width = 4,
      dataTableOutput("tableau")
    ),
    box(
      title = "Barplot des dons d'alliance du 4 mars 2018",
      status = "info",
      width = 8,
      ggiraphOutput("plot")
      
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
  
  output$tableau <- renderDataTable(
    tab
  )
  
  output$plot <- renderggiraph({
    gg <- tab %>% 
      ggplot(aes(x = Participants, y = Dons , fill = Rang)) + 
      theme_bw() +
      geom_bar_interactive(aes(tooltip = Dons, data_id = Participants),stat="identity") +
      coord_flip() +
      geom_hline(yintercept = 50, linetype = 2) +
      scale_fill_brewer(palette = "Blues") +
      scale_y_continuous(name = "Dons d'alliance en K", breaks=seq(0,max(tab$Dons),50))
    
    
    ggiraph(code = print(gg), hover_css = "fill-opacity:.6;cursor:pointer;",
            width = 0.8, width_svg = 6,
            height_svg = 6, selection_type = "none")
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


