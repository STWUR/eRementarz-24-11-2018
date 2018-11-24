library(shiny)
library(ranger)
library(plotly)

load("models.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Wycena mieszkań"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "rok_budowy",
                  label = "Podaj zakres lat budowy",
                  min = 1950,
                  max = 2018, 
                  value = c(1985, 2000), 
                  sep = "", 
                  step = 10),
      checkboxGroupInput(inputId = "dzielnica",
                         label = "podaj dzielnice", 
                         choices = c("Brak", "Fabryczna", "Krzyki", 
                                     "Psie Pole", "Stare Miasto", 
                                     "Śródmieście"),
                         selected = "Psie Pole")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Tabela", tableOutput(outputId = "pred_table")),
        tabPanel("Wykres", plotlyOutput(outputId = "pred_plot"))
      )
      
    )
  )
)

server <- function(input, output) {
  
  pred_df <- reactive({
    new_data <- data.frame(n_pokoj = 3,
                           metraz = 50,
                           pietro = 1,
                           pietro_maks = 2,
                           expand.grid(rok = seq(input[["rok_budowy"]][1],
                                                 input[["rok_budowy"]][2],
                                                 by = 5),
                                       dzielnica = input[["dzielnica"]]))
    
    
    data.frame(new_data, cena_m2 = predict(model_best, new_data)[["predictions"]])
  })
  
  output[["pred_table"]] <- renderTable({
    pred_df()
  })
  
  output[["pred_plot"]] <- renderPlotly({
    ggplotly(ggplot(pred_df(), aes(x = rok, y = cena_m2, color = dzielnica)) +
      geom_point() +
      geom_line() +
      theme_bw())
  })
  
}

shinyApp(ui = ui, server = server)

