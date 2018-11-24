library(shiny)
library(ranger)

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
      tableOutput(outputId = "pred_table")
    )
  )
)

server <- function(input, output) {
  
  output[["pred_table"]] <- renderTable({
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
}

shinyApp(ui = ui, server = server)

