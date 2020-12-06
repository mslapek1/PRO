library(shiny)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

source("taryfy.R")
currentProvider <- "Tauron"
currentTariff <- "Standardowa"
data <- read.csv("forBarplot_HomeC.csv")


generateSummary <- function(date_from, date_to) {
  date_from <- as.POSIXlt(date_from, format = "%Y-%m-%d")
  date_to <- as.POSIXlt(date_to, format = "%Y-%m-%d")
  df <- data %>% filter(time >= date_from & time <= date_to)
  
  df <- df %>% group_by(day, hour) %>% 
    summarise(use..kW. = sum(use..kW.)) %>% 
    mutate(day = day + 1)
  
  standard <- df %>% left_join(standard, by = "hour") %>% 
    group_by(taryfa, dostawca) %>% 
    summarise(koszt = sum(use..kW. * price / 60))
  
  dn <- df %>% left_join(dn, by = "hour") %>% 
    group_by(taryfa, dostawca) %>% 
    summarise(koszt = sum(use..kW. * price / 60))
  
  weekendowa <- df %>% left_join(weekendowa, by = c("hour", "day")) %>% 
    group_by(taryfa, dostawca) %>% 
    summarise(koszt = sum(use..kW. * price / 60))
  
  standard %>% 
    rbind(dn) %>% 
    rbind(weekendowa) %>% 
    ungroup()
}

generateBestOffer <- function() {
  summary <- generateSummary(as.character(Sys.Date() - 366), as.character(Sys.Date()))
  
  ourPrice <- summary %>%
    filter(taryfa == currentTariff & dostawca == currentProvider) %>%
    pull(koszt)

  summary <- summary %>%
    filter(taryfa != currentTariff, dostawca != currentProvider) %>%
    mutate(saving = ourPrice - koszt)

  # inside our provider
  insideProvider <- summary %>%
    filter(dostawca == currentProvider, saving > 120) %>%
    arrange(desc(saving))
  if (nrow(insideProvider) >= 1) {
    return(insideProvider)
    return(list(czyJestesNaDobrejTaryfie = FALSE,
                taryfa = insideProvider[[1, "taryfa"]],
                kwota = round(insideProvider[[1, "saving"]]),
                dostawcaNowy = NULL))
  } else {
    # outside our provider
    outsideProvider <- summary %>%
      filter(dostawca != currentProvider, saving > 360) %>%
      arrange(desc(saving))
    if (nrow(outsideProvider) >= 1) {
      return(list(czyJestesNaDobrejTaryfie = FALSE,
                  taryfa = outsideProvider[[1, "taryfa"]],
                  kwota = round(outsideProvider[[1, "saving"]]),
                  dostawcaNowy = outsideProvider[[1, "dostawca"]]))
    } else {
      # no good change possible
      return(list(czyJestesNaDobrejTaryfie = TRUE,
                  taryfa = currentTariff,
                  kwota = 0,
                  dostawcaNowy = NULL))
    }
  }
}

bestOffer <- generateBestOffer()

ui <- fluidPage(
  titlePanel("Jak dobrze radzę sobie z zużyciem prądu?"),
  verbatimTextOutput("comment"),
  headerPanel("Porównanie opłat w taryfach i operatorach"),
  fluidRow(
    column(3,
           checkboxGroupInput("operators", h3("Wybierz operatorów"),
                              choices=c("PGE", "Tauron", "Innogy", "Energa", "Enea"),
                              selected="PGE")
    ),
    column(9,
           selectInput("datesSC", h3("Wybierz zakres czasu"),
                       choices=c("ostatni tydzień" = 1,
                                 "ostatni miesiąc" = 2,
                                 "ostatni rok" = 3)),
           dateRangeInput("dates", h4("dokładny zakres"),
                          format = "dd-mm-yyyy",
                          language = "pl",
                          separator = " do ",
                          start = Sys.Date() - 6,
                          end = Sys.Date(),
                          max = Sys.Date()),
           plotOutput("plot"),
           plotOutput("boxplot")
    )
  )
)

server <- function(input, output, session){
  # input$operators - wybrani operatorzy
  # input$dates - wektor składający się z dwóch dat wyznaczonych w inpucie
  
  # output$comment - treść komentarza
  # output$plot - wykres kolumnowy
  # output$boxplot - boxplot
  
  # linijka niżej służy do szybkiego ustawienia zakresu dat, więc proszę nie ruszać
  observe(updateDateRangeInput(session,
                               "dates",
                               start = as.Date(ifelse(input$datesSC == 1, Sys.Date() - 7,
                                                    ifelse(input$datesSC == 2, Sys.Date() - 31, Sys.Date() - 365)),
                                             origin = "1970-01-01"),
                               end = Sys.Date()))
  
  output$comment <- reactive(input$dates)
  
  
  output$plot <- renderPlot({
    
    ggplot(summary, aes(x = dostawca, y = koszt, fill = taryfa)) +
      geom_bar(stat = "identity", width = 0.7, position = position_dodge(width = 0.8)) +
      ylab("Opłaty za dany okres") + 
      xlab(NULL) 
  })

}

shinyApp(ui = ui, server = server)