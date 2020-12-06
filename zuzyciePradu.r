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
  # logo dostawcy
  tags$b(imageOutput("zdjecie", width = "20%", height = "250px", inline = TRUE)),
  # warunek 2 - czy obecna taryfa w porządku?
  if(bestOffer$czyJestesNaDobrejTaryfie) {
    tags$b(HTML("<div style='text-align:center;float:left; font-size: 30px; width: 62%; border: 5px solid #228B22;'><div>BRAWO!</div></br><div>Twój wybór dostawcy i taryfy są optymalne!</div></div>"))
  },
  if(bestOffer$czyJestesNaDobrejTaryfie) {
    tags$b(imageOutput("zdjecie2", width = "20%", height = "250px", inline = TRUE))
  },
  # warunek 3 - jesli nie jest w porzadku to...
  if(!bestOffer$czyJestesNaDobrejTaryfie) {
    tags$b(HTML(paste0("<div style='text-align:center;float:left; font-size: 30px; width: 62%; border: 5px solid #FF0000;'><div>Jeśli miałbyś plan taryfowy",
                       "<span style='color: cyan;'> ", bestOffer$taryfa, " </span>",
                       "zaoszczędziłbyś...</div></br><div style='font-size: 80px; color: magenta;'>",
                       bestOffer$kwota, " zł",
                       "</div></br><div>w minionym roku(najlepsza oferta).</div></div>")))
  },
  if(!bestOffer$czyJestesNaDobrejTaryfie) {
    tags$b(imageOutput("zdjecie3", width = "20%", height = "250px", inline = TRUE))
  },
  headerPanel("Porównanie opłat w taryfach i operatorach"),
  fluidRow(
    column(3,
           checkboxGroupInput("operators", h3("Wybierz operatorów"),
                              choices = c("PGE", "Tauron", "Innogy", "Energa", "Enea"),
                              selected = "PGE")
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
  
  
  
  output$plot <- renderPlot({
    
    ggplot(generateSummary(input$dates[1], input$dates[2]), aes(x = dostawca, y = koszt, fill = taryfa)) +
      geom_bar(stat = "identity", width = 0.7, position = position_dodge(width = 0.8)) +
      ylab("Opłaty za dany okres") + 
      xlab(NULL) 
  })
  
  output$zdjecie <- renderImage({
    filename <- normalizePath(file.path('.',
                                        paste('img/', ifelse(is.null(bestOffer$dostawcaNowy), 
                                                             currentProvider, 
                                                             bestOffer$dostawcaNowy), 
                                              input$n, '.png', sep = '')))
    
    # Return a list containing the filename
    list(src = filename, width = "18%", style = "float:left;", height = "15%")
  }, deleteFile = FALSE)
  
  output$zdjecie2 <- renderImage({
    filename <- normalizePath(file.path('.',
                                        paste('img/HAPPY', input$n, '.png', sep='')))
    
    # Return a list containing the filename
    list(src = filename, width = "18%", style = "float:left;", height = "15%")
  }, deleteFile = FALSE)
  
  output$zdjecie3 <- renderImage({
    filename <- normalizePath(file.path('.',
                                        paste('img/EXCLAMATION_MARK', input$n, '.jpg', sep='')))
    
    # Return a list containing the filename
    list(src = filename, width = "18%", style = "float:left;", height = "15%")
  }, deleteFile = FALSE)

}

shinyApp(ui = ui, server = server)