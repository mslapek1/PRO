library(shiny)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

source("taryfy.R")
# currentProvider <- "Tauron"
# currentTariff <- "Standardowa"
currentProvider <- "PGE"
currentTariff <- "Standardowa"
data <- read.csv("forBarplot_HomeC.csv")
data$time2 <- as.POSIXct(data$time, format="%Y-%m-%d")




generateSummary <- function(date_from, date_to, 
                            providers = c("PGE", "Tauron", "Energa", "Innogy", "Enea")) {
  date_from <- as.POSIXlt(date_from, format = "%Y-%m-%d")
  date_to <- as.POSIXlt(date_to, format = "%Y-%m-%d")
  df <- data %>% 
    filter(time2 >= date_from & time2 <= date_to)
  
  df <- df %>% mutate(day = day + 1) %>% 
    group_by(day, hour) %>%
    summarise(use..kW. = sum(use..kW.))
  
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
    filter(dostawca %in% providers) %>% 
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
  imageOutput("zdjecie", width = "20%", height = "250px", inline = TRUE),
  # warunek 2 - czy obecna taryfa w porządku?
  if(bestOffer$czyJestesNaDobrejTaryfie) {
    tags$b(HTML("<div style='text-align:center;float:left; font-size: 30px; width: 62%; border: 5px solid #228B22;'><div>BRAWO!</div></br><div>Twój wybór dostawcy i taryfy są optymalne!</div></div>"))
  },
  if(bestOffer$czyJestesNaDobrejTaryfie) {
    imageOutput("zdjecie2", width = "20%", height = "250px", inline = TRUE)
  },
  # warunek 3 - jesli nie jest w porzadku to...
  if(!bestOffer$czyJestesNaDobrejTaryfie) {
    tags$b(HTML(paste0("<div style='text-align:center;float:left; font-size: 30px; width: 62%; border: 5px solid #FF0000;'><div>Jeśli miałbyś plan taryfowy",
                       "<span style='color: cyan;'> ", bestOffer$taryfa, " </span>",
                       if(bestOffer$dostawcaNowy != currentProvider) {paste0("w <span style='color: cyan;'> ", bestOffer$dostawcaNowy, " </span>")},
                       "zaoszczędziłbyś...</div></br><div style='font-size: 80px; color: magenta;'>",
                       bestOffer$kwota, " zł",
                       "</div></br><div>w minionym roku.</div><div style='font-weight: normal; font-size: 15px; color: gray; text-align: right;'>(najlepsza oferta)</div></div>")))
  },
  if(!bestOffer$czyJestesNaDobrejTaryfie) {
    imageOutput("zdjecie3", width = "20%", height = "250px", inline = TRUE)
  },
  headerPanel("Porównanie opłat w taryfach i operatorach"),
  fluidRow(
    column(3,
           checkboxGroupInput("operators", h3("Wybierz operatorów"),
                              choices = c("PGE", "Tauron", "Innogy", "Energa", "Enea"),
                              selected = c("PGE", "Tauron", "Innogy", "Energa", "Enea"))
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
                          max = Sys.Date())
    )
  ),
  plotOutput("plot"),
  headerPanel("Progres zużycia prądu"),
  selectInput("dates2", h3("Wybierz zakres czasu"),
              choices=c("ostatni dzień" = 1,
                        "ostatni tydzień" = 2,
                        "ostatni miesiąc" = 3)),
  plotOutput("plot2"),
  uiOutput("commentary"),
  headerPanel("Porada"),
  uiOutput("advice")
)

server <- function(input, output, session){
  # input$operators - wybrani operatorzy
  # input$dates - wektor składający się z dwóch dat wyznaczonych w inpucie
  # output$comment - treść komentarza
  # output$plot - wykres kolumnowy
  
  # NOWE:
  # output$plot2 - wykres zużycia prądu w porównaniu z przewidzianym
  # zmienne weekPart i predPart (w tym momencie linijki 201-202)
  # output$advice - miejsce na porady
  
  # interaktywne ustawienie zakresu dat
  observe(updateDateRangeInput(session,
                               "dates",
                               start = as.Date(ifelse(input$datesSC == 1, Sys.Date() - 7,
                                                      ifelse(input$datesSC == 2, Sys.Date() - 31, Sys.Date() - 365)),
                                               origin = "1970-01-01"),
                               end = Sys.Date()))
  
  
  
  output$plot <- renderPlot({
    ggplot(generateSummary(input$dates[1], input$dates[2], input$operators), aes(x = dostawca, y = koszt, fill = taryfa)) +
      geom_bar(stat = "identity", width = 0.7, position = position_dodge(width = 0.8)) +
      ylab("Opłaty za dany okres") + 
      xlab(NULL) + 
      theme(text = element_text(size = 24))
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
  
  
  # WAŻNE
  weekPart <- 0.25
  predPart <- 0.36
  # weekPart - jaką część energii zużytej w POPRZEDNIM dniu/tygodniu/roku stanowi energia zużyta w tym d./t./r.
  # predPart - jaką część energii PRZEWIDZIANEJ do zużycia stanowi energia zużyta w tym d./t./r.
  
  # wartości obu zmiennych są na razie przykładowe, 
  # należy zmienić je na te ustawiane funkcjami
  
  # komentarz na dole
  okres <- function(){
    return(ifelse(input$dates2==1, "dniu",
                  ifelse(input$dates2==2, "tygodniu", "miesiącu")))
  }
  ileEnergii <- function(x){
    return(ifelse(x==1,
                  "tyle samo energii, co",
                  paste0("o ",
                         round(abs(1-x)*100),
                         "% ",
                         ifelse(x<1,
                                "mniej",
                                "więcej"),
                         " energii niż")))
    
  }
  
  output$commentary <- renderUI({
    tags$p(HTML(paste0("W tym ", okres(), " zużyłeś ",
                       ileEnergii(weekPart),
                       " w poprzednim ", okres(), " i ",
                       ileEnergii(predPart),
                       " normalnie.",
                       ifelse(weekPart<=1 & predPart<=1,
                              " Brawo, oby tak dalej :)", "")
    )))
  })
  
  output$advice <- renderUI({
    # tu wchodzi HTML
  })
}

shinyApp(ui = ui, server = server)