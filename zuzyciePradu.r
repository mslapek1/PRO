library(shiny)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

source("taryfy.R")
# currentProvider <- "Innogy"
# currentTariff <- "Weekendowa"
currentProvider <- "Tauron"
currentTariff <- "Weekendowa"
data <- read.csv("forBarplot_HomeC.csv")
data$time2 <- as.POSIXct(data$time, format = "%Y-%m-%d")


generateSummary <- function(date_from, date_to) {
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
  
  data <- standard %>% 
    rbind(dn) %>% 
    rbind(weekendowa) %>% 
    ungroup()
  
  y_saving <- data %>% 
    filter(dostawca == currentProvider, taryfa == currentTariff) %>% 
    pull(koszt)
  
  data <- data %>% mutate(difference = koszt - y_saving) %>%
    mutate(kolor = ifelse(difference < 0, "green", "red")) %>% 
    mutate(difference = ifelse(difference >= 0, 
                               paste0("+", round(difference, 0), " zł"), 
                               paste0(as.character(round(difference, 0))," zł")))
  data$kolor[data$dostawca == currentProvider & data$taryfa == currentTariff] <- "black"
  data$difference <- ifelse(data$dostawca == currentProvider & data$taryfa == currentTariff, 
                            paste0(round(y_saving, 0), " zł"), 
                            data$difference)
  data
}

generateBestOffer <- function() {
  summary <- generateSummary(as.character(Sys.Date() - 366), as.character(Sys.Date()))
  
  ourPrice <- summary %>%
    filter(taryfa == currentTariff & dostawca == currentProvider) %>%
    pull(koszt)
  
  summary <- summary %>%
    filter(taryfa != currentTariff | dostawca != currentProvider) %>%
    mutate(saving = ourPrice - koszt)
  
  return(summary %>% filter(saving > 0) %>% slice_max(saving, n = 2))
}

bestOffer <- generateBestOffer()

ui <- fluidPage(
  titlePanel("Jak dobrze radzę sobie ze zużyciem prądu?"),
  
  # warunek 2 - czy obecna taryfa w porządku?
  if(nrow(bestOffer) == 0) {
    HTML("<br/><div style='text-align:center;float:left; font-size: 30px; width: 62%; border: 5px solid #228B22;'><br/><div><b>BRAWO!</b></div></br></br><div>Twój wybór dostawcy i taryfy są optymalne!</div><div>Żadna inna oferta nie zapewniłaby Ci niższych rachunków.</div><br/></div>")
  },
  if(nrow(bestOffer) == 0) {
    imageOutput("zdjecie2", width = "20%", height = "100px", inline = TRUE)
  },
  # warunek 3 - jesli nie jest w porzadku to...
  if(nrow(bestOffer) > 0) {
    HTML(paste0("<br/><div style='text-align:center;float:left; font-size: 30px; width: 80%; border: 5px solid #FF0000;'>",
                "<br/>",
                "Jeśli miałbyś plan taryfowy",
                       "<span style='font-weight: bold; color: #40e0d0;'> \"", bestOffer$taryfa[1], "\" </span>",
                       if(bestOffer$dostawca[1] != currentProvider) {paste0("w <span style='font-weight: bold; color: #00ced1;'> ", bestOffer$dostawca[1], " </span>")},
                       "zaoszczędziłbyś <span style='font-weight: bold; color: magenta;'>",
                       round(bestOffer$saving[1], 2), " zł",
                       "</span> w minionym roku.", 
                if(nrow(bestOffer) > 1) {
                  paste0("<br/><br/><br/>",
                    "Jeśli miałbyś plan taryfowy",
                    "<span style='font-weight: bold; color: #40e0d0;'> \"", bestOffer$taryfa[2], "\" </span>",
                    if(bestOffer$dostawca[2] != currentProvider) {paste0("w <span style='font-weight: bold; color: #00ced1;'> ", bestOffer$dostawca[2], " </span>")},
                    "zaoszczędziłbyś <span style='font-weight: bold; color: magenta;'>",
                    round(bestOffer$saving[2], 2), " zł",
                    "</span> w minionym roku.")
                }, 
                "<br/><br/></div>"))
  },
  if(nrow(bestOffer) > 0) {
    imageOutput("zdjecie3", width = "20%", height = "100px", inline = TRUE)
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
                       choices = c("ostatni tydzień" = 1,
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
              choices = c("ostatni dzień" = 1,
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
    data <- generateSummary(input$dates[1], input$dates[2])
    
    y_saving <- data %>% 
      filter(dostawca == currentProvider, taryfa == currentTariff) %>% 
      pull(koszt)
    
    data <- data %>% 
      filter(dostawca %in% input$operators)
    
    data$dostawca <- as.factor(data$dostawca)
    #data$taryfa <- as.factor(data$taryfa)
    x_base <- which(levels(data$dostawca) == currentProvider) # currentProvider
    aux <- data %>% filter(dostawca == currentProvider) %>% nrow()
    # dla aux = 2 troche slaby algorytm
    if(aux == 2){
      if(currentTariff == "Nocna") x_base <- x_base - 0.125
      if(currentTariff == "Standardowa") x_base <- x_base + 0.125 # ?? zalozenie ze nie ma weekendowej
      if(currentTariff == "Weekendowa") x_base <- x_base + 0.125
    }
    if(aux == 3){
      if(currentTariff == "Nocna") x_base <- x_base - 0.25
      if(currentTariff == "Weekendowa") x_base <- x_base + 0.25
    }
    
    ggplot(data, aes(x = dostawca, y = koszt, fill = taryfa, label = difference, color = kolor)) +
      geom_bar(stat = "identity", width = 0.7, position = position_dodge(width = 0.8)) + 
      geom_hline(yintercept = y_saving, linetype = "dashed") +
      annotate(
        "text",
        x = x_base + 0.02, y = y_saving * 1.2,
        label = "Twoja taryfa",
        vjust = 0, hjust = 0, size = 6.5, color = "grey20",
        fontface = "bold"
      ) +
      annotate(
        "curve",
        x = x_base, y = y_saving * 1.2,   
        xend = x_base, yend = y_saving * 1.09, 
        arrow = arrow(length = unit(0.4, "cm"), type = "closed"),
        color = "grey20"
      ) +
      geom_text(position = position_dodge(width = 0.8), 
                size = 7, vjust = -0.25, show.legend = FALSE) +
      scale_colour_manual(values = c("green" = "darkgreen", "red" = "#d41b56", "black" = "#000000")) +
      theme(panel.grid.major.y = element_line(color = "lightgray"),
            panel.grid.major.x = element_blank(),
            panel.background = element_blank(),
            text = element_text(size = 25),
            axis.title.x = element_blank()
      ) +
      ylab("Opłata [zł]") +
      scale_fill_manual(name = "Taryfa", 
                        values = c("Standardowa" = "#ffff99", "Nocna" = "#beaed4", "Weekendowa" = "#80b1d3")) +
      guides(color = FALSE)
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