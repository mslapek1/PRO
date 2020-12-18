#NOT TO RUN
#########################
#begining

data <- read.csv("domek_we_francji.csv")
names(data)[2] <-"time"
data$time2 <- as.POSIXct(data$time, format = "%Y-%m-%d")

#agregation by day
data_to_model <- data
data_to_model$day <- yday( data$time2)

data_to_model <- data_to_model %>% group_by( day) %>%
  summarise( time = min(time2), use..kW. = sum( use..kW.))

#day model
#zamiana dni na wartosc cykliczna (+14 poniewaz wedlg modelu najwieksze zuzycie jest ~14 stycznia)
data_to_model$day_in_cos <- cos( (data_to_model$day +14 -1) /366 *2*pi)

n <- nrow( data_to_model)
#for train not using pre-last and last row
model_day_l <- lm(use..kW.~day_in_cos,data_to_model[ -c(n-1,n) ,])
#model_next_day_l <- lm(use..kW.~day_in_cos,data_to_model)

data_to_model$pred_day <- predict(model_day_l, data_to_model[,c("day_in_cos")])

#aggregation by day, week, month
data_to_model$week <- week(data_to_model$time)
data_to_model$month <- month(data_to_model$time)

data_model_day <- data_to_model %>% select( day, time, use..kW., pred_day)
names(data_model_day)[4] <- "prediction"

data_model_week <- data_to_model %>% group_by( week) %>%
  summarise( time = min(time), use..kW. = sum( use..kW.), prediction = sum( pred_day))

data_model_month <- data_to_model %>% group_by( month) %>%
  summarise( time = min(time), use..kW. = sum( use..kW.), prediction = sum( pred_day))



#########################
#server


#wybor odpowiedniego modelu
data_model <- reactive({
  #dzien
  if (input$dates2==1) {
    data_model <- data_model_day
  }
  #tydzien
  else if (input$dates2==2) {
    data_model <- data_model_week
  }
  #miesiac
  else {
    data_model <- data_model_month
  }
})


##### Progres zużycia prądu
#GGPLOT DO POTRAWY TODO
output$plot2 <- renderPlot({
  ggplot( tail(data_model(),10), aes( time),
          main="model in red") +
    geom_point( aes( y= use..kW.)) +
    geom_point( aes( y= prediction), color="red")
})


# WAŻNE
n <- reactive( nrow(data_model()))
weekPart <- reactive( data_model()$use..kW.[n()-1] / data_model()$use..kW.[n()-2] )
predPart <- reactive( data_model()$use..kW.[n()-1] / data_model()$prediction[n()-1] )
# weekPart() - jaką część energii zużytej w POPRZEDNIM dniu/tygodniu/roku stanowi energia zużyta w tym d./t./r.
# predPart() - jaką część energii PRZEWIDZIANEJ do zużycia stanowi energia zużyta w tym d./t./r.

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
  tags$p(HTML(paste0("W ubiegłym ", okres(), " zużyłeś ",
                     ileEnergii(weekPart()),
                     " w poprzednim ", okres(), " i ",
                     ileEnergii(predPart()),
                     " normalnie.",
                     ifelse(weekPart()<=1 & predPart()<=1,
                            " Brawo, oby tak dalej :)", "")
  )))
})
