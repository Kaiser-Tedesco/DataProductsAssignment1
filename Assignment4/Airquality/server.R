library(shiny)
data("airquality")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  # Model 1
  air<-na.omit(airquality)
  ozone<-air$Ozone
  Temp<-air$Temp
  model1<-lm(ozone~Temp)
  # Predict using slider input.
  model1pred<-reactive({
    TempInput<-input$sliderTemp
    predict(model1, newdata=data.frame(Temp=TempInput))
  })
  
  # Plot the data.
  output$plot1<-renderPlot({
    TempInput<-input$sliderTemp
    plot(airquality$Temp, airquality$Ozone, xlab = "Temperature", ylab = "Ozone", bty="n", pch=16)
    points(TempInput, model1pred(), col="red", pch=13, cex=3)
  })
  
  # 4
  output$pred1<-renderText({
    model1pred()
  })
  
})