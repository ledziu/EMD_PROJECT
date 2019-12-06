library(reshape2)
library(dplyr)
library(tidyr)
library(knitr)
library(ggplot2)
library(plotly)
library(corrplot)
library(shiny)
library(reshape)
library(randomForest)


df <- read.csv("sledzie.csv",header=TRUE,colClasses = c("integer","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","integer","numeric"),na.strings = "?")

df <- data.frame(df)
df_no_na <- na.omit(df)



herring_length <- df_no_na$length

n <- 50
grouped_by_n <- tapply(herring_length, rep(seq_along(herring_length), each = n, length.out = length(herring_length)), mean)
num_of_groups <- length(grouped_by_n)

ui <- fluidPage(
  titlePanel("Wielkość śledzia"),
  sidebarLayout(    
    sidebarPanel(
      h3(
        strong("", style = "font-si24pt")),
      p(""),
      sliderInput("slider", label = "", min = 0, max = num_of_groups, value = 200),
      uiOutput('logo')
    ),
    mainPanel(
      plotOutput("distPlot")
    ) 
  )
)

server <- function(input, output, session) {
  output$logo <- renderUI({
    img(src = "https://www.ilegotowac.pl/img/0626/f.jpg", width = as.integer(input$slider))
  })
}

runApp(list(ui = ui, server = server))
