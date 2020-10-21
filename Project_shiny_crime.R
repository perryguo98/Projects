library(shiny)
library(httr)
library(jsonlite)
library(tidyverse)
library(DT)

#
#
# For reference on reactive code: https://mastering-shiny.org/basic-reactivity.html

get.data <- function(url) {
  
####################################################################
  resp<-GET(url)
  geysers.resp.text2 <- content(resp, "text", encoding = "UTF-8")
  geysers.resp.text_json2 <- fromJSON(geysers.resp.text2, flatten = TRUE)
  data.df <- as.data.frame(geysers.resp.text_json2)
  
  
  
  
  
####################################################################
  return(data.df)
}


####################################################################
cat.df<-get.data("https://data.police.uk/api/crime-categories")
categories<-cat.df$url
####################################################################


####################################################################
force.df<-get.data("https://data.police.uk/api/forces")
forces<-force.df$id
####################################################################

ui <- fluidPage(
  titlePanel("Lab: Crime Data"),
  h4("Request url"),
  # this component is just to see the request url.
  verbatimTextOutput("url.txt"),
  sidebarLayout(
  sidebarPanel(
     uiOutput("categories"),
     

####################################################################
  uiOutput("forces"),
   uiOutput("date"),

####################################################################
   ),
   
   mainPanel(
     plotOutput("outcome.cat.plot"),
     tableOutput("data.table")
   )
 )
)

server <- function(input, output, session) {
  # Renders (and re-renders) the selectInput "sel.cat".
  output$categories <- renderUI({
    selectInput('sel.cat', 'Crime Category', categories, selectize=FALSE)
  })
  

####################################################################
  output$forces <- renderUI({
    selectInput('sel.force', 'forces', forces, selectize=FALSE)
  })
  
  
####################################################################

  # Renders (and re-renders) the textInput "date".
  output$date <- renderUI({
    textInput("txt.date", "Date: yyyy-mm", "")
  })
  
  # This call to renderPrint assembles the request url based on user inputs. It prints it to the verbatimTextOutput called "url.txt".
  output$url.txt <- renderPrint(
    if(input$txt.date=="")
      paste("https://data.police.uk/api/crimes-no-location?category=",input$sel.cat,"&force=",input$sel.force, sep="")
    else
      paste("https://data.police.uk/api/crimes-no-location?category=",input$sel.cat,"&force=",input$sel.force,"&date=",input$txt.date, sep="")
  )
  
  # This code block assembles the request url with parameters based on user inputs: crime category,
  # force, and (optionally) a date in the form yyyy-mm. If the resulting dataframe is empty, the string "no results"
  # is returned.
  results <- reactive({
    if(input$txt.date=="")
      request<-paste("https://data.police.uk/api/crimes-no-location?category=",input$sel.cat,"&force=",input$sel.force, sep="")
    else
      request<-paste("https://data.police.uk/api/crimes-no-location?category=",input$sel.cat,"&force=",input$sel.force,"&date=",input$txt.date, sep="")
    data<- get.data(request)
    if(nrow(data)>0)
      return(subset(data, select=c("category", "month", "outcome_status.category",	"outcome_status.date")))
    else
      return("no results")
  })
  
  
  output$outcome.cat.plot<-renderPlot({
    data<-results()
    if(data!="no results") {
      count.df<-count(data, vars = outcome_status.category)
      count.top.df<-subset(count.df, ((count.df$n/nrow(data))*100) >= 10 )
      ggplot(count.top.df, aes(vars, n)) + geom_bar(stat = "identity")+ xlab("outcome category") + ylab("N") 
    }
    else
      "no results"
  })
  

####################################################################
output$data.table<-renderTable(results())
####################################################################
}

shinyApp(ui, server)