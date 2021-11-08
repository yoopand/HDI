library(shiny)
library(shinythemes)
library(plotly)


setwd("C:\\Users\\korawik\\Desktop\\MED-3\\Datviz\\HDI\\HDI datasets\\")

HDI <- read.csv('HDI_total_edit.csv',header = TRUE)
income <- read.csv('HDI_Income.csv',header = TRUE)
life_expactancy <- read.csv('HDI_lifeExpactancy.csv',header = TRUE)
education <- read.csv('HDI_edu.csv',header = TRUE)  

# Define UI

# Define UI for app that draws a histogram ----
ui <- 
    fluidPage(
        theme = shinytheme("cerulean"),
        navbarPage("HDI",
                   tabPanel("SummaryPlot",
                            # Sidebar layout with input and output definitions ----
                            sidebarLayout(
                                
                                # Sidebar panel for inputs ----
                                sidebarPanel(
                                    
                                    selectInput(inputId = "country_selected",
                                                label="Select Country",
                                                choices = c(HDI$country),
                                                selected = "Thailand"),
                                ),      
                                # Main panel for displaying outputs ----
                                mainPanel(
                                    
                                    # Output: Histogram ----
                                    plotlyOutput(outputId = "distPlot")
                                    
                                )
                            )
                   ),
                   
                   tabPanel("Education Ranking",
                            
                            plotlyOutput(outputId = "eduPlot")
                            
                   ),
                   
                   tabPanel("Life Expectancy Ranking",
                            
                            plotlyOutput(outputId = "LifePlot")
                            
                   ),
                   
                   tabPanel("Income Ranking",
                            
                            plotlyOutput(outputId = "IncomePlot")
                            
                   )
                   
        )
    )


# Define server logic required to draw a histogram ----
server <- function(input, output) {
    library(ggplot2)
    library(dplyr)
    library(plotly)
    library(tidyverse)
    
    index_type <-c(HDI,education,income,life_expactancy)
    
    output$distPlot <- renderPlotly({
        
        
        
        pt<-ggplot() +
            geom_line(data = HDI%>%filter(country==input$country_selected),      aes(x = Year, y = HDI), color = "grey",alpha=0.6,size = 3,labels="HDI")+ # must include argument label "data"
            
            geom_line(data = education%>%filter(country==input$country_selected),aes(x = year, y = EI), color = "#009999")+         #green
            
            geom_line(data = income%>%filter(country==input$country_selected),   aes(x = year, y = IncomeIndex), color = "#4393C3")+         #blue
            
            geom_line(data = life_expactancy%>%filter(country==input$country_selected), aes(x = year, y = LEI), color = "#B2182B")+ #red
            
            labs(title="HDI comparison",
                 #subtitle = " HDI(grey)=Human Development Index\n EI(green)=Education Index\n LEI(red)=Life Expactancy Index\n II(blue)=Income Index",
                 y="INDEX")+
            expand_limits(x = 1990, y = c(0.0,1.0))
        
        #pt<-ggplotly(pt)
        
        pt
        
    }) 
    
    output$eduPlot <- renderPlotly ({
        
        eduPlot <- ggplot() +
            geom_line(data=education, aes(x=year,y=EI,color=country,frame=ranking))+ 
            expand_limits(x = 1990, y = c(0.0,1.0))
        
        eduPlot
        
    })  
    
    output$LifePlot <- renderPlotly ({
        
        LifePlot <- ggplot() +
            geom_line(data=life_expactancy, aes(x=year,y=LEI,color=country
                                                ,frame=ranking)) +
            expand_limits(x = 1990, y = c(0.0,1.0))
        
        LifePlot
        
    }) 
    
    output$IncomePlot <- renderPlotly ({
        
        IncomePlot <- ggplot() +
            geom_line(data=income, aes(x=year,y=IncomeIndex,color=country
                                       ,frame=ranking)) +
            expand_limits(x = 1990, y = c(0.0,1.0))
        
        IncomePlot
        
    }) 
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
