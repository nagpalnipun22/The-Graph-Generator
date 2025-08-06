# install.packages('shiny')
# install.packages('readxl')
# install.packages('ggplot2')
# install.packages('dplyr')
# install.packages('ggthemes')
library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(ggthemes)

cars <- read_xlsx(path = 'C:/Users/DELL/OneDrive/Desktop/R Programs/R Shiny/project 1/data/CARS.xlsx')

ui <- fluidPage(

  titlePanel('The Graph Generator'),
  sidebarLayout(
    sidebarPanel(
      selectInput('choose','Select the Origin',choices = unique(cars$Origin))
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.choose == 'Asia'",plotOutput('asia')),
      conditionalPanel(
        condition = "input.choose == 'Europe'",plotOutput('europe')),
      conditionalPanel(
        condition = "input.choose == 'USA'",plotOutput('usa')
      )
  )
)
)

server <- function(input, output, session) {
  
  summary_data <- cars %>% 
    select(Origin,Make,MSRP) %>% 
    group_by(Origin,Make) %>% 
    summarise(MSRP = mean(MSRP))

  asiadata <- summary_data %>%
    filter(Origin == 'Asia')
  
  eurdata <- summary_data %>%
    filter(Origin == 'Europe')
  
  usadata <- summary_data %>%
    filter(Origin == 'USA')
  
  asia <- asiadata
  asia$Make <- factor(asia$Make, levels = asia$Make[order(asia$MSRP)])
  
  europe <- eurdata
  europe$Make <- factor(europe$Make, levels = europe$Make[order(europe$MSRP)])
  
  usa <- usadata
  usa$Make <- factor(usa$Make, levels = usa$Make[order(usa$MSRP)])
  
  output$usa <- renderPlot({ggplot(usa,aes(Make,MSRP))+
      geom_bar(stat = 'identity')+
      geom_label(aes(Make,MSRP,label = round(MSRP)))+
      coord_flip()+
      theme_pander()+
      theme(plot.subtitle = element_text(face = 'bold',
                                         color = '#822b2b'),
            plot.caption = element_text(face = 'italic'))+
      labs(title = 'Mean MSRP of Manufacturers in USA',
           subtitle = 'Mean MSRP V/S Make',
           caption = 'Source : dataset sourced from sashelp.cars dataset from SAS Studio',
           x = 'Manufacturers in USA',
           y = 'Mean MSRP')})
  
  output$europe <- renderPlot({ggplot(europe,aes(Make,MSRP))+
      geom_bar(stat = 'identity')+
      geom_label(aes(Make,MSRP,label = round(MSRP)))+
      coord_flip()+
      theme_pander()+
      theme(plot.subtitle = element_text(face = 'bold',
                                         color = '#822b2b'),
            plot.caption = element_text(face = 'italic'))+
      labs(title = 'Mean MSRP of Manufacturers in Europe',
           subtitle = 'Mean MSRP V/S Make',
           caption = 'Source : dataset sourced from sashelp.cars dataset from SAS Studio',
           x = 'Manufacturers in Europe',
           y = 'Mean MSRP')
  })
  
  output$asia <- renderPlot({ggplot(asia,aes(Make,MSRP))+
      geom_bar(stat = 'identity')+
      geom_label(aes(Make,MSRP,label = round(MSRP)))+
      coord_flip()+
      theme_pander()+
      theme(plot.subtitle = element_text(face = 'bold',
                                         color = '#822b2b'),
            plot.caption = element_text(face = 'italic'))+
      labs(title = 'Mean MSRP of Manufacturers in Asia',
           subtitle = 'Mean MSRP V/S Make',
           caption = 'Source : dataset sourced from sashelp.cars dataset from SAS Studio',
           x = 'Manufacturers in Asia',
           y = 'Mean MSRP')})
} 

shinyApp(ui = ui, server = server)