install.packages('tidyverse')

library(stringr)
library(magrittr)
library(lubridate)
library(dplyr)
library(readxl)
library(readr)
library(ggplot2)
library(corrplot)

###Loading shiny package to show all visualisations in interactive shiny app.
library(shiny)
##Import file
online <- read_xlsx('online_popularity_data.xlsx')
names(online)
online <- as_tibble(online)
## Select subsets by data channels

lifestyle <- online %>% filter(data_channel_is_lifestyle==1)
entertainment <- online %>% filter(data_channel_is_entertainment==1)
bus <- online %>% filter(data_channel_is_bus==1)
socmed <- online %>% filter(data_channel_is_socmed==1)
tech <- online %>% filter(data_channel_is_tech==1)
world <- online %>% filter(data_channel_is_world==1)

##Add column' channel' in each channel dataset
lifestyle1 <- lifestyle %>% mutate('channel'= rep('lifestyle', nrow(lifestyle)))
entertainment1 <- entertainment %>% mutate('channel'= rep('entertainment', nrow(entertainment)))
bus1 <- bus %>% mutate('channel' = rep('bus', nrow(bus)))
socmed1 <- socmed %>% mutate('channel'= rep('socmed', nrow(socmed)))
tech1 <- tech %>% mutate('channel'= rep('tech', nrow(tech)))
world1 <- world %>% mutate('channel' = rep('world', nrow(world)))

##Combine 
mydata<- rbind(lifestyle1, entertainment1, bus1, socmed1, tech1, world1)
### Create shinyApp


ui <- fluidPage(
  titlePanel('Explore the impact of article properties'),
  fluidRow(column(3,
                  ###Create sidebar panel to select different channels
                  wellPanel(selectInput('channel','Select channel',
                                        c('All data','Lifestyle',
                                             'Entertainment',
                                             'Bus' ,
                                             'Socmed',
                                             'Tech' ,
                                             'World'))
                            #sliderInput('top','Choose a number of top highest share',
                                        #value = 10, min = 1, max= 100)
                  ),
                  #Select article properties
                  wellPanel(selectInput('properties','Investigating the following properties',
                                         c('Number of tokens in the title',
                                           'Number of tokens in the content', 
                                           'Number of links',
                                           'Number of images',
                                           'Number of videos', 'Was the article pulished on the weekend'	))),
                  #Create a statistic table for shares of each channel
                  wellPanel(tableOutput('summary'))),
           column(9,
                  tabsetPanel(
                    tabPanel('Data',
                             mainPanel(plotOutput('sct0'),'Top 10 percent highest shares',
                                       tableOutput('tab')
                            )),
                    
                    tabPanel('Visualization',
                             mainPanel(
                                    plotOutput('sct1'),
                                    plotOutput('plot'))
                             
                             ),
                    tabPanel('Coefficient',
                             plotOutput('cor'),
                             tableOutput('cor_tab'))
                    )
                  )
  )
)

server <- function(input, output) {
  ##Input
  data <- reactive({
  a<-  switch (input$channel,
            'Lifestyle' = lifestyle,
            'Entertainment' = entertainment,
            'Bus' = bus,
            'Socmed' = socmed,
            'Tech' = tech,
            'World' = world,
            'All data'= mydata
    )
  a %>% select(-c(1:4,18:23))
  })
  
  property <-reactive({
    switch(input$properties,
           'Number of tokens in the title'= data()$n_tokens_title,
           'Number of tokens in the content'= data()$n_tokens_content,
           'Number of links' = data()$num_hrefs,
           'Number of images'= data()$num_imgs,
           'Number of videos' = data()$num_videos,
           'Was the article pulished on the weekend' = data()$is_weekend
           ) 
  })
  #Create a statistic table for shares of each channel
  output$summary <- renderTable(
    mydata %>% group_by(channel) %>%
                    summarise('min'= min(shares),
                           'max'= max(shares),
                           'median'= median(shares),
                           'mean'= mean(shares))
  )
  
 ##Scatter plot of the entire dataset (mydata)
  #shows relationship between shares and each ariticle property
  output$sct0 <- renderPlot({
    ggplot(mydata) + geom_point(aes(x= property(), y= shares, color= channel, alpha =0.5)) + 
      xlab(input$properties)
  })
  
  ##Boxplot for shares group by article properties
  output$plot <- renderPlot({
    
  
    ggplot(data(), aes(y=shares,x=factor(property()))) +geom_boxplot() 
   
    })
  ##Show highest 10% share data
  output$tab <- renderTable({
  data<- slice(data() %>% arrange(desc(shares)),1:floor(nrow(data())*0.1))
  cbind(id= c(1:floor(nrow(data())*0.1)), data)
  })
  
  output$sct1 <- renderPlot({
   
    ggplot(data(), aes(y = shares, x= property())) +geom_point(colour= 'orange', alpha=0.5) +
      geom_smooth()+
      xlab(input$properties)
  })
  
  #
  output$sct2 <- renderPlot({
  ggplot(data() ) +geom_point(aes(x= n_tokens_content, y= shares), col= 'yellow', alpha= 0.5) 
   
  })
  
  output$cor <- renderPlot({
   corrplot(cor(data() %>% select(shares, n_tokens_title,
                                  n_tokens_content, num_hrefs,
                                  num_imgs,
                                  num_videos, is_weekend)
               ),  method = 'number', col='orange')
  })
  
  output$cor_tab <- renderTable({
    cor(data() %>% select(shares, n_tokens_title,
                          n_tokens_content, num_hrefs,
                          num_imgs,
                          num_videos, is_weekend))
  })
 
}


shinyApp(ui, server)

