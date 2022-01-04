# import library
library(shiny)
library(ggbiplot)
library(FactoMineR)
library(factoextra)
library(DT)
library(shinybusy)
tags$head(
  # Note the wrapping of the string in HTML()
  tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      body {
        background-color: black;
        color: white;
      }
      h2 {
        font-family: 'Yusei Magic', sans-serif;
      }
      .shiny-input-container {
        color: #474747;
      }"))
)
linebreaks <- function(n){HTML(strrep(br(), n))}

shinyUI(
  
  navbarPage("Iris Dataset Explorer",
        tabPanel("Intro",
        
          mainPanel(
            
        
            actionButton("play", "Play GIF"),
            actionButton("stop", "Stop GIF"),  
            h1("HW4 - NCCU DataScience "),
            h2("110153140 張立暘"),
            br(),
            use_busy_gif(src = "data/meme.gif", position ="free" ,height = 300, width = 300),            

            linebreaks(18),
            
            
            h3("
               I try some plot method to show the iris dataset, and do the anlysis of pca and ca.
               "),
            h3("
              You can navagation these pages by upper tab  !!!
               ")
          )
        ),
        tabPanel("Raw Data",
                 column(2,
                        h1("The raw data"),
                        p("Iris flower dataset is a multivariate data set introduced by  Ronald Fisher in his 1936 paper.
                    If you'd like to know more, read more detail with the official website via",
                        span(tags$a(href="https://archive.ics.uci.edu/ml/datasets/iris", "here."))),
                        htmlOutput("picture")
                        
                        ),
                        
                 column(10, DT::dataTableOutput("contents", height = "100%"))                 

        ),  
        tabPanel("Explorer Data",
                 sidebarPanel(
                   
                   sliderInput(inputId='num',
                               label='Number of observations to view:',
                               min=1,
                               max=nrow(iris),
                               value=60,
                               step=5),
                   
                   selectInput(inputId='x_lab',
                             label='X attribute:',
                             choices=c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width'),
                             selected='Sepal.Length'
                             ),
                 
                   selectInput(inputId='y_lab',
                             label='Y attribute:',
                             choices=c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width'),
                             selected='Sepal.Width'
                             )
                ),                 
                 mainPanel(
                    
                   fluidRow(
                     column(6, h3('Scatter Plot'),h4("render by x, y attribute"),plotOutput('scatter_plot')),
                     
                     column(6,h3('Box Plot'),h4("render by y attribute"),plotOutput('box_plot')),

                    ),
                   
                   fluidRow(
                     column(6, h3('Histogram Plot'),h4("render by x attribute"),plotOutput('hist_plot')),
                     
                     column(6,h3('Volcano Plot'),h4("render by x attribute"),plotOutput('volcano_plot')),
                     
                   )
              
                )
                 
        ),
        tabPanel("PCA Analysis",
                 sidebarPanel(
                   
                   sliderInput(inputId='obs',
                               label='Number of observations to view:',
                               min=1,
                               max=nrow(iris),
                               value=15,
                               step=1),
                   selectInput(inputId='PCA_X',
                               label='PCA plot X variable:',
                               choices=c('PC1','PC2','PC3','PC4')),
                   selectInput(inputId='PCA_Y',
                               label='PCA plot Y variable:',
                               choices=c('PC1','PC2','PC3','PC4'),
                               selected='PC2'),
                   
                   selectInput(inputId='CA_X',
                               label='CA plot X variable:',
                               choices=c('Dim1','Dim2','Dim3')),
                   selectInput(inputId='CA_Y',
                               label='CA plot Y variable:',
                               choices=c('Dim1','Dim2','Dim3'),
                               selected = "Dim2"),
                   selectInput(inputId='CA_label',
                               label='CA plot label:',
                               choices=c('all','row','col'),)
                 ),
                 mainPanel(
                   h3('Log Iris Data PCA Plot'),
                   plotOutput('pca_plot'),
                   br(),
                   h3('Iris Data CA Biplot'),
                   plotOutput('ca_plot')
                 )
        ),                
))