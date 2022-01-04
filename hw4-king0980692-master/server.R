library(shiny)
library(ggbiplot)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(gridExtra)
library(DT)
library(shinybusy)

packages <- c("ggplot2", "ggbiplot", "ggbiplot", "shinybusy", "gridExtra", "factoextra", "FactoMineR")

install.packages(setdiff(packages, rownames(installed.packages())))  

shinyServer(function(input,output,session){
  # process iris data 
  data(iris)
  
  
  the_data_fn <- reactive(iris)
  
  ir_summary<-do.call(cbind,lapply(iris[,1:4],summary))
  ir_log<-log(iris[,1:4])
  ir_species<-iris[,5]
  
  # process pca data
  ir.pca<-prcomp(ir_log,center=TRUE,scale=TRUE)
  pca.std<-ir.pca$sdev
  pca.var<-pca.std^2
  pca.pve<-pca.var/sum(pca.var)
  pca.cpve<-cumsum(pca.pve)
  # PCA PVE df
  pve<-data.frame(Std=pca.std,
                  Var=pca.var,
                  PVE=pca.pve,
                  CPVE=pca.cpve,
                  row.names=c('PC1','PC2','PC3','PC4'))
  
  # process CA data
  ir.ca<-CA(iris[,1:4],graph=FALSE)
  # CA PVE df
  eig<-as.data.frame(ir.ca$eig)
  colnames(eig)<-c('Eigenvalue','PVE','CPVE')
  
  # reactive df
  ir_summary.df<-reactive({ir_summary})
  ir.df<-reactive({iris[1:input$obs,]})
  pve.df<-reactive({pve})
  eig.df<-reactive({eig})
  
  # output plot PCA 
  output$pca_plot<-renderPlot({
    ggbiplot(ir.pca,obs.scale=2,var.scale=1,groups=ir_species,  circle = TRUE, 
     choices=c(as.numeric(substr(input$PCA_X,3,3)),as.numeric(substr(input$PCA_Y,3,3))))+
      scale_color_discrete(name='')+
      theme(legend.direction='horizontal',legend.position='top')

  })
  
  # outplot plot CA
  output$ca_plot<-renderPlot({
    fviz_ca_biplot(ir.ca,axes=c(as.numeric(substr(input$CA_X,4,4)),as.numeric(substr(input$CA_Y,4,4))),geom='text',label=input$CA_label)+
      labs(title='')
  })
  
  # output data table
  output$iris_summary_table<-renderTable({
    ir_summary.df()},
    rownames=TRUE)
  
  output$iris_table<-renderTable({
    head(ir.df(),n=input$obs)}
    ,rownames=TRUE)
  
  output$pve_table<-renderTable({
    pve.df()},
    rownames=TRUE)
  
  output$eig_table<-renderTable({
    eig.df()},
    rownames=TRUE)
  
  output$contents <-  DT::renderDataTable({
    #
    the_data_fn()
  })
  theme_update(text = element_text(size=18))
  
  output$scatter_plot = renderPlot({

    scatter <- ggplot(data=iris[1:input$num,], aes_string(x = input$x_lab, y = input$y_lab)) 
    ggplot(data=iris, aes_string(x = input$x_lab, y = input$y_lab)) 
    scatter + geom_point(aes(color=Species, shape=Species)) +
      xlab(input$x_lab) +  ylab(input$y_lab) +
      ggtitle("Scatter Plot")
  })
  output$box_plot = renderPlot({
    box <- ggplot(data=iris[1:input$num,], aes_string(x = "Species", y = input$y_lab))
    box + geom_boxplot(aes(fill=Species)) +
      labs( x = "Species") + ylab( input$y_lab ) + ggtitle("Iris Boxplot") +
      stat_summary(fun.y=mean, geom="point", shape=5, size=4) 
  })
  
  output$hist_plot = renderPlot({
    histogram <- ggplot(data=iris[1:input$num,],aes_string(x = input$x_lab))
    histogram + geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) + 
      xlab(input$x_lab) +  labs(y="Frequency") + ggtitle(paste("Histogram of ",input$x_lab))
  })
  
  output$volcano_plot = renderPlot({
    vol <- ggplot(data=iris[1:input$num,],aes_string(x = input$x_lab))
    vol + stat_density(aes(ymax = ..density..,  ymin = -..density.., 
                           fill = Species, color = Species), 
                       geom = "ribbon", position = "identity") +
    facet_grid(. ~ Species) + coord_flip() + xlab(input$x_lab) 
    
  })
  meme <- "images/meme.gif"  
  output$meme <- renderText(c('<img src="', meme,'">'))
  output$picture<-renderText({c('<img src="', "http://2.bp.blogspot.com/-bhCkc1RUYEo/VdExWgHpkmI/AAAAAAAAP80/ryQ8c5J7YDk/s1600/Iris_germanica_%2528Purple_bearded_Iris%2529%252C_Wakehurst_Place%252C_UK_-_Diliff.jpg"
,'width="240"','height="240"','">')})
  
  observeEvent(input$play, {
    play_gif()
  })
  
  observeEvent(input$stop, {
    stop_gif()
  })

})