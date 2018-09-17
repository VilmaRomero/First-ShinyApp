library(shiny)
library(dplyr)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$text1 <- renderText({ 
    paste(input$distribution,"Distribution")
  })
   
  output$Distribution_Plot <- renderPlot({
    
    # Parameters from ui.R
    p <- input$p
    n <- input$n
    r <- input$r
      
    # Probability Distribution  
    # dist_name <- input$distribution
    bernoulli <- data_frame(x = 0:1, y = c(1-p,p))
    binomial <- data_frame(x = 0:n, y = dbinom(x,n,p) )
    
    # Number of failures until first success
    geometric <- data_frame(x = 0:n, y = dgeom(x,p) ) 
    negative_binomial <- data_frame(x = 0:n, y = choose(x+r-1,r-1)*(p^r)*((1-p)^x) )
  
    # draw the probability distribution
    library(ggplot2)

    if(input$distribution=="Bernoulli")	{
    ggplot(bernoulli,aes(x=x, ymax=y, ymin=0)) +
      geom_linerange(size=3,color=rgb(139/255,28/255,98/255,0.6)) +
      geom_point(aes(y = y),color=rgb(139/255,28/255,98/255,0.6),size=2) +
      scale_x_continuous(breaks = 0:2) +
      scale_y_continuous(limits = c(0,1),breaks = seq(0,1,by=0.1)) +
      labs(x = "X", y = "P(x)", 
           title = "X = Number of successes in one Bernoulli trial, X ~ B(1,p)") +
      theme_light()} else{if(input$distribution=="Binomial")	{
        
        ggplot(binomial,aes(x=x, ymax=y, ymin=0)) +
          geom_linerange(size=3,color=rgb(139/255,28/255,98/255,0.6)) +
          geom_point(aes(y = y),color=rgb(139/255,28/255,98/255,0.6),size=2) +
          scale_x_continuous(breaks = 0:n) +
          scale_y_continuous(limits = c(0,1),breaks = seq(0,1,by=0.1)) +
          labs(x = "X", y = "P(x)", 
               title = "X = Number of successes in n Bernoulli trials, X ~ B(n,p)") +
          theme_light()} else{ if(input$distribution=="Geometric")	{
            
            ggplot(geometric,aes(x=x, ymax=y, ymin=0)) +
              geom_linerange(size=3,color=rgb(139/255,28/255,98/255,0.6)) +
              geom_point(aes(y = y),color=rgb(139/255,28/255,98/255,0.6),size=2) +
              scale_x_continuous(breaks = 0:n) +
              scale_y_continuous(limits = c(0,1),breaks = seq(0,1,by=0.1)) +
              labs(x = "X", y = "P(x)", 
                   title = "X = Number of failures until the first success, X ~ G(p)")+
              theme_light()
          } else{
            ggplot(negative_binomial,aes(x=x, ymax=y, ymin=0)) +
              geom_linerange(size=3,color=rgb(139/255,28/255,98/255,0.6)) +
              geom_point(aes(y = y),color=rgb(139/255,28/255,98/255,0.6),size=2) +
              scale_x_continuous(breaks = 0:n) +
              scale_y_continuous(limits = c(0,1),breaks = seq(0,1,by=0.1)) +
              labs(x = "X", y = "P(x)", 
                   title = "X = Number of failures until r successes are reached, X ~ BN(r,p)") +
              theme_light()
          }}}
  })
  
   #  # Probability Distribution Table
    output$Probability_Distribution <- DT::renderDataTable({
      
      if(input$show_data){
      # Parameters from from ui.R
      p <- input$p
      n <- input$n
      r <- input$r
      
      if(input$distribution=="Bernoulli"){
        data_frame(x = 0:1, y = c(1-p,p))
      } else {if(input$distribution=="Binomial") {
        data_frame(x = 0:n, y = dbinom(x,n,p))} 
        else{if(input$distribution=="Geometric"){
          data_frame(x = 0:n, y = dgeom(x,p))
        }else{data_frame(x = 0:n, y = choose(x+r-1,r-1)*(p^r)*((1-p)^x) )
          }
        }
      }
      }
      },rownames = FALSE,
      options = list(pageLength = 5))
})
