# LAB 2
# TIM ZHOU, MIKAELA HOFFMAN-STAPLETON, VINCENT RIDEOUT, ARDA AYSU, ZEFENG ZHANG

#### PLEASE RUN IN FULL-SCREEN MODE ####

library(shiny)
library(ggplot2)
library(data.table)

### DEFINES THE GGPLOT THEME
THEME <- theme(
  panel.background = element_rect(fill = '#f9f9f9'),
  
  panel.grid.major = element_line(size = 0.3, color = 'grey70'),
  panel.grid.minor = element_line(size = 0.2, color = 'grey80'),
  
  plot.title = element_text(size = 18, margin = margin(b = 35),
                            family = 'Helvetica',
                            face = 'bold',
                            hjust = 0.5),
  
  plot.subtitle = element_text(size = 16, margin = margin(b = 30),
                               family = 'Helvetica',
                               hjust = 0.5),
  
  axis.title.x = element_text(size = 18, margin = margin(t = 30)),
  axis.title.y = element_text(size = 18, vjust = 10),
  
  axis.text.x = element_text(size = 14, margin = margin(t = 10)),
  axis.text.y = element_text(size = 14),
  
  axis.ticks = element_blank(),
  
  axis.line = element_line(size = 1, color = 'grey40'),
  
  legend.background = element_rect(fill = "white", color = "grey"),
  legend.margin = margin(0.5, 1, 0.5, 1, 'cm'),
  legend.position = c(0.91, 0.8),
  
  legend.title = element_blank(),
  legend.text = element_text(size = 14),
  legend.key = element_rect(fill = "white", colour = NULL),
  legend.key.width = unit(1, 'line'),
  
  plot.margin = unit(c(1, 1, 1, 1), 'cm')
)


server <- function(input, output, session) {
  
  output$testDirection <- renderUI({
    choices <- c('', 'one.sided', 'two.sided')
    names(choices) <- c('click for dropdown', 'one-sided', 'two-sided')
    selectizeInput(inputId = 'testDirection',
                   label = h5('I want to design a'),
                   choices = choices,
                   multiple = F,
                   selected = NULL)
  })
  
  output$testType <- renderUI({
    req(input$testDirection)
    
    choices <- c('', 'means', 'proportions')
    names(choices) <- c('click for dropdown',
                        'means (t-test)',
                        'proportions (z-test)')
    
    selectizeInput(inputId = 'testType',
                   label = h5('test of'),
                   choices = choices,
                   multiple = F,
                   selected = NULL)
  })
  
  output$controlAmount <- renderUI({
    req(input$testDirection)
    req(input$testType)
    
    numericInput(
      inputId = 'controlAmount',
      label = h5(paste('where the', gsub('s', '', input$testType), 'in control is')),
      step = 0.05,
      value = numeric(0)
    )
  })
  
  output$variance <- renderUI({
    req(input$testDirection)
    req(input$testType)
    
    if (input$testType == 'proportions') {
      return(NULL)
    }
    
    numericInput(
      inputId = 'variance',
      label = h5(paste('assuming an equal variance of')),
      value = numeric()
    )
  })
  
  output$allowRange <- renderUI({
    req(input$testDirection)
    req(input$testType)
    
    choices <- c('', 'none', 'siglvl', 'power', 'delta')
    names(choices) <- c('click for dropdown',
                        'none of the parameters',
                        'significance level',
                        'power',
                        'effect size')
    
    selectizeInput(inputId = 'allowRange',
                   label = h5('and allowing a range of values for'),
                   choices = choices,
                   multiple = F,
                   selected = NULL)
  })
  
  ### Whenever the user selects the quantity
  ### that could range, do everything inside
  ### the parentheses.
  
  ### In particular, this checks if the user
  ### wants a range to occur, then creates the
  ### UI elements appropriately
  
  observeEvent(input$allowRange, {
    output$siglvl <- renderUI({
      req(input$allowRange)
      if (input$allowRange == 'siglvl') {
        tagList(
          h5('The significance level should be between'),
          div(style = 'display: inline-block;'
              , numericInput(inputId = 'siglvlLower',
                             label = '',
                             value = 0.01
                             , width = '100px')),
          
          span(style = 'display: inline-block;
               text-align: center;
               vertical-align: middle;
               padding: 5px;', 'and'),
          
          div(style = 'display: inline-block;'
              , numericInput(inputId = 'siglvlUpper',
                             label = '',
                             value = 0.05
                             , width = '100px'))
          )
      } else {
        sliderInput(inputId = 'siglvlValue',
                    label = h5('The desired significance level is'),
                    min = 0.001, 
                    max = 0.999,
                    value = 0.05,
                    width = '100%')
      }
    })
    
    output$power <- renderUI({
      req(input$allowRange)
      
      if (input$allowRange == 'power') {
        tagList(
          h5('The test power should be between'),
          div(style = 'display: inline-block;'
              , numericInput(inputId = 'powerLower',
                             label = '',
                             value = 0.8
                             , width = '100px')),
          
          span(style = 'display: inline-block;
               text-align: center;
               vertical-align: middle;
               padding: 5px;', 'and'),
          
          div(style = 'display: inline-block;'
              , numericInput(inputId = 'powerUpper',
                             label = '',
                             value = 0.9
                             , width = '100px'))
          )
      } else {
        sliderInput(inputId = 'powerValue',
                    label = h5('The desired test power is'),
                    min = 0.001, 
                    max = 0.999,
                    value = 0.8,
                    width = '100%')
      }
    })
    
    output$effectSize <- renderUI({
      req(input$allowRange)
      if (input$allowRange == 'delta') {
        tagList(
          h5('The difference in', input$testType, 'is significant if it is between'),
          div(style = 'display: inline-block;'
              , numericInput(inputId = 'effectSizeLower',
                             label = '',
                             value = ifelse(input$testType == 'proportions', 0.05, numeric(0))
                             , width = '100px')),
          
          span(style = 'display: inline-block;
               text-align: center;
               vertical-align: middle;
               padding: 5px;', 'and'),
          
          div(style = 'display: inline-block;'
              , numericInput(inputId = 'effectSizeUpper',
                             label = '',
                             value = ifelse(input$testType == 'proportions',
                                            min(abs(c(1 - input$controlAmount, input$controlAmount))) / 2,
                                            numeric(0))
                             , width = '100px'))
          )
      } else {
        numericInput(inputId = 'effectSizeValue',
                     label = h5(paste('The difference in', input$testType, 'is significant if it is at least')),
                     value = ifelse(input$testType == 'means',
                                    input$variance,
                                    min(abs(c(1 - input$controlAmount, input$controlAmount)))) / 2,
                     width = '100%')
      }
    })
    
    output$submitButton <- renderUI({
      req(input$allowRange != '')
      
      actionButton(
        inputId = 'submitButton',
        label = 'Calculate!'
      )
    })
    
    output$sampleSize <- renderUI({})
    output$plots <- renderUI({})
  })
  
  ### When the user hits calculate, do everything that
  ### is inside the braces
  
  observeEvent(input$submitButton, {
    
    getSampleSize <- function(p1, p2, siglvl, power, delta, variance, direction) {
      tryCatch({
        if (input$testType == 'means') {
          N <- power.t.test(n = NULL, delta = delta, sd = sqrt(variance), sig.level = siglvl,
                            power = power, type = "two.sample", alternative = direction)$n
        } else {
          N <- power.prop.test(n = NULL, p1 = p1, p2 = p2, sig.level = siglvl, power = power,
                               alternative = direction)$n
        }
        print(N)
        return(ceiling(N))
      }, error = function(e) {
        return('error')
      }, finally = {})
    }
    
    ## nice easy way to store our plotting parameters
    plotParams <- reactiveValues()
    
    calcN <- reactive({
      if (input$testType == 'proportions') {
        validate(
          need(input$controlAmount <= 1 & input$controlAmount >= 0, paste('Please enter a valid proportion for control.'))
        )
      } else if (input$testType == 'means') {
        validate(
          need(input$controlAmount, paste('Please enter a valid', gsub('s', '', input$testType), 'for control.')),
          need(input$variance >= 0, paste('Please enter a valid variance for control.'))
        )
      }
      
      if (input$allowRange == 'siglvl') {
        validate(
          need(input$siglvlLower > 0, 'Please choose valid significance level range.'),
          need(input$siglvlLower <= input$siglvlUpper, 'Please choose valid significance level range.'),
          need(input$siglvlUpper < 1, 'Please choose valid significance level range.')
        )
        
        plotParams$xlab <- 'Significance Level'
        plotParams$xlim <- c(0.8*input$siglvlLower,
                             min(1.1, 1.25*input$siglvlUpper))
        
        plotParams$x <- seq(input$siglvlLower,
                            input$siglvlUpper, 0.01)
        
        ## calculate N using Mikaela's function			
        N <- sapply(plotParams$x,
                    function(i) {
                      getSampleSize(input$controlAmount, input$controlAmount + input$effectSizeValue,
                                    i, input$powerValue, input$effectSizeValue, input$variance, input$testDirection)[1]
                    })
        
      } else if (input$allowRange == 'power') {
        validate(
          need(input$powerLower > 0, 'Please choose valid significance level range.'),
          need(input$powerLower <= input$powerUpper, 'Please choose test power range.'),
          need(input$powerUpper < 1, 'Please choose valid significance level range.')
        )
        
        plotParams$xlab <- 'Power'
        plotParams$xlim <- c(0.8*input$powerLower,
                             min(1.1, 1.25*input$powerUpper))
        
        plotParams$x <- seq(input$powerLower,
                            input$powerUpper, 0.01)
        
        ## calculate N using Mikaela's function			
        N <- sapply(plotParams$x,
                    function(i) {
                      getSampleSize(input$controlAmount, input$controlAmount + input$effectSizeValue,
                                    input$siglvlValue, i, input$effectSizeValue, input$variance, input$testDirection)[1]
                    })
        
      } else if (input$allowRange == 'delta') {
        validate(
          need(input$effectSizeLower > 0, 'Please choose valid effect size range.'),
          need(input$effectSizeUpper > 0, 'Please choose valid effect size range.'),
          need(input$effectSizeLower <= input$effectSizeUpper, 'Please choose valid effect size range.')
        )
        
        plotParams$xlab <- 'Effect Size'
        plotParams$xlim <- c(0.8*input$effectSizeLower,
                             1.25*input$effectSizeUpper)
        
        if (input$testType == 'proportions') {
          plotParams$x <- seq(input$effectSizeLower,
                              input$effectSizeUpper,
                              0.01)
        } else {
          plotParams$x <- seq(input$effectSizeLower,
                              input$effectSizeUpper,
                              length.out = 50)
        }
        
        ## calculate N using Mikaela's function			
        N <- sapply(plotParams$x,
                    function(i) {
                      getSampleSize(input$controlAmount, input$controlAmount + i,
                                    input$siglvlValue, input$powerValue, i, input$variance, input$testDirection)[1]
                    })
        
      } else {
        validate(
          need(input$siglvlValue, 'Please choose valid significance level.'),
          need(input$powerValue, 'Please choose valid test power level.'),
          need(input$effectSizeValue, 'Please choose valid effect size level.')
        )
        ## calculate N using Mikaela's function
        N <- getSampleSize(input$controlAmount, input$controlAmount + input$effectSizeValue,
                           input$siglvlValue, input$powerValue, input$effectSizeValue, input$variance,
                           input$testDirection)[1]
        
      }
    })
    
    if (input$allowRange == '') {
      return() ## if they didn't select a range option, do nothing
      
    } else if (input$allowRange == 'none') { ## if no parameters are allowed a range
      output$sampleSize <- renderUI({
        N <- calcN()
        
        if (N != 'error') {
          display <- h4(paste0('Your sample size needs to be at least ', N[1], '.'),
                        style = 'margin-top: 15%; font-size: 200%;',	
                        align = 'center')
        } else {
          display <- h4('Any sample size will suffice.',
                        style = 'margin-top: 5%',
                        align = 'center')
        }
        display	
      })
      
      output$plots <- renderPlot({})
      
    } else { ## if they allowed one of the parameters to have a range
      
      output$sampleSize <- renderUI({
        req(input$submitButton)
        N <- calcN()
        
        if (is.numeric(N[1]) & N[1] > 1) {
          if (length(unique(N)) > 1) {
            display <- h4(paste0('The minimum sample size needed will be between ',
                                 min(N), ' and ', max(N), '.'),
                          align = 'center')
          } else {
            display <- h4(paste0('The minimum sample size needed is ', max(N), '.'),
                          align = 'center')
          }
        } else {
          display <- h4('Any sample size will suffice.', align = 'center')
        }
        display
      })
      
      output$plots <- renderPlot({
        req(input$submitButton)
        N <- calcN()
        
        if (is.numeric(N[1]) & N[1] > 1 & length(unique(N)) > 1) {		
          ylab <- 'Minimum Sample Size Needed'
          ylim <- c(0, round(1.2*max(N)))
          
          data <- data.frame(x = plotParams$x, y = N)
          
          p <- ggplot(data = as.data.frame(data),
                      aes(x = x, y = y)) +
            
            xlab(plotParams$xlab) +	
            xlim(plotParams$xlim) +	
            
            ylab(ylab) +
            ylim(ylim) +
            
            ggtitle(paste('Minimum Sample Size Needed as Function Of', plotParams$xlab)) +
            
            geom_line(color = 'dodgerblue',
                      size = 0.5) +
            geom_point(color = 'dodgerblue',
                       size = 3) +
            
            THEME
        } else {
          p <- NULL
        }
        p
      })
    }
  })
}

ui <- fluidPage(
  titlePanel(strong('A/B Test Sample Size Calculator')),
  
  br(),
  
  sidebarLayout(
    sidebarPanel(
      tags$style(" .well { background-color: white;
                 border: 0px;
                 webkit-box-shadow: none;
                 box-shadow: none; } "),
      
      uiOutput(outputId = 'testDirection')
      , uiOutput(outputId = 'testType')
      , uiOutput(outputId = 'controlAmount')
      , uiOutput(outputId = 'variance')
      , uiOutput(outputId = 'allowRange')
      
      , width = 3
      ),
    
    mainPanel(
      br(),
      fluidRow(
        br(),
        column(3, uiOutput(outputId = 'siglvl'), align = 'center', offset = 0),
        column(3, uiOutput(outputId = 'power'), align = 'center', offset = 1),
        column(3, uiOutput(outputId = 'effectSize'), align = 'center', offset = 1)
      ),
      br(),
      fluidRow(
        column(1, offset = 5, uiOutput(outputId = 'submitButton'))
      ),
      fluidRow(
        column(1,
               uiOutput(outputId = 'sampleSize'),
               plotOutput(outputId = 'plots',
                          height = '500px',
                          width = '100%'),
               style = "width: 80%;
               padding-top: 3%;
               padding-left: 10%;")
        ),
      width = 9
        )
    )
    )

shinyApp(ui = ui, server = server)
