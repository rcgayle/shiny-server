library(shiny)
source('samphist.r')
ui<-fluidPage(tags$h1('Distribution of Sample Means'),
      fluidRow(column(4,
                sliderInput(
                   inputId='min',
                   label='Population Min:',
                   value=40, min=0, max=100),
                sliderInput(
                   inputId='max',
                   label='Population Max:',
                   value=60, min=0, max=100),
                sliderInput(
                   inputId='n',
                   label='Sample Size:',
                   value=30, min=1, max=400),
                 sliderInput(
                   inputId='N',
                   label='Number of Samples:',
                   value=200, min=20, max=2000)),
               column(8, plotOutput('means'))),
               fluidRow(
               column(1, 
                    actionButton(inputId='go', label='Reset')),
               column(11, tags$p('The app creates a "population" of 1,000,000 random observations from the distribution Unif[min, max].  The histogram is the distribution of the sample means for the selected number of samples and sample size from the population along with an approximating normal pdf. Hit "Reset" to start and to see the histogram after each adjustment of values.')))
               )

server<-function(input, output){
               pop<-eventReactive(input$go,{runif(1000000, input$min, input$max)})
               n<-eventReactive(input$go, {input$n})
               N<-eventReactive(input$go, {input$N})
               output$means<-renderPlot({
                   samphist(pop(),n(),N())
                                        })
                                }
shinyApp(ui=ui, server=server)



