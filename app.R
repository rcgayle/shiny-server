library(shiny)
source('distances.r')
source('multiplot.r')
ui <- fluidPage(
tags$style(type = "text/css",
             "h1 {color: darkred; font-weight: bold; font-size: 30px;}",
             "h4 {color: darkred; font-weight: bold; font-size: 16px;}",
             "h2 {color: darkred; font-weight: bold; font-size: 14px;}",
             "h3 {color: darkred; font: italic; font-size: 14px
                            font-weight: bold;}",
             "body {background-color: gainsboro;}",
             "label {font-size: 16px; color: darkred;
                            font-weight: bold}"),
    tags$head(tags$style(HTML(
            ".col-sm-4 { width: 100%;}
             .col-sm-8 { width: 100%;}
                 ",
            "#table1 {font-size: 17px; color: darkred; font-weight: bold;}",
            "#table2 {font-size: 17px; color: red; font-weight: bold;}",
            "#text1 {font-size: 20px;color: darkred; font-weight: bold;}",
            "#text2 {font-size: 20px;color: red; font-weight: bold;}"
                   ))),
    tags$h1('Use a Plot or Table to Filter a Data Frame'),
    mainPanel(
    column(3,
        fileInput("file1", "Choose CSV File"),
        selectInput("col1", "Select a Data Column", character(0)),
        selectInput("col2", "Select a Data Column", character(0)),
        actionButton(inputId='go1', label='Confirm Selections',
            style="color: darkred; font-size: 14px; font-weight: bold; 
                   background-color: white;"),
        hr(),                                             
        actionButton(inputId='go3', label='Reset Columns',
            style="color: darkred; font-size: 14px; font-weight: bold; 
                   background-color: white;"),
        hr(),
                                                     
        actionButton(inputId='go4', label='Upload New Data',
            style="color: darkred; font-size: 14px; font-weight: bold; 
                   background-color: white;")                                                
              ),
    column(9,
    tabsetPanel(
      tabPanel(tags$h4("Read.Me"),
         tags$h4('How This Works'),
         hr(),
         tags$h4("Step One: Upload a .csv file to an R dataframe using the button at top left. These data are shown as a table under the 'Orgininal Data' tab. The application supports the uploading only of .csv files."),
         tags$h4("Step Two: Using the drop-down menus at left, select two variables from the data frame.  Clicking on 'Confirm Selections' will produce a 'clickable' plot of the two variables under the 'Plot' tab above if either one is numeric. The first column selected is depicted horizontally, the second vertically. If neither is numeric, both will be coerced to factors and, instead of a plot, a contingency for the two factors will be shown. The first factor's levels are on the vertical left margin, the second's on the lower horizontal margin (Sorry; it has to do with R's table() function.)"),
         tags$h4("Step Three: If a plot is shown, by clicking on points in the plot, you can then select cases of interest. 'Save Clicked Cases' will subset the original dataframe to the clicked instances viewable as a table under the 'Filtered Data' tab above at right.  To select a different collection of cases, first clear the ones already selected. Note that, particularly if a discrete numeric variable is selected, a given point in the plot might correspond to multiple instances in the data frame.  All of these will be included in the subsetted data by clicking on that point.  If a contigency table is shown, clicking on cells in the contingency table will select cases with the corresponding combination of factor values.  It's possible to select multiple cells.  Since the various cells in the contingency table correspond to mutually exclusive subsets of the data frame, clicking on multiple cells effects the union of the corresponding subsets.  As is the case with plots, 'Save Clicked Cases' will subset the original dataframe and 'Clear Clicked Cases' will clear all selected instances."),
         hr(),
         tags$h4("This new dataframe is viewable as a table and downloadable under the tab 'Filtered Data' above at right. Yo can click immediately below to download a 'test' .csv file to use as fodder for the app."),
        hr(),
        downloadButton("downloadData1", "Download")
              ),
      tabPanel(tags$h4("Original Data"),
          hr(),
          verbatimTextOutput('text1'),
          tableOutput('table1')
              ),
      tabPanel(tags$h4("Plot/Table"),
          hr(),
          plotOutput("plot", click="plot_click", height='400px', width='100%'),
          hr(),
          fluidRow(
          actionButton(inputId='go2', label='Save Clicked Cases',
          style="color: darkred; font-size: 14px; font-weight: bold; 
                   background-color: white;"),
          actionButton(inputId='go5', label='Clear Clicked Cases',
          style="color: darkred; font-size: 14px; font-weight: bold; 
                   background-color: white;")
                   )
              ),
      tabPanel(tags$h4("Filtered Data"),
          verbatimTextOutput('text2'),
          tableOutput('table2'),
          tags$h4('Download Filtered Data Frame as a CSV File'),
          hr(),
          downloadButton("downloadData2", "Download")
               )
               )
            )
            
    ))

server <- function(input, output, session) {

  v<-reactiveValues(df1=data.frame(), df2=data.frame(), GO1=FALSE, GO2=FALSE,
        I=c(), J=c(), x=c(), y=c(), NC=NULL, IT=c(),  PT='',CIx=c(), 
        CIy=c(), TPx=c(), TPy=c(), n=c(), m=c(), xname='', yname='')
  
  
  testdf<-read.csv('testdf.csv')
  
  data <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    read.csv(inFile$datapath)
  })
  
  observeEvent(data(), {
    updateSelectInput(session, "col1", choices = names(data()))
    updateSelectInput(session, "col2", choices = names(data()))
    v$df1<-data()
                        }
              )
              
 output$text1<-renderText(
             if (dim(v$df1)[1]==0)
                  {'No data have been uploaded.'}
             else 
               {paste0('The data contain ', dim(v$df1)[1],' instances of ',
                          dim(v$df1)[2],' variables.')}
                          )
              
 observeEvent(input$go1, {
          req(data())
          v$x<-data()[[input$col1]][!is.na(data()[[input$col1]])]
          v$xname<-input$col1
          v$y<-data()[[input$col2]][!is.na(data()[[input$col2]])]
          v$yname<-input$col2
          v$GO1<-TRUE
          v$PT<-MultiPlot(v$x, v$y, v$xname, v$yname)
          if (v$PT=='C'){
               v$CIx<-FindColInd(input$col1, v$df1)
               v$CIy<-FindColInd(input$col2, v$df1)
               v$n<-length(levels(v$x))
               v$m<-length(levels(v$y))
               v$TPy<-rep(1:v$n, v$m)
               v$TPx<-c()
               for (j in 1:v$m){
                     v$TPx<-c(v$TPx, rep(j,v$n))
                               }
                        }
          else {}
            })
            
observeEvent(input$plot_click,{
     if (v$PT=='N'){
        v$NC<-input$plot_click
        v$IT<-ClkNearPt(v$NC, v$x, v$y)
         if (length(setdiff(v$I, v$IT))==length(v$I))
             v$I<-c(v$I, v$IT)     
         else {}
        for (i in length(v$I)){
            for (j in 1:length(v$x)){
                if (v$x[j]==v$x[v$I[i]] & v$y[j]==v$y[v$I[i]])
                         {v$J<-c(v$J, j)}
                else {}                                   
                                    }
                                 }
                    } 
      else if  (v$PT=='C'){
         v$NC<-input$plot_click
         v$IT<-ClkNearPt2(v$NC, v$TPx, v$TPy)
         if (length(setdiff(v$I, v$IT))==length(v$I))
             v$I<-c(v$I, v$IT)     
         else {}
                          }
                               }
            )
            
            
 observeEvent(input$go2,{
          v$GO2<-TRUE
          if (v$PT=='N'){
               v$df2<-v$df1[v$J,]
                        }
          else if (v$PT=='C'){
            for (k in 1:length(v$I)){
             v$df2<-rbind(v$df2, SubFr1(v$df1,v$x,v$CIx, v$y,v$CIy,v$I[k]))
                                    }
                             }
                         }
                )
                
 observeEvent(input$go5,{
          v$I<-c()
          v$J<-c()
          v$df2<-data.frame()
                         }
                )  
                
  observeEvent(input$go3,{
          v$GO1<-FALSE
          v$GO2<-FALSE
          v$TPx<-c()
          v$TPy<-c()
          v$CIx<-c()
          v$CIx<-c()
          v$n<-c()
          v$m<-c()
          v$I<-c()
          v$df2<-data.frame()
                         }
                ) 
                
     observeEvent(input$go4,{
          v$GO1<-FALSE
          v$GO2<-FALSE
          v$x<-c()
          v$y<-c()
          v$TPx<-c()
          v$TPy<-c()
          v$CIx<-c()
          v$CIx<-c()
          v$I<-c()
          v$J<-c()
          v$df1<-data.frame()
          v$df2<-data.frame()
                         }
                ) 

  
  
  output$table1 <- renderTable(if (dim(v$df1)[1]==0)
                          {}
                         else
                          {v$df1[1:min(dim(v$df1)[1], 10),]})
  

  output$table2 <- renderTable(if (v$GO2==FALSE | dim(v$df2)[1]==0)
                              {}
                              else
                              {v$df2[1:min(dim(v$df2)[1], 10),]}
                              ) 
                              
                
  
                                
  output$text2<-renderText(       
       if (v$GO2==TRUE & v$PT=='N')
                     {
          if (length(v$J)==1)
            {paste0('The selection contains ',length(v$J),' case.')}
          else {paste0('The selection contains ',length(v$J),' cases.')}
                      }
       else if (v$GO2==TRUE & v$PT=='C')
                      {
          if (dim(v$df2)[1]==1)
                 {paste0('The selection contains ',dim(v$df2)[1],' case.')}
          else {paste0('The selection contains ',dim(v$df2)[1],' cases.')}
                      }
       else {}
                         )
                   
  output$plot<- renderPlot(
    {
    if (dim(v$df1)[1]==0 | v$GO1==FALSE)
           {Eplot()}
    else if (v$GO1==TRUE & v$GO2==FALSE & length(v$x)!=length(v$y))
           {ERR()}
    else if (v$GO1==TRUE & length(v$I)==0 & length(v$x)==length(v$y))
           {MultiPlot(v$x, v$y, v$xname, v$yname)}
    else if (v$GO1==TRUE & length(v$I)!=0 & length(v$x)==length(v$y))
           {MultiPlotPts(v$x, v$y, v$xname, v$yname, v$I)}
    else {}
    }
                           )
                           
output$downloadData1 <- downloadHandler(
         filename = function() {
                      paste('testdf', ".csv", sep = "")
                             },
       content = function(file) {
       write.csv(testdf, file, row.names = FALSE)
    }
  )
                               
output$downloadData2 <- downloadHandler(
         filename = function() {
                      paste('NAME', ".csv", sep = "")
                             },
       content = function(file) {
       write.csv(v$df2, file, row.names = FALSE)
    }
  )
  
                                            }

shinyApp(ui, server)