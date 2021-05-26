
library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)
library(randomForest)
library(rsconnect)

ui <- fluidPage(
    theme=shinytheme("yeti"),
    titlePanel("Abalone Age"),
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", HTML("<strong>Upload data as .csv</strong>
                                     <br>Requires features: 
                                     <br>Sex <br>Length <br>Diameter 
                                     <br>Height <br>Whole weight <br>Shucked weight 
                                     <br>Viscera weight <br>Shell weight
                                     <br><br>"),
                      multiple=FALSE,
                      accept=c(".csv", "text/csv",
                               "text/comma-separated-values",
                               "text/plain")
                      ),
            selectInput("pred.type", "Model type:",
                        list(`Random Forest Regression`="Reg",
                             `Random Forest Classification`="Class")),
            HTML("<br>"),
            downloadButton("download", "Download predictions"),
            width=3
            ),             
        mainPanel(dataTableOutput("tableout"))
        )
    )

server <- function(input, output){
    reactiveDf <- reactive({
        rf_mod <- readRDS("rf_mod.rds") # read in rf_mod (regression)
        rf_mod2 <- readRDS("rf_mod2.rds") # read in rf_mod2 (classification)
        req(input$file1)
        # assume first row is header here, 
        df <- read.csv(input$file1$datapath, header=TRUE, stringsAsFactors=TRUE)
        colnames(df) <- c("NA", "Sex", "Length", "Diameter", 
                          "Height", "Whole_weight", "Shucked_weight", 
                          "Viscera_weight", "Shell_weight")
        df <- df[, -1]
        # Remove length
        df <- df[, c("Sex", "Diameter", "Whole_weight", 
                     "Shucked_weight", "Viscera_weight", 
                     "Shell_weight")]
        testdata <- df
        
        if(input$pred.type=="Reg"){
            df$`Rings Prediction` <- round(predict(rf_mod, newdata=testdata))
            df$`Age Prediction` <- df$`Rings Prediction` + 1.5
        }
        if(input$pred.type=="Class"){
            df$`Rings Prediction` <- predict(rf_mod2, newdata=testdata)
        }
        
        return(df)
    })
    
    output$tableout <- renderDataTable({
        req(input$file1)
        return(datatable(reactiveDf()))
    })
    
    output$download <- downloadHandler(
        filename=function(){paste("data-", Sys.Date(), ".csv", sep="")},
        content=function(file){write.csv(reactiveDf(), file, row.names=FALSE)}
    )
}

shinyApp(ui=ui, server=server)
