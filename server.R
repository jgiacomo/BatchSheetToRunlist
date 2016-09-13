
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source("batchSheetToDataFrame.R")
source("numInputToIntegers.R")

shinyServer(function(input, output) {

  output$batch <- renderTable({
      batchFile <- input$batchSheet

      if(is.null(batchFile)){
          return(NULL)
      }
      
      # read_excel(), used by batchSheetToDF(), will not accept the temp file
      # created by fileInput() to store the downloaded file. So, we must append
      # the correct extension to the file so read_excel will know it is an excel
      # file. Also, batchSheetToDF() cannot get the batch number automatically
      # from the temp file since the name is changed. Therefore, we will append
      # the original file name to the end of the temporary file name before
      # passing it to batchSheetToDF().
      
      file.rename(batchFile$datapath,
                  paste(batchFile$datapath, batchFile$name, sep=""))
      batchSheetToDF(paste(batchFile$datapath, batchFile$name, sep=""))
  })
  
  output$test <- renderText({
      input$typ_OX2
  })
  
})
