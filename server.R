
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source("batchSheetToDataFrame.R")
source("numInputToIntegers.R")
source("abbreviateSampleName.R")
source("measInfoFromBatchDF.R")
source("sampleInfoFromBatchDF.R")

# read the runlist template into a character vector
rlTemp <- readLines("runlist.template")

# Function for changing sample type based on user input
chgSmType <- function(df, inID, smType){
    if(inID != ""){
        typPos <- numInputToIntegers(inID)
        df[df$Pos %in% typPos,]$SmType <- smType
    }
    return(df)
}

# Function for changing groups and their runs based on user input
chgGroup <- function(df, inGrp, group, inRun){
    if(inGrp != ""){
        grpPos <- numInputToIntegers(inGrp)
        df[df$Pos %in% grpPos,]$Group <- group
        
        if(inRun !=""){
            if(!is.na(as.integer(inRun))){
                df[df$Pos %in% grpPos,]$Runs <- inRun
            }
        }
    }
    return(df)
}

shinyServer(function(input, output, session) {
  
    batchDF <- reactive({
        # Create a data frame from a batch sheet (Excel)
        batchFile <- input$batchSheet
        
        if(is.null(batchFile)){
            return(NULL)
        }
        
        # read_excel(), used by batchSheetToDF(), will not accept the temp file 
        # created by fileInput() to store the downloaded file. So, we must
        # append the correct extension to the file so read_excel will know it is
        # an excel file. Also, batchSheetToDF() cannot get the batch number
        # automatically from the temp file since the name is changed. Therefore,
        # we will append the original file name to the end of the temporary file
        # name before passing it to batchSheetToDF().
        
        file.rename(batchFile$datapath,
                    paste(batchFile$datapath, batchFile$name, sep=""))
        bdf <- batchSheetToDF(paste(batchFile$datapath, batchFile$name, sep=""))
        bdf$SmType <- "unk"
        bdf$Group <- "0"
        bdf$Runs <- "5"
        
        # Abbreviate any sample IDs that are longer than 16 characters
        bdf$ID <- abbreviateSampleName(bdf$ID)
        
        return(bdf)
    })
    
    runlist <- reactive({
        # Create the runlist using the batch data frame
        bdf <- batchDF()
        
        # Get sample types
        bdf <- chgSmType(bdf, input$typ_OX2, "OX2")
        bdf <- chgSmType(bdf, input$type_OX1, "OX1")
        bdf <- chgSmType(bdf, input$typ_C7, "C7")
        bdf <- chgSmType(bdf, input$typ_UPGC, "UPCG")
        bdf <- chgSmType(bdf, input$typ_Blank, "Blank")
        bdf <- chgSmType(bdf, input$typ_C1, "C1")
        bdf <- chgSmType(bdf, input$typ_C2, "C2")
        bdf <- chgSmType(bdf, input$typ_C6, "C6")
        
        # Get groups and runs
        bdf <- chgGroup(bdf, input$grp0, "0", input$run0)
        bdf <- chgGroup(bdf, input$grp1, "1", input$run1)
        bdf <- chgGroup(bdf, input$grp2, "2", input$run2)
        bdf <- chgGroup(bdf, input$grp3, "3", input$run3)
        bdf <- chgGroup(bdf, input$grp4, "4", input$run4)
        bdf <- chgGroup(bdf, input$grp5, "5", input$run5)
        bdf <- chgGroup(bdf, input$grp6, "6", input$run6)
        bdf <- chgGroup(bdf, input$grp7, "7", input$run7)
        
        # Create text for the sample info and measurement info of the runlist
        sampleInfo <- sampleInfoFromBatchDF(bdf)
        measInfo <- measInfoFromBatchDF(bdf)
        
        # Merge sample info and measurement info into the runlist template
        sampleLine <- grep("Pos  SmType Sample Name      Sample Name 2",
                           rlTemp) + 2
        fore <- rlTemp[1:sampleLine]
        aft <- rlTemp[(sampleLine+1):length(rlTemp)]
        
        runlist <- c(fore, sampleInfo, aft)
        
        measLine <- grep("Item Pos  Grp Sum Runs Md Tlimit Climit Warm",
                         runlist) + 2
        fore <- runlist[1:measLine]
        aft <- runlist[(measLine+1):length(runlist)]
        
        runlist <- c(fore, measInfo, aft)
        
        # Not sure why NAs show up at end of file but need to remove them
        #runlist <- runlist[-grep("NA",runlist)]
        
    })
    
    output$rl <- renderText({
        validate(need(input$batchSheet,"Please select a batch sheet"))
        rl <- runlist()
        paste(rl,"\n",sep="",collapse="")
        
    })
    
    output$dlRunlist <- downloadHandler(filename="runlist",
                                        content=function(file){
                                            rl <- runlist()
                                            fileConn <- file(file, open="wb")
                                            writeLines(rl,
                                                       fileConn,
                                                       sep="\n")
                                            close(fileConn)
                                        })
    
})
