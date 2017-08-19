
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)
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
    
    observe({
        # Positions of OX2 standards
        OX2pos <- c(batchDF()[grepl('OX2|OXII',batchDF()$ID2),]$Pos,
                    batchDF()[grepl('OX2|OXII',batchDF()$Comment),]$Pos)
        
        # Positions of machine blanks
        MBpos <- batchDF()[grepl('999-16[0-9]{3}',batchDF()$ID2),]$Pos
        
        # Positions of chemical blanks
        CBpos <- c(batchDF()[grepl('bla-|blank|Blank|BLANK',
                                   batchDF()$ID2),]$Pos,
                   batchDF()[grepl('blank|Blank|BLANK|BK ',
                                   batchDF()$Comment),]$Pos)
        
        # Positions of C1, C2, C6, and C7 samples
        C1pos <- c(batchDF()[grepl('C1-|C1 ',batchDF()$ID2),]$Pos,
                   batchDF()[grepl('C1-|C1 ',batchDF()$Comment),]$Pos)
        
        C2pos <- c(batchDF()[grepl('C2-|C2 ',batchDF()$ID2),]$Pos,
                   batchDF()[grepl('C2-|C2 ',batchDF()$Comment),]$Pos)
        
        C6pos <- c(batchDF()[grepl('C6-|C6 ',batchDF()$ID2),]$Pos,
                   batchDF()[grepl('C6-|C6 ',batchDF()$Comment),]$Pos)
        
        C7pos <- c(batchDF()[grepl('C7-|C7 ',batchDF()$ID2),]$Pos,
                   batchDF()[grepl('C7-|C7 ',batchDF()$Comment),]$Pos)
        
        # Collapse position vectors to comma separated text
        OX2pos <- paste(OX2pos, collapse=",")
        MBpos <- paste(MBpos, collapse=",")
        CBpos <- paste(CBpos, collapse=",")
        C1pos <- paste(C1pos, collapse=",")
        C2pos <- paste(C2pos, collapse=",")
        C6pos <- paste(C6pos, collapse=",")
        C7pos <- paste(C7pos, collapse=",")
        
        # Update textInput boxes with these found positions
        updateTextInput(session, inputId='typ_OX2',label="OX2", value=OX2pos)
        updateTextInput(session, inputId='typ_Mblank',label="Machine Blank",
                        value=MBpos)
        updateTextInput(session, inputId='typ_Blank',label="Chemical Blank",
                        value=CBpos)
        updateTextInput(session, inputId='typ_C1',label="C1", value=C1pos)
        updateTextInput(session, inputId='typ_C2',label="C2", value=C2pos)
        updateTextInput(session, inputId='typ_C6',label="C6", value=C6pos)
        updateTextInput(session, inputId='typ_C7',label="C7", value=C7pos)
        
        print(paste(C2pos, collapse=","))
    })
    
    runlist <- reactive({
        # Create the runlist using the batch data frame
        bdf <- batchDF()
        
        # Get sample types
        bdf <- chgSmType(bdf, input$typ_OX2, "OX2")
        bdf <- chgSmType(bdf, input$type_OX1, "OX1")
        bdf <- chgSmType(bdf, input$typ_C7, "C7")
        bdf <- chgSmType(bdf, input$typ_Mblank, "UPCG")
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
        
        # Set the source based on user input
        runlist[which(grepl('batch source',runlist))] <-
            paste0("batch source    ", input$source)
        
        # Enter the batch number
        batchNumber <- bdf[1,]$Batch
        runlist[which(grepl('AMS Batch',runlist))] <- 
            paste0("#   AMS Batch ", batchNumber)
        
        return(runlist)
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
    
    output$bSheetTbl <- renderDataTable({
        validate(need(input$batchSheet, "Please select a batch sheet"))
        btbl <- batchDF()
        btbl <- btbl %>% select(Pos, ID, ID2, Comment)
    }, options=list(pageLength=25),
       rownames=FALSE
    )
    
})
