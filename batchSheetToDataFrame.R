batchSheetToDF <- function(batchFile){
    #
    # Takes a batch file (Accium Excel file) and converts the sample information
    # into a data frame.
    #
    # Input:
    #       batchFile = An Accium created standard batch sheet in Excel format.
    #
    # Value:
    #       a data frame containing all the sample information from the batch
    #       sheet.
    #
    
    library(dplyr)
    library(readxl)
    library(readr)
    library(stringr)
    
    fileDF <- NULL
    sheetNames <- excel_sheets(batchFile)
    sheetNames <- sheetNames[!grepl('Sheet1',sheetNames)]
    for(sheets in sheetNames){
        
        tempDF <- read_excel(batchFile, sheet=sheets, col_names=FALSE,
                             skip=6)
        
        # Check if first column is empty (sometimes happens with a few sheets)
        if(all(is.na(tempDF[,1]))){
            tempDF <- tempDF[,-1]
        }
        
        # Add comment column (sometimes needs to be done with some sheets
        # where the comments were empty). Numeric to match other sheets.
        tempDF$newCol <- as.numeric(NA)
        
        # Remove the check box columns
        tempDF <- tempDF[,c(1,2,3,6)]
        
        names(tempDF) <- c("Pos","ID","ID2","Comment")
        tempDF$Batch <- str_extract(batchFile,
         '(?<=sheets/AMS )(?s)(.*)(?=.xls)|(?<=sheets\\\\AMS )(?s)(.*)(?=.xls)')
        tempDF <- tempDF[!is.na(tempDF$ID),]
        tempDF <- tempDF %>% select(Batch, everything())
        
        fileDF <- rbind(fileDF, tempDF)
    }
    
    return(fileDF)
}