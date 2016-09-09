sampleInfoFromBatchDF <- function(batchDF){
    # This function takes the data from a batch sheet and creates the proper
    # sample information for insertion into the runlist. The sample type
    # information must have already been inserted into the data frame.
    #
    # Arguments:
    #     batchDF - a data frame of batch sheet data with sample types.
    #
    # Values:
    #     A character vector conataining the sample information for the runlist.
    #     Each element is a line in the runlist and is properly padded for the
    #     fixed width format of the runlist.
    #
    
    library(stringr)
    
    # Check to be sure the sample type is in the data frame
    if(is.null(batchDF$SmType)){
        stop("SmType column is missing.")
    }
    
    # Check if the sample name has been abbreviated (if needed)
    if(!all(nchar(batchDF$ID)<=16)){
        stop("Sample names need to be abbreviated.")
    }
    
    # Create the necessary columns with padding for the fixed width file
    batchDF$lead <- "cat"
    batchDF$Pos <- str_pad(batchDF$Pos, width=4, side="left")
    batchDF$SmType <- str_pad(batchDF$SmType, width=6, side="right")
    batchDF$SampleName <- str_pad(batchDF$ID, width=16, side="right")
    batchDF$SampleName2 <- "1"
    
    # Create the character vector from the data frame
    smpInfo <- paste(batchDF$lead,
                     batchDF$Pos,
                     batchDF$SmType,
                     batchDF$SampleName,
                     batchDF$SampleName2,
                     sep=" ")
    
    return(smpInfo)
}