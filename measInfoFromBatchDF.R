measInfoFromBatchDF <- function(batchDF,
                                bioRuns="5",
                                datingRuns="10",
                                Tlimit="1200",
                                Climit="40000",
                                warm="100",
                                blankWarm="500"){
    # This function takes the data from a batch sheet and creates the proper
    # measurement information for insertion into the runlist. The sample type
    # and grouping information must have already been inserted into the data
    # frame.
    #
    # Arguments:
    #     batchDF - a data frame of batch sheet data with sample types.
    #     bioRuns - number of runs to complete on bioanalytical samples.
    #     datingRuns - number of runs to complete on dating samples.
    #     Tlimit - time limit for run (deciseconds)
    #     Climit - count limit for a run (number of 14C counts)
    #     warm - time of warmup before starting run (deciseconds)
    #     blankWarm - time of warmup for blanks (deciseconds)
    #
    # Values:
    #     A character vector conataining the measurement information for the
    #     runlist. Each element is a line in the runlist and is properly padded
    #     for the fixed width format of the runlist.
    #
    
    library(stringr)
    
    # Check to be sure the sample type is in the data frame
    if(is.null(batchDF$SmType)){
        stop("SmType column is missing.")
    }
    
    # Check to be sure the group information is in the data frame
    if(is.null(batchDF$Group)){
        stop("Group column is missing.")
    }
    
    # Create the necessary columns with padding for the fixed width file
    batchDF$lead <- "run"
    batchDF$Item <- str_pad(seq(1,nrow(batchDF)), width=4, side="left")
    batchDF$Pos <- str_pad(batchDF$Pos, width=4, side="left")
    batchDF$Group <- str_pad(batchDF$Group, width=3, side="left")
    batchDF$Summary <- "  0"  # To be corrected later.
    batchDF$Runs <- str_pad(bioRuns, width=4, side="left")  # Corrected later.
    batchDF$Md <- " C"  # To be corrected later.
    batchDF$Tlimit <- str_pad(Tlimit, width=6, side="left")
    batchDF$Climit <- str_pad(Climit, width=6, side="left")
    batchDF$warm <- str_pad(warm, width=4, side="left")
    
    # Get all sample types from batch
    batchSmTypes <- unique(batchDF$SmType)
    
    # Set proper Summary, Md, and Climit values based on sample type
    if("OX1" %in% batchSmTypes){
        batchDF[batchDF$SmType=="OX1",]$Summary <- "  1"
        batchDF[batchDF$SmType=="OX1",]$Md <- " T"
        batchDF[batchDF$SmType=="OX1",]$Climit <- "     0"
    }
    
    if("OX2" %in% batchSmTypes){
        batchDF[batchDF$SmType=="OX2",]$Summary <- "  2"
        batchDF[batchDF$SmType=="OX2",]$Md <- " T"
        batchDF[batchDF$SmType=="OX2",]$Climit <- "     0"
    }
    
    if("C6" %in% batchSmTypes){
        batchDF[batchDF$SmType=="C6",]$Summary <- "  3"
        batchDF[batchDF$SmType=="C6",]$Md <- " T"
        batchDF[batchDF$SmType=="C6",]$Climit <- "     0"
    }
    
    if("C7" %in% batchSmTypes){
        batchDF[batchDF$SmType=="C7",]$Summary <- "  4"
        batchDF[batchDF$SmType=="C7",]$Md <- " T"
        batchDF[batchDF$SmType=="C7",]$Climit <- "     0"
    }
    
    if("UPCG" %in% batchSmTypes){
        batchDF[batchDF$SmType=="UPCG",]$Summary <- "  5"
        batchDF[batchDF$SmType=="UPCG",]$Md <- " T"
        batchDF[batchDF$SmType=="UPCG",]$Climit <- "     0"
        batchDF[batchDF$SmType=="UPCG",]$warm <- str_pad(blankWarm, width=4,
                                                         side="left")
    }
    
    if("Blank" %in% batchSmTypes){
        batchDF[batchDF$SmType=="Blank",]$Summary <- "  6"
        batchDF[batchDF$SmType=="Blank",]$Md <- " T"
        batchDF[batchDF$SmType=="Blank",]$Climit <- "     0"
        batchDF[batchDF$SmType=="Blank",]$warm <- str_pad(blankWarm, width=4,
                                                         side="left")
    }
    
    if("Air" %in% batchSmTypes){
        batchDF[batchDF$SmType=="Air",]$Summary <- "  7"
        batchDF[batchDF$SmType=="Air",]$Md <- " T"
        batchDF[batchDF$SmType=="Air",]$Climit <- "     0"
        batchDF[batchDF$SmType=="Air",]$warm <- str_pad(blankWarm, width=4,
                                                          side="left")
    }
    
    # Set proper Runs if dating samples
    if(any(grepl('^1232-',batchDF$ID))){
        batchDF[grepl('^1232-',batchDF$ID),]$Runs <- 
            str_pad(datingRuns, width=4,side="left")
    }
    
    # Create the character vector from the data frame
    measInfo <- paste(batchDF$lead,
                     batchDF$Item,
                     batchDF$Pos,
                     batchDF$Group,
                     batchDF$Summary,
                     batchDF$Runs,
                     batchDF$Md,
                     batchDF$Tlimit,
                     batchDF$Climit,
                     batchDF$warm,
                     sep=" ")
    
    return(measInfo)
}