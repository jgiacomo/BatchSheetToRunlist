changeIonSources <- function(runlist, source = c(1,2)){
    # This function is for changing the ion source number in the runlist file
    # of the NEC CAMS AMS system.
    #
    # Arguments:
    #     runlist - a NEC runlist file
    #     source - integer source number (1 or 2)
    #
    # Values:
    #     None - this rewrites the runlist after updating the ion source number.
    #
    
    rl <- readLines(runlist)
    
    if(source==1){
        rl[which(grepl('batch source',rl))] <- "batch source    S1"
    }
    if(source==2){
        rl[which(grepl('batch source',rl))] <- "batch source    S2"
    }
    
    # To ensure unix end-of-lines are preserved the file must be opened in
    # binary mode before the text is written with the line-feed set to "\n"
    fileConn <- file(runlist, open="wb")
    writeLines(rl, fileConn, sep="\n")
    close(fileConn)  # Always close connections
}