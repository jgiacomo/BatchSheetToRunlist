insertSampleInfoInRunlist <- function(runlist, text){
    # This function is for inserting sample information into the runlist file
    # of the NEC CAMS AMS system.
    #
    # Arguments:
    #     runlist - a NEC runlist file
    #     text - a character vector of the text you wish to insert. A newline
    #            will be inserted after every element of the vector.
    #
    # Values:
    #     None - this rewrites the runlist file after inserting the sample info.
    #
    
    rl <- readLines(runlist)
    breakLine <- grep("Pos  SmType Sample Name      Sample Name 2",rl) + 2
    fore <- rl[1:breakLine]
    aft <- rl[breakLine+1:length(rl)]
    
    newRL <- c(fore, text, aft)
    
    # To ensure unix end-of-lines are preserved the file must be opened in
    # binary mode before the text is written with the line-feed set to "\n"
    fileConn <- file(runlist, open="wb")
    writeLines(newRL, fileConn, sep="\n")
    close(fileConn)  # Always close connections
}