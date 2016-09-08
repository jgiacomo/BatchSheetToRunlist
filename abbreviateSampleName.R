abbreviateSampleName <- function(sampleNames){
    #
    # Takes a batch sheet which has been converted to a data frame object and
    # converts the ID field (Sample Name in the batch sheet) to an abbreviated
    # version. Many times for bioanalytical samples the sample names is longer
    # than the 16 characters provided for by the NEC AMS runlist. In this case
    # the sample name must be abbreviated into a (still unique) string which
    # fits within the 16 character limit. This function aims to provide that
    # functionality.
    #
    # Arguments:
    #           sampleNames - A vector of sample names from a batch sheet.
    #
    # Value:
    #       A vector of abbreviated (if needed) sample names.
    #
    
    library(stringr)
    
    # Check the input vector
    if(!is.vector(sampleNames) & !is.character(sampleNames)){
        stop("Error in abbreviateSampleName: input is not a character vector.")
    }
    
    # Define the function to use in the sapply() call
    snCombine <- function(x) paste(x[c(1,4,5)], collapse="-")
    
    # Use str_split() to split the sample names and the above function to paste
    # the useful parts back together. The if/else keeps from abbreviating sample
    # names which do not need to be.
    ifelse(nchar(sampleNames)>16,
           sapply(str_split(sampleNames,"-"), FUN=snCombine),
           sampleNames)
}