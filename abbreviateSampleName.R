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
    
    # Use ifelse() to only perform the abbreviation to sample names longer than
    # 16 characters. Then use gsub() to select the protocol number, graphite
    # batch number, and sample number from the sample name and use these as
    # replacements to the original string.
    ifelse(nchar(sampleNames)>16,
           gsub("^(\\d+-).*(G\\d+-\\d+$)","\\1\\2",sampleNames),
           sampleNames)
}