####################################################################
# log maker
# Will Vieira
# Nov 28, 2021
####################################################################


####################################################################
# Steps
# - create a file log which files have been sampled
#   - fileName
#   - selectionEcoute
#   - selectionRemplecement
#   - selectionValidation
# - Create another file log with descriptive summary of sampling
####################################################################



make_list <- function(sampled, nbValidation, output)
{

    # Create log with sampled files
    ##########################################

    # remove file extension
    sampled$fileName_ne <- tools::file_path_sans_ext(sampled$fileName)


    # create 0 - 1 columns to define if sample is main or over
    sampled$selectionEcoute <- ifelse(sampled$sampleType == 'main', 1, 0)
    sampled$selectionRemplecement <- ifelse(sampled$sampleType == 'over', 1, 0)
    
    
    # sample from main files to validation
    sampled$selectionValidation <- 0
    toValidate <- sample(which(sampled$selectionEcoute == 1), nbValidation)
    sampled$selectionValidation[toValidate] <- 1


    # Filter columns
    sampled_export <- sampled[, c('songMeter', 'fileName_ne', 'selectionEcoute', 'selectionRemplecement', 'selectionValidation')]


    # rename columns
    names(sampled_export)[1:2] <- c('sonometre', 'fichier')


    # save text file
    write.csv2(
        sampled_export,
        file = file.path(output, 'liste_selectionne.txt'),
        row.names = FALSE
    )

}


# Function to print message to console and log file at the same time
logMsg <- function(msg, fileName = file.path(outputFolder, sono, 'selection_log.txt'), console = TRUE)
{
    # firt break a line at the end
    msg_break <- paste0(msg, '\n')
    
    # print console
    if(console)
        cat(msg_break)

    # print file
    cat(
        msg_break,
        file = fileName,
        append = TRUE
    )
}
