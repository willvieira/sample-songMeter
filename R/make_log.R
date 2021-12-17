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

    # subset for selected files only
    sampled <- subset(sampled, main == 1 | over == 1)

    # remove file extension
    sampled$fileName_ne <- tools::file_path_sans_ext(sampled$fileName)


    # rename 'main' and 'over' columns
    names(sampled)[grep('main', names(sampled))] <- 'selectionEcoute'
    names(sampled)[grep('over', names(sampled))] <- 'selectionRemplecement'


    # sample from main files to validation
    sampled$selectionValidation <- 0
    toValidate <- sample(which(sampled$selectionEcoute == 1), nbValidation)
    sampled$selectionValidation[toValidate] <- 1


    # sample one 10 min file for Atlas
    sampled$Atlas <- 0
    atlas <- sample(which(sampled$selectionEcoute == 1 & round(sampled$duration, 0) == 600), 1)
    sampled$Atlas[atlas] <- 1


    # Filter columns
    sampled_export <- sampled[, c('songMeter', 'fileName_ne', 'incl_prob', 'selectionEcoute', 'selectionRemplecement', 'selectionValidation', 'Atlas')]


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
logMsg <- function(msg, fileName = file.path(outputFolder, sono, 'selection_log.txt'), console = TRUE, append = TRUE)
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
        append = append
    )
}
