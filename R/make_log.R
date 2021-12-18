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

    # list all local files without filter
    allFiles <- listAudio(
        input = sampled$input[1],
        songMeter = sampled$songMeter[1],
        logMsg = FALSE
    )
    

    # sample from main files to validation
    sampled$selectionValidation <- 0
    toValidate <- sample(which(sampled$main == 1), nbValidation)
    sampled$selectionValidation[toValidate] <- 1


    # sample one 10 min file for Atlas
    sampled$Atlas <- 0
    atlas <- sample(which(sampled$main == 1 & sampled$groupeDure == '10min'), 1)
    sampled$Atlas[atlas] <- 1


    # Filter columns and format file size from Kb to Mb
    dt_export <- allFiles[, c('songMeter', 'fileName', 'fileSize')]
    dt_export$fileSize <- dt_export$fileSize/1e3

    # create new columns to be added from sampled
    dt_export[, c('apres_filtre', 'selection_ecoute', 'selection_remplacement', 'selection_validation', 'atlas')] <- 0
    dt_export$incl_prob <- NA


    # merge cols from sampled
    sampled_rows <- match(sampled$fileName, allFiles$fileName)

    dt_export$apres_filtre[sampled_rows] <- 1
    dt_export$selection_ecoute[sampled_rows] <- sampled$main
    dt_export$selection_remplacement[sampled_rows] <- sampled$over
    dt_export$selection_validation[sampled_rows] <- sampled$selectionValidation
    dt_export$atlas[sampled_rows] <- sampled$Atlas
    dt_export$incl_prob[sampled_rows] <- sampled$incl_prob


    # rename columns
    names(dt_export)[1:2] <- c('sonometre', 'fichier')


    # save text file
    write.csv(
        dt_export,
        file = file.path(output, 'liste_selectionne.csv'),
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
