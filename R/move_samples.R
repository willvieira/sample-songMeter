####################################################################
# Move sampled files into their respective groups
# Will Vieira
# Feb 15, 2021
####################################################################


####################################################################
# For a given Sound level meter
#   - create folders
#   - Move all files from sampled dt
####################################################################


move_files <- function(sampled, input, output)
{
    # song meter name
    songMeter_input <- file.path(input, sampled$songMeter[1])
    songMeter_output <- file.path(output, sampled$songMeter[1])


    # subset for selected files only
    sampled <- subset(sampled, main == 1 | over == 1)


    # create a sample type column
    if(any(sampled$main == 1 & sampled$over == 1))
        stop('The same audio was selected for main AND over')
    
    sampled$sampleType = ifelse(sampled$main == 1, 'main', 'over')


    # create file name with directory `from` and `to`
    sampled$from <- file.path(
        songMeter_input,
        sampled$fileName
    )
    
    sampled$to <- file.path(
        songMeter_output,
        ifelse(
            sampled$groupeDure == '3min',
            'audio_3min', 'audio_10min'
        ),
        ifelse(
            sampled$sampleType == 'main',
            sampled$fileName,
            paste0(
                'selection_remplacement/',
                sampled$fileName
            )
        )
    )


    # copy
    moveResults <- file.copy(
        sampled$from,
        sampled$to,
        copy.date = TRUE
    )


    # Message
    if(all(moveResults))
    {
        logMsg(
            paste('\nMove files:\nAll', length(moveResults), 'files have been moved successfully!')
        )
    }else{
        msg <- 'We had problems moving the following files:\n'
        missingRows <- sampled[which(!moveResults), c('songMeter', 'fileName')]
        msg <- paste0(
            msg,
            paste0(paste0(' - ',
                    apply(missingRows, 1, function(x) file.path(x[1], x[2]))), collapse = '\n')
        )
        
        logMsg(msg)
    }
}
