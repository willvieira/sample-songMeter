####################################################################
# Move sampled files into their respective groups
# Will Vieira
# Feb 15, 2021
####################################################################


####################################################################
# For a given Sound level meter
#   - create folders for each nesting/day period (A1-E6)
#   - Move all files from sampled dt
####################################################################


move_files <- function(sampled)
{
    songMeter <- sampled$songMeter[1]

    # list all nesting/day periods
    periods <- sort(unique(sampled$period))

    # create a folder for all nesting/day periods
    invisible(lapply(
                as.list(file.path(songMeter,
                paste(rep(c('Echantillons', 'Echantillons_supplementaires'),
                each = length(periods)), periods, sep = '/'))), dir.create, recursive = TRUE))

    # folder for Atlas
    dir.create(file.path(songMeter, 'Atlas'))
    
    # move atlas file
    fileAtlas <- sampled$fileName[sampled$sampleType == 'atlas']
    file.rename(from = file.path(songMeter, fileAtlas),
                to = file.path(songMeter, 'Atlas', fileAtlas))

    # create file name with directory `from` and `to`
    sampled_mainOver <- subset(sampled, sampleType %in% c('main', 'over'))

    sampled_mainOver$from <- file.path(sampled_mainOver$songMeter,
                                       sampled_mainOver$fileName)
    
    sampled_mainOver$to <- file.path(sampled_mainOver$songMeter,
                                     ifelse(sampled_mainOver$sampleType == 'main', 'Echantillons', 'Echantillons_supplementaires'),
                                     sampled_mainOver$period,
                                     sampled_mainOver$fileName)

    moveResults <- file.rename(sampled_mainOver$from, sampled_mainOver$to)

    # Message
    if(all(moveResults))
    {
        cat('   All', length(moveResults) + 1, 'files have been moved successfully!\n\n\n')
    }else{
        msg <- 'We had problems moving the following files:\n'
        missingRows <- sampled_mainOver[which(!moveResults), c('songMeter', 'fileName')]
        cat(msg,
            paste0(paste0(' - ',
                    apply(missingRows, 1, function(x) file.path(x[1], x[2]))), collapse = '\n'), '\n\n')
    }
}
