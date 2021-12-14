####################################################################
# Sample
# Will Vieira
# Feb 12, 2021
####################################################################


####################################################################
# Steps
# - For each period group (A1, A2, ..., B1, B2, ...)
#   - Sample and extraSample
#   - For each sampled file, test if sound quality is good (TODO)
#   - If so assign sample number, if not sample again
# - return dt
####################################################################



# Input:
#   - dt output from classify_period() function
#   - sampleSize: number of samples by period group (e.g. if sampleSize = 1, and groups = 5 * 6, total sample = 30)
#   - overSample: number of extra samples by period group
# Output: dt file with two extra columns defining sample and extraSample rows [0, 1]
sample_files <- function(dt, sampleSize = 1, overSample = 1)
{
    # log arguments into  temp file
    logMsg(
        paste0(
            '\nSample audio:\n',
            'sampleSize: ', sampleSize, '\n',
            'overSample: ', overSample
        ),
        console = FALSE
    )

    groups <- unique(dt$period)

    # split data into groups
    dt_groups <- split(dt, dt$period)
    
    # check if sampleSize + overSample is enough for each group
    nbByGroup <- unlist(lapply(dt_groups, nrow))
    if(length(which(nbByGroup < (sampleSize + overSample))) > 0)
    {
        missingSamples <- names(which(nbByGroup < (sampleSize + overSample)))
        msg <- paste0('The following groups do not have enough files to sample (SampleSize + overSample = ', sampleSize + overSample, '):\n')
        msg <- paste0(msg, paste0(paste0('- ', missingSamples, ' = ', nbByGroup[missingSamples]), collapse = '\n'))
        logMsg(msg)
    }


    # data frame to store sampled rows
    sampled <- data.frame()


    # Sample main and over for each day and nesting period
    for(gp in groups)
    {
        # subset to specific group
        dt_gp <- subset(dt, period %in% gp)

        # sample main file
        sampledRow <- sample(rownames(dt_gp), sampleSize)

        # sample over, excluding already selected main file
        overSampledRow <- sample(
            rownames(dt_gp)[-which(rownames(dt_gp) == sampledRow)],
            overSample
        )

        # create gp data.frame to be merged with `sampled` data.frame
        sampled_gp <- dt[c(sampledRow, overSampledRow), ]
        sampled_gp$sampleType <- c(rep('main', sampleSize), rep('over', overSample))

        sampled <- rbind(sampled, sampled_gp)
    }


    # log
    msg <- paste0(' For the ', length(groups), ' nesting and day periods:\n')
    msg <- paste0(msg, ' - ', nrow(subset(sampled, sampleType == 'main')), ' main samples\n')
    msg <- paste0(msg, ' - ', nrow(subset(sampled, sampleType == 'over')), ' over samples')
    # msg <- paste0(msg, ' - ', nrow(subset(sampled, sampleType == 'atlas')), ' atlas samples\n')
    
    logMsg(msg)

    return( sampled )

}
