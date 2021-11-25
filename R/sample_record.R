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
        warning(msg)
    }

    # data frame to store sampled rows
    sampled <- data.frame()


    #############################################################
    # First sample 5 files of 10 minutes duration from Dawn period (A1 to E3)
    #############################################################
    
    for(gp in unique(substring(groups, 1, 1)))
    {
        # Filter all 10 min files within group gp 1 to 3
        dt_gp_10min <- subset(dt, period %in% paste0(gp, 1:3) & duration == 600)

        # sampleSize sample
        sampledRow <- sample(rownames(dt_gp_10min), sampleSize)
        
        # overSample sample (sampling overSample within the same period of main sample)
        dt_gp_10min <- dt_gp_10min[-which(rownames(dt_gp_10min) == sampledRow), ] # remove already sampled row
        overSampledRow <- sample(rownames(subset(dt_gp_10min, period == dt[sampledRow, 'period'])), overSample)

        # create gp data.frame to be merged with `sampled` data.frame
        sampled_gp <- dt[c(sampledRow, overSampledRow), ]
        sampled_gp$sampleType <- c(rep('main', sampleSize), rep('over', overSample))

        sampled <- rbind(sampled, sampled_gp)
    }
    

    #############################################################
    # Now sample `sampleSize` and `overSample` for the remaining groups not yet selected in the step above
    #############################################################

    # filter dt to groups not yet selected in the step above
    groupsToSample <- setdiff(groups, unique(sampled$period))
    dt_gp <- subset(dt, period %in% groupsToSample)

    # sample `sampleSize` + `overSample` by group
    sampled_ls <- lapply(split(dt_gp, dt_gp$period), 
                         function(x) x[sample(nrow(x), sampleSize + overSample), ])

    # add sampleTye column
    sampled_ls <- lapply(sampled_ls,
                         function(x) cbind(x, sampleType = c(rep('main', sampleSize), rep('over', overSample))))

    # list to data.frame and fix rownames
    sampled_dt <- do.call(rbind, sampled_ls)
    rownames(sampled_dt) <- gsub('.*\\.', '', rownames(sampled_dt)) # remove everything before `.`


    #############################################################
    # Final sample to select 3 groups in the first period of nigth (A-E5)
    # and let the remaining groups for the second period of nigth (A-E6)
    #############################################################

    # sample groups from first and second period of nigth
    firstPeriod_nigth <- sample(LETTERS[1:5], 3)
    secondPeriod_nigth <- setdiff(LETTERS[1:5], firstPeriod_nigth)

    # Remove the not selected groups
    groupsToRemove <- c(paste0(firstPeriod_nigth, 6), paste0(secondPeriod_nigth, 5))
    sampled_dt <- subset(sampled_dt, !(period %in% groupsToRemove)) 

    # merge with main dt
    sampled <- rbind(sampled, sampled_dt)


    #############################################################
    # Select Atlas file from Dawn period with at least 10 minutes record
    #############################################################

    # Filter all files from Dawn with at least 10 minutes duration
    dawnFiles <- subset(dt, period %in% paste0(rep(LETTERS[1:5], each = 3), 1:3) & duration >= 600)
    
    # get row names of already sampled files from this period
    alreadySampled_dawn <- rownames(subset(sampled, duration >= 600 & period %in% paste0(rep(LETTERS[1:5], each = 3), 1:3)))
    
    # sample atlas file
    specialRow <- sample(rownames(dawnFiles[which(!rownames(dawnFiles) %in% alreadySampled_dawn), ]), 1)

    # add atlas file to sampled dt
    specialDt <- dt[specialRow, ]
    specialDt$sampleType <- 'atlas'
    sampled <- rbind(sampled, specialDt)

    # Return message
    msg <- paste0(' For the ', length(groups), ' nesting and day periods:\n')
    msg <- paste0(msg, ' - ', nrow(subset(sampled, sampleType == 'main')), ' main samples\n')
    msg <- paste0(msg, ' - ', nrow(subset(sampled, sampleType == 'over')), ' over samples\n')
    msg <- paste0(msg, ' - ', nrow(subset(sampled, sampleType == 'atlas')), ' atlas samples\n')
    cat(msg)    
    
    return( sampled )

}
