####################################################################
# Plot files from a sound level meter
# Will Vieira
# Feb 15, 2021
####################################################################


plot_files <- function(dt, sampled = NULL, colorDay = FALSE, fileSuffix = NULL)
{
    dt_tz <- lubridate::tz(dt$time[1])
    songMeter <- dt$songMeter[1]

    # define sequence of days (y axis)
    startDay <- lubridate::date(min(dt$time))
    endDay <- lubridate::date(max(dt$time)) - lubridate::days(1)
    seqDays <- seq(startDay, endDay, by = 'day')    

    # function to transform any time within a day into a continous value of seconds (from 0 to 86400 seconds in a day)
    timeToSeconds <- function(Time)
        (lubridate::hour(Time) * 3600) + (lubridate::minute(Time) * 60) + lubridate::second(Time)

    # function to convert to human readable date
    sf <- suppressMessages(lubridate::stamp_date('Jan 17, 1999'))

    # Get only day from time
    dt$day <- lubridate::date(dt$time)

    # Get position of each start time in seconds
    dt$startRecord <- timeToSeconds(dt$time)
    dt$endRecord <- dt$startRecord + dt$duration

    # Ajust startRecord and endRecord so that audios recorded after midnigth are shown after those from the dusk period
    minStartRecord <- min(subset(dt, period %in% paste0(LETTERS[1:5], 1))$startRecord)
    minStartRecord <- minStartRecord - 300 # minus 5 minutes
    dt$startRecord[which(dt$startRecord < minStartRecord)] <- dt$startRecord[which(dt$startRecord < minStartRecord)] + 86400

    # calculate final record position in seconds
    dt$endRecord <- dt$startRecord + dt$duration

    # For all recorders starting after midnight, calculate DAY - 1
    dt$day[which(dt$startRecord > 86400)] <- dt$day[which(dt$startRecord > 86400)] - lubridate::days(1)

    # define sequence of time (x axis)
    seqHours <- seq(round(minStartRecord/3600, 0), 24 + round(minStartRecord/3600, 0))
    seqSeconds <- seqHours * 3600
    seqHours[which(seqHours > 23)] <- seqHours[which(seqHours > 23)] - 24

    # Basic plot
    if(is.null(fileSuffix)) {
        fileName <- file.path(songMeter, paste0(songMeter, '.pdf'))
    }else{
        fileName <- file.path(songMeter, paste0(songMeter, fileSuffix, '.pdf'))
    }
    pdf(height = 9.12, width = 12.43, file = fileName)
    par(mar = c(2, 7, 2, 0.5))
    plot(0, pch = '', xlim = c(minStartRecord, 86400 + minStartRecord), ylim = c(endDay + lubridate::days(1), startDay), bty = 'n', ann = FALSE, xaxt = 'n', yaxt = 'n')
    mtext(sf(seqDays), side = 2, at = seqDays, las = 1, line = 5.5, cex = 0.8, adj = 0)
    axis(2, seqDays, labels = FALSE, line = 0.5)
    mtext(paste0(seqHours, 'h'), side = 1, at = seqSeconds, las = 0, line = -.5, cex = 0.8)
    axis(1, seqSeconds, labels = FALSE, line = -1)

    # day period colors
    if(colorDay)
    {
        dayColors <- setNames(c('#7f5539', '#cb997e', '#ddbea9', '#b7b7a4', '#9fa58d', '#63705c'), 1:6)
    }else{
        dayColors <- setNames(rep('#7b999e', 6), 1:6)
    }
       
    # add files as lines
    for(i in 1:nrow(dt))
        lines(x = c(dt$startRecord[i], dt$endRecord[i]), y = c(dt$day[i], dt$day[i]), lwd = 8, col = dayColors[substring(dt$period[i], 2, 2)])

    # Label nesting periods
    nestingGroups <- gsub('1', '', grep('1', unique(dt$period), value = TRUE))
    for(i in nestingGroups)
    {   
        groupDayRange <- as.numeric(range(subset(dt, period %in% paste0(i, 1:6))$day))
        # if is the last nesting period, do not print line
        if(!i == nestingGroups[length(nestingGroups)])
            abline(h = groupDayRange[2] + 0.5)
        mtext(paste('Group', i), side = 2, line = 5.8, cex = 0.85, at = groupDayRange[1] + (groupDayRange[2] - groupDayRange[1])/2)
    }

    # Label day periods (TODO)

    # add sampled files if TRUE
    if(!is.null(sampled))
    {
        sampleTypes <- c('main', 'over', 'atlas')
        sampleColors <- setNames(c('#F95700FF', '#00A4CCFF', '#002a29'), sampleTypes)
        for(tp in sampleTypes)
        {
            # filter sample type
            sample_dt <- subset(sampled, sampleType == tp)
            # get dt info of sampled files
            sample_dt <- dt[match(sample_dt$fileName,dt$fileName), ]
            
            # add files as lines
            for(i in 1:nrow(sample_dt))
                lines(x = c(sample_dt$startRecord[i], sample_dt$endRecord[i]), y = c(sample_dt$day[i], sample_dt$day[i]), lwd = 8, col = sampleColors[tp])
        }
    }

    # legend
    if(!is.null(sampled))
    {
        if(colorDay)
        {
            leg <- c('Main samples', 'Over samples', 'Atlas', 'Dawn1', 'Dawn2', 'Dawn3', 'Dusk', 'Night1', 'Night2')
            legColors <- c('#F95700FF', '#00A4CCFF', '#002a29', dayColors)
        }else{
            leg <- c('Main samples', 'Over samples', 'Atlas', 'All files')
            legColors <- c('#F95700FF', '#00A4CCFF', '#002a29', '#7b999e')
        }
    }else{
        if(colorDay)
        {
            leg <- c('Dawn1', 'Dawn2', 'Dawn3', 'Dusk', 'Night1', 'Night2')
            legColors <- dayColors
        }else{
            leg <- 'All files'
            legColors <- '7b999e'
        }
    }

    legend((max(seqSeconds) - min(seqSeconds))/2, seqDays[1], legend = leg, lty = 1, lwd = 6, col = legColors, bty = 'n', cex = 0.75)

    invisible(dev.off())

}
