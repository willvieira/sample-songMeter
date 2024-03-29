####################################################################
# Plot files from a sound level meter
# Will Vieira
# Feb 15, 2021
####################################################################


plot_files <- function(dt, outputFile = NULL)
{
    # define timezone
    dt_tz <- lubridate::tz(dt$time[1])

    # define sequence of days (y axis)
    startDay <- lubridate::date(min(dt$time))
    endDay <- lubridate::date(max(dt$time))
    seqDays <- seq(startDay, endDay, by = 'day')    

    # function to transform any time within a day into a continous value of seconds (from 0 to 86400 seconds in a day)
    timeToSeconds <- function(Time)
        (lubridate::hour(Time) * 3600) + (lubridate::minute(Time) * 60) + lubridate::second(Time)

    # function to convert to human readable date
    sf <- suppressMessages(lubridate::stamp_date('Jan 17, 99'))

    # Get only day from time
    dt$day <- lubridate::date(dt$time)

    # Get position of each start time in seconds
    dt$startRecord <- timeToSeconds(dt$time)
    dt$endRecord <- dt$startRecord + dt$duration

    # # Ajust startRecord and endRecord so that audios recorded after midnigth are shown after those from the dusk period
    # minStartRecord <- min(subset(dt, period %in% paste0(LETTERS[1:5], 1))$startRecord)
    # minStartRecord <- minStartRecord - 300 # minus 5 minutes
    # dt$startRecord[which(dt$startRecord < minStartRecord)] <- dt$startRecord[which(dt$startRecord < minStartRecord)] + 86400

    # # calculate final record position in seconds
    # dt$endRecord <- dt$startRecord + dt$duration

    # # For all recorders starting after midnight, calculate DAY - 1
    # dt$day[which(dt$startRecord > 86400)] <- dt$day[which(dt$startRecord > 86400)] - lubridate::days(1)

    # # define sequence of time (x axis)
    # seqHours <- seq(round(minStartRecord/3600, 0), 24 + round(minStartRecord/3600, 0))
    # seqSeconds <- seqHours * 3600
    # seqHours[which(seqHours > 23)] <- seqHours[which(seqHours > 23)] - 24


    # define sequence of time (x axis)
    seqSecs <- setNames(0:24 * 3600, paste0(0:24, 'h'))


    pdf(height = length(unique(dt$day))/5, width = 12.43, file = outputFile)
    par(mar = c(2, 7, 2, 0.5))
    plot(0, pch = '', xlim = c(0, 86400), ylim = c(endDay + lubridate::days(1), startDay), bty = 'n', ann = FALSE, xaxt = 'n', yaxt = 'n')
    mtext(sf(seqDays), side = 2, at = seqDays, las = 1, line = 5.5, cex = 0.8, adj = 0)
    axis(2, seqDays, labels = FALSE, line = 0.5)
    mtext(names(seqSecs), side = 1, at = seqSecs, las = 0, line = -.5, cex = 0.8)
    axis(1, seqSecs, labels = FALSE, line = -1)


    if('main' %in% names(dt))
    {
        sampleColors = setNames(c('red', 'violet'), c('main', 'over'))
        for(tp in c('main', 'over'))
        {
            # filter sample type
            sampled <- which(dt[, tp] == 1)

            # add files as lines
            for(i in sampled)
                lines(
                    x = c(dt$startRecord[i], dt$endRecord[i]),
                    y = c(dt$day[i], dt$day[i]),
                    lwd = 10, col = sampleColors[tp]
                )
        }
    }

    # Label nesting periods and day periods
    if('period' %in% names(dt))
    {
        # get unique day period codes
        dayPeriod_name <- unique(gsub('*_.', '', unique(dt$period)))

        # color day period
        dayPeriod_col <- setNames(
            viridis::viridis(length(dayPeriod_name)),
            dayPeriod_name
        )

        # get unique nesting periods
        nestingPeriod_name <- na.omit(unique(gsub('.*_', '', dt$period)))

        # add line to split nesting groups  
        for(i in nestingPeriod_name)
        {   
            groupDayRange <- as.numeric(
                range(
                    subset(dt, period %in% paste0(dayPeriod_name, '_', i))$day
                )
            )

            # skip if it's the last group
            if(!i == nestingPeriod_name[length(nestingPeriod_name)])
                abline(h = groupDayRange[2] - 0.5)
            
            mtext(paste('Group', i), side = 2, line = 5.8, cex = 0.85, at = groupDayRange[1] + (groupDayRange[2] - groupDayRange[1])/2)
        }

        # add files as lines
        for(i in 1:nrow(dt))
            lines(
                x = c(dt$startRecord[i], dt$endRecord[i]),
                y = c(dt$day[i], dt$day[i]),
                lwd = 6, col = dayPeriod_col[gsub('*_.', '', dt$period[i])]
            )
    }else if('incl_prob' %in% names(dt)) {

        # define color gradient in function of probability
        dt$col <- viridis::viridis(30)[as.numeric(cut(dt$incl_prob, breaks = 30))]

        # add files as lines
        for(i in 1:nrow(dt))
            lines(
                x = c(dt$startRecord[i], dt$endRecord[i]),
                y = c(dt$day[i], dt$day[i]),
                lwd = 6, col = dt$col[i]
            )
    }else{
        # add files as lines
        for(i in 1:nrow(dt))
            lines(
                x = c(dt$startRecord[i], dt$endRecord[i]),
                y = c(dt$day[i], dt$day[i]),
                lwd = 6, col = '#63705c'
            )
    }


    # legend
    if('period' %in% names(dt))
    {
        if('main' %in% names(dt))
        {
            leg <- c(
                'Main samples', 'Over samples',
                dayPeriod_name
            )
            legColors <- c(sampleColors, dayPeriod_col)
        }else{
            leg <- dayPeriod_name
            legColors <- dayPeriod_col
        }

        legend(86400/2, seqDays[1], legend = leg, lty = 1, lwd = 6, col = legColors, bty = 'n', cex = 0.75)
    }else if('incl_prob' %in% names(dt)) {
        
        if('main' %in% names(dt))
        {
            leg <- c(
                'Main samples', 'Over samples'
            )
            legColors <- c(sampleColors)

            legend(86400/2, seqDays[1], legend = leg, lty = 1, lwd = 6, col = legColors, bty = 'n', cex = 0.75)
        }else{
            
        }

        # legend(86400/2, seqDays[1], legend = leg, lty = 1, lwd = 6, col = legColors, bty = 'n', cex = 0.75)

    }

    invisible(dev.off())
}
