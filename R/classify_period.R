####################################################################
# Classify 24h period AND nesting period from log data.frame
# Will Vieira
# Feb 3, 2021
####################################################################



####################################################################
# Nesting period (A, B, C, ...)
# - Classified into X equals groups
#
# 24h period (1, 2, 3, 4, 5, and 6):
# - Aurore-Dawn
# - Crépuscule-Dusk
# - Nuit-Night
#
# Final columns in the form of "A1"
####################################################################


# Input:
#   - data.frame from listAudio_sm4() function
#   - start AND end date to sample (in the form of `start = lubridate::as_date('2020-12-01')` )
#   - groups: number of groups within date range. If we have 44 days and 4 groups, each group will have 11 days.
# Output: data.frame with same structure as the output of listAudio_sm4(), with
# an extra columns with 24h period and nesting period
classify_period <- function(dt, startDate, endDate, groups = 5)
{
    songMeter <- dt$songMeter[1]

    # Get days of sample
    uniqueDays <- unique(lubridate::date(dt$time))

    # Create a sequence of days from `startDate` to `endDate` and
    # check if uniqueDays found in dt are present in the sequence
    seqAllDays <- seq(startDate, endDate, by = 'day')
    matchDays <- seqAllDays %in% uniqueDays
    if(!all(matchDays))
         cat(paste('There are missing days in the song meter. These are the days in which no file was recorded:\n',
                     paste0(paste0(' - ', seqAllDays[!matchDays]), collapse = '\n')), '\n\n')
    
    # filter uniqueDays within start and end dates
    uniqueDays <- uniqueDays[uniqueDays >= startDate & uniqueDays <= endDate]

    # Location and time zone from song meter to be used to estimate the daily sunrise and sunset time
    logFile <- read_log(dir(songMeter, full.names = TRUE, pattern = '.txt')[1])
    lat <- unique(logFile$latitude)
    lon <- unique(logFile$longitude)
    dtTZ <- lubridate::tz(dt$time[1])

    # add an empty column in dt to define the 24h AND nesting periods (e.g. A1)
    dt$period <- rep(NA, nrow(dt))
    
    # Define groups for nesting period (A, B, C, D, ...)
    nestingGroups <- split(uniqueDays, cut(uniqueDays, groups))

    # Check if nesting groups are evenly split
    if(!length(unique(lengths(nestingGroups))) == 1)
    {
        nestingPeriod <- rep(LETTERS[1:groups], lengths(nestingGroups))
        cat(length(uniqueDays), 'days cannot be divided evenly in', groups, 'groups. Dividing the days into groups like:\n')
        print(table(nestingPeriod))
    }else{
        nestingPeriod <- rep(LETTERS[1:groups], lengths(nestingGroups))
    }

    # Now for each day, split in the 24h (day) period
    for(Day in 1:length(uniqueDays))
    {
        # Calculate sunrise an sunset time for the specific day and location
        sunlight <- suncalc::getSunlightTimes(date = uniqueDays[Day],
                                              lat = lat, lon = lon,
                                              keep = c('sunrise', 'sunset'),
                                              tz = dtTZ)

        # match calculated sunset and sunrise with song meter `sunset` and `sunrise`
        # If the difference between all files and the calculated sunset and sunrise is smaller than 5 minutes
        # I will classify the target file as the observed sunset/sunrise, if not I keep using the calculated values (because sometimes there are missing files)
        if(min(abs(sunlight$sunrise - dt$time)) < lubridate::minutes(5))
        {
            sunrise <- dt$time[which.min(abs(sunlight$sunrise - dt$time))]
        }else{
            sunrise <- sunlight$sunrise
        }

        if(min(abs(sunlight$sunset - dt$time)) < lubridate::minutes(5))
        {
            sunset <- dt$time[which.min(abs(sunlight$sunset - dt$time))]
        }else{
            sunset <- sunlight$sunset
        }

        
        
        # Calculate the time limits of each of the 3 groups within a day (Dawn, dusk, Nigth)
        
        # Extra time of 10 minutes to make sure all files are within the min/max range
        # So if song meter starts 1h before sunrise, I will look for files within 1h10min before
        extraTime <- lubridate::minutes(10)

        # Dawn 1 [sunrise - 1h, sunrise + 0.5h]
        dawnRows1 <- which(dt$time > (sunrise - lubridate::hours(1) - extraTime) &
                           dt$time < (sunrise + lubridate::minutes(30) + extraTime))

        # Dawn 2 [sunrise + 1h, sunrise + 2.5h]
        dawnRows2 <- which(dt$time > (sunrise + lubridate::hours(1) - extraTime) &
                           dt$time < (sunrise + lubridate::hours(2) + lubridate::minutes(30) + extraTime))

        # Dawn 3 [sunrise + 3h, sunrise + 4.5h]
        dawnRows3 <- which(dt$time > (sunrise + lubridate::hours(3) - extraTime) &
                           dt$time < (sunrise + lubridate::hours(4) + lubridate::minutes(30) + extraTime))

        # Dusk [sunset - 0.5h, sunset + 0.5h]
        duskRows <- which(dt$time > (sunset - lubridate::minutes(30) - extraTime) &
                          dt$time < (sunset + lubridate::minutes(30) + extraTime))
        
        # Nigth 1 [sunset + 1.5h, sunset + 2.5h] <- probably a mistake with camera set, adding 30 minutes
        nightRows1 <- which(dt$time > (sunset + lubridate::hours(1) + lubridate::minutes(30) - extraTime) &
                            dt$time < (sunset + lubridate::hours(2) + lubridate::minutes(40) + extraTime))

        # Nigth 2 [sunset + 3.5h, sunset + 4.5h] <- probably a mistake with camera set, adding 30 minutes
        nightRows2 <- which(dt$time > (sunset + lubridate::hours(3) + lubridate::minutes(30) - extraTime) &
                            dt$time < (sunset + lubridate::hours(5) + lubridate::minutes(10) + extraTime))


        dt$period[c(dawnRows1, dawnRows2, dawnRows3, duskRows, nightRows1, nightRows2)] <-
            paste0(nestingPeriod[Day],
            c(rep(1, length(dawnRows1)),
                rep(2, length(dawnRows2)),
                rep(3, length(dawnRows3)),
                rep(4, length(duskRows)),
                rep(5, length(nightRows1)),
                rep(6, length(nightRows2))))
            
        # Check if we have all 24h periods (12 + 3 + 4)
        # if(any(!c(length(c(dawnRows1, dawnRows2, dawnRows3)), length(duskRows), length(c(nightRows1, nightRows2))) == c(12, 3, 4)))
        #     warning(paste(songMeter, ': Day', uniqueDays[Day], 'is missing a record on the period',
        #         c('Dawn', 'Dusk', 'Nigth')[which(!(c(length(c(dawnRows1, dawnRows2, dawnRows3)), length(duskRows), length(c(nightRows1, nightRows2))) == c(12, 3, 4)))], '.\n'))
    }

    # remove lines with NA (period of time out of the start and end day range)
    dt <- dt[which(!is.na(dt$period)), ]

    return( dt )
}
