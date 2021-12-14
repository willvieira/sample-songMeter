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
classify_period <- function(dt, program, info, nesting_groups = 5)
{
    # subset program to specific song meter
    program <- subset(program, inventairecode == info$InventaireCode)


    # save program
    write.csv2(
        program,
        file.path(outputFolder, unique(dt$songMeter), 'sample_program.csv'),
        row.names = FALSE
    )


    # log arguments into temp file
    logMsg(
        paste0(
            '\nClassify audio:\n',
            'nesting_groups: ', nesting_groups
        ),
        console = FALSE
    )


    # Get days of sample
    uniqueDays <- unique(lubridate::date(dt$time))


    # Create a sequence of days from `startDate` to `endDate` and
    # check if uniqueDays found in dt are present in the sequence
    seqAllDays <- seq(
        min(program$ech_datedebut),
        max(program$ech_datefin),
        by = 'day'
    )
    
    matchDays <- seqAllDays %in% uniqueDays
    if(!all(matchDays))
         logMsg(
             paste0(
                'There are missing days in the song meter. These are the days in which no file was recorded:\n',
                paste0(paste0(' - ', seqAllDays[!matchDays]), collapse = '\n'))
        )
    
    # filter uniqueDays within start and end dates
    uniqueDays <- uniqueDays[
        uniqueDays >= min(program$ech_datedebut) &
        uniqueDays <= max(program$ech_datefin)
    ]


    # Define groups for nesting period (A, B, C, D, ...)
    nestingGroups <- split(uniqueDays, cut(uniqueDays, nesting_groups))

    # Check if nesting groups are evenly split
    if(!length(unique(lengths(nestingGroups))) == 1)
    {
        nestingPeriod <- rep(LETTERS[1:nesting_groups], lengths(nestingGroups))

        logMsg(
            paste(length(uniqueDays), 'days cannot be divided evenly in', nesting_groups, 'nesting groups. Dividing the days into groups as follows:\n',
            paste0(names(table(nestingPeriod)), collapse = ' | '), '\n',
            paste0(table(nestingPeriod), collapse = ' | '))
        )
    }else{
        nestingPeriod <- rep(LETTERS[1:nesting_groups], lengths(nestingGroups))
    }


    # add an empty column in dt to define the 24h AND nesting periods (e.g. A1)
    dt$period <- NA


    # Now for each day, split in the 24h (day) period
    for(Day in 1:length(uniqueDays))
    {     
        # get sunrise and sunset for the day
        sunlight <- suncalc::getSunlightTimes(
            date = uniqueDays[Day],
            lat = info$Lat_DegDecValide,
            lon = info$Long_DegDecValide,
            keep = c('sunrise', 'sunset'),
            tz = lubridate::tz(dt$time[1])
        )
   
        # classify audio for each nesting period
        for(dayPeriod in program$ech_descriptif)
        {
            program_p <- subset(program, ech_descriptif == dayPeriod)
            
            # is the specific `Day` within the range of days?
            if(uniqueDays[Day] %in% seq(program_p$ech_datedebut, program_p$ech_datefin, by = 'day')) {
                
                time_range <- program_range(
                    sunlight = sunlight,
                    program = program_p
                )

                periodRows <- which(
                    dt$time > time_range$startTime &
                    dt$time < time_range$endTime
                )

                dt$period[periodRows] <- paste0(
                    dayPeriod,
                    '_',
                    nestingPeriod[Day]
                )
            }
        }
    }

    # remove lines with NA (period of time out of the start and end day range)
    dt <- dt[which(!is.na(dt$period)), ]

    return( dt )
}



# function to get range of max and min time given (sunset/sunrise) and program set
program_range <- function(sunlight, program)
{
    # Extra time of 10 minutes to make sure all files are within the min/max range
    extraTime <- lubridate::minutes(5)


    # get start time
    if(substr(program$ech_heuredebut, 1, 4) == 'SSET') {
        if(substr(program$ech_heuredebut, 5, 5) == '+') {
            startTime <- 
                sunlight$sunset +
                HMS(program$ech_heuredebut)
        }else{
            startTime <- 
                sunlight$sunset -
                HMS(program$ech_heuredebut)
        }
    }else if(substr(program$ech_heuredebut, 1, 4) == 'SRIS') {
        if(substr(program$ech_heuredebut, 5, 5) == '+') {
            startTime <-
                sunlight$sunrise +
                HMS(program$ech_heuredebut)
        }else{
            startTime <-
                sunlight$sunrise -
                HMS(program$ech_heuredebut)
       }
    }else{
        stop('No pattern "SSET" or "SRIS" found in `ech_heuredebut`')
    }

    # get end time
    if(substr(program$ech_heurefin, 1, 4) == 'SSET') {
        if(substr(program$ech_heurefin, 5, 5) == '+') {
            endTime <- 
                sunlight$sunset +
                HMS(program$ech_heurefin)
        }else{
            endTime <- 
                sunlight$sunset -
                HMS(program$ech_heurefin)
        }
    }else if(substr(program$ech_heurefin, 1, 4) == 'SRIS') {
        if(substr(program$ech_heurefin, 5, 5) == '+') {
            endTime <-
                sunlight$sunrise +
                HMS(program$ech_heurefin)
        }else{
            endTime <-
                sunlight$sunrise -
                HMS(program$ech_heurefin)
       }
    }else{
        stop('No pattern "SSET" or "SRIS" found in `ech_heurefin`')
    }

    return(
        list(
            startTime = startTime - extraTime,
            endTime = endTime + extraTime
        )
    )
}



# function to extract hour, minute and seconds from string
HMS <- function(string)
{
    if(nchar(string) != 11)
        stop('ech_heuredebut/ech_heurefin does not have 11 characters (error in HMS function)')

    # get everything after pattern "SSSS+"
    hhmmss <- substr(string, 6, 11)

    # split string by chuncks of two
    hhmmss_split <- substring(hhmmss, seq(1, 5, 2), seq(2, 6, 2))

    # put all together with `:` and transform in date format with lubridate
    lubridate::hms(paste0(hhmmss_split, collapse = ':'))
}
