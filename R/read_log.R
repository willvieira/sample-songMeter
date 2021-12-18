####################################################################
# Internal functions to read log files and list files for both song meters (SM4 and Bar-LT)
# Will Vieira
# January 10, 2021
####################################################################

# read log file from SM4 song meter
# Return data.frame
read_sm4 <- function(File)
{
    # read text file as a full string (skipping first line as it has the incorrect col name)
    textFile <- read.table(File, skip = 1)

    # Some log files have the header in the meadle of the file (don't know the reason)
    # So I will look for the word `DATE` in every line, if TRUE, I will remove the line
    if(any(lengths(apply(textFile, 1, function(x) grep('DATE', x))) > 0))
    {
        textFile <- textFile[!lengths(apply(textFile, 1, function(x) grep('DATE', x))) > 0, ]
        # Retransform back to data.frame
        textFile <- data.frame(V1 = textFile)
    }
    
    # Split character by comma and keep data.frame format
    textFile <- as.data.frame(do.call(rbind, strsplit(textFile$V1, ',')))

    # Correct latitude and longitude
    # Let positive if latitude is on North
    textFile[textFile[, 4] == 'n', 3] <- as.numeric(textFile[textFile[, 4] == 'n', 3]) * +1
    textFile[textFile[, 4] == 's', 3] <- as.numeric(textFile[textFile[, 4] == 's', 3]) * -1
    # Let positive if longitude is on w
    textFile[textFile[, 6] == 'e', 5] <- as.numeric(textFile[textFile[, 6] == 'e', 5]) * +1
    textFile[textFile[, 6] == 'w', 5] <- as.numeric(textFile[textFile[, 6] == 'w', 5]) * -1
    # Drop N W columns
    textFile <- textFile[, -c(4, 6)]

    # Transform first (date) and second (time) in one column of type `POSIXct`
    # First get timezone from coordinates
    camTZ <- lutz::tz_lookup_coords(lat = as.numeric(textFile[1, 3]),
                                    lon = as.numeric(textFile[1, 4]),
                                    method = 'accurate')
    
    # Merge date + time and create POSIXct with timezone
    textFile$time <- lubridate::ymd_hms(paste(textFile[, 1], textFile[, 2]), tz = camTZ)
    # Drop date and time columns
    textFile <- textFile[, -c(1, 2)]

    # Rename and reorder columns
    colnames(textFile) <- c('latitude', 'longitude', 'power', 'temperature', 'nbFiles', 'mic0_type', 'mic1_type', 'time')
    textFile <- textFile[, c(8, 1:7)]

    # reclassify some columns
    textFile[, 2:5] <- sapply(textFile[, 2:5], as.numeric)
    textFile$nbFiles <- as.factor(textFile$nbFiles)

    return(textFile)
}

# read log file from bar-LT recorder
# Return data.frame
read_BarLT <- function(file)
{
    # read text file as a full string (skipping first and second line as it has the incorrect col name)
    textFile <- read.table(file, skip = 2, fill = NA, sep = ',')

    # get latitude and longitude
    latlong <- gsub('\\].*', '', gsub('.*\\[', '', textFile[1, 4]))
    latitude <- as.numeric(substr(latlong, 1, 8))
    longitude <- as.numeric(gsub('^.{8}', '', latlong))

    # Transform first (date), second (time), and third (timezone) in one column of type `POSIXct`
    camTZ <- lutz::tz_lookup_coords(lat = latitude,
                                    lon = longitude,
                                    method = 'accurate')                    
    timeVec <- lubridate::dmy_hms(paste0(textFile[, 1], textFile[, 2]), tz = camTZ)

    # file names (remove prefix generic local 0:/)
    fileNames <- gsub('0:/', '', textFile$V4)

    # Camera status
    decibel <- textFile$V5
    batteryVoltage <- textFile$V7
    batteryFull <- textFile$V8
    CID <- textFile$V11
    SD_capacity <- textFile$V13
    SD_freeSpace <- textFile$V12

    # create output data.frame
    newTextFile <- data.frame(time = timeVec,
                              latitude = rep(latitude, length(timeVec)),
                              longitude = rep(longitude, length(timeVec)),
                              fileName = fileNames,
                              decibel = decibel,
                              batteryVoltage = batteryVoltage,
                              batteryFull = batteryFull,
                              SD_capacity = SD_capacity,
                              SD_freeSpace = SD_freeSpace)

    return(newTextFile)
}


# Wrapper of above functions
# Return data.frame
read_log <- function(file)
{
    # check if file exists
    if(!file.exists(file)) stop('File does not exist. Check the argument `file`.')
    
    # check file extension to decide which function use
    fileExt <- tools::file_ext(file)

    if(fileExt == 'txt')
    {
        return( read_sm4(file) )
    }else if(fileExt == 'csv')
    {
        return( read_BarLT(file) )
    }else{

        stop( paste0('File extension (`.', fileExt, '`) is not supported. You should try either a `.csv` or `.txt` file.') )
    }
}


# List audio files from a specific song meter directory
# Input:
#   - directory
#   - durationRange_3: vector of minimum and maximum range of size for 3 min files
#   - durationRange_10: vector of minimum and maximum range of size for 10 min files
# Return data.frame similar to log
listAudio <- function(input, songMeter, durationRange_3 = NA, durationRange_10 = NA, start_date = NA, end_date = NA, logMsg = TRUE)
{
    # log arguments into temp file
    if(logMsg)
        logMsg(
            paste0(
                'songMeter:', songMeter, '\n',
                paste0(rep('#', 50), collapse = ''), '\n\n',
                'List audio:\n',
                'durationRange_3: ', paste(durationRange_3, collapse = ','), '\n',
                'durationRange_10: ', paste(durationRange_10, collapse = ','),
                '\n',
                'start_date: ', start_date,
                '\n',
                'end_date: ', end_date
            ),
            console = FALSE,
            append = FALSE
        )


    files <- dir(file.path(input, songMeter), pattern = '.wav')

    # log
    if(logMsg)
        logMsg(
            paste('A total of', length(files), '.wav files were found in the song meter folder')
        )

    # get size of files
    fileSizes <- file.size(file.path(input, songMeter, files))


    # calculate recording time based on file size (and sample rate and bits)
    audio1 <- tuneR::readWave(
        file.path(
            input,
            songMeter,
            files[which(fileSizes/1e3 > 5000)[1]]
        )
    )
    duration <- fileSizes/
        (audio1@samp.rate * (audio1@bit/8) * ifelse(audio1@stereo, 2, 1))


    # Filter files within size range in Kb    
    if( !is.na(durationRange_3[1]) )
    {
        if( !is.na(durationRange_10[1]) )
        {
            toKeep <- which(
                duration > durationRange_3[1] &
                duration <= durationRange_3[2] |
                duration > durationRange_10[1] &
                duration <= durationRange_10[2]
            )

            # filter files
            files <- files[toKeep]
            fileSizes <- fileSizes[toKeep]
            duration <- duration[toKeep]

            # create a group to classify files into 3 or 10 min
            groupeDure <- ifelse(
                 duration > durationRange_3[1] &
                duration <= durationRange_3[2],
                '3min',
                '10min'
            )
        }else{
            if(logMsg)
                logMsg('Ignore filtering files by duration because at least one of the `durationRange_` arguments are `NA`')
            
            groupeDure <- NA
        }
    }else{
        if(logMsg)
            logMsg('Ignore filtering files by duration because at least one of the `durationRange_` arguments are `NA`')

        groupeDure <- NA
    }


    ## Extract time from file name
    # remove extension and recorder name
    yearHour <- gsub('^[^_]*_', '', gsub('.wav', '', files))
    formatedTime <- lubridate::ymd_hms(yearHour, tz = 'America/Toronto') # TODO



    # Filter data range
    if(!is.na(start_date))
    {
        # Get days of sample
        uniqueDays <- unique(lubridate::date(formatedTime))


        # Create a sequence of days from `start_date` to `end_date` and
        # check if uniqueDays found in dt are present in the sequence
        seqAllDays <- seq(
            start_date,
            end_date,
            by = 'day'
        )
        
        matchDays <- seqAllDays %in% uniqueDays
        if(!all(matchDays))
            if(logMsg)
                logMsg(
                    paste0(
                        'There are missing days in the song meter. These are the days in which no file was recorded:\n',
                        paste0(paste0(' - ', seqAllDays[!matchDays]), collapse = '\n'))
                )
        
        # filter uniqueDays within start and end dates
        daysToKeep <- which(
            lubridate::date(formatedTime) >= start_date &
            lubridate::date(formatedTime) <= end_date
        )

        formatedTime <- formatedTime[daysToKeep]
        files <- files[daysToKeep]
        fileSizes <- fileSizes[daysToKeep]
        duration <- duration[daysToKeep]
        groupeDure <- groupeDure[daysToKeep]

    }else{
        daysToKeep <- 1:length(formatedTime)
    }



    if(logMsg)
        logMsg(
            paste('Saving', length(daysToKeep), 'after filtering (if specified)')
        )


    data.frame(
        time = formatedTime,
        input = input,
        songMeter = songMeter,
        fileName = files,
        fileSize = fileSizes/1e3,
        duration = duration,
        groupeDure = groupeDure
    )

}




# Function to read and format ARU program code
# Input: excel file
# output: data.frame
read_program <- function(File)
{
    # read excel file
    df <- readxl::read_excel(File)

    # put all col names to lower to avoid typo
    names(df) <- tolower(names(df))

    # class some columns
    df$ech_datedebut <- lubridate::ymd(df$ech_datedebut)
    df$ech_datefin <- lubridate::ymd(df$ech_datefin)

    return ( as.data.frame(df) )
}
