####################################################################
# Inclusion probability
# Will Vieira
# Dec 16, 2021
####################################################################



# function to assign inclusion probability as a function of distance to sunset and sunrise
incl_prob <- function(dt, info, sd_sunrise, sd_sunset, logMsg = TRUE)
{
    colsToKeep <- names(dt)

    # log arguments into temp file
    if(logMsg)
        logMsg(
            paste0(
                '\nInclusion probability:\n',
                'sd_sunset: ', sd_sunset, '\n',
                'sd_sunrise: ', sd_sunrise
            ),
            console = FALSE
        )

    # function to transform any time within a day into a continous value of seconds (from 0 to 86400 seconds in a day)
    timeToSeconds <- function(Time)
        (lubridate::hour(Time) * 3600) + (lubridate::minute(Time) * 60) + lubridate::second(Time)

    # Get sunset and sunrise form each day
    sunlight <- suncalc::getSunlightTimes(
            date = lubridate::date(dt$time),
            lat = info$Lat_DegDecValide,
            lon = info$Long_DegDecValide,
            keep = c('sunrise', 'sunset'),
            tz = lubridate::tz(dt$time[1])
    )
    
    # transform time to seconds
    dt$sunrise_secs <- timeToSeconds(sunlight$sunrise)
    dt$sunset_secs <- timeToSeconds(sunlight$sunset)
    dt$timeSecs <- timeToSeconds(dt$time)


    # get probability distribution in function of sunrise and sunset
    dt$incProb_sunrise <- dnorm(dt$timeSecs, dt$sunrise_secs, sd_sunrise)
    dt$incProb_sunset <- dnorm(dt$timeSecs, dt$sunset_secs, sd_sunset)


    return(dt[,c(colsToKeep, 'timeSecs', 'incProb_sunrise', 'incProb_sunset')])
}
