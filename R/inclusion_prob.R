####################################################################
# Inclusion probability
# Will Vieira
# Dec 16, 2021
####################################################################



# function to assign inclusion probability as a function of distance to sunset and sunrise
incl_prob <- function(dt, info, sd)
{
    colsToKeep <- names(dt)

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
    dt$TimeSecs <- timeToSeconds(dt$time)


    # get probability distribution in function of sunrise and sunset
    dt$incProb_sunrise <- dnorm(dt$TimeSecs, dt$sunrise_secs, sd)
    dt$incProb_sunset <- dnorm(dt$TimeSecs, dt$sunset_secs, sd)


    return(dt[,c(colsToKeep, 'TimeSecs', 'incProb_sunrise', 'incProb_sunset')])
}
