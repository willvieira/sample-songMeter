# Script to generate fake audio data
#########################################


# define variables
date_start <- lubridate::ymd('2020-05-01')
date_end <- lubridate::ymd('2020-07-31')

input <- 'fake_volume'
songMeter <- '000-000-1-INVENTAIRE'

audio_duration <- 600 # in seconds
pause <- 1200 # pause time betweeen audio in seconds
file_size <- 115000 # kb

# # programs
# ARU_program <- data.frame(
#     sousprog = c('morning', 'evening'),
#     start = c('SUSI'),
# )


sono_dt <- data.frame()
for(Day in 1:length(seq(date_start, date_end, by = 'day')))
{

    # sunrise and sunset
    sunlight <- suncalc::getSunlightTimes(
            date = seq(date_start, date_end, by = 'day')[Day],
            lat = 54.72768,
            lon = -67.45711,
            keep = c('sunrise', 'sunset'),
            tz = 'America/Toronto'
    )


    # range of audios from sunset/sunrise
    range_hours_sunrise <- c(lubridate::hours(3), lubridate::hours(6))
    range_hours_sunset <- c(lubridate::hours(6), lubridate::hours(2))

    
    # SUNRISE
    nb_audio <- (range_hours_sunrise[1] + range_hours_sunrise[2])/lubridate::seconds(audio_duration + pause)

    sunrise_audio <- sunlight$sunrise -
        range_hours_sunrise[1] +
        lubridate::seconds((0:nb_audio) * (audio_duration + pause))


    # SUNSET
    nb_audio <- (range_hours_sunset[1] + range_hours_sunset[2])/lubridate::seconds(audio_duration + pause)

    sunset_audio <- sunlight$sunset -
        range_hours_sunset[1] +
        lubridate::seconds((0:nb_audio) * (audio_duration + pause))

    
    # apend df
    sono_dt <- rbind(
        sono_dt,
        data.frame(
            time = c(sunrise_audio, sunset_audio),
            input = input,
            songMeter = songMeter,
            fileName = paste0(
                songMeter,
                '_',
                c(
                    format(sunrise_audio, '%y%m%d_%H%M%S'),
                    format(sunset_audio, '%y%m%d_%H%M%S')
                ),
                '.wav'
            ),
            fileSize = file_size,
            duration = audio_duration
        )
    )
    
}
