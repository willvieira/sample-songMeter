####################################################################
# Script to model sound quality based on ACI
# Will Vieira
# Feb 8, 2021
####################################################################




# Prepare dataset
    
    # read csv
    dataset = read.csv('rawData/Yanienhonhndeh_oiseaux2019_Willian.csv')

    # transfrom to real data
    dataset$Time <- lubridate::as_datetime(paste(dataset$Date_A, dataset$Date_M, dataset$Date_J, dataset$eBirdJ_Start.time, sep = '-'), tz = 'America/Toronto')

#



# Load the audio files

    # define the name of the files
    dataset$File <- paste0(dataset$Étiquettes.de.lignes, '_',
                           dataset$Date_A,
                           sprintf("%02d", dataset$Date_M),
                           sprintf("%02d", dataset$Date_J), '_',
                           unlist(lapply(lapply(strsplit(dataset$eBirdJ_Start.time, ':'),
                                function(x) sprintf("%02s", x)),
                                    function(x) paste0(x, collapse = ''))),
                           '.wav')

    # Find which folder the files are
    location <- '../../../../Volumes/One Touch/SOBQ_Yanienhonhndeh/'
    dirs <- grep(unique(dataset$Étiquettes.de.lignes), dir(location), value = TRUE)
    
    for(i in dirs)
    {
        dataset$File[dataset$File %in% dir(paste0(location, i))] <- 
                paste0(location, i, '/',
                dataset$File[dataset$File %in% dir(paste0(location, i))])
    }

    # Check if file exists
    dataset$File[which(!file.exists(dataset$File))]

    # row 14 does not work (seconds are wrong); manually editing here
    dataset$File[14] <- '../../../../Volumes/One Touch/SOBQ_Yanienhonhndeh/S4A09922_1_Yanienhonhndeh2019/S4A09922_20190517_001800.wav'

#




# loop to read audio file, calculate ACI, and delete file for memory save

    ACI_ls <- list()
    for(File in 1:nrow(dataset))
    {
        aud <- tuneR::readWave(dataset$File[File])
        duration <- length(aud@left)/aud@samp.rate

        # add quality + ACI into list
        ACI_ls[[File]] <- c(dataset$Qlt_audio[File], seewave::ACI(aud, nbwindows = duration/20))

        # progress
        cat('   Calculating for file', File, 'of', nrow(dataset), '\r')
    }

    # merge into dataframe and save
    ACI_quality <- setNames(data.frame(do.call(rbind, ACI_ls)), c('quality', 'ACI'))
    saveRDS(ACI_quality, file = 'rawData/ACI_quality2.RDS')

    # Plot ACI as a function of audio quality
    boxplot(ACI ~ quality, ACI_quality)
#




# Model audio quality as a function of ACI

    aud1 <- tuneR::readWave(dataset$File[1])
    aud2 <- tuneR::readWave(dataset$File[24])

    duration1 <- length(aud1@left)/aud1@samp.rate
    duration2 <- length(aud2@left)/aud2@samp.rate

    seewave::ACI(aud1, nbwindows = duration1/20)
    seewave::ACI(aud2, nbwindows = duration2/20)

#