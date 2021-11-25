# Identify if I am in a directory of either SM4 or Bar-LT recorder
# Return factor recorder type

indentify_recorder <- function(Dir = '.')
{
    # check file formats inside `Dir`
    allFiles <- dir(Dir)
    file_ext <- tools::file_ext(allFiles)

    # first lookfor SM4 files
    if('txt' %in% file_ext)
    {
        # verify if `Summary` is present in .txt file
        # If it is, return SM4
        if(sum(grep('Summary', grep('.txt', allFiles, value = TRUE))) == 0)
        {
            warning('I did not find Summary.txt file for SM4 recorder.\n')
            if(sum(grep('csv', allFiles)) > 0)
                warning('And I found .csv files, it is probably a Bar-LT recorder.\n')
        }else {
            return ( as.factor('SM4') )
        }
    }
3
    if('csv' %in% file_ext) 
    {
        # verify if `Reclog.csv` exists
        if(sum(grep('Reclog.csv', allFiles)) == 0)
        {
            warning('I did not find Reclog.csv file for Bar-LT recoreder\n')
        }else{
            return ( as.factor('Bar-LT'))
        }
    }

    return ( NA )
}
