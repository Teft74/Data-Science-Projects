pollutantmean <- function(directory, pollutant, id = 1:332) {
      ## 'directory' is a character vector of length 1 indicating
      ## the location of the CSV files
      datalist <- list.files(directory, full.names=TRUE)
      ## 'pollutant' is a character vector of length 1 indicating
      ## the name of the pollutant for which we will calculate the
      ## mean; either "sulfate" or "nitrate".
      
      ## 'id' is an integer vector indicating the monitor ID numbers
      ## to be used
      dat <-data.frame()
      for(i in id){
            dat<- rbind(dat, read.csv(datalist[i]))
      }
      ## Return the mean of the pollutant across all monitors list
      ## in the 'id' vector (ignoring NA values)
      ## NOTE: Do not round the result!
      dat_subset <- dat[pollutant]
      dat_subset <- dat_subset[!is.na(dat_subset)]
      dat_subset <- dat_subset[!is.nan(dat_subset)]
      mean(dat_subset)
}