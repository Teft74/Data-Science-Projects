complete <- function(directory, id = 1:332) {
      ## 'directory' is a character vector of length 1 indicating
      ## the location of the CSV files
      datalist <- list.files(directory, full.names=TRUE)
      ## 'id' is an integer vector indicating the monitor ID numbers
      ## to be used
      dat <-data.frame()
      for(i in id){
            dat<- rbind(dat, read.csv(datalist[i]))
      }
      ## Return a data frame of the form:
      ## id nobs
      ## 1  117
      ## 2  1041
      ## ...
      ## where 'id' is the monitor ID number and 'nobs' is the
      ## number of complete cases
      dat <- dat[!is.na(dat$sulfate),]
      dat <- dat[!is.nan(dat$sulfate),]
      dat <- dat[!is.nan(dat$nitrate),]
      dat <- dat[!is.na(dat$nitrate),]  ## dat consist only of complete cases
      dat_id <- dat[,4]                   ## only ids
      df <- data.frame(NULL)
      for(i in id){
            df <- rbind(df, c(i, length(dat_id[dat_id == i])))
      }
      names(df) <- c("id", "nobs")
      df
}