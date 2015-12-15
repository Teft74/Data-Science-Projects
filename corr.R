corr <- function(directory, threshold = 0) {
      ## 'directory' is a character vector of length 1 indicating
      ## the location of the CSV files
      datalist <- list.files(directory, full.names=TRUE)
      dat <-data.frame()
      for(i in 1:332){
            dat<- rbind(dat, read.csv(datalist[i]))
      }
     
      dat <- dat[!is.na(dat$sulfate),]
      dat <- dat[!is.nan(dat$sulfate),]
      dat <- dat[!is.nan(dat$nitrate),]
      dat <- dat[!is.na(dat$nitrate),] ##data of complete cases
      ## 'threshold' is a numeric vector of length 1 indicating the
      ## number of completely observed observations (on all
      ## variables) required to compute the correlation between
      ## nitrate and sulfate; the default is 0
       
      ## Return a numeric vector of correlations
      ## NOTE: Do not round the result!
      correl <- c(NULL)
      
      for(i in 1:332){
            if(nrow(subset(dat, dat$ID == i))>threshold)
                  
                  correl[i] <- cor(subset(dat, dat$ID == i)$sulfate, subset(dat, dat$ID == i)$nitrate)
      }
      
      correl[!is.na(correl)]
      }