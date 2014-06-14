  pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    l<-length(id)
    if (id[1]>=1 && id[1]<=332 && id[l]>=1 && id[l]<=332 ){
      t_observ<-c()
      loc_dir<-"C:\\Users\\Ann\\Desktop\\R Course\\"
      for (i in id){
        if (i<10){
          i<-paste("00",i,sep="")
        }
        else{
          if(i<100){
            i<-paste("0",i,sep="")
          }
        }
      pfile<-read.csv(paste(loc_dir,directory,"\\",i,".csv", sep=""))
      x<-pfile[pollutant]
      x<-x[!is.na(x)]
      t_observ<-append(t_observ,x,after=length(t_observ))
      }
      result<-round(mean(t_observ),digits=3)
    }
  }