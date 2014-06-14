complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  l<-length(id)
  rcount<-0
  if (id[1]>=1 && id[1]<=332 && id[l]>=1 && id[l]<=332){
    
    loc_dir<-"C:\\Users\\Ann\\Desktop\\R Course\\"
    
    df<-data.frame(id=integer(l),nobs=integer(l),stringsAsFactors = FALSE)
    for (i in id){
      r_i<-i
      if (i<10){
        i<-paste("00",i,sep="")
      }
      else{
        if(i<100){
          i<-paste("0",i,sep="")
        }
      }
      rcount<-rcount+1
      pfile<-read.csv(paste(loc_dir,directory,"\\",i,".csv", sep=""))
      a<-complete.cases(pfile)
      b<-length(a[a==TRUE])
      df$id[rcount]=r_i
      df$nobs[rcount]=b
      
    }
df
}
}
