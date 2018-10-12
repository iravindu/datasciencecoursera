#PART 1
pollutantmean<-function(directory, pollutant, id = 1:332){
  filelist = list.files(path = directory, pattern = ".csv", full.names = TRUE)
  values <- numeric()
  
  for (i in id){
    data<-read.csv(filelist[i])
    values<-c(values, data[[pollutant]])
  }
  mean(values, na.rm = TRUE)
}

#PART 2
complete<-function(directory, id = 1:332){
  filelist = list.files(path = directory, pattern = ".csv", full.names = TRUE)
  values <- numeric()
  
  for (i in id){
    data<-read.csv(filelist[i])
    #new insertion
    completeData<-data[complete.cases(data),]
    values<-c(values,nrow(completeData))
  }
  dataf<-data.frame(id  , nobs = values)
  dataf
}


#part3
x<-c(1,2,3,4,5)
y<-c(3,4,9,8,10)
cor(x,y)
corr<-function(directory,threshold =0){
  filelist = list.files(path = directory, pattern = ".csv", full.names = TRUE)
  values <- numeric()
  id = 1:332
  for (i in id){
    data<-read.csv(filelist[i])
    completeData<-data[complete.cases(data),]
    
    #new insertion
    if(nrow(completeData)>threshold){
    correlation<-cor(completeData$sulfate, completeData$nitrate)
    values<-c(values,correlation)
  }
    
  }
  values
}
  