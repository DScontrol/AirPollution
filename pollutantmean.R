# R Programming: Programmimg Assignament
# Part 1
# By Alex M

#Write a function named 'pollutantmean' that calculates the mean of a pollutant 
#(sulfate or nitrate) across a specified list of monitors. The function 'pollutantmean' 
#takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID 
#numbers, 'pollutantmean' reads that monitors' particulate matter data from the 
#directory specified in the 'directory' argument and returns the mean of the 
#pollutant across all of the monitors, ignoring any missing values coded as NA. 
#A prototype of the function is as follows

#print(R.version.string)

pollutantmean<-function(directory,pollutant, id=1:332) {
  
  print(directory)
  
  # leemos los archivos
  
  cont<-0
  for (i in id) {
          #preparamos el nombre del fichero
          if (i<10) { filename<-paste("00",i,sep="")}
          if (i>=10) { filename<-paste("0",i,sep="")}
          if (i>=100) { filename<-paste(i,sep="")}
    
          filename<-paste(filename,".csv",sep="")
          print(filename)      
          filepath<-paste(getwd(),"/",directory,"/",filename,sep="")
          print(filepath)      
          #guardamos el contenido del fichero
          dataaux<-read.csv(filepath, header=TRUE, sep=",")
          str(dataaux)
          if (cont==0)  {Alldata<-dataaux
                          rm(dataaux)
                         }
            else { Alldata<- merge.data.frame(Alldata,dataaux, all=TRUE)
                   rm(dataaux)
                  }
          cont<-cont+1
          str(Alldata)
     }
  
  #Una vez almacenados los ficheros calculamos la media del atriubuto solicitado
  if(identical(pollutant,"nitrate")) {
                                       AlldataNA<-Alldata[!is.na(Alldata$nitrate), ]
                                       str(AlldataNA)
                                       M<-mean(AlldataNA$nitrate)
  }
  if(identical(pollutant,"sulfate")) {
                                      AlldataNA<-Alldata[!is.na(Alldata$sulfate), ]
                                      str(AlldataNA)
                                      M<-mean(AlldataNA$sulfate)
    }
  M
}
#source("pollutantmean.R")
#pollutantmean("specdata", "sulfate", 1:10)
#pollutantmean("specdata","nitrate",70:72) no sale igual!!!!
#pollutantmean("specdata","nitrate", 23)

