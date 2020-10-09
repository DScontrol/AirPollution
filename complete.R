# R Programming: Programmimg Assignament
# Part 2
# By Alex M

#Write a function that reads a directory full of files and reports the number of completely observed 
#cases in each data file. The function should return a data frame where the first column is the name 
#of the file and the second column is the number of complete cases. A prototype of this function follows

complete <- function(directory, id=1:332){
  
  n<-c(1:length(id))
  cont<-1
  # leemos los archivos
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
    data<-read.csv(filepath, header=TRUE, sep=",") 
    dataNotNA<-data[!is.na(data$nitrate) & !is.na(data$sulfate) & !is.na(data$Date) & !is.na(data$ID) , ]
    n[cont]<-nrow(dataNotNA)
    cont<-cont+1
    
  }
  n
  
}

# FALTA CONSTRUIR EL DATA FRAME!!
  
#source("complete.R")
#complete("specdata", 1)
#-------------------------
##   id nobs
## 1  1  117

#complete("specdata", c(2, 4, 8, 10, 12))
###   id nobs
## 1  2 1041
## 2  4  474
## 3  8  192
## 4 10  148
## 5 12   96

#complete("specdata", 30:25)
##   id nobs
## 1 30  932
## 2 29  711
## 3 28  475
## 4 27  338
## 5 26  586
## 6 25  463

#complete("specdata", 3)
##   id nobs
## 1  3  243