get_path <- function(id, directory = "~/GIT/R Programming /HW1/specdata/"){ #opening function
      #takes id (int) directory (char), returns string with location of corresponding CSV 
      
      #GETTING THE NAME
      
      id_char <- toString(id) 
      if (nchar(id_char)==3){
            file_name <- paste(id_char,".csv", sep="") #we store the name as char
      }
      else if(nchar(id_char)==2){ #if the id has two digits
            file_name <- paste("0",id_char,".csv", sep="") #we need to add a zero in front 
      } 
      else if (nchar(id_char) == 1){ #else: the ID has one digit
            file_name <- paste("00",id_char,".csv",sep="") # we add two zeros
      }
      
      #GETTING THE PATH
      path <- paste(directory, file_name,sep="")
      
      #RETURNING THE PATH
      path
      
}#closing function

how_many_complete <- function(id = 1, directory = "~/GIT/R Programming /HW1/specdata"){
      #takes an id, returns number of complete cases within the corresponding file
      sum(complete.cases(read.csv(get_path(id))))
}

one_cor <-function(id = "1"){
      path <- get_path(id)
      temp <- read.csv(path)
      temp <- temp[complete.cases(temp),]
      
      return(cor(temp[["sulfate"]],temp[["nitrate"]]))
      
}#closing function

corr<- function(directory = "~/GIT/R Programming /HW1/specdata", treshold = 0){
      correlations <- numeric() #initializing for results
      for(id in 1:323){
            if(how_many_complete(id)>=treshold){
                  correlations <- append(correlations,one_cor(id))
            }#closing if statement
      }#closing for loop
      correlations #returning
}#closing function


#corr<- function(directory = "~/GIT/R Programming /HW1/specdata", treshold = 0){
#      corr_vector <- numeric()
#      
#      for(i in 1:332){
#            if(how_many_complete(i)>=treshold){
#                  print(paste(i, "satisfies the codition"))
#                  file <- read.csv(get_path(i))
#                  print("file loaded")
#                  file <- file[complete.cases(file),]
#                  print("complete cases found")
#                  sulfate <- as.numeric(file[["sulftate"]])
#                  nitrate <- as.numeric(file[["nitrate"]])
#                  #correlation<- cor(,)
#                  #corr_vector <- append(corr_vector, correlation)
#            } #closing if statement
#      } #closing loop
#      corr_vector
#}#closing function

