get_path <- function(id, directory = "~/GIT/R Programming /HW1/specdata"){ #opening function
      #takes id (int) directory (char), returns string with location of corresponding CSV 
      
      #GETTING THE NAME
      
      #if the id has 3 digits, no changes required, just conversion to string 
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
      
      path <- paste(directory,"/", file_name,sep="")
      
      path
}


complete_one<-function(id,directory = "~/GIT/R Programming /HW1/specdata"){
      #takes id of a csv, returns list with file name and a number of compete cases
      
      path <- get_path(id)
      #print(path)
      file_name <- path #todo: extract name from path
      temp <- read.csv(path)
      complete_number <- sum(complete.cases(temp))
      to_bind<- c(toString(id),complete_number)
      to_bind
      
      
}#closing function





complete <- function( id =1:332,directory="~/GIT/R Programming /HW1/specdata"){
      #df_result <- data.frame(files=character(length(id)),counts=numeric(length(id))) 
      files_vect <- character(length(id))
      counts_vect<- numeric(length(id))
      
      for (i in id){

            values <- complete_one(i)
            files_vect[i] <- values[1]
            counts_vect[i] <- values[2]

            
            
      }#closing for loop
      
      df_result <- cbind(files_vect,counts_vect)
      colnames(df_result) <- c("file id","complete cases")
      df_result[id,]

}#closing function