get_path <- function(id, directory = "~/GIT/R Programming /HW1/specdata"){ #opening function
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
      
      if(tail(directory)!="/"){
            path <- paste(directory,"/",file_name,sep="")
      }
      else{
            path <- paste(directory, file_name,sep="")
      }
      
      #RETURNING THE PATH
      
      path
}#closing function

pollutantmean <- function(id = 1:332, directory = "~/GIT/R Programming /HW1/specdata"
                          , pollutant ="sulfate"){#opening fc
      
      df_result <- data.frame()
      df_result[pollutant] <-numeric() 
      directory <- directory
      
      for(i in id){#opening for loop
            path <- get_path(i, directory)
            temp <- read.csv(path)
            df_result <- rbind(df_result,temp[pollutant])

      } #closing for loop
      
      mean(na.omit(df_result[[pollutant]]))
      
}#closing function

pollutevector <- function(id = 1:10, directory = "~/GIT/R Programming /HW1/specdata"
                          , pollutant ="sulfate"){#opening fc
      
      df_result <- data.frame()
      df_result[pollutant] <-numeric() 
      directory <- directory
      
      for(i in id){#opening for loop
            path <- get_path(i, directory)
            temp <- read.csv(path)
            df_result <- rbind(df_result,temp[pollutant])

      } #closing for loop
      
      na.omit(df_result[[pollutant]])
      
}#closing function
