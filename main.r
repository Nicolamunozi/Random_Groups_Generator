library(tidyverse)
library(readxl)
library(writexl)

#Hola como estay

#functions: 

#Load Data:
load_data <- function(file, path= file.path("C:","Users","niko_","Desktop",
                                            "Proyectos",
                                            "Python",
                                            "minitrabajos")){
    file <- file.path(path, file)
    data <-read_xlsx(file)
    data
  }


#Preprocess Data:
process_data <- function(data){
          n_filas <- dim(data)[1]
          ID <- as.double(c(1:n_filas))
          data <- data %>% 
                    mutate(ID=ID)
          data <- data[,c(4,1,2,3)]
          data
} 


#A function if there is an observation you should want to remove:


#Generating groups:
get_groups <- function(data, group_zise){
                groups <- list()
                len_groups <- dim(data)[1]/group_zise
                for (num_groups in 1:len_groups){
                  groups[[num_groups]] <- sample(data$ID, group_zise)
                  data_2 <- tibble(1,"a","b","c")
                  names(data_2) <- names(data) #necesario?
                  for (i in 1:group_zise){
                    data_2[i, ] <- data[data$ID == groups[[num_groups]][i],]
                  }
                
                  data <- setdiff(data, data_2)
                }  
                  groups
}

#Group variable to data:

insert_groups <- function(data, group_zise){
  
  groups <- get_groups(data=data, group_zise = group_zise )
  len_groups <- length(groups)
  data <- data %>% 
            mutate(Groups = 0)
  for (j in 1:len_groups){
    for (i in groups[[j]]) {
      data$Groups[i] <- j 
    }
  }
  data[order(data$Groups),] 
}

#Groups for people who doesn't fit to the data:  


#Final accommodations of the data:

data_formating <- function(data, variables = c(5,2,3,4), as_df = FALSE){
  
  if (as_df) {
  data <- as.data.frame(data[, variables])
  }
  else{
    data <- data[, variables]
  }
  names(data) <- c("Groups", "Name", "Last Name", "Email")
  data
} 

#Data exporting:
#(should it be for more formats?)

data_exporting <- function(data, path=file.path("C:", "Users","niko_",
                                                "Desktop",
                                                "Proyectos", 
                                                "Python",
                                                "minitrabajos")){
  
  write_xlsx(data, path = file.path(path, "groups.xlsx")) 
} 

#Integrated function:

randomizer <- function(file, group_zise, ...){
  
  data <- load_data(file = file)
  data <- process_data(data)
  data <- insert_groups(data, group_zise)
  data <- data_formating(data)
  data_exporting(data)
  data
}

#Probando las funciones:

datos <- load_data(file= "lista.xlsx")
datos <- process_data(datos)
datos <- insert_groups(data= datos, group_zise = 5)
datos <- data_formating(datos)
datos
data_exporting(datos)
c
randomizer("lista.xlsx", 5)

