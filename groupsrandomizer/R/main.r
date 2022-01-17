library(tidyverse)
library(readxl)
library(writexl)

#TODO: Crear ambiente del paquete.

#functions:
#Utils

len_groups <- function(data){

  for(i in 1:max(data$Groups)){
    print(paste("Group",i,"has",sum((datos$Groups == i)), "members"))
  }
}

#Load Data:

#TODO: Generalizar tipo de archivo, agregar csv y otros formatos excel.


load_data <- function(file=file.choose()){
    data <-read_xlsx(file)
    data
  }


#Preprocess Data:
#Notar que la data viene en un orden, que es "Nombre","Apellido","Mail"
#
#TODO: Generalizar orden y cantidad de columnas
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

#TODO: Vectorizar
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

label_groups <- function(data, group_zise){

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

#Assingning to a group the people wiouth groups.

fill_groups <- function(data,upper = TRUE){
  if (upper) {
    data$Groups <- replace(data$Groups,
                           data$Groups==0,
                           sample(c(1:max(data$Groups)),sum(data$Groups==0),
                                  replace = FALSE))
  }
  else{
    m_groups <- max(data$Groups)
    k <- sum(data$Groups==1)
    while ( sum(data$Groups==0) < k-1){
      j <- sample(1:k,1)
      i <- sample(1:m_groups,1)
      if (sum(data$Groups==i)==k){
        data[data$Groups==i,][j,1]=0
        next
      }
      next
    }
    data[data$Groups==0,][,1] = m_groups + 1
  }

  data[order(data$Groups),]
}

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
#TODO: Generalizar tipo output (FORMATO).


  write_xlsx(data, path = file.path(path, name))
}

#Integrated function:

randomizer <- function(group_zise){

  data <- load_data()
  data <- process_data(data)
  data <- label_groups(data, group_zise)
  data <- data_formating(data)
  data_exporting(data)
  data
}

#Probando las funciones:

# datos <- load_data()
# datos <- process_data(datos)
# datos <- label_groups(data= datos, group_zise = 4)
# datos <- data_formating(datos)
# datos
# data_exporting(datos)
#
# datos <- randomizer(8)
# datos <- fill_groups(data = datos, upper = FALSE)
#
# groups_len(data=datos)



