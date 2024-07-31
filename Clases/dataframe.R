## Data frame
df <- data.frame(
  coll = c("Tbis", "is", "a", "column")
  ,col2 = 1:4
  ,col3 = letters[1:4]
  ,stringAsFactors = TRUE
)

#Factores
levels(df$col1)

df_1 <- rbind(df, c("2", 6, "o"))

## indexing
c(1:4)[3]

df_1[2,3]

is.na(df_1)

## Funciones
find_sample <- function(x=10){
  for_index <- sample(1:10, size =  x, replace = TRUE)
                      return(for_index)
  
}
find_sample(10)


## apply
generate_df <- function(x){
  d1 <- data.frame(
    a = sample(letters, size = 10, replace = TRUE)
    ,b = sample(1:10, size = 10, replace = TRUE)
  )
  return(d1)
}

list_res <- lapply(1:100, generate_df)
list_res[90]