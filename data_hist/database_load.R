Sys.setenv(MYSQL_HOME = 'C:/Program Files/R/R-3.0.2/library/RMySQL')
library(RMySQL)
r.loc <- "C:/Users/rjrow.ASURITE/bc/data_hist"
setwd(r.loc)
specify_decimal <- function(x, k) format(round(x, k), nsmall=k)


con <- dbConnect(MySQL(),
                 user = "wbc_master",
                 password = "wbcpassword",
                 dbname = "wbc", 
                 host = "wbc.cotzely14ram.us-west-2.rds.amazonaws.com")



# Initially load in the greater phoenix data and send off to db
greater.phoenix <- read.csv("greater_phoenix_data_1_3_2015.csv")
dbWriteTable(con, "gpbc_panelists_updated" , greater.phoenix, overwrite = TRUE)

on.exit(dbDisconnect(con))


files <- list.files(r.loc)
files <- files[grep("load.csv", files)]



for( i in 1:length(files))
{
print(files[i])
data <- read.csv(files[i])
table.name <- gsub("_load.csv","", files[i])
table.name <- tolower(table.name)

con <- dbConnect(MySQL(),
                 user = "wbc_master",
                 password = "wbcpassword",
                 dbname = "wbc", 
                 host = "wbc.cotzely14ram.us-west-2.rds.amazonaws.com")


dbWriteTable(con, table.name , data, overwrite = TRUE)
dbDisconnect(con)

}


on.exit(dbDisconnect(con))



all_cons <- dbListConnections(MySQL())
for(con in all_cons){dbDisconnect(con)}


#C:\Program Files\R\R-3.0.2\library\RMySQL
