library(reshape2)
#library(qdap)
library(data.table)
library(plyr)


####################################################################################################
# Necessities to begin working

cwd <- getwd()

wbc.data.path <- file.path(cwd, "data","qualtrics_pull_wbc_02_03_2015.csv")
states.data.path <- file.path(cwd, "data","states.csv")
resources.path <- file.path(cwd,"data","clean_qualtrics_resource.R")
config.path <- file.path(cwd, "config.R")

source(resources.path)
source(config.path)

data <- read.csv(wbc.data.path,skip = 1)
states <- read.csv(states.data.path)

# Prep States
states$State <- as.character(states$State)
states$State <- paste0(states$State, ".")


############################################################
# Functions
`%ni%` <- Negate(`%in%`)


####################################################################################################
# Data clean


data.v1 <- subset(data, select = names(data) %ni% exclude.columns)

names.set <- names(data.v1)

for(i in 1:length(questions))
{
  
  names.set <- gsub(questions[i],questions.new[i], names.set)
  names.set <- gsub("new.mexico", "newmexico", names.set)
  names.set <- gsub("EmailAddress","PrimaryEmail", names.set)
}

names(data.v1) <- names.set

survey.data <- subset(data.v1, select = names(data.v1) %ni% c("ResponseID",
                                 "ResponseSet",
                                 "Name",
                                 "States",
                                 "Organization",
                                 "ExternalDataReference",
                                 "IPAddress",
                                 "Status",
                                 "StartDate",
                                 "EndDate",
                                 "Finished"))

survey.data$PrimaryEmail <- as.character(survey.data$PrimaryEmail)
survey.data[which(survey.data$PrimaryEmail == ""), 1] <- "Richard.Wobbekind@colorado.edu"


survey.melt <- melt(survey.data, id.vars = c("PrimaryEmail"))
States <- sapply(strsplit(as.character(survey.melt$variable),"\\."),"[[",1)
question.data <- sapply(strsplit(as.character(survey.melt$variable),"\\."),"[[",2)

data.v2 <- cbind(survey.melt, States, question.data)
data.v2$variable <- NULL

data.v3 <- dcast(data.v2, PrimaryEmail + States ~ question.data)



data.v4 <- data.v3[!(is.na(data.v3[,4:17])),]
ind <- apply(data.v4, 1, function(x) all(is.na(x)))
data.v5 <- data.v4[!ind, ]
#data.v5$States <- NULL
names(data.v5) <- gsub("comment","comments", names(data.v5))



