library(reshape2)
library(qdap)
library(data.table)
library(plyr)


####################################################################################################
# Necessities to begin working

cwd <- getwd()

wbc.data.path <- file.path(cwd, "data","qualtrics_pull_wbc.csv")
states.data.path <- file.path(cwd, "data","states.csv")
resources.path <- file.path(cwd,"data","clean_qualtrics_resource.R")

source(resources.path)

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
# input:
#        data # qualtrics download
# output:

data.v1 <- subset(data, select = names(data) %ni% exclude.columns)

names.set <- names(data.v1)

for(i in 1:length(questions))
{
  names.set <- gsub(questions[i],questions.new[i], names.set)
  names.set <- gsub("new.mexico", "newmexico", names.set)
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


survey.melt <- melt(survey.data, id.vars = c("EmailAddress"))
states.data <- sapply(strsplit(as.character(survey.melt$variable),"\\."),"[[",1)
question.data <- sapply(strsplit(as.character(survey.melt$variable),"\\."),"[[",2)

data.v2 <- cbind(survey.melt, states.data, question.data)
data.v2$variable <- NULL

data.v3 <- dcast(data.v2, EmailAddress + states.data ~ question.data)


#states.data <- states.data[grepl("_",states.data)]
#answer.dataset.v2 <- cbind(survey.melt, states.data)
#comments <- subset(survey.data, grepl("comments",variable))

