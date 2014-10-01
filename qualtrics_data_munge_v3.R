library(reshape2)
library(qdap)
library(data.table)
library(plyr)

cwd <- getwd()

data.path <- file.path(cwd, "data","qualtrics_pull_wbc.csv")



data <- read.csv("qualtrics_pull_wbc.csv", skip = 1)
states <- read.csv("states.csv")

# Previous Panel Data
panelists <- read.csv("panelists.csv")


states$State <- as.character(states$State)
states$State <- paste0(states$State, ".")
####################################################################################################
# Answer data pull & column name munge
answer.dataset <- subset(data, select = c(EmailAddress, grep("enter",names(data))))

info.dataset <- subset(data, select = c(Name,EmailAddress,EndDate))
`%ni%` <- Negate(`%in%`)

data <- data[, (grepl("[_]", names(data)))]

names.set <- names(data)

questions <- c("2014.Current...Personal.Income",
"2014.Retail.Sales"        ,
"2014.Wage...Salary.Employment",    
"2014.Population.Growth"        ,     
"2014.Single.family.Housing.Permits",
"2014.Gross.Gaming.Revenue"         ,
"2014.Manufacturing.Employment"    ,
"2015.Current...Personal.Income"    ,
"2015.Retail.Sales"                 ,
"2015.Wage...Salary.Employment"     ,
"2015.Population.Growth"            ,
"2015.Single.family.Housing.Permits" ,
"2015.Gross.Gaming.Revenue"        ,
"2015.Manufacturing.Employment") 

questions.new <- c(  "Q1A1",
                     "Q2A1",
                     "Q3A1",
                     "Q4A1",
                     "Q5A1",
                     "Q2A1.ggr",
                     "Q2A1.mfg",
                     "Q1A2",
                     "Q2A2",
                     "Q3A2",
                     "Q4A2",
                     "Q5A2",
                     "Q2A2.ggr",
                     "Q2A2.mfg")

states.clean <- c("arizona.",
                  "california.",
                  "colorado.",
                  "idaho.",
                  "montana.",
                  "nevada.",
                  "new.mexico.",
                  "oregon.",
                  "texas.",
                  "utah.",
                  "washington.",
                  "wyoming.")

for(i in 1:length(questions))
{
  print(questions[i])
  print(questions.new[i])
  names.set <- gsub(questions[i],questions.new[i], names.set)
}


data.v2 <- data
names(data.v2) <- names.set
data.v2$END <- NULL
data.v2$X <- NULL


columns.not.needed <- c("ResponseID",
                        "ResponseSet",
                        "Name",
                        "States",
                        "Organization",
                        "ExternalDataReference",
                        "IPAddress",
                        "Status",
                        "StartDate",
                        "EndDate",
                        "Finished",
                        "Q1A1",
                        "Q2A1",
                        "Q3A1",
                        "Q4A1",
                        "Q5A1",
                        "Q1A2",
                        "Q2A2",
                        "Q3A2",
                        "Q4A2",
                        "Q5A2",
                        "Q2A1_ggr",
                        "Q2A2_ggr",
                        "Q2A1_mfg",
                        "Q2A2_mfg",
                        "arizona",
                        "california",
                        "colorado",
                        "idaho",
                        "montana",
                        "nevada",
                        "new.mexico",
                        "oregon",
                        "texas",
                        "utah",
                        "washington",
                        "wyoming")


data.v2 <- subset(data.v1, select = names(data.v1) %ni% columns.not.needed)

names.set.v2 <- names(data.v2)




# ####################################################################################################
# # Info data.v1set



new.names <- gsub("new.mexico", "newmexico", names(data.v2))
names(data.v2) <- new.names

test.melt <- melt(data.v2, id.vars = c("EmailAddress"))
states.data <- sapply(strsplit(as.character(test.melt$variable), "\\."), `[[`, 1)
states.data <- states.data[grepl("_",states.data)]

answer.dataset.v2 <- cbind(test.melt, states.data)
comments <- subset(answer.dataset.v2, grepl("comments",variable))

answer.v3 <- answer.dataset.v2
answer.v3$variable <- mgsub(states$State, "", answer.v3$variable)
answer.v4 <- subset(answer.v3, !(grepl("comments",variable)))
answer.v5 <- answer.v4[!(is.na(answer.v4$value)),]


#answer.v6 <- dcast(answer.v5, EmailAddress + states.data ~ variable, value.var = value)

answer.v6 <- merge(comments, answer.v5, by = c("EmailAddress","states.data","variable","value"),all=TRUE)
answer.v7 <- answer.v6[!(is.na(answer.v6$value)),]
answer.v7$variable <- mgsub(states$State,"", answer.v7$variable)
answer.v7 <- data.table(answer.v7)
answer.v7 <- as.data.frame(unique(answer.v7))

for(j in 1:length(states.clean))
{
  answer.v7$variable <- gsub(states.clean[j], "", answer.v7$variable)
}

 answer.v8 <- dcast(answer.v7, EmailAddress + states.data ~ variable, value.var = "value")
 answer.v8$states.data[answer.v8$states.data == "NewMexico"] <- "New Mexico"
 answer.v8 <- as.data.frame(answer.v8)
 answer.v8[is.na(answer.v8)] <- ""
#answer.v9 <- complete.cases(answer.v8)

answer.set <- answer.v8

