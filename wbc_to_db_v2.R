library(RMySQL)
library(lubridate)
library(reshape2)



#TODO:
# 
#dbWriteTable(con, "wbc_deployment", wbc.deployment.v2, row.names = FALSE, overwrite = TRUE)
# Build out deployment table


####################################################################################################
# Survey

survey.data.out <- data.v5


con <- dbConnect(MySQL(),
                 user = "wbc_master",
                 password = "wbcpassword",
                 dbname = "wbc", 
                 host = "wbc.cotzely14ram.us-west-2.rds.amazonaws.com")



info.set <- dbGetQuery(con, "SELECT FirstName, 
                   LastName, 
                   PrimaryEmail, 
                   States, 
                   Organization FROM wbc_archive;")

info.set <- data.table(info.set)
info.set <- unique(info.set)

panel.set <- merge(info.set, survey.data.out, by = c("PrimaryEmail"))

panel.set$date <- ymd(Sys.Date())
panel.set$month <- month(panel.set$date)
panel.set$year  <- year(panel.set$date)

#Output new load data set

#dbWriteTable(con , "wbc_load_table", panel.set, overwrite = TRUE, row.names = FALSE)
#dbSendQuery(con, 'INSERT IGNORE INTO wbc_archive SELECT * FROM wbc_load_table')


####################################################################################################
# Function Descriptions here


wbc.archive <- wbc.archive <- dbGetQuery(con, "SELECT 
        FirstName,
        LastName,
        PrimaryEmail,
        States,
        Organization,
        Q1A1,
        Q2A1_ggr,
        Q4A1,
        Q2A1,
        Q5A1,
        Q3A1,
        Q1A2,
        Q4A2,
        Q2A2,
        Q5A2,
        Q3A2,
        Q2A2_ggr,
        Q2A1_mfg,
        Q2A2_mfg,
        date
      FROM wbc_archive;")


generateImportPanel <- function()
{

    
    wbc.archive$States <- tolower(wbc.archive$States)
    wbc.archive$States <- capitalize(wbc.archive$States)
    wbc.archive$date <- mdy(wbc.archive$date)
    wbc.archive$month <- month(wbc.archive$date)
    
    current <- ymd(as.character(Sys.Date()))
    
    wbc.archive$diff <-  current - wbc.archive$date 
    wbc.archive <- wbc.archive[order(wbc.archive$diff),]
    
    
    #########################################################
    # wbc.archive to qualtrics panel
    wbc.archive.v1 <- wbc.archive[!(duplicated(wbc.archive[c("States","Organization")])),]
    wbc.archive.v1$diff <- NULL
    wbc.archive.v1$date <- NULL
    
    wbc.archive.melt <- melt(wbc.archive.v1, id.vars = c("FirstName",
                                                         "LastName",
                                                         "PrimaryEmail",
                                                         "States",
                                                         "Organization"))
    
    
    states <- unique(wbc.archive$States)
    import.panel <- dcast(wbc.archive.v1, FirstName + 
                            LastName+ 
                            PrimaryEmail+
                            States+
                            Organization+
                            Q1A1+
                            Q2A1_ggr+
                            Q4A1+
                            Q2A1+
                            Q5A1+
                            Q3A1+
                            Q1A2+
                            Q2A2_ggr+
                            Q4A2+
                            Q2A2+
                            Q5A2+
                            Q3A2+
                            Q2A1_mfg+
                            Q2A2_mfg ~ States)
    
    
    import.panel.v1 <- dcast(wbc.archive.melt, 
                             FirstName + 
                               LastName+ 
                               PrimaryEmail+
                               Organization
                             ~ States + variable )
    
    
    
    import.panel[,states][is.na(import.panel[states])] <- 0
    import.panel[,states][((import.panel[states]) != 0)] <- 1
    
    
    import.panel[,states] <- sapply(import.panel[,states], as.numeric)
    
    import.panel <- data.table(import.panel)
    names(import.panel)[names(import.panel) == "new mexico"] <- "new.mexico"
    
    
    import.panel.v2 <- merge( wbc.panelists.v1, import.panel.v1, by = c("PrimaryEmail","Organization"), all.x = TRUE)
    question.list <- as.character(names(import.panel.v2))
    question.list <- question.list[grepl("Q", question.list)]
    import.panel.v2[,question.list] <- sapply(import.panel.v2[,question.list], as.numeric)
    import.panel.v2[,question.list] <- sapply(import.panel.v2[,question.list], function(x) {
      specify_decimal(x, 1)
    })
    
    #Output panel here

    
}


generateConsensusPanel <- function()
{
  
      data.columns <- c("Q1A1",
                        "Q2A1_ggr",
                        "Q4A1",
                        "Q2A1",
                        "Q5A1",
                        "Q3A1",
                        "Q1A2",
                        "Q2A2_ggr",
                        "Q4A2",
                        "Q2A2",
                        "Q5A2",
                        "Q3A2",
                        "Q2A1_mfg",
                        "Q2A2_mfg")
      
      wbc.archive[data.columns] <- sapply(wbc.archive[data.columns], as.numeric)
      wbc.archive[data.columns] <- sapply(wbc.archive[data.columns], function(x){specify_decimal(x, 1)})
      wbc.archive[data.columns] <- sapply(wbc.archive[data.columns], as.numeric)
      wbc.archive <- data.table(wbc.archive)
      wbc.archive <- wbc.archive[!duplicated(wbc.archive, by = c("Organization","date","States")),]
      
      
      
      consensus <- wbc.archive[,list( Q1A1      = specify_decimal(mean(Q1A1, na.rm = TRUE),1),
                                      Q2A1_ggr  = specify_decimal(mean(Q2A1_ggr, na.rm = TRUE),1),
                                      Q4A1      = specify_decimal(mean(Q4A1, na.rm = TRUE),1),
                                      Q2A1      = specify_decimal(mean(Q2A1, na.rm = TRUE),1),
                                      Q5A1      = specify_decimal(mean(Q5A1, na.rm = TRUE),1),
                                      Q3A1      = specify_decimal(mean(Q3A1, na.rm = TRUE),1),
                                      Q1A2      = specify_decimal(mean(Q1A2, na.rm = TRUE),1),
                                      Q2A2_ggr  = specify_decimal(mean(Q2A2_ggr, na.rm = TRUE),1),
                                      Q4A2      = specify_decimal(mean(Q4A2, na.rm = TRUE),1),
                                      Q2A2      = specify_decimal(mean(Q2A2, na.rm = TRUE),1),
                                      Q5A2      = specify_decimal(mean(Q5A2, na.rm = TRUE),1),
                                      Q3A2      = specify_decimal(mean(Q3A2, na.rm = TRUE),1),
                                      Q2A1_mfg  = specify_decimal(mean(Q2A1_mfg, na.rm = TRUE),1),
                                      Q2A2_mfg  = specify_decimal(mean(Q2A2_mfg, na.rm = TRUE),1)),
                               by = list( States, month, date)]
      
      
      current.date <- ymd(as.character(Sys.Date()))
      consensus$diff <- current.date - consensus$date
      consensus <- data.frame(consensus[order(consensus$diff)])
      
      #Types of consensus are this month == "consensus" and everything else is "last month consensus"
      types.of.consensus <- unique(consensus$diff)
      consensus$Organization <- NA
      for(i in 1:length(types.of.consensus))
      {
        print(types.of.consensus[i])
        if(i == 1) {consensus[which(consensus$diff == types.of.consensus[i]), "Organization"] <- "Consensus"}
        if(i == 2) {consensus[which(consensus$diff == types.of.consensus[i]), "Organization"] <- "Last Month Consensus"}
        if(i > 2) {consensus[which(consensus$diff == types.of.consensus[i]), "Organization"] <- "Old Consensus"}
      }
      
      #Output Consensus Panel Here
      
      
      #dbWriteTable(con, "wbc_consensus", consensus, overwrite = TRUE, row.names = FALSE)
      #dbSendQuery(con, 'INSERT INTO wbc_old_archive ')
      
}




