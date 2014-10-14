library(RMySQL)
library(lubridate)
library(reshape2)
library(Hmisc)


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

panel.set$date <- as.character(Sys.Date())
panel.set$date <- ymd(panel.set$date)

panel.set$month <- as.character(month(panel.set$date))
panel.set$year  <- as.character(year(panel.set$date))

panel.set$date <- NULL
panel.set$date <- as.character(strftime(as.character(Sys.Date()), "%m/%d/%Y"))

panel.set <- data.frame(panel.set)

panel.set <- panel.set[,wbc.archive.order]
panel.set[is.na(panel.set)] <- ""


#Output new load data set
if(dev.environment == "Production")
{
  dbWriteTable(con , "wbc_load_table", panel.set, overwrite = TRUE, row.names = FALSE)
  dbSendQuery(con, 'INSERT IGNORE INTO wbc_archive SELECT * FROM wbc_load_table')
} else
{
  dbWriteTable(con , "wbc_load_table_test", panel.set, overwrite = TRUE, row.names = FALSE)
  dbSendQuery(con, 'INSERT IGNORE INTO wbc_archive_test SELECT * FROM wbc_load_table_test')
}


####################################################################################################
# Function Descriptions here


specify_decimal <- function(x, k) format(round(x, k), nsmall=k)



###############################################
# Pull out two needed data sets for following cleanup
wbc.panel.path <- file.path(cwd, "data","wbc_panelists_pull.csv")
wbc.panelists <- read.csv(wbc.panel.path)
wbc.panelists.v1 <- subset(wbc.panelists, select = c(Email,
                                                     Organization,
                                                     Arizona,
                                                     California,
                                                     Colorado,
                                                     Idaho,
                                                     Montana,
                                                     Nevada,
                                                     New.Mexico,
                                                     Oregon,
                                                     Texas,
                                                     Utah,
                                                     Washington,
                                                     Wyoming))

len <- length(names(wbc.panelists.v1))

names(wbc.panelists.v1)[3:len] <- 
  tolower(as.character(names(wbc.panelists.v1)[3:len]))
names(wbc.panelists.v1)[names(wbc.panelists.v1) == "Email"] <- "PrimaryEmail"


#############
# 2nd dataset
wbc.archive <- dbGetQuery(con, "SELECT 
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
      FROM wbc_archive_test;")

wbc.archive$States <- tolower(wbc.archive$States)
wbc.archive$States <- capitalize(wbc.archive$States)
wbc.archive$date <- mdy(wbc.archive$date)
wbc.archive$month <- month(wbc.archive$date)

current <- ymd(as.character(Sys.Date()))

wbc.archive$diff <-  current - wbc.archive$date 
wbc.archive <- wbc.archive[order(wbc.archive$diff),]

####################################################################################################

# generateImportPanel <- function()
# {
    
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

    wbc.archive.melt$States <- tolower(wbc.archive.melt$States)
    
    
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
    write.csv(import.panel.v2, file = "import_qualtrics_panel.csv", row.names = FALSE)

    
# }



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
      this.wbc.archive <- wbc.archive[!duplicated(wbc.archive, by = c("Organization","States")),]

      this.consensus <- this.wbc.archive[,list( Q1A1      = specify_decimal(mean(Q1A1, na.rm = TRUE),1),
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
                               by = list(States)]

      current.month <- month(ymd(as.character(Sys.Date())))
      last.wbc.archive <- subset(wbc.archive, month != current.month)
      last.wbc.archive <- last.wbc.archive[!duplicated(last.wbc.archive, by = c("Organization","States")),]

      last.consensus <- last.wbc.archive[,list( Q1A1      = specify_decimal(mean(Q1A1, na.rm = TRUE),1),
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
                                    by = list(States)]

      this.consensus$Organization <- "Consensus"
      last.consensus$Organization <- "Last Month Consensus"

      consensus <- data.frame(rbind(this.consensus, last.consensus))


      if(dev.environment == "Production")
      {
        dbWriteTable(con, "wbc_consensus", consensus, overwrite = TRUE, row.names = FALSE)
      } else
      {
        dbWriteTable(con, "wbc_consensus_test", consensus, overwrite = TRUE, row.names = FALSE)
      }

}


####################################################################################################
# I should be able to take the archive and consensus table now, from the DB (that I just inserted)
# and create the deployment table

generateDeploymentTable <- function()
{

  import.panel <- as.data.frame(import.panel)
  wbc.deployment       <- import.panel[deployment.table.names]
  wbc.deployment[data.columns] <- sapply(wbc.deployment[data.columns], as.numeric)
  wbc.deployment[data.columns] <- sapply(wbc.deployment[data.columns], function(x){specify_decimal(x, 1)})
  
  consensus.deployment <- consensus[deployment.table.names]
  
  wbc.deployment <- rbind(wbc.deployment, consensus.deployment) 
  #wbc.deployment[which(wbc.deployment[data.columns] == " NA"),data.columns] <- ""
  cleaned.data.columns <- sapply(wbc.deployment[,data.columns], function(x){
    gsub(" NA|NaN|  NaN","",x)
    #wbc.deployment[grepl("(?i)na", x) == "TRUE",] <- ""
  })
  
  wbc.deployment.v2 <- wbc.deployment
  wbc.deployment.v2[,data.columns] <- cleaned.data.columns
  
  if(dev.environment == "Production")
  {
    dbWriteTable(con, "wbc_deployment", wbc.deployment.v2, row.names = FALSE, overwrite = TRUE)
  }else
  {
    dbWriteTable(con, "wbc_deployment_test", wbc.deployment.v2, row.names = FALSE, overwrite = TRUE)
  }
    
}


