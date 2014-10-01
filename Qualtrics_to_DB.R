
Sys.setenv(MYSQL_HOME = 'C:/Program Files/R/R-3.0.2/library/RMySQL')
library(RMySQL)
library(lubridate)
specify_decimal <- function(x, k) format(round(x, k), nsmall=k)


con <- dbConnect(MySQL(),
                 user = "eocmaster",
                 password = "eocseidman",
                 dbname = "eocdb", 
                 host = "eoc.cgzanv6lfrne.us-west-2.rds.amazonaws.com")



####################################################################################################
# Panel going out to Database Start at answer.v8

info.panel  <- subset(panelists, select = c(FirstName, LastName, PrimaryEmail, Organization))

#Leave comments in
panel <- answer.v8

names.panel <- names(panel)
names.panel <- as.factor(names.panel)
names.panel <- revalue(names.panel, c(
  "EmailAddress" = "PrimaryEmail",
  "states.data"  = "States",
  "2014.Current...Personal.Income"     = "Q1A1",
  "2014.Retail.Sales"                  = "Q2A1",
  "2014.Wage...Salary.Employment"      = "Q3A1",
  "2014.Population.Growth"             = "Q4A1",
  "2014.Single.family.Housing.Permits" = "Q5A1",
  "2014.Manufacturing.Employment"      = "Q2A1_mfg",
  "2014.Gross.Gaming.Revenue"          = "Q2A1_ggr",
  "2015.Current...Personal.Income"     = "Q1A2",
  "2015.Retail.Sales"                  = "Q2A2",
  "2015.Wage...Salary.Employment"      = "Q3A2",
  "2015.Population.Growth"             = "Q4A2",
  "2015.Single.family.Housing.Permits" = "Q5A2",
  "2015.Manufacturing.Employment"      = "Q2A2_mfg",
  "2015.Gross.Gaming.Revenue"          = "Q2A2_ggr"
))


columns <- c("PrimaryEmail",
             "States",
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
             "Q2A1.mfg",
             "Q2A2.mfg",
             "comments")

data.check <-c("Q1A1",
               "Q2A1",
               "Q3A1",
               "Q4A1",
               "Q5A1",
               "Q1A2",
               "Q2A2",
               "Q3A2",
               "Q4A2",
               "Q5A2",
               "Q2A1.mfg",
               "Q2A2.mfg")

names(panel) <- names.panel



subpanel.info <- subset(panel, select = c(PrimaryEmail, States, comments))
subpanel.data <- subset(panel, select = -c(PrimaryEmail, States, comments ))

subpanel.info <- data.frame(lapply(subpanel.info, as.character), stringsAsFactors=FALSE)
subpanel.data <- data.frame(lapply(subpanel.data, as.character), stringsAsFactors=FALSE)
subpanel.data <- data.frame(lapply(subpanel.data, as.numeric), stringsAsFactors=FALSE)

panel <- as.data.frame(cbind(subpanel.info, subpanel.data))
panel[is.na(panel)] <- 0


#Panel hack, amalgamating columns by adding them



# tmp <- subset(panel, select = c("Q1A1",
#                                 "Q2A1",
#                                 "Q3A1",
#                                 "Q4A1",
#                                 "Q5A1",
#                                 "Q1A2",
#                                 "Q2A2",
#                                 "Q3A2",
#                                 "Q4A2",
#                                 "Q5A2"))
# 
# 
# panel$check <- rowSums(tmp, na.rm = T)
# 
# panel$States[panel$States == "NewMexico"] <- "New Mexico"
# 
# panel.v2 <- data.table(subset(panel, check != 0, select = -c(check)))
# 
# panel.v3 <- merge(panel.v2, info.panel, by = c("PrimaryEmail"))
# 
# updated.emails <- panel.v2$PrimaryEmail
# 
# ####################################################################################################
# # Read in our panelists_complete.csv or query from DB later on. 
# 
# panelists.complete <- read.csv("panelists_complete_in.csv")
# 
# #panelists.complete <- dbReadTable(con, "wbc_panelists_updated")
# 
# panelists.complete$PrimaryEmail <- as.character(panelists.complete$PrimaryEmail)
# panelists.incomplete <- subset(panelists.complete, !(PrimaryEmail %in% updated.emails ))
# 
# panelists.complete.out <- rbind(panelists.incomplete, panel.v3)
# row.names(panelists.complete.out) <- NULL
# 
# #### Send to DB here
# 
# tmp <- subset(panelists.complete.out, select = c("Q1A1",
#                                 "Q2A1",
#                                 "Q3A1",
#                                 "Q4A1",
#                                 "Q5A1",
#                                 "Q1A2",
#                                 "Q2A2",
#                                 "Q3A2",
#                                 "Q4A2",
#                                 "Q5A2"))
# 
# panelists.complete.out$check <- rowSums(tmp,na.rm=T)
# panelists.complete.out.v2 <- subset(panelists.complete.out, check > 0)
# 
# panelists.complete.out.v2[is.na(panelists.complete.out.v2)] <- ""
# 
# duplicated.vector <- !(duplicated(panelists.complete.out.v2[,-c(1,2,3)]))
#   
# panelists.complete.out.v3 <- panelists.complete.out.v2[duplicated.vector,]
# panelists.complete.out.v3$States <- tolower(panelists.complete.out.v3$States)
# 
# 
# ####################################################################################################
# # Draw up some consensus data
# 
# subpanel.data <- subset(panelists.complete.out.v3, select = c(
#                          "Q1A1",
#                          "Q2A1",
#                          "Q3A1",
#                          "Q4A1",
#                          "Q5A1",
#                          "Q1A2",
#                          "Q2A2",
#                          "Q3A2",
#                          "Q4A2",
#                          "Q5A2"))
# 
# subpanel.data <- data.frame(lapply(subpanel.data, as.numeric), stringsAsFactors=FALSE)
# subpanel.data <- data.table(subpanel.data)
# subpanel.data$States <- panelists.complete.out.v3$States
# consensus.v1 <- subpanel.data[,list(Q1A1 = specify_decimal(mean(Q1A1, na.rm = T),1),
#                                     Q2A1 = specify_decimal(mean(Q2A1, na.rm = T),1),
#                                     Q3A1 = specify_decimal(mean(Q3A1, na.rm = T),1),
#                                     Q4A1 = specify_decimal(mean(Q4A1, na.rm = T),1),
#                                     Q5A1 = specify_decimal(mean(Q5A1, na.rm = T),1),
#                                     Q1A2 = specify_decimal(mean(Q1A2, na.rm = T),1),
#                                     Q2A2 = specify_decimal(mean(Q2A2, na.rm = T),1),
#                                     Q3A2 = specify_decimal(mean(Q3A2, na.rm = T),1),
#                                     Q4A2 = specify_decimal(mean(Q4A2, na.rm = T),1),
#                                     Q5A2 = specify_decimal(mean(Q5A2, na.rm = T),1)),
#                               
#                              by = States]
# consensus.matrix <- data.matrix(consensus.v1)
# consensus.means.v1 <- specify_decimal(colSums(consensus.matrix[,2:11])/
#                                         nrow(consensus.matrix),1)
# consensus.means.v2 <- c("Consensus", consensus.means.v1) 
# consensus.v2 <- rbind(consensus.means.v2, consensus.v1)
# consensus.v2$Organization <- "Consensus"
# panelists.complete.out.v4 <- rbind.fill(panelists.complete.out.v3, consensus.v2)
# 
# 
# 
# panelists.complete.out.v4$date <- ymd(Sys.Date())
# 
# panelists.complete.out.v4$month <-  month(panelists.complete.out.v4$date)
# panelists.complete.out.v4$year  <-  year(panelists.complete.out.v4$date)
# 
# 
# 
# 
# dbWriteTable(con , "wbc_panelists_updated", panelists.complete.out.v4, append = TRUE)
# 
# 
# on.exit(dbDisconnect(con))
# 
# 
# # Do we slice up the output?