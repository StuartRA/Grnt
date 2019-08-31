library(readxl)
library(xtable)
library(plyr)

# (Func) Build matrices from raw Excel file

get.tables <- function(xlsxfilepath, colsets) {
  
########## 1. READ DATASET
  
      my.data <- as.data.frame(read_excel(xlsxfilepath))
      
########## 2. CLEAN DATASET
      
      my.data <- my.data[-1,]
      my.data[my.data==""] <- NA
      my.data <- my.data[colSums(!is.na(my.data)) > 0]
      
      my.data[] <- lapply(my.data, function(x) gsub(" \"", " ``",x))
      my.data[] <- lapply(my.data, function(x) gsub(" \'", " `",x))
      my.data[] <- lapply(my.data, function(x) gsub("\\$", "\\\\$",x))
      my.data[] <- lapply(my.data, function(x) gsub("\\&", "\\\\&",x))
      my.data[] <- lapply(my.data, function(x) gsub("\\%", "\\\\%",x))
      my.data[] <- lapply(my.data, function(x) gsub("\\_", "\\\\_",x))
      my.data[] <- lapply(my.data, function(x) gsub("\\#", "\\\\#",x))
      my.data[] <- lapply(my.data, function(x) gsub("^'- ", "",x))
      my.data[] <- lapply(my.data, function(x) gsub("^'-", "",x))
      
      my.data[my.data=="Advanced "] <- "Advanced"
      my.data[my.data=="Expert "] <- "Expert"
      
      my.data$Q118 <- ifelse(!is.na(my.data$Q119_1) & !is.na(my.data$Q119_2) & !is.na(my.data$Q118_2) & my.data$Q118_2!="Name" & my.data$Q118_2!=paste0(my.data$Q119_1, ", ", my.data$Q119_2),
                             paste0(my.data$Q118_2, " \\& ", my.data$Q119_1, ", ", my.data$Q119_2),
                             ifelse(!is.na(my.data$Q119_1) & !is.na(my.data$Q119_2) & is.na(my.data$Q118_2) | !is.na(my.data$Q119_1) & !is.na(my.data$Q119_2) & my.data$Q118_2=="Name",
                                    paste(my.data$Q119_1,my.data$Q119_2,sep=", "), 
                                    my.data$Q118_2))
      
      my.data[c("Q226_1","Q226_2","Q226_3","Q226_4","Q226_5","Q226_6","Q226_7")] <- lapply(my.data[c("Q226_1","Q226_2","Q226_3","Q226_4","Q226_5","Q226_6","Q226_7")], function(x) ifelse(x=="N/A" | x=="n/a",NA,x))
      my.data$Q226 <- paste("\\\\",my.data$Q226_7,my.data$Q226_1,my.data$Q226_2,my.data$Q226_3,my.data$Q226_4,my.data$Q226_5,my.data$Q226_6, sep="\\\\")
      my.data$Q279 <- paste("\\\\",my.data$Q279_1,my.data$Q279_2,my.data$Q279_3,my.data$Q279_4, sep="\\\\")
      
      my.data$Employer <- ifelse(!is.na(my.data$Q305) & my.data$Q305=="U.S. Federal Department of Health and Human Services Agency or Office",
                                 gsub("U.S. Federal government", "U.S. Federal Department of Health and Human Services Agency or Office",my.data$Q304),
                                 ifelse(!is.na(my.data$Q305) & my.data$Q305=="U.S. Federal government (outside the DHHS)",
                                        gsub("U.S. Federal government", "U.S. Federal government (outside the DHHS)",my.data$Q304),
                                        my.data$Q304))
      
      if(any(!is.na(my.data$Q306))) {
        my.data$Employer <- ifelse(!is.na(my.data$Q306) & my.data$Q306=="State Health Department",
                                   gsub("State government", "State Health Department",my.data$Employer),
                                   ifelse(!is.na(my.data$Q306) & my.data$Q306=="State government (not health department)",
                                          gsub("State government", "State government (not health department)", my.data$Employer),
                                          my.data$Employer))
      }
      
      if(any(!is.na(my.data$Q307))) {
        my.data$Employer <- 
          ifelse(!is.na(my.data$Q307) & my.data$Q307=="Local (county or city) Health Department",
                 gsub("Local government", "Local (county or city) Health Department",my.data$Employer),
                 ifelse(!is.na(my.data$Q307) & my.data$Q307=="Local government (not health department)",
                        gsub("Local government", "Local government (not health department)", my.data$Employer),
                        my.data$Employer))
      }
      
      my.data$Employer <- ifelse(!is.na(my.data$Q308) & sapply(my.data$Q308, function(text) grepl("Elementary academic institution",text)),
                                 gsub("Academic institution (elementary, secondary or post\\-secondary)", "Elementary academic institution", my.data$Employer),
                                 ifelse(!is.na(my.data$Q308) & sapply(my.data$Q308, function(text) grepl("Secondary academic institution",text)),
                                        gsub("Academic institution \\(elementary, secondary or post\\-secondary\\)", "Secondary academic institution", my.data$Employer),
                                        ifelse(!is.na(my.data$Q308) & sapply(my.data$Q308, function(text) grepl("Junior/community college",text)),
                                               gsub("Academic institution \\(elementary, secondary or post\\-secondary\\)", "Junior/community college", my.data$Employer),
                                               ifelse(!is.na(my.data$Q308) & sapply(my.data$Q308, function(text) grepl("College, 4 year",text)),
                                                      gsub("Academic institution \\(elementary, secondary or post\\-secondary\\)", "College, 4 year", my.data$Employer),
                                                      ifelse(!is.na(my.data$Q308) & sapply(my.data$Q308, function(text) grepl("University, Master's granting",text)),
                                                             gsub("Academic institution \\(elementary, secondary or post\\-secondary\\)", "University, Master's granting", my.data$Employer),
                                                             ifelse(!is.na(my.data$Q308) & sapply(my.data$Q308, function(text) grepl("Research university, doctoral granting",text)),
                                                                    gsub("Academic institution \\(elementary, secondary or post\\-secondary\\)", "Research university, doctoral granting", my.data$Employer),my.data$Employer))))))
      
      my.data$Q287 <- paste("\\\\",my.data$Q287_1,my.data$Q287_2,my.data$Q287_3, sep="\\\\")
      my.data$Q274 <- ifelse(my.data$Q272=="Outside U.S.",my.data$Q273,my.data$Q272)
      
      #i had to do this this year because many of the column names changed from the prior year...this may need to be changed again next time
      colnames(my.data)[which(colnames(my.data) %in% c("advrelationship","freqcommadv","timefeedbackadv"))] <- c("Q245_advrelationship","Q245_freqcommadv","Q245_timefeedbackadv")
      colnames(my.data)[which(colnames(my.data) %in% c("htopicadv","hresearchadv","hwritingadv","hacadadv","hnonacadadv","hemployadv"))] <- c("Q246_htopicadv","Q246_hresearchadv","Q246_hwritingadv","Q246_hacadadv","Q246_hnonacadadv","Q246_hemployadv")
      colnames(my.data)[which(colnames(my.data) %in% c("advsuppcarpath","comfadvwithinacad","comfadvoutsideacad"))] <- c("Q247_advsuppcarpath","Q247_comfadvwithinacad","Q247_comfadvoutsideacad")
      colnames(my.data)[which(colnames(my.data) %in% c("htopicmen","hresearchmen","hwritingmen","hacadmen","hnonacadmen","hemploymen"))] <- c("Q251_htopicmen","Q251_hresearchmen","Q251_hwritingmen","Q251_hacadmen","Q251_hnonacadmen","Q251_hemploymen")
      colnames(my.data)[which(colnames(my.data) %in% c("AdmOff","StuAcct","FinAid","OIS","RecReg","CTLHlp","SOURCE","Shuttle","SAP","HlthServ","MHlthServ"))] <- c("Serv_Sat_AdmOff","Serv_Sat_StuAcct","Serv_Sat_FinAid","Serv_Sat_OIS","Serv_Sat_RecReg","Serv_Sat_CTLHlp","Serv_Sat_SOURCE","Serv_Sat_Shuttle","Serv_Sat_SAP","Serv_Sat_HlthServ","Serv_Sat_MHlthServ")
      colnames(my.data)[which(colnames(my.data) %in% c("finance","personalwellbeing","space","lab"))] <- c("Q240_finance","Q240_personalwellbeing","Q240_space","Q240_lab")
      colnames(my.data)[which(colnames(my.data) %in% c("workcomm","family","availfac","progreq","coursesched","immigration","otherobst"))] <- c("Q269_workcomm","Q269_family","Q269_availfac","Q269_progreq","Q269_coursesched","Q269_immigration","Q269_otherobst")
      colnames(my.data)[which(colnames(my.data) %in% c("respect","intclimate","socclimate","collegial"))] <- c("Q268_respect","Q268_intclimate","Q268_socclimate","Q268_collegial")
      colnames(my.data)[which(colnames(my.data) %in% c("sameuniv","samefield","recommend","samethesisadv"))] <- c("Q220_sameuniv","Q220_samefield","Q220_recommend","Q220_samethesisadv")
      
      #table(my.data$Q66 to see what the exact labels are)
      n.col.names <- c("MPH","Other Master's (MAPHB, MAS, MBe, MHA, MHS, MPP, MSPH, ScM)","PhD/ScD","DrPH")
      
########## 3. DEFINE MAIN FUNCTION
        my.function <- function(x) {
          
          if(substr(colnames(my.data)[x],1,4) == "Q449" |
             substr(colnames(my.data)[x],1,4) == "Q313" |
             substr(colnames(my.data)[x],1,4) == "Q245" |
             substr(colnames(my.data)[x],1,4) == "Q240") {
            
            my.data[,x] <- sapply(my.data[,x], function(x) gsub("Very good","Very Good", x)) 
            
            my.data[,x][my.data[,x]=="Not applicable"] <- NA
            
            q.col <- c(
              if(colnames(my.data)[x] == "Q449_1") {paste0('BOLD',"Relevance of course content")}
              else if(colnames(my.data)[x] == "Q449_2") {paste0('BOLD',"Quality of course content")}
              else if(colnames(my.data)[x] == "Q449_3") {paste0('BOLD',"Breadth of courses offered")}
              else if(colnames(my.data)[x] == "Q449_4") {paste0('BOLD',"Depth of courses offered")}
              else if(colnames(my.data)[x] == "Q449_5") {paste0('BOLD',"Selection of online courses")}
              else if(colnames(my.data)[x] == "Q449_6") {paste0('BOLD',"Selection of Summer")}
              else if(colnames(my.data)[x] == "Q449_7") {paste0('BOLD',"Quality of overall curriculum")}
              else if(colnames(my.data)[x] == "Q449_8") {paste0('BOLD',"Quality of")}
              else if(colnames(my.data)[x] == "Q449_9") {paste0('BOLD',"Lab rotations (if applicable)")}
              else if(colnames(my.data)[x] == "Q449_10") {paste0('BOLD',"Preparation for the job")}
              else if(colnames(my.data)[x] == "Q449_11") {paste0('BOLD',"Assistance in finding")}
              else if(colnames(my.data)[x] == "Q449_12") {paste0('BOLD',"Overall program quality")}
              else if(colnames(my.data)[x] == "Q449_13") {paste0('BOLD',"Quality of academic advising")}
              else if(colnames(my.data)[x] == "Q449_14") {paste0('BOLD',"Preparation prior to")}
              else if(colnames(my.data)[x] == "Q449_15") {paste0('BOLD',"The opportunity to")}
              else if(colnames(my.data)[x] == "Q313_1") {paste0('BOLD',"Your academic experience at Johns Hopkins")}
              else if(colnames(my.data)[x] == "Q313_2") {paste0('BOLD',"Your student life experience at Johns Hopkins")}
              else if(colnames(my.data)[x] == "Q313_3") {paste0('BOLD',"Your overall experience at Johns Hopkins")}
              else if(colnames(my.data)[x] == "Q245_advrelationship") {paste0('BOLD',"Relationship with your")}
              else if(colnames(my.data)[x] == "Q245_freqcommadv") {paste0('BOLD',"Frequency of communication")}
              else if(colnames(my.data)[x] == "Q245_timefeedbackadv") {paste0('BOLD',"Timeliness of feedback from")}
              else if(colnames(my.data)[x] == "Q245_13") {paste0('BOLD',"Familiarity with your")}
              else if(colnames(my.data)[x] == "Q245_8") {paste0('BOLD',"Guidance in selecting courses")}
              else if(colnames(my.data)[x] == "Q245_9") {paste0('BOLD',"Guidance about other")}
              else if(colnames(my.data)[x] == "Q245_11") {paste0('BOLD',"Guidance in identifying an")}
              else if(colnames(my.data)[x] == "Q245_10") {paste0('BOLD',"Transitioning from")}
              else if(colnames(my.data)[x] == "Q240_finance") {paste0('BOLD',"Financial support")}
              else if(colnames(my.data)[x] == "Q240_personalwellbeing") {paste0('BOLD',"Personal well-being")}
              else if(colnames(my.data)[x] == "Q240_space") {paste0('BOLD',"Personal work space (e.g., desk or office)")}
              else if(colnames(my.data)[x] == "Q240_lab") {paste0('BOLD',"Laboratory, clinical, studio or other physical facilities")},
              "Schoolwide",
              "",
              "MPH",
              "Other Master's",
              "PhD/ScD",
              "DrPH"
            )
            
            n.col <- vector(mode = "numeric", length=length(q.col)-3)
            mat <- matrix(NA,ncol=5,nrow=length(n.col))
            mat.format <- mat
            
            for(i in 1:4) {
              n.col[i] <- sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i]]))
              
              mat[i,] <- c(
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Poor"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Fair"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Good"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Very Good"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Excellent"]))
              )
              
              mat.format[i,] <- format(100*round(mat[i,]/n.col[i],3),nsmall=1)
            }
            
            schoolwide.n <- sum(!is.na(my.data[,x]))
            schoolwide.mat <- c(
              sum(!is.na(my.data[,x][my.data[,x]=="Poor"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Fair"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Good"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Very Good"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Excellent"]))
            )
            schoolwide.mat.format <- format(100*round(schoolwide.mat/schoolwide.n,3),nsmall=1)
            
            
            n.col.full <- c("",schoolwide.n,"",n.col)
            mat.format.full <- rbind(rep("",ncol(mat.format)), schoolwide.mat.format, rep("",ncol(mat.format)), mat.format)
            
            p.col <- c(
              rep("",length(q.col)-1),
              if (format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2) == "0.00") {paste("LESS")}
              else {format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2)}
            )
            
            p.col <- ifelse(p.col=="NaN","N/A",p.col)
            
            return(rbind(
              rep('\\rule{0pt}{2ex}',7),
              cbind(q.col,n.col.full,mat.format.full,p.col)
            ))
          }
          
          
          else if(colnames(my.data)[x] == "Q234") {
            
            q.col <- c(
              "Schoolwide",
              "",
              "MPH",
              "Other Master's",
              "PhD/ScD",
              "DrPH"
            )
            
            n.col <- vector(mode = "numeric", length=length(q.col)-2)
            mat <- matrix(NA,ncol=4,nrow=length(n.col))
            mat.format <- mat
            
            for(i in 1:4) {
              n.col[i] <- sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i]]))
              
              mat[i,] <- c(
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="I don't remember"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="No"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Yes, but I did not attend"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Yes, and I attended"]))
              )
              
              mat.format[i,] <- format(100*round(mat[i,]/n.col[i],3),nsmall=1)
            }
            
            schoolwide.n <- sum(!is.na(my.data[,x]))
            schoolwide.mat <- c(
              sum(!is.na(my.data[,x][my.data[,x]=="I don't remember"])),
              sum(!is.na(my.data[,x][my.data[,x]=="No"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Yes, but I did not attend"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Yes, and I attended"]))
            )
            schoolwide.mat.format <- format(100*round(schoolwide.mat/schoolwide.n,3),nsmall=1)
            
            p.col <- c(
              rep("",length(q.col)-1),
              if (format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2) == "0.00") {paste("LESS")}
              else {format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2)}
            )
            
            p.col <- ifelse(p.col=="NaN","N/A",p.col)
            
            n.col.full <- c(schoolwide.n,"",n.col)
            mat.format.full <- rbind(schoolwide.mat.format, rep("",ncol(mat.format)), mat.format)
            
            
            return(rbind(
              rep('\\rule{0pt}{2ex}',5),
              cbind(q.col,n.col.full,mat.format.full,p.col)
            ))
          }
          
          else if(colnames(my.data)[x] == "Q235") {
            
            q.col <- c(
              "Schoolwide",
              "",
              "MPH",
              "Other Master's",
              "PhD/ScD",
              "DrPH"
            )
            
            n.col <- vector(mode = "numeric", length=length(q.col)-2)
            mat <- matrix(NA,ncol=5,nrow=length(n.col))
            mat.format <- mat
            
            for(i in 1:4) {
              n.col[i] <- sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i]]))
              
              mat[i,] <- c(
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Very Ineffective"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Somewhat Ineffective"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Neither Effective nor Ineffective"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Somewhat Effective"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Very Effective"]))
              )
              
              mat.format[i,] <- format(100*round(mat[i,]/n.col[i],3),nsmall=1)
            }
            
            schoolwide.n <- sum(!is.na(my.data[,x]))
            schoolwide.mat <- c(
              sum(!is.na(my.data[,x][my.data[,x]=="Very Ineffective"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Somewhat Ineffective"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Neither Effective nor Ineffective"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Somewhat Effective"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Very Effective"]))
            )
            schoolwide.mat.format <- format(100*round(schoolwide.mat/schoolwide.n,3),nsmall=1)
            
            p.col <- c(
              rep("",length(q.col)-1),
              if (format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2) == "0.00") {paste("LESS")}
              else {format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2)}
            )
            
            p.col <- ifelse(p.col=="NaN","N/A",p.col)
            
            n.col.full <- c(schoolwide.n,"",n.col)
            mat.format.full <- rbind(schoolwide.mat.format, rep("",ncol(mat.format)), mat.format)
            
            return(rbind(
              rep('\\rule{0pt}{2ex}',5),
              cbind(q.col,n.col.full,mat.format.full,p.col)
            ))
          }
          
          else if(substr(colnames(my.data)[x],1,4) == "Q236" |
                  substr(colnames(my.data)[x],1,4) == "Q237" |
                  substr(colnames(my.data)[x],1,4) == "Q248" |
                  substr(colnames(my.data)[x],1,4) == "Q255" |
                  substr(colnames(my.data)[x],1,4) == "Q258" |
                  substr(colnames(my.data)[x],1,4) == "Q260" |
                  substr(colnames(my.data)[x],1,4) == "Q225" |
                  substr(colnames(my.data)[x],1,4) == "Q276") {
            
            q.col <- c(
              "Schoolwide",
              "",
              "MPH",
              "Other Master's",
              "PhD/ScD",
              "DrPH"
            )
            
            n.col <- vector(mode = "numeric", length=length(q.col)-2)
            mat <- matrix(NA,ncol=2,nrow=length(n.col))
            mat.format <- mat
            
            for(i in 1:4) {
              n.col[i] <- sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i]]))
              
              mat[i,] <- c(
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="No"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Yes"]))
              )
              
              mat.format[i,] <- format(100*round(mat[i,]/n.col[i],3),nsmall=1)
            }
            
            schoolwide.n <- sum(!is.na(my.data[,x]))
            schoolwide.mat <- c(
              sum(!is.na(my.data[,x][my.data[,x]=="No"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Yes"]))
            )
            schoolwide.mat.format <- format(100*round(schoolwide.mat/schoolwide.n,3),nsmall=1)
            
            p.col <- c(
              rep("",length(q.col)-1),
              if (format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2) == "0.00") {paste("LESS")}
              else {format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2)}
            )
            
            p.col <- ifelse(p.col=="NaN","N/A",p.col)
            
            n.col.full <- c(schoolwide.n,"",n.col)
            mat.format.full <- rbind(schoolwide.mat.format, rep("",ncol(mat.format)), mat.format)
            
            return(rbind(
              rep('\\rule{0pt}{2ex}',5),
              cbind(q.col,n.col.full,mat.format.full,p.col)
            ))
          }
          
          
          else if(substr(colnames(my.data)[x],1,4) == "Q246" |
                  substr(colnames(my.data)[x],1,4) == "Q316" |
                  substr(colnames(my.data)[x],1,4) == "Q251") {
            
            q.col <- c(
              if(colnames(my.data)[x] == "Q246_htopicadv") {paste0('BOLD',"Selection of a")}
              else if(colnames(my.data)[x] == "Q246_8") {paste0('BOLD',"Guidance writing a")}
              else if(colnames(my.data)[x] == "Q246_hresearchadv") {paste0('BOLD',"Dissertation or")}
              else if(colnames(my.data)[x] == "Q246_hwritingadv") {paste0('BOLD',"Dissertation or")}
              else if(colnames(my.data)[x] == "Q246_10") {paste0('BOLD',"Guidance in")}
              else if(colnames(my.data)[x] == "Q246_11") {paste0('BOLD',"Guidance from your")}
              else if(colnames(my.data)[x] == "Q246_hacadadv") {paste0('BOLD',"Academic career")}
              else if(colnames(my.data)[x] == "Q246_hnonacadadv") {paste0('BOLD',"Nonacademic career")}
              else if(colnames(my.data)[x] == "Q246_hemployadv") {paste0('BOLD',"Search for")}
              else if(colnames(my.data)[x] == "Q251_htopicmen") {paste0('BOLD',"Selection of a dissertation or culminating experience topic")}
              else if(colnames(my.data)[x] == "Q251_hresearchmen") {paste0('BOLD',"Your dissertation or culminating experience research")}
              else if(colnames(my.data)[x] == "Q251_hwritingmen") {paste0('BOLD',"Writing and revising your dissertation or culminating experience project")}
              else if(colnames(my.data)[x] == "Q251_hacadmen") {paste0('BOLD',"Academic career options")}
              else if(colnames(my.data)[x] == "Q251_hnonacadmen") {paste0('BOLD',"Nonacademic career options")}
              else if(colnames(my.data)[x] == "Q251_hemploymen") {paste0('BOLD',"Search for employment or training")},
              "Schoolwide",
              "",
              "MPH",
              "Other Master's",
              "PhD/ScD",
              "DrPH"
            )
            
            n.col <- vector(mode = "numeric", length=length(q.col)-3)
            na.col <- n.col
            na.col.format <- na.col
            mat <- matrix(NA,ncol=4,nrow=length(n.col))
            mat.format <- mat
            
            for(i in 1:4) {
              n.col[i] <- length(my.data[,x][my.data$Q66==n.col.names[i]])
              na.col[i] <- sum(is.na(my.data[,x][my.data$Q66==n.col.names[i]]))
              na.col.format[i] <- format(100*round(na.col[i]/n.col[i],3),nsmall=1)
              
              mat[i,] <- c(
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Not at all helpful"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Not very helpful"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Somewhat helpful"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Very helpful"]))
              )
              
              mat.format[i,] <- format(100*round(mat[i,]/(n.col[i]-na.col[i]),3),nsmall=1)
            }
            
            schoolwide.n <- length(my.data[,x])
            schoolwide.na <- sum(is.na(my.data[,x]))
            schoolwide.na.format <- format(100*round(schoolwide.na/schoolwide.n,3),nsmall=1)
            schoolwide.mat <- c(
              sum(!is.na(my.data[,x][my.data[,x]=="Not at all helpful"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Not very helpful"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Somewhat helpful"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Very helpful"]))
            )
            schoolwide.mat.format <- format(100*round(schoolwide.mat/(schoolwide.n-schoolwide.na),3),nsmall=1)
            
            
            p.col <- c(
              rep("",length(q.col)-1),
              if (format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2) == "0.00") {paste("LESS")}
              else {format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2)}
            )
            
            p.col <- ifelse(p.col=="NaN","N/A",p.col)
            
            n.col.full <- c("",schoolwide.n,"",n.col)
            na.col.format.full <- c("",schoolwide.na.format,"",na.col.format)
            mat.format.full <- rbind(rep("",ncol(mat.format)), schoolwide.mat.format, rep("",ncol(mat.format)), mat.format)
            
            return(rbind(
              rep('\\rule{0pt}{2ex}',8),
              cbind(q.col,n.col.full,na.col.format.full,mat.format.full,p.col)
            ))
          }
          
          else if(substr(colnames(my.data)[x],1,4) == "Q247" |
                  substr(colnames(my.data)[x],1,4) == "Q268") {
            
            my.data[,x] <- sapply(my.data[,x], function(x) gsub("Strongly Agree","Strongly agree", x))
            
            q.col <- c(
              if(colnames(my.data)[x] == "Q247_advsuppcarpath") {paste0('BOLD',"My advisor would support me in any career path I choose")}
              else if(colnames(my.data)[x] == "Q247_comfadvwithinacad") {paste0('BOLD',"I feel comfortable seeking advice from my advisor on career options within academia")}
              else if(colnames(my.data)[x] == "Q247_comfadvoutsideacad") {paste0('BOLD',"I feel comfortable seeking advice from my advisor on career options outside academia")}
              else if(colnames(my.data)[x] == "Q268_respect") {paste0('BOLD',"Students in my department are treated with respect by faculty")}
              else if(colnames(my.data)[x] == "Q268_intclimate") {paste0('BOLD',"The intellectual climate of my department is positive")}
              else if(colnames(my.data)[x] == "Q268_socclimate") {paste0('BOLD',"The social climate of my department is positive")}
              else if(colnames(my.data)[x] == "Q268_collegial") {paste0('BOLD',"Students in my department are collegial")},
              "Schoolwide",
              "",
              "MPH",
              "Other Master's",
              "PhD/ScD",
              "DrPH"
            )
            
            n.col <- vector(mode = "numeric", length=length(q.col)-3)
            mat <- matrix(NA,ncol=5,nrow=length(n.col))
            mat.format <- mat
            
            for(i in 1:4) {
              n.col[i] <- sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i]]))
              
              mat[i,] <- c(
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Strongly disagree"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Disagree"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Ambivalent"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Agree"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Strongly agree"]))
              )
              
              mat.format[i,] <- format(100*round(mat[i,]/n.col[i],3),nsmall=1)
            }
            
            schoolwide.n <- sum(!is.na(my.data[,x]))
            schoolwide.mat <- c(
              sum(!is.na(my.data[,x][my.data[,x]=="Strongly disagree"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Disagree"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Ambivalent"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Agree"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Strongly agree"]))
            )
            schoolwide.mat.format <- format(100*round(schoolwide.mat/schoolwide.n,3),nsmall=1)
            
            
            n.col.full <- c("",schoolwide.n,"",n.col)
            mat.format.full <- rbind(rep("",ncol(mat.format)), schoolwide.mat.format, rep("",ncol(mat.format)), mat.format)
            
            p.col <- c(
              rep("",length(q.col)-1),
              if (format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2) == "0.00") {paste("LESS")}
              else {format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2)}
            )
            
            p.col <- ifelse(p.col=="NaN","N/A",p.col)
            
            return(rbind(
              rep('\\rule{0pt}{2ex}',7),
              cbind(q.col,n.col.full,mat.format.full,p.col)
            ))
          }
          else if(colnames(my.data)[x] == "Adv_Contct") {
            
            q.col <- c(
              "Schoolwide",
              "",
              "MPH",
              "Other Master's",
              "PhD/ScD",
              "DrPH"
            )
            
            n.col <- vector(mode = "numeric", length=length(q.col)-2)
            mat <- matrix(NA,ncol=4,nrow=length(n.col))
            mat.format <- mat
            
            for(i in 1:4) {
              n.col[i] <- sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i]]))
              
              mat[i,] <- c(
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="<1"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="1perTerm"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="2perTerm"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]==">2perTerm"]))
              )
              
              mat.format[i,] <- format(100*round(mat[i,]/n.col[i],3),nsmall=1)
            }
            
            schoolwide.n <- sum(!is.na(my.data[,x]))
            schoolwide.mat <- c(
              sum(!is.na(my.data[,x][my.data[,x]=="<1"])),
              sum(!is.na(my.data[,x][my.data[,x]=="1perTerm"])),
              sum(!is.na(my.data[,x][my.data[,x]=="2perTerm"])),
              sum(!is.na(my.data[,x][my.data[,x]==">2perTerm"]))
            )
            schoolwide.mat.format <- format(100*round(schoolwide.mat/schoolwide.n,3),nsmall=1)
            
            p.col <- c(
              rep("",length(q.col)-1),
              if (format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2) == "0.00") {paste("LESS")}
              else {format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2)}
            )
            
            p.col <- ifelse(p.col=="NaN","N/A",p.col)
            
            n.col.full <- c(schoolwide.n,"",n.col)
            mat.format.full <- rbind(schoolwide.mat.format, rep("",ncol(mat.format)), mat.format)
            
            return(rbind(
              rep('\\rule{0pt}{2ex}',5),
              cbind(q.col,n.col.full,mat.format.full,p.col)
            ))
          }
          
          else if(substr(colnames(my.data)[x],1,4) == "Q249") {
            
            q.col <- c(
              "MPH",
              "Other Master's",
              "PhD/ScD",
              "DrPH",
              "Schoolwide"
            )
            
            n.col <- c(
              sum(!is.na(my.data[,x][my.data$Q66==n.col.names[1]])),
              sum(!is.na(my.data[,x][my.data$Q66==n.col.names[2]])),
              sum(!is.na(my.data[,x][my.data$Q66==n.col.names[3]])),
              sum(!is.na(my.data[,x][my.data$Q66==n.col.names[4]])),
              sum(!is.na(my.data[,x]))
            )
            
            mat <- c(
              sum(!is.na(my.data[,x][my.data$Q66==n.col.names[1] & my.data[,x]=="Yes"])),
              sum(!is.na(my.data[,x][my.data$Q66==n.col.names[2] & my.data[,x]=="Yes"])),
              sum(!is.na(my.data[,x][my.data$Q66==n.col.names[3] & my.data[,x]=="Yes"])),
              sum(!is.na(my.data[,x][my.data$Q66==n.col.names[4] & my.data[,x]=="Yes"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Yes"]))
            )
            
            n.no <- c(
              sum(!is.na(my.data[,x][my.data$Q66==n.col.names[1] & my.data[,x]=="No"])),
              sum(!is.na(my.data[,x][my.data$Q66==n.col.names[2] & my.data[,x]=="No"])),
              sum(!is.na(my.data[,x][my.data$Q66==n.col.names[3] & my.data[,x]=="No"])),
              sum(!is.na(my.data[,x][my.data$Q66==n.col.names[4] & my.data[,x]=="No"])),
              sum(!is.na(my.data[,x][my.data[,x]=="No"]))
            )
            
            return(
              cbind(q.col,n.col,mat,n.no)
            )
          }
          
          else if(colnames(my.data)[x] == "Q250") {
            
            mat <- matrix(NA,ncol=3,nrow=5)
            n.col <- vector(mode = "numeric", length=nrow(mat))
            
            for(i in 1:4) {
              mat[i,] <- c(
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="A program or department within JHSPH"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="A different school (another division of Johns Hopkins)--please specify:"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="A different university or establishment--please specify:"]))
              )
              
              n.col[i] <- sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i]]))
            }
            mat[nrow(mat),] <- c(
              sum(!is.na(my.data[,x][my.data[,x]=="A program or department within JHSPH"])),
              sum(!is.na(my.data[,x][my.data[,x]=="A different school (another division of Johns Hopkins)--please specify:"])),
              sum(!is.na(my.data[,x][my.data[,x]=="A different university or establishment--please specify:"]))
            )
            n.col[length(n.col)] <- sum(!is.na(my.data[,x]))
            
            return(list(mat,n.col))
          }
          
          else if(colnames(my.data)[x] == "Q256" |
                  colnames(my.data)[x] == "Q259" |
                  colnames(my.data)[x] == "Q261__1") {
            
            q.col <- c(
              "Schoolwide",
              "",
              "MPH",
              "Other Master's",
              "PhD/ScD",
              "DrPH"
            )
            
            n.col <- vector(mode = "numeric", length=length(q.col)-2)
            mat <- matrix(NA,ncol=4,nrow=length(n.col))
            mat.format <- mat
            
            for(i in 1:4) {
              n.col[i] <- sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i]]))
              
              mat[i,] <- c(
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Not at all helpful"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Not very helpful"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Somewhat helpful"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Very helpful"]))
              )
              
              mat.format[i,] <- format(100*round(mat[i,]/n.col[i],3),nsmall=1)
            }
            
            schoolwide.n <- sum(!is.na(my.data[,x]))
            schoolwide.mat <- c(
              sum(!is.na(my.data[,x][my.data[,x]=="Not at all helpful"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Not very helpful"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Somewhat helpful"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Very helpful"]))
            )
            schoolwide.mat.format <- format(100*round(schoolwide.mat/schoolwide.n,3),nsmall=1)
            
            p.col <- c(
              rep("",length(q.col)-1),
              if (format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2) == "0.00") {paste("LESS")}
              else {format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2)}
            )
            
            p.col <- ifelse(p.col=="NaN","N/A",p.col)
            
            n.col.full <- c(schoolwide.n,"",n.col)
            mat.format.full <- rbind(schoolwide.mat.format, rep("",ncol(mat.format)), mat.format)
            
            return(rbind(
              rep('\\rule{0pt}{2ex}',5),
              cbind(q.col,n.col.full,mat.format.full,p.col)
            ))
          }
          
          else if(colnames(my.data)[x] == "Q262" |
                  colnames(my.data)[x] == "Q263" |
                  colnames(my.data)[x] == "Q265" |
                  colnames(my.data)[x] == "Q266") {
            
            if(colnames(my.data)[x] == "Q262" |
               colnames(my.data)[x] == "Q263") {
              q.col <- c(
                "N",
                paste0('BOLD',"Number of presentations"),
                "None",
                "1",
                "2",
                "3",
                "4",
                "5",
                "6",
                "7",
                "8",
                "9",
                "10 or more"
              )
            }
            
            else if(colnames(my.data)[x] == "Q265" |
                    colnames(my.data)[x] == "Q266") {
              q.col <- c(
                "N",
                paste0('BOLD',"Number of publications"),
                "None",
                "1",
                "2",
                "3",
                "4",
                "5",
                "6",
                "7",
                "8",
                "9",
                "10 or more"
              )
            }
            
            
            n.row <- vector(mode = "numeric", length=4)
            mat <- matrix(NA,ncol=length(n.row),nrow=length(q.col)-2)
            mat.format <- mat
            
            for(i in 1:4) {
              n.row[i] <- sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i]]))
              
              mat[,i] <- c(
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="None"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="1"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="2"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="3"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="4"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="5"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="6"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="7"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="8"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="9"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="10 or more"]))
              )
              
              mat.format[,i] <- format(100*round(mat[,i]/n.row[i],3),nsmall=1)
            }
            
            schoolwide.n <- sum(!is.na(my.data[,x]))
            
            schoolwide.mat <- c(
              sum(!is.na(my.data[,x][my.data[,x]=="None"])),
              sum(!is.na(my.data[,x][my.data[,x]=="1"])),
              sum(!is.na(my.data[,x][my.data[,x]=="2"])),
              sum(!is.na(my.data[,x][my.data[,x]=="3"])),
              sum(!is.na(my.data[,x][my.data[,x]=="4"])),
              sum(!is.na(my.data[,x][my.data[,x]=="5"])),
              sum(!is.na(my.data[,x][my.data[,x]=="6"])),
              sum(!is.na(my.data[,x][my.data[,x]=="7"])),
              sum(!is.na(my.data[,x][my.data[,x]=="8"])),
              sum(!is.na(my.data[,x][my.data[,x]=="9"])),
              sum(!is.na(my.data[,x][my.data[,x]=="10 or more"]))
            )
            
            schoolwide.mat.format <- format(100*round(schoolwide.mat/schoolwide.n,3),nsmall=1)
            
            
            p.col <- c(
              rep("",length(q.col)-1),
              if (format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2) == "0.00") {paste("LESS")}
              else {format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2)}
            )
            
            p.col <- ifelse(p.col=="NaN","N/A",p.col)
            
            return(rbind(
              rep('\\rule{0pt}{2ex}',5),
              cbind(q.col,
                    c(schoolwide.n,"",schoolwide.mat.format),
                    rbind(n.row,rep("",length(n.row)),mat.format),
                    p.col)
            ))
          }
          
          else if(colnames(my.data)[x] == "Q264") {
            
            q.col <- c(
              "Schoolwide",
              "",
              "MPH",
              "Other Master's",
              "PhD/ScD",
              "DrPH"
            )
            
            n.col <- vector(mode = "numeric", length=length(q.col)-2)
            mat <- matrix(NA,ncol=3,nrow=length(n.col))
            mat.format <- mat
            
            for(i in 1:4) {
              n.col[i] <- sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i]]))
              
              mat[i,] <- c(
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Your program",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("A research grant",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Other institutional funds",text))))
              )
              
              mat.format[i,] <- format(100*round(mat[i,]/n.col[i],3),nsmall=1)
            }
            
            schoolwide.n <- sum(!is.na(my.data[,x]))
            
            schoolwide.mat <- c(
              sum(unlist(lapply(my.data[,x], function(text) grepl("Your program",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("A research grant",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Other institutional funds",text))))
            )
            
            schoolwide.mat.format <- format(100*round(schoolwide.mat/schoolwide.n,3),nsmall=1)
            
            n.col.full <- c(schoolwide.n,"",n.col)
            mat.format.full <- rbind(schoolwide.mat.format, rep("",ncol(mat.format)), mat.format)
            
            return(rbind(
              rep('\\rule{0pt}{2ex}',4),
              cbind(q.col,n.col.full,mat.format.full)
            ))
          }
          
          else if(substr(colnames(my.data)[x],1,8) == "Serv_Sat" |
                  substr(colnames(my.data)[x],1,5) == "Q263_") {
            
            my.data[,x] <- sapply(my.data[,x], function(x) gsub("Not used","Not Used", x))
            my.data[,x] <- sapply(my.data[,x], function(x) gsub("Very good","Very Good", x))
            my.data[,x] <- sapply(my.data[,x], function(x) gsub(" Poor","Poor", x))
            my.data[,x] <- sapply(my.data[,x], function(x) gsub("VeryDissat","Fair", x))
            
            
            q.col <- c(
              if(colnames(my.data)[x] == "Serv_Sat_AdmOff") {paste0('BOLD',"Admissions Office")}
              else if(colnames(my.data)[x] == "Serv_Sat_StuAcct") {paste0('BOLD',"Business")}
              else if(colnames(my.data)[x] == "Serv_Sat_FinAid") {paste0('BOLD',"Financial Aid Office")}
              else if(colnames(my.data)[x] == "Serv_Sat_OIS") {paste0('BOLD',"International Student")}
              else if(colnames(my.data)[x] == "Serv_Sat_RecReg") {paste0('BOLD',"Records and")}
              else if(colnames(my.data)[x] == "Serv_Sat_22") {paste0('BOLD',"Information")}
              else if(colnames(my.data)[x] == "Serv_Sat_CTLHlp") {paste0('BOLD',"Center for Teaching")}
              else if(colnames(my.data)[x] == "Serv_Sat_21") {paste0('BOLD',"Libraries and")}
              else if(colnames(my.data)[x] == "Serv_Sat_SOURCE") {paste0('BOLD',"SOURCE")}
              else if(colnames(my.data)[x] == "Serv_Sat_Shuttle") {paste0('BOLD',"JHMI-Homewood")}
              else if(colnames(my.data)[x] == "Serv_Sat_SAP") {paste0('BOLD',"Student Assistance")}
              else if(colnames(my.data)[x] == "Serv_Sat_20") {paste0('BOLD',"Disability Support")}
              else if(colnames(my.data)[x] == "Serv_Sat_HlthServ") {paste0('BOLD',"University (Student)")}
              else if(colnames(my.data)[x] == "Serv_Sat_MHlthServ") {paste0('BOLD',"University (Student)")}
              else if(colnames(my.data)[x] == "Q263_1") {paste0('BOLD',"Departmental resources and events, e.g., listservs, networking receptions, etc.")}
              else if(colnames(my.data)[x] == "Q263_2") {paste0('BOLD',"JHSPH Public Health Career Fair")}
              else if(colnames(my.data)[x] == "Q263_3") {paste0('BOLD',"JHSPH Office of Career Services")}
              else if(colnames(my.data)[x] == "Q263_4") {paste0('BOLD',"Professional Development Office (PDO) of the Johns Hopkins Medical Institutions")}
              else if(colnames(my.data)[x] == "Q263_5") {paste0('BOLD',"Other University Resources")},
              "Schoolwide",
              "",
              "MPH",
              "Other Master's",
              "PhD/ScD",
              "DrPH"
            )
            
            n.col <- vector(mode = "numeric", length=length(q.col)-3)
            not_used <- vector(mode = "numeric", length=length(n.col))
            not_used.format <- not_used
            mat <- matrix(NA,ncol=5,nrow=length(n.col))
            mat.format <- mat
            
            for(i in 1:length(n.col)) {
              n.col[i] <- 
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i]]))
              
              mat[i,] <- c(
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Poor"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Fair"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Good"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Very Good"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Excellent"]))
              )
              
              not_used[i] <- sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Not Used"]))
              
              not_used.format[i] <- format(100*round(not_used[i]/n.col[i],3),nsmall=1)
              not_used.format[not_used.format=="NaN"] = "--"
              
              mat.format[i,] <- format(100*round(mat[i,]/(n.col[i]-not_used[i]),3),nsmall=1)
              mat.format[mat.format=="NaN"] = "--"
              
            }
            
            schoolwide.n <- sum(!is.na(my.data[,x]))
            schoolwide.not_used <- sum(!is.na(my.data[,x][my.data[,x]=="Not Used"]))
            schoolwide.not_used.format <- format(100*round(schoolwide.not_used/schoolwide.n,3),nsmall=1)
            schoolwide.mat <- c(
              sum(!is.na(my.data[,x][my.data[,x]=="Poor"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Fair"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Good"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Very Good"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Excellent"]))
            )
            schoolwide.mat.format <- format(100*round(schoolwide.mat/(schoolwide.n-schoolwide.not_used),3),nsmall=1)
            
            
            n.col.full <- c("",schoolwide.n,"",n.col)
            not_used.format.full <- c("",schoolwide.not_used.format,"",not_used.format)
            mat.format.full <- rbind(rep("",ncol(mat.format)), schoolwide.mat.format, rep("",ncol(mat.format)), mat.format)
            
            p.col <- c(
              rep("",length(q.col)-1),
              if (format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2) == "0.00") {paste("LESS")}
              else {format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2)}
            )
            
            p.col <- ifelse(p.col=="NaN","N/A",p.col)
            
            return(rbind(
              rep('\\rule{0pt}{2ex}',9),
              cbind(q.col,n.col.full,not_used.format.full,mat.format.full,p.col)
            ))
          }
          
          
          else if(colnames(my.data)[x] == "Q351") {
            
            q.col <- c(
              "N",
              paste0('BOLD',"Prior degree"),
              "No other degrees",
              "DDS",
              "DO",
              "DrPH",
              "DVM",
              "EdD",
              "JD",
              "MA",
              "MBA",
              "MD",
              "MFA",
              "MHA/MHSA",
              "MHS",
              "MPA",
              "MPH",
              "MPP",
              "MS",
              "MSN",
              "MSPH",
              "OD",
              "PharmD",
              "PhD",
              "ScD",
              "ScM",
              "Other"
            )
            
            n.row <- vector(mode = "numeric", length=4)
            mat <- matrix(NA,ncol=length(n.row),nrow=length(q.col)-2)
            mat.format <- mat
            
            for(i in 1:4) {
              n.row[i] <- sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i]]))
              
              mat[,i] <- c(
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("No other degrees",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("DDS",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("DO",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("DrPH",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("DVM",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("EdD",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("JD",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("MA",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("MBA",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("MD",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("MFA",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("MHA/MHSA",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("MHS,",text))),
                    unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl(",MHS",text))),
                    !is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="MHS"])),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("MPA",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("MPH",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("MPP",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("MS,",text))),
                    unlist(lapply(!is.na(my.data[,x][my.data$Q66==n.col.names[i]]), function(text) substr(text,nchar(text)-2,nchar(text)) == ",MS")),
                    !is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="MS"])),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("MSN",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("MSPH",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("OD",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("PharmD",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("PhD",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("ScD",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("ScM",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Other",text))))
              )
              
              mat.format[,i] <- format(100*round(mat[,i]/n.row[i],3),nsmall=1)
              
            }
            
            schoolwide.n <- sum(!is.na(my.data[,x]))
            
            schoolwide.mat <- c(
              sum(unlist(lapply(my.data[,x], function(text) grepl("No other degrees",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("DDS",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("DO",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("DrPH",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("DVM",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("EdD",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("JD",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("MA",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("MBA",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("MD",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("MFA",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("MHA/MHSA",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("MHS,",text))),
                  unlist(lapply(my.data[,x], function(text) grepl(",MHS",text))),
                  !is.na(my.data[,x][my.data[,x]=="MHS"])),
              sum(unlist(lapply(my.data[,x], function(text) grepl("MPA",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("MPH",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("MPP",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("MS,",text))),
                  unlist(lapply(!is.na(my.data[,x]), function(text) substr(text,nchar(text)-2,nchar(text)) == ",MS")),
                  !is.na(my.data[,x][my.data[,x]=="MS"])),
              sum(unlist(lapply(my.data[,x], function(text) grepl("MSN",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("MSPH",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("OD",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("PharmD",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("PhD",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("ScD",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("ScM",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Other",text))))
            )
            
            schoolwide.mat.format <- format(100*round(schoolwide.mat/schoolwide.n,3),nsmall=1)
            
            return(rbind(
              rep('\\rule{0pt}{2ex}',1),
              cbind(q.col,
                    c(schoolwide.n,"",schoolwide.mat.format),
                    rbind(n.row,"",mat.format))
            ))
          }
          
          
          else if(colnames(my.data)[x] == "Prior_Exp") {
            
            q.col <- c(
              "Schoolwide",
              "",
              "MPH",
              "Other Master's",
              "PhD/ScD",
              "DrPH"
            )
            
            n.col <- vector(mode = "numeric", length=length(q.col)-2)
            mat <- matrix(NA,ncol=5,nrow=length(n.col))
            mat.format <- mat
            
            for(i in 1:4) {
              n.col[i] <- sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i]]))
              
              mat[i,] <- c(
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="<2"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="2to<5"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="5to<10"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="10to<15"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]==">15"]))
              )
              
              mat.format[i,] <- format(100*round(mat[i,]/n.col[i],3),nsmall=1)
            }
            
            schoolwide.n <- sum(!is.na(my.data[,x]))
            
            schoolwide.mat <- c(
              sum(!is.na(my.data[,x][my.data[,x]=="<2"])),
              sum(!is.na(my.data[,x][my.data[,x]=="2to<5"])),
              sum(!is.na(my.data[,x][my.data[,x]=="5to<10"])),
              sum(!is.na(my.data[,x][my.data[,x]=="10to<15"])),
              sum(!is.na(my.data[,x][my.data[,x]==">15"]))
            )
            
            schoolwide.mat.format <- format(100*round(schoolwide.mat/schoolwide.n,3),nsmall=1)
            
            n.col.full <- c(schoolwide.n,"",n.col)
            mat.format.full <- rbind(schoolwide.mat.format, rep("",ncol(mat.format)), mat.format)
            
            p.col <- c(
              rep("",length(q.col)-1),
              if (format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2) == "0.00") {paste("LESS")}
              else {format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2)}
            )
            
            return(rbind(
              rep('\\rule{0pt}{2ex}',5),
              cbind(q.col,n.col.full,mat.format.full,p.col)
            ))
          }
          
          else if(colnames(my.data)[x] == "Q242") {
            
            q.col <- c(
              "N",
              paste0('BOLD',"Source of support"),
              "University funded fellowship",
              "External fellowship",
              "Teaching assistantship",
              "Research assistantship",
              "Other assistantship",
              "Traineeship",
              "Internship",
              "Loans (from any source)",
              "Salary/wages (other than from grant or being a TA)",
              "Personal savings",
              "Spouse's, partner's, or family's earnings or savings",
              "Employer reimbursement/assistance",
              "Other part-time clinical appointment",
              "Other part-time research employment",
              "Other part-time teaching employment",
              "Foreign (non-U.S.) support",
              "Other"
            )
            
            n.row <- vector(mode = "numeric", length=4)
            mat <- matrix(NA,ncol=length(n.row),nrow=length(q.col)-2)
            mat.format <- mat
            
            for(i in 1:4) {
              n.row[i] <- sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i]]))
              
              mat[,i] <- c(
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("University funded fellowship",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("External fellowship",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Teaching assistantship",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Research assistantship",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Other assistantship",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Traineeship",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Internship",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Loans \\(from any source\\)",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Salary/wages \\(other than from grant or being a TA\\)",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Personal savings",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Spouse's, partner's, or family's earnings or savings",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Employer reimbursement/assistance",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Other part-time clinical appointment",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Other part-time research employment",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Other part-time teaching employment",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Foreign \\(non-U.S.\\) support \\(please specify\\)",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Other - Specify",text))))
              )
              
              mat.format[,i] <- format(100*round(mat[,i]/n.row[i],3),nsmall=1)
            }
            
            schoolwide.n <- sum(!is.na(my.data[,x]))
            
            schoolwide.mat <- c(
              sum(unlist(lapply(my.data[,x], function(text) grepl("University funded fellowship",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("External fellowship",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Teaching assistantship",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Research assistantship",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Other assistantship",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Traineeship",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Internship",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Loans \\(from any source\\)",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Salary/wages \\(other than from grant or being a TA\\)",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Personal savings",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Spouse's, partner's, or family's earnings or savings",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Employer reimbursement/assistance",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Other part-time clinical appointment",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Other part-time research employment",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Other part-time teaching employment",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Foreign \\(non-U.S.\\) support \\(please specify\\)",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Other - Specify",text))))
            )
            
            schoolwide.mat.format <- format(100*round(schoolwide.mat/schoolwide.n,3),nsmall=1)
            
            return(rbind(
              rep('\\rule{0pt}{2ex}',1),
              cbind(q.col,
                    c(schoolwide.n,"",schoolwide.mat.format),
                    rbind(n.row,"",mat.format))
            ))
          }
          
          
          else if(colnames(my.data)[x]=="Q243_11" |
                  colnames(my.data)[x]=="Q243_10") {
            my.data[,x] <- sapply(my.data[,x],function(x) {
              vector <- gsub(",","",x)
              vector <- gsub(" ","",vector)
              vector <- gsub("\\$","",vector)
              vector <- gsub("\\\\","",vector)
              vector <- gsub("none","0",vector)
              vector <- gsub("None","0",vector)
              return(as.numeric(vector))
            }
            )
            
            q.col <- c(
              "N",
              paste0('BOLD',"Money owed"),
              "None",
              "\\$20,000 or less",
              "\\$20,001-\\$40,000",
              "\\$40,001-\\$60,000",
              "\\$60,001-\\$80,000",
              "\\$80,001-\\$100,000",
              "\\$100,001 or more"
            )
            
            n.row <- vector(mode = "numeric", length=4)
            mat <- matrix(NA,ncol=length(n.row),nrow=length(q.col)-2)
            mat.format <- mat
            
            for(i in 1:4) {
              n.row[i] <- sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i]]))
              
              mat[,i] <- c(
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]==0])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]>=1 & my.data[,x]<=20000])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]>=20001 & my.data[,x]<=40000])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]>=40001 & my.data[,x]<=60000])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]>=60001 & my.data[,x]<=80000])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]>=80001 & my.data[,x]<=100000])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]>=100001]))
              )
              
              mat.format[,i] <- format(100*round(mat[,i]/n.row[i],3),nsmall=1)
            }
            
            schoolwide.n <- sum(!is.na(my.data[,x]))
            
            schoolwide.mat <- c(
              sum(!is.na(my.data[,x][my.data[,x]==0])),
              sum(!is.na(my.data[,x][my.data[,x]>=1 & my.data[,x]<=20000])),
              sum(!is.na(my.data[,x][my.data[,x]>=20001 & my.data[,x]<=40000])),
              sum(!is.na(my.data[,x][my.data[,x]>=40001 & my.data[,x]<=60000])),
              sum(!is.na(my.data[,x][my.data[,x]>=60001 & my.data[,x]<=80000])),
              sum(!is.na(my.data[,x][my.data[,x]>=80001 & my.data[,x]<=100000])),
              sum(!is.na(my.data[,x][my.data[,x]>=100001]))
            )
            
            schoolwide.mat.format <- format(100*round(schoolwide.mat/schoolwide.n,3),nsmall=1)
            
            p.col <- c(
              rep("",length(q.col)-1),
              if (format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2) == "0.00") {paste("LESS")}
              else {format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2)}
            )
            
            p.col <- ifelse(p.col=="NaN","N/A",p.col)
            
            return(rbind(
              rep('\\rule{0pt}{2ex}',4),
              cbind(q.col,
                    c(schoolwide.n,"",schoolwide.mat.format),
                    rbind(n.row,rep("",length(n.row)),mat.format),
                    p.col)
            ))
          }
          
          
          else if(colnames(my.data)[x] == "Q271") {
            
            q.col <- c(
              "N",
              paste0('BOLD',"Postgraduate plans"),
              "Returning to, or continuing in, pregraduate employment",
              "Medical internship/residency",
              "Post-doctoral or other fellowship",
              "Have signed contract or made definite commitment for employment",
              "Negotiating with one or more specific organizations",
              "Seeking position but have no specific prospects",
              "Volunteer (e.g., Peace Corps, mission work)",
              "Enrolling in another full-time degree program (e.g., MD, DDS, JD, MBA, etc.)",
              "Studying for a credentialing examination (e.g., National Boards)",
              "Do not plan to work or study (e.g., family commitments, etc.)",
              "Other"
            )
            
            n.row <- vector(mode = "numeric", length=4)
            mat <- matrix(NA,ncol=length(n.row),nrow=length(q.col)-2)
            mat.format <- mat
            
            for(i in 1:4) {
              n.row[i] <- sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i]]))
              
              mat[,i] <- c(
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Returning to, or continuing in, predoctoral employment"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Medical internship/residency"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Post-doctoral or other fellowship"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Have signed contract or made definite commitment for employment"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Negotiating with one or more specific organizations"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Seeking position but have no specific prospects"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Volunteer (e.g., Peace Corps, mission work)"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Enrolling in another full-time degree program (e.g., MD, DDS, JD, MBA, etc.)"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Studying for a credentialing examination (e.g., National Boards)"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Do not plan to work or study (e.g., family commitments, etc.)"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Other - Specify:"]))
              )
              
              mat.format[,i] <- format(100*round(mat[,i]/n.row[i],3),nsmall=1)
            }
            
            schoolwide.n <- sum(!is.na(my.data[,x]))
            
            schoolwide.mat <- c(
              sum(!is.na(my.data[,x][my.data[,x]=="Returning to, or continuing in, predoctoral employment"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Medical internship/residency"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Post-doctoral or other fellowship"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Have signed contract or made definite commitment for employment"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Negotiating with one or more specific organizations"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Seeking position but have no specific prospects"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Volunteer (e.g., Peace Corps, mission work)"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Enrolling in another full-time degree program (e.g., MD, DDS, JD, MBA, etc.)"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Studying for a credentialing examination (e.g., National Boards)"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Do not plan to work or study (e.g., family commitments, etc.)"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Other - Specify:"]))
            )
            
            schoolwide.mat.format <- format(100*round(schoolwide.mat/schoolwide.n,3),nsmall=1)
            
            p.col <- c(
              rep("",length(q.col)-1),
              if (format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2) == "0.00") {paste("LESS")}
              else {format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2)}
            )
            
            p.col <- ifelse(p.col=="NaN","N/A",p.col)
            
            return(rbind(
              rep('\\rule{0pt}{2ex}',4),
              cbind(q.col,
                    c(schoolwide.n,"",schoolwide.mat.format),
                    rbind(n.row,rep("",length(n.row)),mat.format),
                    p.col)
            ))
          }
          
          
          else if(colnames(my.data)[x] == "Employer") {
            
            q.col <- c(
              "N",
              paste0('BOLD',"Employer"),
              "International organization (UN, WHO, World Bank, etc.)",
              "U.S. Federal Department of Health and Human Services Agency or Office",
              "U.S. Federal government (outside the DHHS)",
              "State Health Department",
              "State government (not health department)",
              "Local (county or city) Health Department",
              "Local government (not health department)",
              "Tribal government",
              "Foreign government",
              "Military",
              "Association, foundation, voluntary, NGO, or other non-profit organization",
              "Elementary academic institution",
              "Secondary academic institution",
              "Junior/community college",
              "College, 4 year",
              "University, Master's granting",
              "Research university, doctoral granting",
              "Pharmaceutical, biotechnology, or medical device firm",
              "Consulting firm",
              "Health insurance company",
              "Hospital or other health care provider",
              "Health Information Technology (IT) company",
              "Marketing public relations, or communications firm",
              "Other industrial or commercial firm",
              "Self-employed",
              "Other"
            )
            
            n.row <- vector(mode = "numeric", length=4)
            mat <- matrix(NA,ncol=length(n.row),nrow=length(q.col)-2)
            mat.format <- mat
            
            for(i in 1:4) {
              n.row[i] <- sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i]]))
              
              mat[,i] <- c(
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("International organization \\(UN, WHO, World Bank, etc.\\)",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("U.S. Federal Department of Health and Human Services Agency or Office",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("U.S. Federal government \\(outside the DHHS\\)",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("State Health Department",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("State government \\(not health department\\)",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Local \\(county or city\\) Health Department",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Local government \\(not health department\\)",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Tribal government",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Foreign government",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Military",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Association, foundation, voluntary, NGO, or other non\\-profit organization",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Elementary academic institution",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Secondary academic institution",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Junior\\/community college",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("College, 4 year",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("University, Master's granting",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Research university, doctoral granting",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Pharmaceutical, biotechnology, or medical device firm",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Consulting firm",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Health insurance company",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Hospital or other health care provider",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Health Information Technology \\(IT\\) company",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Marketing, public relations, or communications firm",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Other industrial or commercial firm",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Self\\-employed",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Other \\(please specify\\)",text))))
              )
              
              mat.format[,i] <- format(100*round(mat[,i]/n.row[i],3),nsmall=1)
            }
            
            schoolwide.n <- sum(!is.na(my.data[,x]))
            
            schoolwide.mat <- c(
              sum(unlist(lapply(my.data[,x], function(text) grepl("International organization \\(UN, WHO, World Bank, etc.\\)",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("U.S. Federal Department of Health and Human Services Agency or Office",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("U.S. Federal government \\(outside the DHHS\\)",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("State Health Department",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("State government \\(not health department\\)",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Local \\(county or city\\) Health Department",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Local government \\(not health department\\)",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Tribal government",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Foreign government",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Military",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Association, foundation, voluntary, NGO, or other non\\-profit organization",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Elementary academic institution",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Secondary academic institution",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Junior\\/community college",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("College, 4 year",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("University, Master's granting",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Research university, doctoral granting",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Pharmaceutical, biotechnology, or medical device firm",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Consulting firm",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Health insurance company",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Hospital or other health care provider",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Health Information Technology \\(IT\\) company",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Marketing, public relations, or communications firm",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Other industrial or commercial firm",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Self\\-employed",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Other \\(please specify\\)",text))))
            )
            
            schoolwide.mat.format <- format(100*round(schoolwide.mat/schoolwide.n,3),nsmall=1)
            
            return(rbind(
              rep('\\rule{0pt}{2ex}',length(n.row)+1),
              cbind(q.col,
                    c(schoolwide.n,"",schoolwide.mat.format),
                    rbind(n.row,rep("",length(n.row)),mat.format))
            ))
          }
          
          
          else if(colnames(my.data)[x] == "Q303") {
            
            q.col <- c(
              "Schoolwide",
              "",
              "MPH",
              "Other Master's",
              "PhD/ScD",
              "DrPH"
            )
            
            n.col <- vector(mode = "numeric", length=length(q.col)-2)
            mat <- matrix(NA,ncol=2,nrow=length(n.col))
            mat.format <- mat
            
            for(i in 1:4) {
              n.col[i] <- sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i]]))
              
              mat[i,] <- c(
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="PT"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="FT"]))
              )
              
              mat.format[i,] <- format(100*round(mat[i,]/n.col[i],3),nsmall=1)
            }
            
            schoolwide.n <- sum(!is.na(my.data[,x]))
            schoolwide.mat <- c(
              sum(!is.na(my.data[,x][my.data[,x]=="PT"])),
              sum(!is.na(my.data[,x][my.data[,x]=="FT"]))
            )
            schoolwide.mat.format <- format(100*round(schoolwide.mat/schoolwide.n,3),nsmall=1)
            
            
            n.col.full <- c(schoolwide.n,"",n.col)
            mat.format.full <- rbind(schoolwide.mat.format, rep("",ncol(mat.format)), mat.format)
            
            p.col <- c(
              rep("",length(q.col)-1),
              if (format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2) == "0.00") {paste("LESS")}
              else {format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2)}
            )
            
            p.col <- ifelse(p.col=="NaN","N/A",p.col)
            
            return(rbind(
              rep('\\rule{0pt}{2ex}',1),
              cbind(q.col,n.col.full,mat.format.full,p.col)
            ))
          }
          
          else if(colnames(my.data)[x] == "Q265__1") {
            
            q.col <- c(
              "Schoolwide",
              "",
              "MPH",
              "Other Master's",
              "PhD/ScD",
              "DrPH"
            )
            
            n.col <- vector(mode = "numeric", length=length(q.col)-2)
            mat <- matrix(NA,ncol=3,nrow=length(n.col))
            mat.format <- mat
            
            for(i in 1:4) {
              n.col[i] <- sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i]]))
              
              mat[i,] <- c(
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="No"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Yes"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Don't know"]))
              )
              
              mat.format[i,] <- format(100*round(mat[i,]/n.col[i],3),nsmall=1)
            }
            
            schoolwide.n <- sum(!is.na(my.data[,x]))
            schoolwide.mat <- c(
              sum(!is.na(my.data[,x][my.data[,x]=="No"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Yes"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Don't know"]))
            )
            schoolwide.mat.format <- format(100*round(schoolwide.mat/schoolwide.n,3),nsmall=1)
            
            
            n.col.full <- c(schoolwide.n,"",n.col)
            mat.format.full <- rbind(schoolwide.mat.format, rep("",ncol(mat.format)), mat.format)
            
            p.col <- c(
              rep("",length(q.col)-1),
              if (format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2) == "0.00") {paste("LESS")}
              else {format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2)}
            )
            
            p.col <- ifelse(p.col=="NaN","N/A",p.col)
            
            return(rbind(
              rep('\\rule{0pt}{2ex}',1),
              cbind(q.col,n.col.full,mat.format.full,p.col)
            ))
          }
          
          else if(colnames(my.data)[x] == "Emp_Hlth") {
            
            q.col <- c(
              "Schoolwide",
              "",
              "MPH",
              "Other Master's",
              "PhD/ScD",
              "DrPH"
            )
            
            n.col <- vector(mode = "numeric", length=length(q.col)-2)
            mat <- matrix(NA,ncol=2,nrow=length(n.col))
            mat.format <- mat
            
            for(i in 1:4) {
              n.col[i] <- sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i]]))
              
              mat[i,] <- c(
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="N"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Y"]))
              )
              
              mat.format[i,] <- format(100*round(mat[i,]/n.col[i],3),nsmall=1)
            }
            
            
            schoolwide.n <- sum(!is.na(my.data[,x]))
            schoolwide.mat <- c(
              sum(!is.na(my.data[,x][my.data[,x]=="N"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Y"]))
            )
            schoolwide.mat.format <- format(100*round(schoolwide.mat/schoolwide.n,3),nsmall=1)
            
            
            n.col.full <- c(schoolwide.n,"",n.col)
            mat.format.full <- rbind(schoolwide.mat.format, rep("",ncol(mat.format)), mat.format)
            
            p.col <- c(
              rep("",length(q.col)-1),
              if (format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2) == "0.00") {paste("LESS")}
              else {format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2)}
            )
            
            p.col <- ifelse(p.col=="NaN","N/A",p.col)
            
            return(rbind(
              rep('\\rule{0pt}{2ex}',1),
              cbind(q.col,n.col.full,mat.format.full,p.col)
            ))
          }
          
          else if(colnames(my.data)[x] == "Emp_Resp") {
            
            q.col <- c(
              "N",
              paste0('BOLD',"Area of responsibility"),
              "Administration",
              "Community organization",
              "Consultation services",
              "Direct provision of health care services",
              "Health education",
              "Planning/evaluation/policy analysis",
              "Research, non-laboratory",
              "Research, laboratory",
              "Teaching",
              "Other"
            )
            
            n.row <- vector(mode = "numeric", length=4)
            mat <- matrix(NA,ncol=length(n.row),nrow=length(q.col)-2)
            mat.format <- mat
            
            for(i in 1:4) {
              n.row[i] <- sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i]]))
              
              mat[,i] <- c(
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Admin",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("CommOrg",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Consult",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("ProvHlthCare",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("HlthEd",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("PlanEvalAnal",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("RsrchNonLab",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("RsrchLab",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Teach",text)))),
                sum(unlist(lapply(my.data[,x][my.data$Q66==n.col.names[i]], function(text) grepl("Othr",text))))
              )
              
              mat.format[,i] <- format(100*round(mat[,i]/n.row[i],3),nsmall=1)
            }
            
            schoolwide.n <- sum(!is.na(my.data[,x]))
            
            schoolwide.mat <- c(
              sum(unlist(lapply(my.data[,x], function(text) grepl("Admin",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("CommOrg",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Consult",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("ProvHlthCare",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("HlthEd",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("PlanEvalAnal",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("RsrchNonLab",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("RsrchLab",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Teach",text)))),
              sum(unlist(lapply(my.data[,x], function(text) grepl("Othr",text))))
            )
            
            schoolwide.mat.format <- format(100*round(schoolwide.mat/schoolwide.n,3),nsmall=1)
            
            return(rbind(
              rep('\\rule{0pt}{2ex}',length(n.row)+1),
              cbind(q.col,
                    c(schoolwide.n,"",schoolwide.mat.format),
                    rbind(n.row,rep("",2),mat.format))
            ))
          }
          
          
          else if(colnames(my.data)[x]=="Emp_Salary_9") {
            my.data[,x] <- sapply(my.data[,x],function(x) {
              vector <- gsub(",","",x)
              vector <- gsub(" ","",vector)
              vector <- gsub("\\$","",vector)
              vector <- gsub("\\\\","",vector)
              return(as.numeric(vector))
            }
            )
            
            q.col <- c(
              "N",
              paste0('BOLD',"Annual salary"),
              "Less than \\$30,001",
              "\\$30,001-\\$60,000",
              "\\$60,001-\\$90,000",
              "\\$90,001-\\$120,000",
              "\\$120,001-\\$150,000",
              "More than \\$150,000"
            )
            
            n.row <- vector(mode = "numeric", length=4)
            mat <- matrix(NA,ncol=length(n.row),nrow=length(q.col)-2)
            mat.format <- mat
            
            for(i in 1:4) {
              n.row[i] <- sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i]]))
              
              mat[,i] <- c(
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]<=30000])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]>=30001 & my.data[,x]<=60000])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]>=60001 & my.data[,x]<=90000])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]>=90001 & my.data[,x]<=120000])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]>=120001 & my.data[,x]<=150000])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]>=150001]))
              )
              
              mat.format[,i] <- format(100*round(mat[,i]/n.row[i],3),nsmall=1)
            }
            
            schoolwide.n <- sum(!is.na(my.data[,x]))
            
            schoolwide.mat <- c(
              sum(!is.na(my.data[,x][my.data[,x]<=30000])),
              sum(!is.na(my.data[,x][my.data[,x]>=30001 & my.data[,x]<=60000])),
              sum(!is.na(my.data[,x][my.data[,x]>=60001 & my.data[,x]<=90000])),
              sum(!is.na(my.data[,x][my.data[,x]>=90001 & my.data[,x]<=120000])),
              sum(!is.na(my.data[,x][my.data[,x]>=120001 & my.data[,x]<=150000])),
              sum(!is.na(my.data[,x][my.data[,x]>=150001]))
            )
            
            schoolwide.mat.format <- format(100*round(schoolwide.mat/schoolwide.n,3),nsmall=1)
            
            
            p.col <- c(
              rep("",length(q.col)-1),
              if (format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2) == "0.00") {paste("LESS")}
              else {format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2)}
            )
            
            p.col <- ifelse(p.col=="NaN","N/A",p.col)
            
            return(rbind(
              rep('\\rule{0pt}{2ex}',4),
              cbind(q.col,
                    c(schoolwide.n,"",schoolwide.mat.format),
                    rbind(n.row,rep("",length(n.row)),mat.format),
                    p.col)
            ))
            
            
          }
          
          else if(colnames(my.data)[x]=="Salry_Comp") {
            
            q.col <- c(
              "N",
              paste0('BOLD',"Salary comparison"),
              "Less",
              "Same",
              "1-5\\% more",
              "6-10\\% more",
              "11-25\\% more",
              "26-50\\%",
              "\\textgreater50\\% more",
              "Not employed prior to"
            )
            
            n.row <- vector(mode = "numeric", length=4)
            mat <- matrix(NA,ncol=length(n.row),nrow=length(q.col)-2)
            mat.format <- mat
            
            for(i in 1:4) {
              n.row[i] <- sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i]]))
              
              mat[,i] <- c(
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Less"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Same"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="1-5\\%+"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="6-10\\%+"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="11-25\\%+"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="26-50\\%+"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]==">50\\%+"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="NotEmpPrior"]))
              )
              
              mat.format[,i] <- format(100*round(mat[,i]/n.row[i],3),nsmall=1)
            }
            
            schoolwide.n <- sum(!is.na(my.data[,x]))
            schoolwide.mat <- c(
              sum(!is.na(my.data[,x][my.data[,x]=="Less"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Same"])),
              sum(!is.na(my.data[,x][my.data[,x]=="1-5\\%+"])),
              sum(!is.na(my.data[,x][my.data[,x]=="6-10\\%+"])),
              sum(!is.na(my.data[,x][my.data[,x]=="11-25\\%+"])),
              sum(!is.na(my.data[,x][my.data[,x]=="26-50\\%+"])),
              sum(!is.na(my.data[,x][my.data[,x]==">50\\%+"])),
              sum(!is.na(my.data[,x][my.data[,x]=="NotEmpPrior"]))
            )
            schoolwide.mat.format <- format(100*round(schoolwide.mat/schoolwide.n,3),nsmall=1)
            
            p.col <- c(
              rep("",length(q.col)-1),
              if (format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2) == "0.00") {paste("LESS")}
              else {format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2)}
            )
            
            p.col <- ifelse(p.col=="NaN","N/A",p.col)
            
            return(rbind(
              rep('\\rule{0pt}{2ex}',4),
              cbind(q.col,
                    c(schoolwide.n,"",schoolwide.mat.format),
                    rbind(n.row,rep("",length(n.row)),mat.format),
                    p.col)
            ))
          }
          
          else if(colnames(my.data)[x]=="Q269_workcomm" |
                  colnames(my.data)[x]=="Q269_family" | 
                  colnames(my.data)[x]=="Q269_availfac" |
                  colnames(my.data)[x]=="Q269_progreq" |
                  colnames(my.data)[x]=="Q269_coursesched" |
                  colnames(my.data)[x]=="Q269_immigration" |
                  colnames(my.data)[x]=="Q269_otherobst") {
            
            
            q.col <- c(
              if(colnames(my.data)[x] == "Q269_workcomm") {paste0('BOLD',"Work/financial commitments")}
              else if(colnames(my.data)[x] == "Q269_family") {paste0('BOLD',"Family obligations")}
              else if(colnames(my.data)[x] == "Q269_availfac") {paste0('BOLD',"Availability of faculty")}
              else if(colnames(my.data)[x] == "Q269_progreq") {paste0('BOLD',"Program structure or requirements")}
              else if(colnames(my.data)[x] == "Q269_coursesched") {paste0('BOLD',"Course availability")}
              else if(colnames(my.data)[x] == "Q269_immigration") {paste0('BOLD',"Immigration laws or regulations")}
              else if(colnames(my.data)[x] == "Q269_otherobst") {paste0('BOLD',"Other")},
              "Schoolwide",
              "",
              "MPH",
              "Other Master's",
              "PhD/ScD",
              "DrPH"
            )
            
            n.col <- vector(mode = "numeric", length=length(q.col)-3)
            mat <- matrix(NA,ncol=3,nrow=length(n.col))
            mat.format <- mat
            
            
            for(i in 1:4) {
              n.col[i] <- sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i]]))
              
              mat[i,] <- c(
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Not an obstacle"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="A minor obstacle"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="A major obstacle"]))
              )
              
              mat.format[i,] <- format(100*round(mat[i,]/n.col[i],3),nsmall=1)
            }
            
            
            schoolwide.n <- sum(!is.na(my.data[,x]))
            schoolwide.mat <- c(
              sum(!is.na(my.data[,x][my.data[,x]=="Not an obstacle"])),
              sum(!is.na(my.data[,x][my.data[,x]=="A minor obstacle"])),
              sum(!is.na(my.data[,x][my.data[,x]=="A major obstacle"]))
            )
            schoolwide.mat.format <- format(100*round(schoolwide.mat/schoolwide.n,3),nsmall=1)
            
            
            n.col.full <- c("",schoolwide.n,"",n.col)
            mat.format.full <- rbind(rep("",ncol(mat.format)), schoolwide.mat.format, rep("",ncol(mat.format)), mat.format)
            
            p.col <- c(
              rep("",length(q.col)-1),
              if (format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2) == "0.00") {paste("LESS")}
              else {format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2)}
            )
            
            p.col <- ifelse(p.col=="NaN","N/A",p.col)
            
            return(rbind(
              rep('\\rule{0pt}{2ex}',1),
              cbind(q.col,n.col.full,mat.format.full,p.col)
            ))
          }
          
          else if(substr(colnames(my.data)[x],1,4) == "Q278") {
            
            my.data[,x] <- sapply(my.data[,x], function(x) gsub("Agree,Disagree",NA, x))
            my.data[,x] <- sapply(my.data[,x], function(x) gsub("Strongly agree,Agree","Agree", x))
            my.data[,x] <- sapply(my.data[,x], function(x) gsub("Strongly disagree,Don't know","Strongly disagree", x))
            my.data[,x] <- sapply(my.data[,x], function(x) gsub("Strongly agree,Don't know","Strongly agree", x))
            
            
            q.col <- c(
              if(colnames(my.data)[x] == "Q278_1") {paste0('BOLD',"I feel free to express")}
              else if(colnames(my.data)[x] == "Q278_2") {paste0('BOLD',"I feel free to express")}
              else if(colnames(my.data)[x] == "Q278_3") {paste0('BOLD',"Students are")}
              else if(colnames(my.data)[x] == "Q278_4") {paste0('BOLD',"Students are")}
              else if(colnames(my.data)[x] == "Q278_5") {paste0('BOLD',"Students are")}
              else if(colnames(my.data)[x] == "Q278_6") {paste0('BOLD',"Students are")}
              else if(colnames(my.data)[x] == "Q278_7") {paste0('BOLD',"Professors upheld")},
              "Schoolwide",
              "",
              "MPH",
              "Other Master's",
              "PhD/ScD",
              "DrPH"
            )
            
            n.col <- vector(mode = "numeric", length=length(q.col)-3)
            mat <- matrix(NA,ncol=5,nrow=length(n.col))
            mat.format <- mat
            
            
            for(i in 1:4) {
              n.col[i] <- sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i]]))
              
              mat[i,] <- c(
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Don't know"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Strongly disagree"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Disagree"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Agree"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Strongly agree"]))
              )
              
              mat.format[i,] <- format(100*round(mat[i,]/n.col[i],3),nsmall=1)
            }
            
            
            schoolwide.n <- sum(!is.na(my.data[,x]))
            schoolwide.mat <- c(
              sum(!is.na(my.data[,x][my.data[,x]=="Don't know"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Strongly disagree"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Disagree"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Agree"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Strongly agree"]))
            )
            schoolwide.mat.format <- format(100*round(schoolwide.mat/schoolwide.n,3),nsmall=1)
            
            
            n.col.full <- c("",schoolwide.n,"",n.col)
            mat.format.full <- rbind(rep("",ncol(mat.format)), schoolwide.mat.format, rep("",ncol(mat.format)), mat.format)
            
            p.col <- c(
              rep("",length(q.col)-1),
              if (format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2) == "0.00") {paste("LESS")}
              else {format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2)}
            )
            
            p.col <- ifelse(p.col=="NaN","N/A",p.col)
            
            return(rbind(
              rep('\\rule{0pt}{2ex}',1),
              cbind(q.col,n.col.full,mat.format.full,p.col)
            ))
          }
          
          else if(substr(colnames(my.data)[x],1,4) == "Q266") {
            
            q.col <- c(
              if(colnames(my.data)[x] == "Q266_1") {paste0('BOLD',"Safe")}
              else if(colnames(my.data)[x] == "Q266_2") {paste0('BOLD',"Welcoming")}
              else if(colnames(my.data)[x] == "Q266_3") {paste0('BOLD',"Competitive")}
              else if(colnames(my.data)[x] == "Q266_4") {paste0('BOLD',"Diverse")},
              "Schoolwide",
              "",
              "MPH",
              "Other Master's",
              "PhD/ScD",
              "DrPH"
            )
            
            n.col <- vector(mode = "numeric", length=length(q.col)-3)
            mat <- matrix(NA,ncol=4,nrow=length(n.col))
            mat.format <- mat
            
            for(i in 1:4) {
              n.col[i] <- sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i]]))
              
              mat[i,] <- c(
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Not at all"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="A little"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Moderately"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Very"]))
              )
              
              mat.format[i,] <- format(100*round(mat[i,]/n.col[i],3),nsmall=1)
            }
            
            
            schoolwide.n <- sum(!is.na(my.data[,x]))
            schoolwide.mat <- c(
              sum(!is.na(my.data[,x][my.data[,x]=="Not at all"])),
              sum(!is.na(my.data[,x][my.data[,x]=="A little"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Moderately"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Very"]))
            )
            schoolwide.mat.format <- format(100*round(schoolwide.mat/schoolwide.n,3),nsmall=1)
            
            
            n.col.full <- c("",schoolwide.n,"",n.col)
            mat.format.full <- rbind(rep("",ncol(mat.format)), schoolwide.mat.format, rep("",ncol(mat.format)), mat.format)
            
            p.col <- c(
              rep("",length(q.col)-1),
              if (format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2) == "0.00") {paste("LESS")}
              else {format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2)}
            )
            
            p.col <- ifelse(p.col=="NaN","N/A",p.col)
            
            return(rbind(
              rep('\\rule{0pt}{2ex}',1),
              cbind(q.col,n.col.full,mat.format.full,p.col)
            ))
          }
          
          else if(substr(colnames(my.data)[x],1,4) == "Q220") {
            
            q.col <- c(
              if(colnames(my.data)[x] == "Q220_sameuniv") {paste0('BOLD',"Would you select Johns Hopkins?")}
              else if(colnames(my.data)[x] == "Q220_samefield") {paste0('BOLD',"Would you select the same field of study?")}
              else if(colnames(my.data)[x] == "Q220_recommend") {paste0('BOLD',"Would you recommend Johns Hopkins to someone considering your field of study?")}
              else if(colnames(my.data)[x] == "Q220_samethesisadv") {paste0('BOLD',"Would you select the same thesis advisor?")},
              "Schoolwide",
              "",
              "MPH",
              "Other Master's",
              "PhD/ScD",
              "DrPH"
            )
            
            n.col <- vector(mode = "numeric", length=length(q.col)-3)
            mat <- matrix(NA,ncol=5,nrow=length(n.col))
            mat.format <- mat
            
            for(i in 1:4) {
              n.col[i] <- sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i]]))
              
              mat[i,] <- c(
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Definitely not"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Probably not"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Maybe"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Probably"])),
                sum(!is.na(my.data[,x][my.data$Q66==n.col.names[i] & my.data[,x]=="Definitely"]))
              )
              
              mat.format[i,] <- format(100*round(mat[i,]/n.col[i],3),nsmall=1)
            }
            
            schoolwide.n <- sum(!is.na(my.data[,x]))
            schoolwide.mat <- c(
              sum(!is.na(my.data[,x][my.data[,x]=="Definitely not"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Probably not"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Maybe"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Probably"])),
              sum(!is.na(my.data[,x][my.data[,x]=="Definitely"]))
            )
            schoolwide.mat.format <- format(100*round(schoolwide.mat/schoolwide.n,3),nsmall=1)
            
            
            n.col.full <- c("",schoolwide.n,"",n.col)
            mat.format.full <- rbind(rep("",ncol(mat.format)), schoolwide.mat.format, rep("",ncol(mat.format)), mat.format)
            
            p.col <- c(
              rep("",length(q.col)-1),
              if (format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2) == "0.00") {paste("LESS")}
              else {format(round(suppressWarnings(chisq.test(mat)$p.value),2),nsmall = 2)}
            )
            
            p.col <- ifelse(p.col=="NaN","N/A",p.col)
            
            return(rbind(
              rep('\\rule{0pt}{2ex}',1),
              cbind(q.col,n.col.full,mat.format.full,p.col)
            ))
          }
          
        }
########## 4. APPLY MAIN FUNCTION AND RETURN LIST OF MATRICES
        
      return (lapply(colsets, my.function) )
}



