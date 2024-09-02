
library(readxl)
library(tidyverse)
library(readr)
library(lubridate, warn.conflicts = FALSE)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)

    dat <- read_excel("Data/LHN & STEP - PhoneScreen and CONSORT Data V2.xlsx", skip = 1)  %>%
                        filter(!is.na(`SCREEN NO.`)) %>%
                        rename(Inqstat = as.factor("INQUIRY STATUS")) %>%
                        rename(Inqdate = "INQUIRY DATE (VOICEMAIL LEFT)") %>%
                        rename(IngLine = "Indicate whether inquiry was made via Learning collaborative phone line or other") %>%
                        rename(ReasonNoEnroll3mo = `IF NO, PLEASE SELECT THE CODE THAT BEST DESCRIBES THE REASON`) %>%
                        mutate(Inqdate=lubridate::as_date(Inqdate)) %>%
                        mutate(Inqdate_month = month(Inqdate)) %>%
                        mutate(Inqdate_day = mday(Inqdate)) %>%
                        mutate(heardAboutInititative=as.factor(`SELECT CODE THAT BEST DESCRIBES HOW THEY HEARD ABOUT THIS INITIATIVE`)) %>%
                        mutate(REGION=recode(REGION,"STEP"="2")) %>%
                        mutate(REGION=as.factor(REGION)) %>%
                        mutate(Community_Referral=as.factor(`SELECT THE COMMUNITY CODE THAT BEST CAPTURES THE REFERRER PROFILE`)) %>%
                        mutate(Clinical_Referral=as.factor(`SELECT THE CLINICAL CODE THAT BEST CAPTURES THE REFERRER PROFILE`)) %>%    
                        mutate(eligibility=as.factor(`PLEASE SELECT THE CODE THAT BEST SUITS THE ELIGIBILITY STATUS OF THIS PARTICIPANT`)) %>%
                        mutate(elig_YN=case_when(eligibility == "1 Eligible - met all inclusion criteria (including consent)" ~ TRUE,   TRUE ~ FALSE)) %>%
                        mutate(Over18=AGE>=18) %>%
                        rename(FU_admitted=as.factor("HAS THE PATIENT BEEN SUCCESFULLY ADMITTED INTO TREATMENT BY THE 3 MOS MARK?" )) %>%
                        mutate(FU_admitted=as.character(FU_admitted)) %>%
                        rename(DateConsent="DATE OF CONSENT") %>%
                        #mutate(DateConsent=as.numeric(DateConsent)) %>%
                        mutate(DateConsent=lubridate::as_date(DateConsent)) %>%
                        rename(DateAdmittedLMHA="IF YES, PLEASE INDICATE THE DATE OF ADMISSION") %>%
                        mutate(DateAdmittedLMHA=lubridate::as_date(DateAdmittedLMHA)) %>%
                        mutate(LMHA=as.factor(`NAME OF LMHA THE PATIENT HAS BEEN REFERRED TO`)) 

#############################
#Numbers for text informatics   
##############################                  
    #No. of inquries made overall - for text
        tot_inq<-dat %>% 
                    #group_by("SCREEN NO.") %>%
                    nrow()
        
    # monthly queries

        dat_monthlylyqueries<-dat %>%
            mutate( Inqmonth = month(Inqdate, label=TRUE)) %>% 
            group_by(Inqmonth) %>%
            summarise(n=n()) %>%
            drop_na() %>%
            mutate(cumulativeN=cumsum(n))
            
        monthlyqueries<-dat_monthlylyqueries %>%
                                                tail(1)

    #WEEKLY queries
        dat_weeklyqueries <-dat %>%
                        mutate( Inqweek = floor_date(Inqdate, "week")) %>% 
                        group_by(Inqweek, .drop=FALSE) %>%
                        summarise(n=n()) %>%
                        mutate(cumsum=cumsum(n), REGION="All") 

        dat_weeklyqueries <-dat_weeklyqueries[nrow(dat_weeklyqueries)+1,]<-list(date("2024-01-26 UTC"), 0, 0, "All")
    
        numWeeks<-nrow(dat_weeklyqueries)
        weeklyquerychange<-dat_weeklyqueries$n[numWeeks]-dat_weeklyqueries$n[numWeeks-1]

        Other_line<-dat %>%
            filter(IngLine=="OTHER") %>%
            select(IngLine) %>%
            summarise(n())

        LC_line<-dat %>%
            filter(!IngLine=="OTHER") %>%
            select(IngLine) %>%
            summarise(n())

########################
#quarterly comparisons
########################
Quarter1total <-  dat %>%
                      filter(between(Inqdate, as.Date("2024-01-31"), as.Date("2024-05-01"))) %>%
                      nrow()

Quarter2total <- dat %>%
                      filter(between(Inqdate, as.Date("2024-05-01"), as.Date("2024-08-01"))) %>%
                      nrow()

percentageChange_raw <- ((Quarter2total - Quarter1total)/Quarter1total)*100

percentageChange <- formatC(as.numeric(as.character(round(percentageChange_raw, 0))), digits = 0, format = "f")

Eligbile_Text<-dat %>%
    #filter(Inqstat %in% "Eligibility status confirmed") %>% 
    group_by(eligibility) %>% 
    #endregionselect(eligibility) %>% print(n=100)
    filter(elig_YN==TRUE) %>%
    summarise(n=n()) 

Ineligible_Text<-dat %>%
    filter(Inqstat %in% "Eligibility status confirmed") %>%
    group_by(elig_YN) %>%
    #select(eligibility) %>%
    filter(elig_YN==FALSE) %>%
    summarise(n=n()) 

PendingActivecases_text<-dat %>%
                    filter(Inqstat %in% "Pending - active") %>%
                    group_by(Inqstat) %>%
                    summarise(n=n())

PendingInactivecases_text<-dat %>%
                    filter(Inqstat %in% "Pending - inactive") %>%
                    group_by(Inqstat) %>%
                    summarise(n=n())

Under18_Text<-dat%>%
                #filter(Inqstat %in% "Eligibility status confirmed") %>%
                filter(elig_YN==TRUE) %>%
                group_by(Over18) %>%
                filter(Over18==FALSE) %>%
                summarise(n=n())

number_admitted<-dat %>%
                    #filter(Inqstat %in% "Eligibility status confirmed") %>%
                    filter(elig_YN=="TRUE") %>%
                    #filter(FU_admitted=="YES") %>%
                    mutate(waitTime_inq2admit=DateAdmittedLMHA-DateConsent) %>%
                    mutate(FU_admitted_numeric=case_when(FU_admitted=="AWAITING CONFIRMATION"~0,
                                                FU_admitted=="YES"~1, 
                                                FU_admitted=="NO"~2, TRUE~0)) %>%
                    filter(FU_admitted_numeric==1) %>% nrow()

number_not_admitted<-dat %>%
                    #filter(Inqstat %in% "Eligibility status confirmed") %>%
                    filter(elig_YN=="TRUE") %>%
                    #filter(FU_admitted=="YES") %>%
                    mutate(waitTime_inq2admit=DateAdmittedLMHA-DateConsent) %>%
                    mutate(FU_admitted_numeric=case_when(FU_admitted=="AWAITING CONFIRMATION"~0,
                                                FU_admitted=="YES"~1, 
                                                FU_admitted=="NO"~2, TRUE~0)) %>%
                    filter(FU_admitted_numeric==2) %>% nrow()

number_awaiting_admission<-dat %>%
                    #filter(Inqstat %in% "Eligibility status confirmed") %>%
                    filter(elig_YN=="TRUE") %>%
                    #filter(FU_admitted=="YES") %>%
                    mutate(waitTime_inq2admit=DateAdmittedLMHA-DateConsent) %>%
                    mutate(FU_admitted_numeric=case_when(FU_admitted=="AWAITING CONFIRMATION"~0,
                                                FU_admitted=="YES"~1, 
                                                FU_admitted=="NO"~2, TRUE~0)) %>%
                    filter(FU_admitted_numeric==0) %>% nrow()

statewideAverage<-dat %>%
                    #filter(Inqstat %in% "Eligibility status confirmed") %>%
                    filter(elig_YN=="TRUE") %>% 
                    #filter(FU_admitted=="YES") %>%
                    mutate(waitTime_inq2admit=DateAdmittedLMHA-DateConsent) %>% 
                    filter(!FU_admitted=="NO") %>%
                    #mutate(case_when(waitTime_inq2admit=="NA"~0)) %>%
                    mutate(FU_admitted_numeric=case_when(FU_admitted=="AWAITING CONFIRMATION"~0,
                                                FU_admitted=="YES"~1, 
                                                FU_admitted=="NO"~2, TRUE~0)) %>%
                    filter(FU_admitted_numeric==1) %>%
                    select(LMHA, REGION, FU_admitted_numeric, waitTime_inq2admit) %>%
                    #group_by(LMHA, REGION) %>%
                    summarise(`Patients admitted`=sum(FU_admitted_numeric), `Averagewaittime`=median(waitTime_inq2admit)) 
