library(readxl)
library(tidyverse)
library(readr)
library(lubridate, warn.conflicts = FALSE)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
#setwd("/Users/tg625/Documents/PDA/Directory/LC_REPORT/Code")
#####################################################
# 1) Make Dataframe
#####################################################
    dat <- read_excel("/Users/tg625/Documents/PDA/Directory/LC_REPORT/Data/LHN & STEP - PhoneScreen and CONSORT Data V2.xlsx", skip = 1)  %>%
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
                        #mutate(waittime=)


depn_insti <- "#f9570c"
col_dislike_alot <- "#b29882"
col_dislike <- "#edad88"
col_neutral <- "#c7cdd1"
col_like <- "#829cb2"
col_like_alot <- "#3e6487"
missing <- "#636363"
new <- "#DB6B0B"
other <- "#F9CA0C"
purp <- "#b2829c"

 #####################################################
# data wrangle
#####################################################

plotdat<- dat %>%
                    filter(elig_YN=="TRUE") %>% 
                   mutate(waitTime_inq2admit=DateAdmittedLMHA-DateConsent) %>% 
                    mutate(FU_admitted_numeric=case_when(FU_admitted=="AWAITING CONFIRMATION"~0,
                                                FU_admitted=="YES"~1, 
                                                FU_admitted=="NO"~2, TRUE~0)) %>%
                    filter(FU_admitted_numeric==1) %>%
                    select(REGION, FU_admitted_numeric, waitTime_inq2admit) %>%
                    group_by(REGION) %>%
                    summarise(`No. admitted`=sum(FU_admitted_numeric), 
                              waitTime_inq2admit = as.numeric(waitTime_inq2admit)) 

#####################################################
# make plot
#####################################################
       plt<- plotdat %>% 
                ggplot(aes(x=REGION, y=waitTime_inq2admit, fill=REGION)) +
                geom_boxplot() +
                theme_minimal() +
                scale_fill_manual(values=c(depn_insti,col_like, col_dislike_alot))

    finalplot<-plt + geom_jitter(shape=16, position=position_jitter(0.2))

#####################################################
# save
#####################################################

ggsave(finalplot, filename = "/Users/tg625/Documents/PDA/Directory/LC_REPORT/Figures/DUP_by_region_boxplot.pdf")
