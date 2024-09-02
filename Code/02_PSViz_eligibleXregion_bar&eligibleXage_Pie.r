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

pie_dat<-dat %>%
    #filter(Inqstat %in% "Eligibility status confirmed") %>%
    filter(elig_YN==TRUE) %>%
    mutate(AGE=case_when(Over18==TRUE~"Over 18",
                            Over18==FALSE~"Under 18")) %>%
    mutate(AGE=as.factor(AGE)) %>%
    group_by(AGE) %>%
    select(AGE) %>%
    reframe(N=n()) 

#####################################################
# 2 make plots
#####################################################

eligibleXregion_bar<-dat %>%
    filter(Inqstat %in% "Eligibility status confirmed") %>%
    #mutate(LMHA=recode(LMHA, "STEP"="CMHA (STEP)")) %>%
    group_by(REGION, eligibility) %>%
    filter(!REGION %in% c("Out of catchment/no location given")) %>%
    #drop_na %>%
    mutate(elig_YN_new=case_when(elig_YN==FALSE~"Ineligible",
                                 elig_YN==TRUE~"Eligible")) %>% #select(REGION, eligibility)
    ggplot(aes(x=REGION, fill = forcats::fct_rev(elig_YN_new))) +
    #geom_bar()
    geom_bar(reverse = TRUE) +
    theme_classic() + 
    theme(legend.title=element_blank()) +
    #scale_fill_brewer(palette = "Dark2") +
    #scale_x_discrete(labels = scales::label_wrap(5)) +
    ylab("Count of (in)eligible\nindivdiuals") +
    xlab("Region") +
    scale_fill_manual(values = c(alpha("#D95F02", 0.2), alpha("#D95F02", 1)))

pie<-pie_dat %>%
    ggplot(aes(x="", y=N, fill=AGE)) +
    geom_bar(stat="identity", width=1, colour="white") +
    coord_polar(theta="y", start=0) +
    theme_void() +
    theme(legend.title=element_blank())

eligibleUnder18_pie<-pie +
    annotate("text", label=pie_dat$N[1], x=1 ,y=pie_dat$N[2] + pie_dat$N[1]/2 ) +
    annotate("text", label=pie_dat$N[2], x=1 , y=pie_dat$N[2]/2  ) +
                scale_fill_manual(values = c("#fd862a", "#D95F02"))