library(readxl)
library(tidyverse)
library(readr)
library(lubridate, warn.conflicts = FALSE)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
#setwd("/Users/tg625/Documents/PDA/Directory/LC_REPORT/Code")

 myColours=c("Other"=depn_insti, "Brochure/flyer/mailer"=col_dislike_alot, 
             "Newspaper/radio/TV"=col_dislike, "Bus ad/billboard/cinema"=col_neutral,
             "website/social media"= col_like,"Infoline/211/988"=col_like_alot,
             "Presentation"= missing, "Provider/clinician/agency"=new, "Unknown"=other)     

#####################################################
# 1) Make Dataframes
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

comm_df<-dat %>%
    dplyr::select(Community_Referral, heardAboutInititative) %>% 
     mutate(heardAboutInititative=recode(heardAboutInititative, 
                                        "9 Other (please specify in next column)" = "Other", 
                                        "1 Brochure/flyer/mailer" = "Brochure/flyer/mailer",	
                                        "2 Newspaper/radio/TV" = "Newspaper/radio/TV",
                                        "3 Bus ad/billboard/cinema ad" = "Bus ad/billboard/cinema",
                                        "4 website/social media" = "website/social media",	
                                        "5 Infoline/211/988" = "Infoline/211/988",
                                        "6 Presentation" = "Presentation",
                                        "7 Provider/clinician/agency" = "Provider/clinician/agency",	
                                        "8 Unknown/cannot remember"	= "Unknown/cannot remember")) %>%
    mutate(Community_Referral=recode(Community_Referral,   
                                        "1 - Patient" = "Patient",
                                        "2 - Family" = "Family",	
                                        "3 - Police" = "Police",	
                                        "4 - Teacher or guidance counselor"= "Teacher or guidance counselor",
                                        "5 - Other (community)" = "Other")) %>%
    filter(!Community_Referral=="NA") %>%
    filter(!heardAboutInititative=="NA") %>%
    #mutate(heardAboutInititative=str_sub(heardAboutInititative, 3)) %>%
    group_by(Community_Referral) %>%
    reframe(Community_Referral, n=n(), heardAboutInititative=heardAboutInititative) %>%
    group_by(Community_Referral) #%>%
    #mutate(Community_Referral=str_sub(Community_Referral, 5))


 clin_df<-dat %>% 
        dplyr::select(Clinical_Referral, heardAboutInititative) %>% 
        mutate(heardAboutInititative=recode(heardAboutInititative, 
                                        "9 Other (please specify in next column)" = "Other", 
                                        "1 Brochure/flyer/mailer" = "Brochure/flyer/mailer",	
                                        "2 Newspaper/radio/TV" = "Newspaper/radio/TV",
                                        "3 Bus ad/billboard/cinema ad" = "Bus ad/billboard/cinema",
                                        "4 website/social media" = "website/social media",	
                                        "5 Infoline/211/988" = "Infoline/211/988",
                                        "6 Presentation" = "Presentation",
                                        "7 Provider/clinician/agency" = "Provider/clinician/agency",	
                                        "8 Unknown/cannot remember"	= "Unknown")) %>%     
        mutate(Clinical_Referral = recode(Clinical_Referral,
                                        "1 - Emergency department"= "Emergency department",
                                        "2 - Psychiatric inpatient"	="Psychiatric inpatient",
                                        "3 - Intensive outpatient"	="Intensive outpatient",
                                        "4 - Primary care provider" ="Primary care provider",	
                                        "5 - Outpatient mental health"	="Outpatient mental health",
                                       " 6 - Acute evaluation" ="Acute evaluation",
                                        "7 - mobile evaluation"	="Mobile evaluation",
                                        "8 - Other mental health"="Other mental health"	,
                                        "9 - other medical provider"="Other medical provider",
                                        "10 - Other (clinical)" ="Other")) %>%
        filter(!Clinical_Referral=="NA") %>%
        filter(!heardAboutInititative=="NA") %>%
        #mutate(heardAboutInititative=str_sub(heardAboutInititative, 3)) %>%
        group_by(Clinical_Referral) %>%
        reframe(Clinical_Referral, n=n(), heardAboutInititative=heardAboutInititative) %>%
        group_by(Clinical_Referral)
        #mutate(Clinical_Referral=str_sub(Clinical_Referral, 5)) 

#####################################################
# 2) Make plots
#####################################################

referrersCommunityPlot<-comm_df %>%
    ggplot(aes(y=Community_Referral)) +
    geom_bar(aes(fill=heardAboutInititative)) +
    theme_classic()+
    theme(legend.position="bottom",
          legend.title=element_blank(),
          axis.title=element_blank())+
    scale_y_discrete(labels = scales::label_wrap(10)) +
    guides(fill=guide_legend(nrow=3)) + #scale_fill_brewer(palette = "Dark2") 
     #scale_fill_manual(values = c("#ea6459", "#70f1bc", "#d418b9", "#9b34d7", "#5e00a5","#82eef6", "#bed41d", "#fcea43", "#fbe392"))
    scale_fill_manual(name="heardAboutInititative", values=myColours)


referrersClinicalPlot<-clin_df %>%
    ggplot(aes(y=Clinical_Referral)) +
    geom_bar(aes(fill=heardAboutInititative)) +
    theme_classic()+
    theme(legend.position="bottom",
          legend.title=element_blank(),
          axis.title=element_blank())+
    #scale_y_discrete(labels = scales::label_wrap(10)) +
    guides(fill=guide_legend(nrow=3)) +#+scale_fill_brewer(palette = "Dark2")
    #scale_fill_manual(values = c("#ea6459", "#70f1bc", "#d418b9", "#9b34d7", "#5e00a5","#82eef6", "#bed41d", "#fcea43", "#fbe392"))
    scale_fill_manual(name="heardAboutInititative", values=myColours)



