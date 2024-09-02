---
title: "LC Monthly Report - March 2024"
author: "LC team"
date: now
title-block-banner: true
format: 
  pdf
execute:
    echo: false
---
```{r}
#| warning: false

library(readxl)
library(tidyverse)
library(readr)
library(lubridate, warn.conflicts = FALSE)
library(ggplot2)
library(RColorBrewer)

#####################################################
# 1) Make Dataframe
#####################################################
    dat <- read_excel("/Users/tg625/Downloads/LHN & STEP - PhoneScreen and CONSORT Data V2.xlsx", skip = 1)  %>%
                        filter(!is.na(`SCREEN NO.`)) %>%
                        rename(Inqstat = as.factor("INQUIRY STATUS")) %>%
                        rename(Inqdate = "INQUIRY DATE (VOICEMAIL LEFT)") %>%
                        rename(IngLine = "Indicate whether inquiry was made via Learning collaborative phone line or other") %>%
                        mutate(Inqdate=lubridate::as_date(Inqdate)) %>%
                        mutate(Inqdate_month = month(Inqdate)) %>%
                        mutate(Inqdate_day = mday(Inqdate)) %>%
                        mutate(heardAboutInititative=as.factor(`SELECT CODE THAT BEST DESCRIBES HOW THEY HEARD ABOUT THIS INITIATIVE`)) %>%
                        mutate(REGION=as.factor(REGION)) %>%
                        mutate(Community_Referral=as.factor(`SELECT THE COMMUNITY CODE THAT BEST CAPTURES THE REFERRER PROFILE`)) %>%
                        mutate(Clinical_Referral=as.factor(`SELECT THE CLINICAL CODE THAT BEST CAPTURES THE REFERRER PROFILE`)) %>%    
                        mutate(eligibility=as.factor(`PLEASE SELECT THE CODE THAT BEST SUITS THE ELIGIBILITY STATUS OF THIS PARTICIPANT`)) %>%
                        mutate(elig_YN=case_when(eligibility == "1 Eligible - met all inclusion criteria (including consent)" ~ TRUE,   TRUE ~ FALSE)) %>%
                        mutate(Over18=AGE>18) %>%
                        rename(FU_admitted=as.factor("HAS THE PATIENT BEEN SUCCESFULLY ADMITTED INTO TREATMENT BY THE 3 MOS MARK?" )) %>%
                        rename(DateConsent="DATE OF CONSENT") %>%
                        rename(DateAdmittedLMHA="IF YES, PLEASE INDICATE THE DATE OF ADMISSION") %>%
                        mutate(DateAdmittedLMHA=lubridate::as_date(DateAdmittedLMHA)) %>%
                        mutate(LMHA=as.factor(`NAME OF LMHA THE PATIENT HAS BEEN REFERRED TO`)) 
                        #mutate(waittime=)
                       #mutate(Inq_mostRecentMonth = casewhen(Today - Month == 0 ~ TRUE))

    Month <- period(months = 1)
    Today <- today(tzone = "")
    LastMonth <- Today-Month
    #interval(Today-LastMonth)


    #####################################
    #Inquiries by week, by region 
    #####################################    
        dat_weeklyqueries_region <-dat %>%
                        mutate( Inqweek = floor_date(Inqdate, "week")) %>% 
                        group_by(Inqweek, REGION, .drop=FALSE) %>%
                        summarise(n=n()) %>%
                        group_by(REGION) %>%
                        mutate(cumsum=cumsum(n)) 

        regions<-dat_weeklyqueries_region$REGION %>% unique
        missingRows<-data.frame(Inqweek=rep(lubridate::as_date("2024-01-26 UTC"), length(regions)), 
                                                                            REGION=regions, 
                                                                            n=rep(0, length(regions)), 
                                                                            cumsum=rep(0, length(regions))) %>%
                                                                            as_tibble
        dat_weeklyqueries_region<-rbind(missingRows, dat_weeklyqueries_region)
```
## Inquiries made to the Learning Collaborative

```{r}

#| warning: false

##############################
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
            mutate(cumulativeN=cumsum(n))
            
        monthlyqueries<-dat_monthlylyqueries %>%
                                                tail(1)

    #WEEKLY queries
        dat_weeklyqueries <-dat %>%
                        mutate( Inqweek = floor_date(Inqdate, "week")) %>% 
                        group_by(Inqweek, .drop=FALSE) %>%
                        summarise(n=n()) %>%
                        mutate(cumsum=cumsum(n)) 

        dat_weeklyqueries[nrow(dat_weeklyqueries)+1,]<-list(date("2024-01-26 UTC"), 0, 0)
    
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


```

The total number of inquiries made since the start of the LC launch (Feb 1st 2024) was `{r} tot_inq` (see @fig-inquiries). Of these inquiries, `{r} Other_line` calls were made via the LC direct line, and `{r} LC_line` via other routes. There were  `{r} monthlyqueries$n` queries during `{r} monthlyqueries$Inqmonth` .  

```{r}
#| warning: false
#| label: fig-inquiries
#| fig-cap: "Figure showing inquiries for the LHN scheme"
#| fig-subcap: 
#|  - "Total number of inquiries by region"
#|  - "Cumulative weekly increase in inquiries made to the LHN scheme overall (black), and split by regions 1-5)."
#| layout-ncol: 2

    #####################################
    #Inquiries by week, by region PLOT
    #####################################    
    inq_tabxRegion<-  dat %>%
                            group_by(REGION) %>%
                            #filter(!REGION=="Out of catchment") %>%
                            select(Inqstat) %>%
                            group_by(REGION) %>%
                            summarise(N=n())
        
        knitr::kable(inq_tabxRegion) #,format="markdown")

        dat_weeklyqueries_region  %>%
                                    #filter(REGION %in% c(1:5)) %>%
                                        ggplot() +
                                            geom_line(aes(x=Inqweek, y=cumsum, color=REGION)) +
                                            geom_line(data=dat_weeklyqueries, aes(x=Inqweek, y=cumsum))+
                                            theme_classic() +
                                            ylab("Cumulative Total Inquiries") +
                                            xlab("Week no.") + 
                                            scale_fill_brewer(palette = "Dark2") 

```

## Information about referrers - who made the inquiries?

```{r}
#| label: fig-referrersRoutes
#| fig-cap: "Referrals to the LC via clinical and community nodes. This is further split by how these groups heard about the initiative"
#| fig-subcap: 
#|  - "Community referral nodes"
#|  - "Clinical referral nodes"
#| fig-align: center
#| warning: false
#| layout-ncol: 2

comm_df<-dat %>%
    dplyr::select(Community_Referral, heardAboutInititative) %>%
    filter(!Community_Referral=="NA") %>%
    filter(!heardAboutInititative=="NA") %>%
    mutate(heardAboutInititative=str_sub(heardAboutInititative, 3)) %>%
    group_by(Community_Referral) %>%
    reframe(Community_Referral, n=n(), heardAboutInititative=heardAboutInititative) %>%
    group_by(Community_Referral) %>%
    mutate(Community_Referral=str_sub(Community_Referral, 5))

comm_df %>%
    ggplot(aes(y=Community_Referral)) +
    geom_bar(aes(fill=heardAboutInititative)) +
    theme_classic()+
    theme(legend.position="bottom",
          legend.title=element_blank(),
          axis.title=element_blank())+
    scale_y_discrete(labels = scales::label_wrap(10)) +
    guides(fill=guide_legend(nrow=3)) + scale_fill_brewer(palette = "Dark2") 

clin_df<-dat %>% 
    dplyr::select(Clinical_Referral, heardAboutInititative) %>%
    filter(!Clinical_Referral=="NA") %>%
    filter(!heardAboutInititative=="NA") %>%
    mutate(heardAboutInititative=str_sub(heardAboutInititative, 3)) %>%
    group_by(Clinical_Referral) %>%
    reframe(Clinical_Referral, n=n(), heardAboutInititative=heardAboutInititative) %>%
    group_by(Clinical_Referral) %>%
    mutate(Clinical_Referral=str_sub(Clinical_Referral, 5)) 

clin_df %>%
    ggplot(aes(y=Clinical_Referral)) +
    geom_bar(aes(fill=heardAboutInititative)) +
    theme_classic()+
    theme(legend.position="bottom",
          legend.title=element_blank(),
          axis.title=element_blank())+
    #scale_y_discrete(labels = scales::label_wrap(10)) +
    guides(fill=guide_legend(nrow=3)) +scale_fill_brewer(palette = "Dark2")


# get summarys stats for text
commMax<-comm_df %>%
    summarise(n=n()) %>%
    filter(n==max(n)) 

clinMax<-clin_df %>%
    summarise(n=n()) %>%
    filter(n==max(n))  


```

The majority of people have been referred to the LC via the `{r} clinMax$Clinical_Referral` clinical node or the `{r} commMax$Community_Referral` community node. Indivdiuals from within these nodes have heard about the initiative in various ways (see @fig-referrersRoutes). 
```{r}

Eligbile_Text<-dat %>%
    filter(Inqstat %in% "Eligibility status confirmed") %>% 
    group_by(eligibility) %>%
    #endregionselect(eligibility) %>% print(n=100)
    filter(eligibility %in% "1 Eligible - met all inclusion criteria (including consent)") %>%
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
                filter(Inqstat %in% "Eligibility status confirmed") %>%
                filter(elig_YN==TRUE) %>%
                group_by(Over18) %>%
                filter(Over18==TRUE) %>%
                summarise(n=n())

    #mutate(heardAboutInititative=as.factor(`SELECT CODE THAT BEST DESCRIBES HOW THEY HEARD ABOUT THIS INITIATIVE`)) %>%

  
```


## Who was found to be eligible for the Learning Collaborative initiative?

So far, `{r} Eligbile_Text$n` people have been found to be eligible for the Learning Collaborative.  Of the `{r} Eligbile_Text$n` eligible for the LC, `{r} Under18_Text$n` were under 18 (see @fig-EligXRegion). A further `{r} Ineligible_Text$n` were deemed to be ineligible (see @tbl-ineligibility) and provided with appropriate signposting. The remaining cases were individuals either awaiting further assessment (n=`{r} PendingActivecases_text$n`), or cases that are expected to remain unresvolved due to e.g., loss of contact with the inquirer (n=`{r} PendingInactivecases_text$n`).

```{r}
#| label: fig-EligXRegion
#| fig-cap: "Capturing eligibility information by region and age"
#| fig-subcap: 
#|  - "Count of eligible (orange) vs ineligible (green) indvidiuals by region"
#|  - "Pie chart showning how many eligible indivdiuals were under 18yrs."
#| warning: false
#| layout-ncol: 2

#number of eligibile people by region 
dat %>%
    filter(Inqstat %in% "Eligibility status confirmed") %>%
    group_by(REGION, eligibility) %>%
    rename(`Eligible for LC`=elig_YN) %>%
    mutate(`Eligible for LC`=case_when(`Eligible for LC`==TRUE~"Ineligible",
                                        `Eligible for LC`==FALSE~"Eligible")) %>%
    ggplot(aes(x=REGION, fill=`Eligible for LC`)) +
    geom_bar() +
    theme_classic() + 
    theme(legend.title=element_blank()) +
    scale_fill_brewer(palette = "Dark2") +
    scale_x_discrete(labels = scales::label_wrap(5)) +
    ylab("Count of (in)eligible\nindivdiuals") +
    xlab("Region")


pie<-dat %>%
    filter(Inqstat %in% "Eligibility status confirmed") %>%
    filter(elig_YN==TRUE) %>%
    mutate(Over18=case_when(Over18==TRUE~"Over 18",
                            Over18==FALSE~"Under 18")) %>%
    group_by(Over18) %>%
    select(Over18) %>%
    reframe(n()) %>%
    #ggplot(aes(x=REGION, fill=Over18)) +
    #geom_bar() +
    #theme_classic()
    ggplot(aes(x="", y=`n()`, fill=Over18)) +
    geom_bar(stat="identity", width=1, colour="white") +
    coord_polar("y", start=0) +
     theme_void() 

     pie +
  #theme(axis.text.x=element_blank())+
  geom_text(aes(y = `n()`/2 + c(0, cumsum(`n()`)[-length(`n()`)]), 
                label = `n()`)) +
                 scale_fill_manual(values = c( "#D95F02", "#fd862a"))


```
```{r}
#| label: tbl-ineligibility
#| tbl-cap: "Table showing reasons for ineligibility"
#| warning: false

tab_dat<-dat %>%
    filter(Inqstat %in% "Eligibility status confirmed") %>%
    filter(!eligibility=="1 Eligible - met all inclusion criteria (including consent)") %>%
     mutate(`Reasons for Ineligibility`=str_sub(eligibility, 3)) %>%
    #mutate(eligibility_test = paste0(str_split(eligibility, ' - '))) %>% select(eligibility_test)                       
    group_by(`Reasons for Ineligibility`) %>%
    summarise(N=n())

    knitr::kable(head(tab_dat), format="markdown")

```
## Referrals to LMHAs across Connecticut

Below is a table showing the facilities that indivdiuals have been referred to within the LC. Information is also provided as to how many of these indivdiuals have already been officially enrolled at the LMHA. 

Add in average wait time days (consent - date of enrollment to LMHA)?

```{r}
#| label: tbl-LMHAreferreals
#| tbl-cap: "Table showing number of referrals to each LMHA"
#| warning: false

tabLMHArefs<-dat %>%
    filter(Inqstat %in% "Eligibility status confirmed") %>%
    filter(elig_YN=="TRUE") %>%
    #filter(FU_admitted=="YES") %>%
    mutate(waitTime_inq2admit=DateAdmittedLMHA-Inqdate) %>%
    mutate(FU_admitted_numeric=case_when(FU_admitted=="NA"~0,
                                FU_admitted=="YES"~1, TRUE~0)) %>%
    select(LMHA, REGION, FU_admitted_numeric, waitTime_inq2admit) %>%
    group_by(LMHA) %>%
    summarise(`No. referred to LMHA`=n(), `No. admitted to LMHA`=sum(FU_admitted_numeric), `Average  wait time`=mean(waitTime_inq2admit)) 

    knitr::kable(head(tabLMHArefs), format="markdown")

```

```{r}

#| label: tbl-waittime
#| tbl-cap: "Table showing number of referrals to each LMHA"
#| warning: false

#waittime <- dat %>%
             #   filter(Inqstat %in% "Eligibility status confirmed") %>%
              #  filter(elig_YN=="TRUE") %>%
              #   filter(FU_admitted=="YES") %>%
              #  select(LMHA, DateConsent, DateAdmittedLMHA, Inqdate, FU_admitted) %>%
                
             #   mutate(waitTime_inq2admit=DateAdmittedLMHA-Inqdate) %>%
             #   select(LMHA, waitTime_inq2admit, FU_admitted) %>%
             #   group_by(LMHA) %>%
             #   summarise(meanwaititme=mean(waitTime_inq2admit), FU_admitted_count=count(FU_admitted=="YES"))
#    knitr::kable(head(waittime), format="markdown")

```



## Follow up Information

Here we could have information about who ended up enrolled in care vs who didnt - and why. 
