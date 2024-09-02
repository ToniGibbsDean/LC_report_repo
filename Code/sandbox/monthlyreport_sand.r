    

#using the same approach as the weekly - but get the difference between monthly inq differences from 1st feb but also the date fo the actual STEP meeting
   inq_increase<-dat %>%
                    data.frame(day=seq(as.Date('2022-01-01'), by = 'week', length.out=10))
                    filter(Inqdate %in% 2024-02-05)


   
    #No. inquiries have increased over the last month 
   
    dat$Inqdate %>% floor_date(Sys.Date() - months(1), "month")



    # monthly queries

dat_monthlylyqueries<-dat %>%
    mutate( Inqmonth = month(Inqdate, label=TRUE)) %>% 
    group_by(Inqmonth) %>%
    summarise(n=n()) %>%
    mutate(cumsum(n))
    
monthlyqueries<-dat_monthlylyqueries %>%
    tail(1)

numMonths<-nrow(dat_monthlylyqueries)
monthlyquerychange<-dat_monthlylyqueries$n[numMonths]-dat_monthlylyqueries$n[numMonths-1]

```
```{r}

#| label: fig-weeklyqueries
#| fig-cap: "Showing the weekly queries, note that these are calendar weeks. "
#| warning: false
#| 


    dat_weeklyqueries_region  %>%
    ggplot() +
        geom_line(aes(x=Inqweek, y=cumsum, color=REGION)) +
        geom_line(data=dat_weeklyqueries, aes(x=Inqweek, y=cumsum))+
        theme_minimal() 

```

  dat_weeklyqueries  %>%
        ggplot(aes(x=Inqweek, y=cumsum)) +
            geom_line() +
            theme_minimal() 