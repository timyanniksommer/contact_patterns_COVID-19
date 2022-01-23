---
title: "Contact Data"
author: "Tim Sommer, Xin Yu, Ronja Steinmetz"
date: "26.09.2021"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, results='hide', error = FALSE, warning=FALSE, message=FALSE)
# this chunk simply contains the chunk settings
```

# Preparation

### Load R-Packages


```{r packages, echo=TRUE, results='hide', error = FALSE, warning=FALSE, message=FALSE}
library('socialmixr')
# https://cran.r-project.org/web/packages/socialmixr/vignettes/introduction.html
library('ggplot2')
library('tidyverse')
library('plyr')
library('dplyr')
library('tidyr')
library('data.table')
library('RColorBrewer') # used to customize (heatmap) color
library('table1')
library('ComplexHeatmap') # heatmap
#library('ggpubr') #plot combine
library('ggmosaic')
library('gridExtra')
library('scales')
library('grid')
library('Rmisc')
library('cowplot')
library('gtable')
library('ggpubr')
```


### prepare and import data

This chunk creates name vectors for the waves and name and label vectors 
respectively for the places. 
```{r name vectors for wave and place}
#WAVES <- c("W01",
#           "W02",
 #          "W03",
  #         "W04",
   #        "W05",
    #       "W06",
     #      "W07",
      #     "W08")

places <- c("place_home_own",
            "place_home_else",
            "place_work",
            "place_worship",
            "place_transport",
            "place_school",
            "place_shop_essential", 
            "place_shop_non_essential",
            "place_entertainment",
            "place_sport",
            "place_park",
            "place_healthcare",
            "place_beauty",
            "place_oth")

CONTACT_N <- sub("place_", "", places)
CONTACT_N <- paste0(CONTACT_N, "_contact_n")

```

$\color{red}{\text{Don't forget to change the path!}}$
```{r create path for data importing}
DATA <- list.files(path = "C:/Users/ronja/OneDrive/Desktop/Seminar_Statistik/data/",
                   pattern = "RData",
                   full.names = TRUE)
```

The following chunk imports the data, aggregates the waves and creates three 
datasets. (1) self, (2) hh and (3) outside dataset.

```{r import data and aggregates waves, echo=TRUE, results='hide', error = FALSE, warning=FALSE, message=FALSE}
                              
counter <- 0 # Startpunkt für den loop
for(current_file in DATA) {
  counter <- counter + 1
  load(current_file, .GlobalEnv) # im vorherigen Chunk haben wir die file paths 
  # über DATA hinterlegt
  WAVE <- current_file
  WAVE <- sub("C:/Users/ronja/OneDrive/Desktop/Seminar_Statistik/data/wave ", "", WAVE) # WAVE = current_file
  WAVE <- as.integer(sub("_anonymised[.]RData", "", WAVE))  
  print(WAVE)
  print(sum(is.na(d_self_anonymised$new_id)))
  
  # d_self_anonymised
  d_self_anonymised$wave <- WAVE
  
  for(current_variable in names(d_self_anonymised)) {
    if(class(d_self_anonymised[, current_variable])[1] %in% c("factor")) { 
      d_self_anonymised[, current_variable] <- as.character(d_self_anonymised[, current_variable])
    }
    if(class(d_self_anonymised[, current_variable])[1] %in% c("haven_labelled")) { 
      d_self_anonymised[, current_variable] <- as.integer(d_self_anonymised[, current_variable])
    }
  }
  
  # dt_hh_anonymised
  dt_hh_anonymised$wave <- WAVE

  dt_hh_anonymised$contact_id <- as.double(as.character(dt_hh_anonymised$contact_id))
  
  for(current_variable in names(dt_hh_anonymised)) {
    if(class(dt_hh_anonymised[, current_variable])[1] %in% c("factor")) { 
      dt_hh_anonymised[, current_variable] <- as.character(dt_hh_anonymised[, current_variable])
    }
    if(class(dt_hh_anonymised[, current_variable])[1] %in% c("haven_labelled")) { 
      dt_hh_anonymised[, current_variable] <- as.integer(dt_hh_anonymised[, current_variable])
    }
  }

  #dt_outside_anonymised
  dt_outside_anonymised$wave <- WAVE
  dt_outside_anonymised$contact_id <- as.integer(as.character(dt_outside_anonymised$contact_id))
  for(current_variable in names(dt_outside_anonymised)) {
    if(class(dt_outside_anonymised[, current_variable])[1] %in% c("factor")) { 
      dt_outside_anonymised[, current_variable] <- as.character(dt_outside_anonymised[, current_variable])
    }
    if(class(dt_outside_anonymised[, current_variable])[1] %in% c("haven_labelled")) { 
      dt_outside_anonymised[, current_variable] <- as.integer(dt_outside_anonymised[, current_variable])
    }
  }  
  
  # rename datasets and bind
  if(counter == 1) {
    d_self_all <- d_self_anonymised
    dt_hh_all <- dt_hh_anonymised
    dt_outside_all <- dt_outside_anonymised
  } else {
    d_self_all <- bind_rows(d_self_all, d_self_anonymised)
    dt_hh_all <- bind_rows(dt_hh_all, dt_hh_anonymised)
    dt_outside_all <- bind_rows(dt_outside_all, dt_outside_anonymised)
  }
  rm(d_self_anonymised, dt_hh_anonymised, dt_outside_anonymised) 
}
```

# Data Cleaining 



### Cleaning of Data 

```{r check levels of place-variables dt_outside_all transform place_oth and yes no answers}
# for the place-variables in dt_hh_all and dt_outside_all the questionnaire asks
# about meetings in general, so it's either yes, no or NA (for other places: NA 
# or 1=YES)

## create dummy (smaller dataset with dt_outside_all)
dummy <- d_self_all %>% select(new_id, wave, age_group)
dummynames<-c("new_id", "wave","age_group_participant")
colnames(dummy) <- dummynames
dummy <- merge(dummy, dt_outside_all, by=c("new_id", "wave"), all.y=T)

dummy$place_oth <- as.numeric(dummy$place_oth)
dummy$place_oth[is.na(dummy$place_oth)] <- 0 #change to 0

# the other places can take "yes", "no" and NA, now change to "yes"=1, "no"=0
dummy$place_home_own<-ifelse(dummy$place_home_own=="Yes",1,0)
dummy$place_home_else<-ifelse(dummy$place_home_else=="Yes",1,0)
dummy$place_work<-ifelse(dummy$place_work=="Yes",1,0)
dummy$place_worship<-ifelse(dummy$place_worship=="Yes",1,0)
dummy$place_transport<-ifelse(dummy$place_transport=="Yes",1,0)
dummy$place_school<-ifelse(dummy$place_school=="Yes",1,0)
dummy$place_shop_essential<-ifelse(dummy$place_shop_essential=="Yes",1,0)
dummy$place_shop_non_essential<-ifelse(dummy$place_shop_non_essential=="Yes",1,0)
dummy$place_entertainment<-ifelse(dummy$place_entertainment=="Yes",1,0)
dummy$place_sport<-ifelse(dummy$place_sport=="Yes",1,0)
dummy$place_park<-ifelse(dummy$place_park=="Yes",1,0)
dummy$place_healthcare<-ifelse(dummy$place_healthcare=="Yes",1,0)
dummy$place_beauty<-ifelse(dummy$place_beauty=="Yes",1,0)
dummy$place_inside<-ifelse(dummy$place_inside=="Yes",1,0)
dummy$place_outside<-ifelse(dummy$place_outside=="Yes",1,0)


```



Age bezieht sich auf das Alter der arbeitenden Person, daher Personen <15 raus
```{r remove work contacts that are <15 years old dt_outside_all, results='hide', error = FALSE, warning=FALSE, message=FALSE}
                
dummy$place_work[(dummy$age_group_participant=="Under 1" | 
                    dummy$age_group_participant=="1-4" |
                    dummy$age_group_participant=="5-9" | 
                    dummy$age_group_participant=="10-14") & 
                   dummy$place_work==1 ] <- 0

```


```{r NA handling in place-variables dt_outside_all}

levels(dummy$place_home_own) #check for factors (example)
dummy$place_home_own[is.na(dummy$place_home_own)] <- 0 #change to 0

dummy$place_home_else[is.na(dummy$place_home_else)] <- 0 #change to 0

dummy$place_work[is.na(dummy$place_work)] <- 0 #change to 0

dummy$place_worship[is.na(dummy$place_worship)] <- 0 #change to 0

dummy$place_transport[is.na(dummy$place_transport)] <- 0 #change to 0

dummy$place_school[is.na(dummy$place_school)] <- 0 #change to 0

dummy$place_shop_essential[is.na(dummy$place_shop_essential)] <- 0 #change to 0

dummy$place_shop_non_essential[is.na(dummy$place_shop_non_essential)] <- 0 #change to 0

dummy$place_entertainment[is.na(dummy$place_entertainment)] <- 0 #change to 0

dummy$place_sport[is.na(dummy$place_sport)] <- 0 #change to 0

dummy$place_park[is.na(dummy$place_park)] <- 0 #change to 0

dummy$place_healthcare[is.na(dummy$place_healthcare)] <- 0 #change to 0

dummy$place_beauty[is.na(dummy$place_beauty)] <- 0 #change to 0

dummy$place_inside[is.na(dummy$place_inside)] <- 0 #change to 0

dummy$place_outside[is.na(dummy$place_outside)] <- 0 #change to 0

dt_outside_all <- dummy %>% select(-c(age_group_participant)) # new data frame and delete age_group participant

rm(dummy) #delete dummy and get the final "dt_outside"
```


```{r check levels of place-variables dt_hh_all transform place_oth and yes no answers, results='hide', error = FALSE, warning=FALSE, message=FALSE}

## create dummy (smaller dataset with dt_hh_all)
dummy <- d_self_all %>% select(new_id, wave, age_group)
dummynames<-c("new_id","wave", "age_group_participant")
colnames(dummy) <- dummynames
dummy <- merge(dummy, dt_hh_all, by=c("new_id", "wave"), all.y=T)

dummy$place_oth <- as.factor(dummy$place_oth) #change character to factor

dummy$place_oth[is.na(dummy$place_oth)] <- 0 #change to 0


dummy$place_home_own<-ifelse(dummy$place_home_own=="Yes",1,0)
dummy$place_home_else<-ifelse(dummy$place_home_else=="Yes",1,0)
dummy$place_work<-ifelse(dummy$place_work=="Yes",1,0)
dummy$place_worship<-ifelse(dummy$place_worship=="Yes",1,0)
dummy$place_transport<-ifelse(dummy$place_transport=="Yes",1,0)
dummy$place_school<-ifelse(dummy$place_school=="Yes",1,0)
dummy$place_shop_essential<-ifelse(dummy$place_shop_essential=="Yes",1,0)
dummy$place_shop_non_essential<-ifelse(dummy$place_shop_non_essential=="Yes",1,0)
dummy$place_entertainment<-ifelse(dummy$place_entertainment=="Yes",1,0)
dummy$place_sport<-ifelse(dummy$place_sport=="Yes",1,0)
dummy$place_park<-ifelse(dummy$place_park=="Yes",1,0)
dummy$place_healthcare<-ifelse(dummy$place_healthcare=="Yes",1,0)
dummy$place_beauty<-ifelse(dummy$place_beauty=="Yes",1,0)
dummy$place_inside<-ifelse(dummy$place_inside=="Yes",1,0)
dummy$place_outside<-ifelse(dummy$place_outside=="Yes",1,0)


```

Age bezieht sich auf das Alter der arbeitenden Person, daher Personen <15 raus
```{r remove work contacts that are <15 years old dt_hh_all, results='hide', error = FALSE, warning=FALSE, message=FALSE}

dummy$place_work[(dummy$age_group_participant=="Under 1" | 
                    dummy$age_group_participant=="1-4" |
                    dummy$age_group_participant=="5-9" | 
                    dummy$age_group_participant=="10-14") & 
                   dummy$place_work==1 ] <- 0
```


```{r NA handling in place-variables dt_hh_all}

dummy$place_home_own[is.na(dummy$place_home_own)] <- 0 #change to 0

dummy$place_home_else[is.na(dummy$place_home_else)] <- 0 #change to 0

dummy$place_work[is.na(dummy$place_work)] <- 0 #change to 0

dummy$place_worship[is.na(dummy$place_worship)] <- 0 #change to 0

dummy$place_transport[is.na(dummy$place_transport)] <- 0 #change to 0

dummy$place_school[is.na(dummy$place_school)] <- 0 #change to 0

dummy$place_shop_essential[is.na(dummy$place_shop_essential)] <- 0 #change to 0

dummy$place_shop_non_essential[is.na(dummy$place_shop_non_essential)] <- 0 #change to 0

dummy$place_entertainment[is.na(dummy$place_entertainment)] <- 0 #change to 0

dummy$place_sport[is.na(dummy$place_sport)] <- 0 #change to 0

dummy$place_park[is.na(dummy$place_park)] <- 0 #change to 0

dummy$place_healthcare[is.na(dummy$place_healthcare)] <- 0 #change to 0

dummy$place_beauty[is.na(dummy$place_beauty)] <- 0 #change to 0

dummy$place_inside[is.na(dummy$place_inside)] <- 0 #change to 0

dummy$place_outside[is.na(dummy$place_outside)] <- 0 #change to 0

dt_hh_all <- dummy %>% select(-c(age_group_participant)) # new data frame and delete age_group participant

rm(dummy) #delete dummy and get final "dt_hh"
```

This chunk deals with the problem of different hh_p_inc variables. 
```{r merge households}
# Haushaltsgrößen
d_self_all <- d_self_all %>% mutate(hh_p_incl_0 = ifelse(wave > 0, (hh_p_incl_0+1), hh_p_incl_0))

#reorder
d_self_all <- d_self_all %>% 
relocate(hh_p_incl_new_0, .after = hh_p_incl_0)

#merge #hh_p_incl_0 & #hh_p_incl_new_0 at position with NAs
d_self_all <- d_self_all %>% 
  mutate(hh_p_incl_0 = replace(hh_p_incl_0, is.na(hh_p_incl_0), hh_p_incl_new_0[2985:5679]))

#drop variable since we do not need anymore
d_self_all$hh_p_incl_new_0 <- NULL

d_self_all$age_group_new <- d_self_all$age_group
```
  
                     
```{r count multiple contacts}
# with respect to all places

dt_hh_all$place_multiple <- rowSums(dt_hh_all[, setdiff(places, "place_oth")],
                                    na.rm = TRUE)
table(dt_hh_all$place_multiple)
dt_hh_all$place_multiple <- as.integer(dt_hh_all$place_multiple >= 2) # sum of at least 2 places

dt_outside_all$place_multiple <- rowSums(dt_outside_all[, setdiff(places, "place_oth")],
                                         na.rm = TRUE)
table(dt_outside_all$place_multiple)
dt_outside_all$place_multiple <- as.integer(dt_outside_all$place_multiple >= 2)
# there's multiple contacts 

```


# Berechnung der Kontakte 

```{r Funktion zur Berechnung der Haushaltskontakte}

hh_contact_number <- function(x, new_id, wave) {
  dummy_hh <- data.table(x)
  dummy_hh <- dummy_hh %>% 
    filter(dummy_hh$hh_met_this_day == "Yes") # only hh member actually met!!!
  if(sum(is.na(dummy_hh$new_id)) > 0) { message("new_id has NAs!") }
  dummy_hh <- dummy_hh %>% 
    group_by(new_id, wave) %>% 
    dplyr::summarise(hh_contact_n = n())
  return(dummy_hh)
}


```

```{r Funktion zur Berechnung der outside Kontakte, results='hide', error = FALSE, warning=FALSE, message=FALSE}

non_hh_contact_number <- function(x, new_id, wave) {
  dummy_outside <- data.table(x)
  if(sum(is.na(dummy_outside$new_id)) > 0) { message("new_id has NAs!") }
  dummy_outside <- dummy_outside %>% 
    group_by(new_id, wave) %>% 
    dplyr::summarise(non_hh_contact_n = n())
  
  dummy_work <- data.table(x)
  dummy_work <- dummy_work %>% 
    filter(place_work == 1)
  dummy_work <- dummy_work %>% 
    group_by(new_id, wave) %>% 
    dplyr::summarise(work_contact_n = n())
  dummy_all <- merge(dummy_outside, dummy_work, by = c("new_id", "wave"), all = T)
  rm(dummy_outside, dummy_work)
  
  dummy_homeown <- data.table(x)
  dummy_homeown <- dummy_homeown %>% 
    filter(place_home_own == 1)
  dummy_homeown <- dummy_homeown %>% 
    group_by(new_id, wave) %>% 
    dplyr::summarise(home_own_contact_n=n())
  dummy_all <- merge(dummy_all, dummy_homeown, by = c("new_id", "wave"), all = T)
  rm(dummy_homeown)
  
  dummy_homeelse <- data.table(x)
  dummy_homeelse <- dummy_homeelse %>% 
    filter(place_home_else == 1)
  dummy_homeelse <- dummy_homeelse %>% 
    group_by(new_id, wave) %>% 
    dplyr::summarise(home_else_contact_n = n())
  dummy_all <- merge(dummy_all, dummy_homeelse, by = c("new_id", "wave"), all = T)
  rm(dummy_homeelse)
  
  dummy_worship <- data.table(x)
  dummy_worship <- dummy_worship %>% 
    filter(place_worship==1)
  dummy_worship <- dummy_worship %>% 
    group_by(new_id, wave) %>% 
    dplyr::summarise(worship_contact_n=n())
  dummy_all <- merge(dummy_all, dummy_worship, by = c("new_id", "wave"), all = T)
  rm(dummy_worship)
  
  dummy_transport <- data.table(x)
  dummy_transport <- dummy_transport %>% 
    filter(place_transport==1)
  dummy_transport <- dummy_transport %>% 
    group_by(new_id, wave) %>% 
    dplyr::summarise(transport_contact_n=n())
  dummy_all <- merge(dummy_all, dummy_transport, by = c("new_id", "wave"), all = T)
  rm(dummy_transport)
  
  dummy_school <- data.table(x)
  dummy_school <- dummy_school %>% 
    filter(place_school==1)
  dummy_school <- dummy_school %>% 
    group_by(new_id, wave) %>% 
    dplyr::summarise(school_contact_n=n())
  dummy_all <- merge(dummy_all, dummy_school, by = c("new_id", "wave"), all = T)
  rm(dummy_school)
  
  dummy_shop_essential <- data.table(x)
  dummy_shop_essential <- dummy_shop_essential %>% 
    filter(place_shop_essential==1)
  dummy_shop_essential <- dummy_shop_essential %>% 
    group_by(new_id, wave) %>% 
    dplyr::summarise(shop_essential_contact_n=n())
  dummy_all <- merge(dummy_all, dummy_shop_essential, by = c("new_id", "wave"), all = T)
  rm(dummy_shop_essential)
  
  dummy_shop_non_essential <- data.table(x)
  dummy_shop_non_essential <- dummy_shop_non_essential %>% 
    filter(place_shop_non_essential==1)
  dummy_shop_non_essential <- dummy_shop_non_essential %>% 
    group_by(new_id, wave) %>% 
    dplyr::summarise(shop_non_essential_contact_n=n())
  dummy_all <- merge(dummy_all, dummy_shop_non_essential, by = c("new_id", "wave"), all = T)
  rm(dummy_shop_non_essential)
  
  dummy_entertainment <- data.table(x)
  dummy_entertainment <- dummy_entertainment %>% 
    filter(place_entertainment==1)
  dummy_entertainment <- dummy_entertainment %>% 
    group_by(new_id, wave) %>% 
    dplyr::summarise(entertainment_contact_n=n())
  dummy_all <- merge(dummy_all, dummy_entertainment, by = c("new_id", "wave"), all = T)
  rm(dummy_entertainment)
  
  dummy_sport <- data.table(x)
  dummy_sport <- dummy_sport %>% 
    filter(place_sport==1)
  dummy_sport <- dummy_sport %>% 
    group_by(new_id, wave) %>% 
    dplyr::summarise(sport_contact_n=n())
  dummy_all <- merge(dummy_all, dummy_sport, by = c("new_id", "wave"), all = T)
  rm(dummy_sport)
  
  dummy_park <- data.table(x)
  dummy_park <- dummy_park %>% 
    filter(place_park==1) 
  dummy_park <- dummy_park %>% 
    group_by(new_id, wave) %>% 
    dplyr::summarise(park_contact_n=n())
  dummy_all <- merge(dummy_all, dummy_park, by = c("new_id", "wave"), all = T)
  rm(dummy_park)
  
  dummy_healthcare <- data.table(x)
  dummy_healthcare <- dummy_healthcare %>% 
    filter(place_healthcare==1) 
  dummy_healthcare <- dummy_healthcare %>% 
    group_by(new_id, wave) %>% 
    dplyr::summarise(healthcare_contact_n=n())
  dummy_all <- merge(dummy_all, dummy_healthcare, by = c("new_id", "wave"), all = T)
  rm(dummy_healthcare)
  
  dummy_beauty <- data.table(x)
  dummy_beauty <- dummy_beauty %>% 
    filter(place_beauty==1) 
  dummy_beauty <- dummy_beauty %>% 
    group_by(new_id, wave) %>% 
    dplyr::summarise(beauty_contact_n=n())
  dummy_all <- merge(dummy_all, dummy_beauty, by = c("new_id", "wave"), all = T)
  rm(dummy_beauty)
  
  dummy_multiple <- data.table(x)
  dummy_multiple <- dummy_multiple %>% 
    filter(place_multiple==1) 
  dummy_multiple <- dummy_multiple %>% 
    group_by(new_id, wave) %>% 
    dplyr::summarise(multiple_contact_n=n())
  dummy_all <- merge(dummy_all, dummy_multiple, by = c("new_id", "wave"), all = T)
  rm(dummy_multiple)
  
  dummy_other <- data.table(x)
  dummy_other <- dummy_other %>%
    filter(!is.na(place_oth))
  dummy_other <- dummy_other %>%
    group_by(new_id, wave) %>%
    dplyr::summarise(oth_contact_n=n())
  dummy_all <- merge(dummy_all, dummy_other, by = c("new_id", "wave"), all = T)
  rm(dummy_other)
  
  
  return(dummy_all)
}
```

The following chunk summarizes the number of contacts via ID and wave.

```{r calculate all hh contacts and outside contacts }
       
# All contacts (inside and outside)

hh_contacts <- hh_contact_number(dt_hh_all, new_id, wave) 
### er zählt das Vorkommen einer ID in dt_hh_all
#Wert stimmt mit hh_p_incl_0 in d_self_all ein
outside_contacts <- non_hh_contact_number(dt_outside_all, new_id, wave) 
#hier zählt er die Ids bei den Outside-Kontakten
                #Achtung: Place-Kontakte können häufiger vorkommen als Kontakte! Ich kann eine 
                            #Person an verschiedene Orten treffen

nrow(d_self_all)
nrow(hh_contacts)      # must be <= nrow(d_self_all)
nrow(outside_contacts) # must be <= nrow(d_self_all)

d_all_wide <- merge(d_self_all, hh_contacts, by=c("new_id", "wave"), all=T)
if(nrow(d_all_wide) != nrow(d_self_all)) { message("More rows than expected!") }
d_all_wide <- merge(d_all_wide, outside_contacts, by=c("new_id", "wave"), all=T)
if(nrow(d_all_wide) != nrow(d_self_all)) { message("More rows than expected!") }


# Inside contacts
aux_hh <- dt_hh_all %>% filter(place_inside==1) 
hh_contacts <- hh_contact_number(aux_hh, new_id, wave)

aux_outside <- dt_outside_all %>% filter(place_inside==1)
outside_contacts <- non_hh_contact_number(aux_outside, new_id, wave)

nrow(d_self_all)
nrow(hh_contacts)      # must be <= nrow(d_self_all)
nrow(outside_contacts) # must be <= nrow(d_self_all)


# rename
names(hh_contacts) <- sub("_contact_n", "_contact_n_inside", names(hh_contacts))
names(outside_contacts) <- sub("_contact_n", "_contact_n_inside", names(outside_contacts))


# merge to one dataset
d_all_wide <- merge(d_all_wide, hh_contacts, by=c("new_id", "wave"), all=T)
if(nrow(d_all_wide) != nrow(d_self_all)) { message("More rows than expected!") }
d_all_wide <- merge(d_all_wide, outside_contacts, by=c("new_id", "wave"), all=T)
if(nrow(d_all_wide) != nrow(d_self_all)) { message("More rows than expected!") }

names(d_all_wide)

# Outside contacts
aux_hh <- dt_hh_all %>% filter(place_outside==1)
hh_contacts <- hh_contact_number(aux_hh, new_id, wave)

aux_outside <- dt_outside_all %>% filter(place_outside==1)
outside_contacts <- non_hh_contact_number(aux_outside, new_id, wave)

nrow(d_self_all)
nrow(hh_contacts)      # must be <= nrow(d_self_all)
nrow(outside_contacts) # must be <= nrow(d_self_all)


# rename
names(hh_contacts) <- sub("_contact_n", "_contact_n_outside", names(hh_contacts))
names(outside_contacts) <- sub("_contact_n", "_contact_n_outside", names(outside_contacts))

d_all_wide <- merge(d_all_wide, hh_contacts, by=c("new_id", "wave"), all=T)
if(nrow(d_all_wide) != nrow(d_self_all)) { message("More rows than expected!") }
d_all_wide <- merge(d_all_wide, outside_contacts, by=c("new_id", "wave"), all=T)
if(nrow(d_all_wide) != nrow(d_self_all)) { message("More rows than expected!") }


```

```{r convert all NA to zero for later calculations}

for(j in 19:ncol(d_all_wide)) { #für bestimmte Variablen
  d_all_wide[[j]][is.na(d_all_wide[[j]])] = 0
}

```



## Berechnung all contacts

```{r Berechnung all contacts}
# Achtung: Place-Kontakte können häufiger vorkommen als Kontakte! Ich kann eine 
# Person an verschiedene Orten treffen
# Achtung: Ich kann eine Person sowohl inside als auch outside treffen (Park, Home)
d_all_wide$all_contact_n <- d_all_wide$hh_contact_n + d_all_wide$non_hh_contact_n
d_all_wide$all_contact_n_inside <- d_all_wide$hh_contact_n_inside + d_all_wide$non_hh_contact_n_inside
d_all_wide$all_contact_n_outside <- d_all_wide$hh_contact_n_outside + d_all_wide$non_hh_contact_n_outside

d_all_wide$all_contact_n[is.na(d_all_wide$all_contact_n)] <- 0 #change to 0 for NA
d_all_wide$all_contact_n_inside[is.na(d_all_wide$all_contact_n_inside)] <- 0 #change to 0 for NA
d_all_wide$all_contact_n_outside[is.na(d_all_wide$all_contact_n_outside)] <- 0 #change to 0 for NA

```


## Berechnung Gruppenkontakte

```{r Berechnung Gruppenkontakte}
###Q79 
###Work Contacts
d_all_wide$Q79a_work_sum <- rowSums(d_all_wide[, c("Q79a_age_met_work_u18",
                                                   "Q79a_age_met_work_1864",
                                                   "Q79a_age_met_work_o64")],
                                    na.rm = TRUE)
d_all_wide$Q79a_work_sum[d_all_wide$age_group=="Under 1" | 
                           d_all_wide$age_group=="1-4" |
                           d_all_wide$age_group=="5-9" |
                           d_all_wide$age_group=="10-14"] <- 0

## add the work Q contacts to the overall work contacts
d_all_wide$work_contact_n_Q79a <- d_all_wide$work_contact_n + d_all_wide$Q79a_work_sum

###Q80 ###School Contacts
d_all_wide$Q80a_school_sum <- rowSums(d_all_wide[, c("Q80a_age_met_uni_u18",
                                                     "Q80a_age_met_uni_1864",
                                                     "Q80a_age_met_uni_o64")],
                                      na.rm = TRUE)
# add the Q school contacts to the other school contacts
d_all_wide$school_contact_n_Q80a <- d_all_wide$school_contact_n + d_all_wide$Q80a_school_sum

###Q81 ###Else Contacts
d_all_wide$Q81a_else_sum <- rowSums(d_all_wide[, c("Q81a_age_met_else_u18",
                                                   "Q81a_age_met_else_1864",
                                                   "Q81a_age_met_else_o64")],
                                    na.rm = TRUE)

```



## Berechnung alle non-hh-contacts

```{r non-hh-contacts}
# also add to all non_hh_
d_all_wide$non_hh_contact_n_Q79 <- d_all_wide$non_hh_contact_n + 
  d_all_wide$Q79a_work_sum + 
  d_all_wide$Q80a_school_sum + 
  d_all_wide$Q81a_else_sum

```

## Berechnung all-contacts
```{r all-contact}
# also add to all_
d_all_wide$all_contact_n_Q79 <- d_all_wide$all_contact_n + 
  d_all_wide$Q79a_work_sum + 
  d_all_wide$Q80a_school_sum + 
  d_all_wide$Q81a_else_sum
```

# Descriptives - Tabel one 

```{r Table one Vorbereitung}
d_all_wide$age_group[is.na(d_all_wide$age_group)] <- "5-9" #17426a14 #wave 6-8
d_self_all$age_group[is.na(d_self_all$age_group)] <- "5-9" #17426a14 #wave 6-8

#Table 1
# label (für Altersgruppen)
d_all_wide$age_group[d_all_wide$age_group=="85 years or older"] <- "85+" 
d_all_wide$age_group[which(d_all_wide$age_group == "Under 1")] <- "<1" 

#new classification preperation
d_all_wide$age_group_tab <- d_all_wide$age_group
d_all_wide$age_group_tab_2 <- d_all_wide$age_group

label(d_all_wide$gender_0) <- "Sex of Participants"
label(d_all_wide$qmktsize_16_0) <- "Region of Participants"
d_all_wide$wave_two <- d_all_wide$wave
d_all_wide$wave_two <- 
  factor(d_all_wide$wave_two,
         labels=c("3/3 - 10/3", # Reference
                  "31/3 - 7/4", 
                  "21/4 - 28/4",
                  "12/5 - 19/5",
                  "2/6 - 9/6",
                  "23/6 - 30/6",
                  "14/7 - 21/7",
                  "4/8 -11/8"))
label(d_all_wide$wave_two) <- "Waves"
d_all_wide$hh_p_incl_0_2 <- d_all_wide$hh_p_incl_0 # Zusammenfassung der hh size
d_all_wide$hh_p_incl_0_2[d_all_wide$hh_p_incl_0_2==1] <- "1"
d_all_wide$hh_p_incl_0_2[d_all_wide$hh_p_incl_0_2==2] <- "2"
d_all_wide$hh_p_incl_0_2[d_all_wide$hh_p_incl_0_2==3] <- "3"
d_all_wide$hh_p_incl_0_2[d_all_wide$hh_p_incl_0_2>=4] <- "4+" # Änderung ggü. Originaldaten
d_all_wide$hh_p_incl_0_2[d_all_wide$hh_p_incl_0_2==10] <- "4+" # Änderung ggü. Originaldaten
d_all_wide$hh_p_incl_0_2[d_all_wide$hh_p_incl_0_2==11] <- "4+" # Änderung ggü. Originaldaten
d_all_wide$hh_p_incl_0_2[d_all_wide$hh_p_incl_0_2==12] <- "4+" # Änderung ggü. Originaldaten
d_all_wide$hh_p_incl_0_2 <- factor(d_all_wide$hh_p_incl_0_2, levels=c("1","2","3","4+"))
label(d_all_wide$hh_p_incl_0_2)     <- "Household Size"

d_all_wide$age_group <- factor(d_all_wide$age_group, levels=c("<1","1-4","5-9","10-14","15-19","20-24",
                                                              "25-34","35-44","45-54","55-64",
                                                              "65-69","70-74","75-79","80-84","85+"))
label(d_all_wide$age_group)     <- "Age Group"


#Another classification
d_all_wide$age_group_tab[d_all_wide$age_group_tab == "<1" | d_all_wide$age_group_tab == "1-4" |
                           d_all_wide$age_group_tab == "5-9"|  d_all_wide$age_group_tab == "10-14" |
                           d_all_wide$age_group_tab == "15-19"] <- "0-19"

d_all_wide$age_group_tab[d_all_wide$age_group_tab == "20-24" | d_all_wide$age_group_tab == "25-34" |
                           d_all_wide$age_group_tab == "35-44"|  d_all_wide$age_group_tab == "45-54" |
                           d_all_wide$age_group_tab == "55-64"] <- "20-64"

d_all_wide$age_group_tab[d_all_wide$age_group_tab == "65-69" | d_all_wide$age_group_tab == "70-74"] <- "65-74"

d_all_wide$age_group_tab[d_all_wide$age_group_tab == "75-79" | d_all_wide$age_group_tab == "80-84" |d_all_wide$age_group_tab == "85+"] <- "75+"

d_all_wide$age_group_tab <- factor(d_all_wide$age_group_tab, levels=c("0-19","20-64","65-74","75+", 
                                                                      "Don't know", "Prefer not to answer"))
label(d_all_wide$age_group_tab)     <- "Age Group"




#Another classification
d_all_wide$age_group_tab_2[d_all_wide$age_group_tab_2 == "<1" | d_all_wide$age_group_tab_2 == "1-4" |
                             d_all_wide$age_group_tab_2 == "5-9" | d_all_wide$age_group_tab_2 == "10-14"] <- "0-14"
d_all_wide$age_group_tab_2[d_all_wide$age_group_tab_2 == "65-69" | d_all_wide$age_group_tab_2 == "70-74"] <- "65-74"
d_all_wide$age_group_tab_2[d_all_wide$age_group_tab_2== "75-79" | d_all_wide$age_group_tab_2 == "80-84" |
                             d_all_wide$age_group_tab_2 == "85+"] <- "75+"

d_all_wide$age_group_tab_2 <- factor(d_all_wide$age_group_tab_2, levels=c("0-14","15-19","20-24","25-34","35-44", 
                                                                          "45-54", "55-64", "65-74", "75+", "Don't know", 
                                                                          "Prefer not to answer"))
label(d_all_wide$age_group_tab_2)     <- "Age Group"


# diese Funktion verändert die Darstellung in der Tabelle " Freq (Prozent in Klammern)"
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.0f%%)", FREQ, PCTnoNA))))
}


#table shows number of households per wave and overall
table1::table1(~age_group + gender_0 + hh_p_incl_0_2 + qmktsize_16_0 |wave_two, data=d_all_wide,
               render.categorical=my.render.cat, overall = NULL, caption = "<h3><b>All Bundesländer</b></h3>")

#table shows number of households per wave and overall
table1::table1(~age_group_tab + gender_0 + hh_p_incl_0_2 + qmktsize_16_0 |wave_two, data=d_all_wide,
               render.categorical=my.render.cat, overall = NULL, caption = "<h3><b>All Bundesländer</b></h3>")

#table shows number of households per wave and overall
table1 <- table1::table1(~age_group_tab_2 + gender_0 + hh_p_incl_0_2 + qmktsize_16_0 |wave_two, data=d_all_wide,
               render.categorical=my.render.cat, overall = NULL, caption = "<h3><b>All Bundesländer</b></h3>")

#Area urban-rural table
table1::table1(~age_group_tab_2 + gender_0 + hh_p_incl_0_2 + wave_two|qmktsize_16_0, data=d_all_wide,
               render.categorical=my.render.cat, overall = NULL, caption = "<h4><b>Table 1 - Overview urban-rural areas</b></h4>")


table1::table1(~age_group_tab_2 + gender_0 + hh_p_incl_0_2 + wave_two|qmktsize_16_0, data=d_all_wide,
                         render.categorical=my.render.cat, overall = NULL, caption = "<h4><b>Table 1 - Overview urban-rural areas</b></h4>")

#library('xtable')
#print(xtable(as.matrix(as.data.frame((table1))), type = "latex", file = "filename2.tex", sizebox =0.7))



#nach Bundesland
table_brandenburg <- d_all_wide %>% 
  filter(bundesland_0 == "Brandenburg")
table_berlin <- d_all_wide %>% 
  filter(bundesland_0 == "Berlin")
table_badenw <- d_all_wide %>% 
  filter(bundesland_0 == "Baden-Württemberg")
table_bayern <- d_all_wide %>% 
  filter(bundesland_0 == "Bayern")
#Brandenburg tab 2
table1::table1(~age_group_tab_2 + +gender_0 + hh_p_incl_0_2 + qmktsize_16_0  |wave_two, data=table_brandenburg, 
               render.categorical=my.render.cat, overall = NULL, caption = "<h3><b>Brandenburg</b></h3>")

#Brandenburg tab
table1::table1(~age_group_tab + +gender_0 + hh_p_incl_0_2 + qmktsize_16_0  |wave_two, data=table_brandenburg, 
               render.categorical=my.render.cat, overall = NULL, caption = "<h3><b>Brandenburg</b></h3>")

#Berlin tab 2
table1::table1(~age_group_tab_2 + +gender_0 + hh_p_incl_0_2 + qmktsize_16_0  |wave_two, data=table_berlin, 
               render.categorical=my.render.cat, overall = NULL, caption = "<h3><b>Berlin</b></h3>")

#Berlin tab
table1::table1(~age_group_tab + +gender_0 + hh_p_incl_0_2 + qmktsize_16_0 |wave_two, data=table_berlin, 
               render.categorical=my.render.cat, overall = NULL, caption = "<h3><b>Berlin</b></h3>")


#Baden-Württemberg tab2
table1::table1(~age_group_tab_2 + +gender_0 + hh_p_incl_0_2 + qmktsize_16_0  |wave_two, data=table_badenw, 
               render.categorical=my.render.cat, overall = NULL, caption = "<h3><b>Baden-Württemberg</b></h3>")

#Baden-Württemberg tab
table1::table1(~age_group_tab + +gender_0 + hh_p_incl_0_2 + qmktsize_16_0 |wave_two, data=table_badenw, 
               render.categorical=my.render.cat, overall = NULL, caption = "<h3><b>Baden-Württemberg</b></h3>")
#Bayern tab2
table1::table1(~age_group_tab_2 + +gender_0 + hh_p_incl_0_2 + qmktsize_16_0 |wave_two, data=table_bayern, 
               render.categorical=my.render.cat, overall = NULL, caption = "<h3><b>Bayern</b></h3>")

#Bayern tab
table1::table1(~age_group_tab + +gender_0 + hh_p_incl_0_2 + qmktsize_16_0 |wave_two, data=table_bayern, 
               render.categorical=my.render.cat, overall = NULL, caption = "<h3><b>Bayern</b></h3>")




```


```{r Table One nach Bundesländern}

#nach Bundesland
table_brandenburg <- d_all_wide %>% 
  filter(bundesland_0 == "Brandenburg")
table_berlin <- d_all_wide %>% 
  filter(bundesland_0 == "Berlin")
table_badenw <- d_all_wide %>% 
  filter(bundesland_0 == "Baden-Württemberg")
table_bayern <- d_all_wide %>% 
  filter(bundesland_0 == "Bayern")
#Brandenburg tab 2
table1::table1(~age_group_tab_2 + +gender_0 + hh_p_incl_0_2 + qmktsize_16_0  |wave_two, data=table_brandenburg, 
               render.categorical=my.render.cat, overall = NULL, caption = "<h3><b>Brandenburg</b></h3>")

#Brandenburg tab
table1::table1(~age_group_tab + +gender_0 + hh_p_incl_0_2 + qmktsize_16_0  |wave_two, data=table_brandenburg, 
               render.categorical=my.render.cat, overall = NULL, caption = "<h3><b>Brandenburg</b></h3>")

#Berlin tab 2
table1::table1(~age_group_tab_2 + +gender_0 + hh_p_incl_0_2 + qmktsize_16_0  |wave_two, data=table_berlin, 
               render.categorical=my.render.cat, overall = NULL, caption = "<h3><b>Berlin</b></h3>")

#Berlin tab
table1::table1(~age_group_tab + +gender_0 + hh_p_incl_0_2 + qmktsize_16_0 |wave_two, data=table_berlin, 
               render.categorical=my.render.cat, overall = NULL, caption = "<h3><b>Berlin</b></h3>")


#Baden-Württemberg tab2
table1::table1(~age_group_tab_2 + +gender_0 + hh_p_incl_0_2 + qmktsize_16_0  |wave_two, data=table_badenw, 
               render.categorical=my.render.cat, overall = NULL, caption = "<h3><b>Baden-Württemberg</b></h3>")

#Baden-Württemberg tab
table1::table1(~age_group_tab + +gender_0 + hh_p_incl_0_2 + qmktsize_16_0 |wave_two, data=table_badenw, 
               render.categorical=my.render.cat, overall = NULL, caption = "<h3><b>Baden-Württemberg</b></h3>")
#Bayern tab2
table1::table1(~age_group_tab_2 + +gender_0 + hh_p_incl_0_2 + qmktsize_16_0 |wave_two, data=table_bayern, 
               render.categorical=my.render.cat, overall = NULL, caption = "<h3><b>Bayern</b></h3>")

#Bayern tab
table1::table1(~age_group_tab + +gender_0 + hh_p_incl_0_2 + qmktsize_16_0 |wave_two, data=table_bayern, 
               render.categorical=my.render.cat, overall = NULL, caption = "<h3><b>Bayern</b></h3>")



```



```{r Descriptives of (all) contacts/Table two}

d_all_wide$pseudo_Q79a_age_met_work_u18 <- d_all_wide$Q79a_age_met_work_u18
d_all_wide$pseudo_Q79a_age_met_work_1864 <- d_all_wide$Q79a_age_met_work_1864
d_all_wide$pseudo_Q79a_age_met_work_o64 <- d_all_wide$Q79a_age_met_work_o64

d_all_wide$pseudo_Q80a_age_met_uni_u18 <- d_all_wide$Q80a_age_met_uni_u18
d_all_wide$pseudo_Q80a_age_met_uni_1864 <- d_all_wide$Q80a_age_met_uni_1864
d_all_wide$pseudo_Q80a_age_met_uni_o64 <- d_all_wide$Q80a_age_met_uni_o64

d_all_wide$pseudo_Q81a_age_met_else_u18 <- d_all_wide$Q81a_age_met_else_u18
d_all_wide$pseudo_Q81a_age_met_else_1864 <- d_all_wide$Q81a_age_met_else_1864
d_all_wide$pseudo_Q81a_age_met_else_o64 <- d_all_wide$Q81a_age_met_else_o64

set.seed(12345678)
#pseudo cut for Table 2 at n=50 - need additional variable for showing all information
d_all_wide$pseudo_Q79a_age_met_work_u18[d_all_wide$pseudo_Q79a_age_met_work_u18>50] <- 50
d_all_wide$pseudo_Q79a_age_met_work_1864[d_all_wide$pseudo_Q79a_age_met_work_1864>50] <- 50
d_all_wide$pseudo_Q79a_age_met_work_o64[d_all_wide$pseudo_Q79a_age_met_work_o64>50] <- 50

d_all_wide$pseudo_Q80a_age_met_uni_u18[d_all_wide$pseudo_Q80a_age_met_uni_u18>50] <- 50
d_all_wide$pseudo_Q80a_age_met_uni_1864[d_all_wide$pseudo_Q80a_age_met_uni_1864>50] <- 50
d_all_wide$pseudo_Q80a_age_met_uni_o64[d_all_wide$pseudo_Q80a_age_met_uni_o64>50] <- 50

d_all_wide$pseudo_Q81a_age_met_else_u18[d_all_wide$pseudo_Q81a_age_met_else_u18>50] <- 50
d_all_wide$pseudo_Q81a_age_met_else_1864[d_all_wide$pseudo_Q81a_age_met_else_1864>50] <- 50
d_all_wide$pseudo_Q81a_age_met_else_o64[d_all_wide$pseudo_Q81a_age_met_else_o64>50] <- 50

###Pseudo Work Contacts
d_all_wide$pseudo_Q79a_work_sum <- rowSums(d_all_wide[, c("pseudo_Q79a_age_met_work_u18",
                                                   "pseudo_Q79a_age_met_work_1864",
                                                   "pseudo_Q79a_age_met_work_o64")],
                                    na.rm = TRUE)

## add the Pseudo work contacts to the overall work contacts
d_all_wide$pseudo_work_contact_n_Q79a <- d_all_wide$work_contact_n + d_all_wide$pseudo_Q79a_work_sum

### Pseudo school Contacts
d_all_wide$pseudo_Q80a_school_sum <- rowSums(d_all_wide[, c("pseudo_Q80a_age_met_uni_u18",
                                                     "pseudo_Q80a_age_met_uni_1864",
                                                     "pseudo_Q80a_age_met_uni_o64")],
                                      na.rm = TRUE)
# add the Pseudo school contacts to the other school contacts
d_all_wide$pseudo_school_contact_n_Q80a <- d_all_wide$school_contact_n + d_all_wide$pseudo_Q80a_school_sum

### Pseudo Else Contacts
d_all_wide$pseudo_Q81a_else_sum <- rowSums(d_all_wide[, c("pseudo_Q81a_age_met_else_u18",
                                                   "pseudo_Q81a_age_met_else_1864",
                                                   "pseudo_Q81a_age_met_else_o64")],
                                    na.rm = TRUE)

d_all_wide$pseudo_non_hh_contact_n_Q79 <- d_all_wide$non_hh_contact_n + 
  d_all_wide$pseudo_Q79a_work_sum + 
  d_all_wide$pseudo_Q80a_school_sum + 
  d_all_wide$pseudo_Q81a_else_sum

# also add to all_
d_all_wide$pseudo_all_contact_n_Q79 <- d_all_wide$all_contact_n + 
  d_all_wide$pseudo_Q79a_work_sum + 
  d_all_wide$pseudo_Q80a_school_sum + 
  d_all_wide$pseudo_Q81a_else_sum



# Median, min, max, quantile                             
# Median, min, max, quantile                             
my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=3, round.integers = F), 
       c("",
         "Mean (SD)"=sprintf("%s (%s)", MEAN, SD), 
         "Median (Min, Max)"=sprintf("%s [%s, %s]", MEDIAN, MIN, MAX),
         "Median (P25, P75)"=sprintf("%s [%s, %s]", MEDIAN, Q1, Q3)
       ))
}

label(d_all_wide$all_contact_n_Q79) <- "All contacts"
label(d_all_wide$hh_contact_n) <- "Household contacts"
label(d_all_wide$non_hh_contact_n_Q79) <- "Non household contacts"
label(d_all_wide$pseudo_all_contact_n_Q79) <- "All contacts truncated at 50" #pseudo
label(d_all_wide$pseudo_non_hh_contact_n_Q79) <- "Non household contacts truncated at 50" #pseudo

table1::table1(~all_contact_n_Q79  + hh_contact_n + non_hh_contact_n_Q79 + pseudo_all_contact_n_Q79 +
               pseudo_non_hh_contact_n_Q79 |wave_two, data=d_all_wide,
               render.continuous=my.render.cont, 
               #extra.col.pos = c(1, 3),
               overall = NULL, caption = "<h3><b>Overview All Contacts</b></h3>")

table1::table1(~all_contact_n_Q79  + hh_contact_n + non_hh_contact_n_Q79 + pseudo_all_contact_n_Q79 +
                 pseudo_non_hh_contact_n_Q79 |qmktsize_16_0, data=d_all_wide,
               render.continuous=my.render.cont, 
               #extra.col.pos = c(1, 3),
               overall = NULL, caption = "<h4><b>Table 2 - Overview urban-rural number of contacts</b></h4>")



#library(xtable)
#print(xtable(as.matrix(as.data.frame((table2))), type = "latex", file = "filename2.tex", sizebox =0.7))
table2 <- table1::table1(~all_contact_n_Q79  + hh_contact_n + non_hh_contact_n_Q79 + pseudo_all_contact_n_Q79 +
                 pseudo_non_hh_contact_n_Q79 |wave_two + qmktsize_16_0, data=d_all_wide,
               render.continuous=my.render.cont, 
               #extra.col.pos = c(1, 3),
               overall = NULL, caption = "<h4><b>Table 2 - Overview urban-rural number of contacts</b></h4>")



```
# Statistical Analysis

```{r statistical analysis I preparing the data}
#wave 1
wave1 <- d_all_wide %>% 
  filter(wave == 1)

wave1_dens <- d_all_wide %>% 
  filter(wave == 1) %>% 
  filter(qmktsize_16_0 == "Densely populated area")

wave1_inter <- d_all_wide %>% 
  filter(wave == 1) %>% 
  filter(qmktsize_16_0 == "Intermediate density area")

wave1_thin <- d_all_wide %>% 
  filter(wave == 1) %>% 
  filter(qmktsize_16_0 == "Thinly populated area")

#wave 2
wave2 <- d_all_wide %>% 
  filter(wave == 2)

wave2_dens <- d_all_wide %>% 
  filter(wave == 2) %>% 
  filter(qmktsize_16_0 == "Densely populated area")

wave2_inter <- d_all_wide %>% 
  filter(wave == 2) %>% 
  filter(qmktsize_16_0 == "Intermediate density area")

wave2_thin <- d_all_wide %>% 
  filter(wave == 2) %>% 
  filter(qmktsize_16_0 == "Thinly populated area")

#wave3
wave3 <- d_all_wide %>% 
  filter(wave == 3)

wave3_dens <- d_all_wide %>% 
  filter(wave == 3) %>% 
  filter(qmktsize_16_0 == "Densely populated area")

wave3_inter <- d_all_wide %>% 
  filter(wave == 3) %>% 
  filter(qmktsize_16_0 == "Intermediate density area")

wave3_thin <- d_all_wide %>% 
  filter(wave == 3) %>% 
  filter(qmktsize_16_0 == "Thinly populated area")

#wave4
wave4 <- d_all_wide %>% 
  filter(wave == 4)

wave4_dens <- d_all_wide %>% 
  filter(wave == 4) %>% 
  filter(qmktsize_16_0 == "Densely populated area")

wave4_inter <- d_all_wide %>% 
  filter(wave == 4) %>% 
  filter(qmktsize_16_0 == "Intermediate density area")

wave4_thin <- d_all_wide %>% 
  filter(wave == 4) %>% 
  filter(qmktsize_16_0 == "Thinly populated area")

#wave5
wave5 <- d_all_wide %>% 
  filter(wave == 5)

wave5_dens <- d_all_wide %>% 
  filter(wave == 5) %>% 
  filter(qmktsize_16_0 == "Densely populated area")

wave5_inter <- d_all_wide %>% 
  filter(wave == 5) %>% 
  filter(qmktsize_16_0 == "Intermediate density area")

wave5_thin <- d_all_wide %>% 
  filter(wave == 5) %>% 
  filter(qmktsize_16_0 == "Thinly populated area")

#wave6
wave6 <- d_all_wide %>% 
  filter(wave == 6)

wave6_dens <- d_all_wide %>% 
  filter(wave == 6) %>% 
  filter(qmktsize_16_0 == "Densely populated area")

wave6_inter <- d_all_wide %>% 
  filter(wave == 6) %>% 
  filter(qmktsize_16_0 == "Intermediate density area")

wave6_thin <- d_all_wide %>% 
  filter(wave == 6) %>% 
  filter(qmktsize_16_0 == "Thinly populated area")

#wave7
wave7 <- d_all_wide %>% 
  filter(wave == 7)

wave7_dens <- d_all_wide %>% 
  filter(wave == 7) %>% 
  filter(qmktsize_16_0 == "Densely populated area")

wave7_inter <- d_all_wide %>% 
  filter(wave == 7) %>% 
  filter(qmktsize_16_0 == "Intermediate density area")

wave7_thin <- d_all_wide %>% 
  filter(wave == 7) %>% 
  filter(qmktsize_16_0 == "Thinly populated area")

# wave8

wave8 <- d_all_wide %>% 
  filter(wave == 8)

wave8_dens <- d_all_wide %>% 
  filter(wave == 8) %>% 
  filter(qmktsize_16_0 == "Densely populated area")

wave8_inter <- d_all_wide %>% 
  filter(wave == 8) %>% 
  filter(qmktsize_16_0 == "Intermediate density area")

wave8_thin <- d_all_wide %>% 
  filter(wave == 8) %>% 
  filter(qmktsize_16_0 == "Thinly populated area")

```

```{r Jarque-Bera Test for normality}
library('tseries')

#all contact
jarque.bera.test(wave1_dens$all_contact_n_Q79)
jarque.bera.test(wave1_inter$all_contact_n_Q79)
jarque.bera.test(wave1_thin$all_contact_n_Q79)

jarque.bera.test(wave2_dens$all_contact_n_Q79)
jarque.bera.test(wave2_inter$all_contact_n_Q79)
jarque.bera.test(wave2_thin$all_contact_n_Q79)

jarque.bera.test(wave8_dens$all_contact_n_Q79)
jarque.bera.test(wave8_inter$all_contact_n_Q79)
jarque.bera.test(wave8_thin$all_contact_n_Q79)


#non-hh contacts

jarque.bera.test(wave1_dens$non_hh_contact_n_Q79)
jarque.bera.test(wave1_inter$non_hh_contact_n_Q79)
jarque.bera.test(wave1_thin$non_hh_contact_n_Q79)

jarque.bera.test(wave2_dens$non_hh_contact_n_Q79)
jarque.bera.test(wave2_inter$non_hh_contact_n_Q79)
jarque.bera.test(wave2_thin$non_hh_contact_n_Q79)

jarque.bera.test(wave8_dens$non_hh_contact_n_Q79)
jarque.bera.test(wave8_inter$non_hh_contact_n_Q79)
jarque.bera.test(wave8_thin$non_hh_contact_n_Q79)

#hh contacts

jarque.bera.test(wave1_dens$hh_contact_n)
jarque.bera.test(wave1_inter$hh_contact_n)
jarque.bera.test(wave1_thin$hh_contact_n)

jarque.bera.test(wave2_dens$hh_contact_n)
jarque.bera.test(wave2_inter$hh_contact_n)
jarque.bera.test(wave2_thin$hh_contact_n)

jarque.bera.test(wave8_dens$hh_contact_n)
jarque.bera.test(wave8_inter$hh_contact_n)
jarque.bera.test(wave8_thin$hh_contact_n)

```

```{r kruskal test}
#non-parametric test

#all contacts
kruskal.test(all_contact_n_Q79 ~ qmktsize_16_0, data = wave1)
kruskal.test(all_contact_n_Q79 ~ qmktsize_16_0, data = wave2)
kruskal.test(all_contact_n_Q79 ~ qmktsize_16_0, data = wave3)
kruskal.test(all_contact_n_Q79 ~ qmktsize_16_0, data = wave4)
kruskal.test(all_contact_n_Q79 ~ qmktsize_16_0, data = wave5)
kruskal.test(all_contact_n_Q79 ~ qmktsize_16_0, data = wave6)
kruskal.test(all_contact_n_Q79 ~ qmktsize_16_0, data = wave7)
kruskal.test(all_contact_n_Q79 ~ qmktsize_16_0, data = wave8)


#non-household contacts
kruskal.test(non_hh_contact_n_Q79 ~ qmktsize_16_0, data = wave1)
kruskal.test(non_hh_contact_n_Q79 ~ qmktsize_16_0, data = wave2)
kruskal.test(non_hh_contact_n_Q79 ~ qmktsize_16_0, data = wave3)
kruskal.test(non_hh_contact_n_Q79 ~ qmktsize_16_0, data = wave4)
kruskal.test(non_hh_contact_n_Q79 ~ qmktsize_16_0, data = wave5)
kruskal.test(non_hh_contact_n_Q79 ~ qmktsize_16_0, data = wave6)
kruskal.test(non_hh_contact_n_Q79 ~ qmktsize_16_0, data = wave7)
kruskal.test(non_hh_contact_n_Q79 ~ qmktsize_16_0, data = wave8)


#household contacts
kruskal.test(hh_contact_n ~ qmktsize_16_0, data = wave1)
kruskal.test(hh_contact_n ~ qmktsize_16_0, data = wave2)
kruskal.test(hh_contact_n ~ qmktsize_16_0, data = wave3)
kruskal.test(hh_contact_n ~ qmktsize_16_0, data = wave4)
kruskal.test(hh_contact_n ~ qmktsize_16_0, data = wave5)
kruskal.test(hh_contact_n ~ qmktsize_16_0, data = wave6)
kruskal.test(hh_contact_n ~ qmktsize_16_0, data = wave7)
kruskal.test(hh_contact_n ~ qmktsize_16_0, data = wave8)

#all contacts truncated group contacts at 50
kruskal.test(pseudo_all_contact_n_Q79 ~ qmktsize_16_0, data = wave1)
kruskal.test(pseudo_all_contact_n_Q79 ~ qmktsize_16_0, data = wave2)
kruskal.test(pseudo_all_contact_n_Q79 ~ qmktsize_16_0, data = wave3)
kruskal.test(pseudo_all_contact_n_Q79 ~ qmktsize_16_0, data = wave4)
kruskal.test(pseudo_all_contact_n_Q79 ~ qmktsize_16_0, data = wave5)
kruskal.test(pseudo_all_contact_n_Q79 ~ qmktsize_16_0, data = wave6)
kruskal.test(pseudo_all_contact_n_Q79 ~ qmktsize_16_0, data = wave7)
kruskal.test(pseudo_all_contact_n_Q79 ~ qmktsize_16_0, data = wave8)



#non-household contacts truncated group contacts at 50
kruskal.test(pseudo_non_hh_contact_n_Q79 ~ qmktsize_16_0, data = wave1)
kruskal.test(pseudo_non_hh_contact_n_Q79 ~ qmktsize_16_0, data = wave2)
kruskal.test(pseudo_non_hh_contact_n_Q79 ~ qmktsize_16_0, data = wave3)
kruskal.test(pseudo_non_hh_contact_n_Q79 ~ qmktsize_16_0, data = wave4)
kruskal.test(pseudo_non_hh_contact_n_Q79 ~ qmktsize_16_0, data = wave5)
kruskal.test(pseudo_non_hh_contact_n_Q79 ~ qmktsize_16_0, data = wave6)
kruskal.test(pseudo_non_hh_contact_n_Q79 ~ qmktsize_16_0, data = wave7)
kruskal.test(pseudo_non_hh_contact_n_Q79 ~ qmktsize_16_0, data = wave8)

```

```{r dunn test}
library('dunn.test')

#all contacts
dunn.test(wave1$all_contact_n_Q79, g=wave1$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave2$all_contact_n_Q79, g=wave2$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave3$all_contact_n_Q79, g=wave3$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave4$all_contact_n_Q79, g=wave4$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave5$all_contact_n_Q79, g=wave5$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave6$all_contact_n_Q79, g=wave6$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave7$all_contact_n_Q79, g=wave7$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave8$all_contact_n_Q79, g=wave8$qmktsize_16_0,method="bonferroni", alpha=0.05)

#non-household contacts
dunn.test(wave1$non_hh_contact_n_Q79, g=wave1$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave2$non_hh_contact_n_Q79, g=wave2$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave3$non_hh_contact_n_Q79, g=wave3$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave4$non_hh_contact_n_Q79, g=wave4$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave5$non_hh_contact_n_Q79, g=wave5$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave6$non_hh_contact_n_Q79, g=wave6$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave7$non_hh_contact_n_Q79, g=wave7$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave8$non_hh_contact_n_Q79, g=wave8$qmktsize_16_0,method="bonferroni", alpha=0.05)

#household contacts
dunn.test(wave1$hh_contact_n, g=wave1$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave2$hh_contact_n, g=wave2$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave3$hh_contact_n, g=wave3$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave4$hh_contact_n, g=wave4$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave5$hh_contact_n, g=wave5$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave6$hh_contact_n, g=wave6$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave7$hh_contact_n, g=wave7$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave8$hh_contact_n, g=wave8$qmktsize_16_0,method="bonferroni", alpha=0.05)

#all contacts truncated group contacts at 50
dunn.test(wave1$pseudo_all_contact_n_Q79, g=wave1$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave2$pseudo_all_contact_n_Q79, g=wave2$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave3$pseudo_all_contact_n_Q79, g=wave3$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave4$pseudo_all_contact_n_Q79, g=wave4$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave5$pseudo_all_contact_n_Q79, g=wave5$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave6$pseudo_all_contact_n_Q79, g=wave6$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave7$pseudo_all_contact_n_Q79, g=wave7$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave8$pseudo_all_contact_n_Q79, g=wave8$qmktsize_16_0,method="bonferroni", alpha=0.05)

#non-household contacts truncated group contacts at 50
dunn.test(wave1$pseudo_non_hh_contact_n_Q79, g=wave1$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave2$pseudo_non_hh_contact_n_Q79, g=wave2$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave3$pseudo_non_hh_contact_n_Q79, g=wave3$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave4$pseudo_non_hh_contact_n_Q79, g=wave4$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave5$pseudo_non_hh_contact_n_Q79, g=wave5$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave6$pseudo_non_hh_contact_n_Q79, g=wave6$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave7$pseudo_non_hh_contact_n_Q79, g=wave7$qmktsize_16_0,method="bonferroni", alpha=0.05)
dunn.test(wave8$pseudo_non_hh_contact_n_Q79, g=wave8$qmktsize_16_0,method="bonferroni", alpha=0.05)



### Betrachtung der distribution

# hist(wave1_dens$all_contact_n_Q79, breaks =1000, freq = FALSE, xlim = range(0,100))
# lines(0:30, dpois(0:30, median(test1$all_contact_n_Q79)), col = 'red')
# lines(0:30, dpois(0:30, 2), col = 'blue')
# lines(0:30, dbinom(0:30, 3, 0.5, log = FALSE), col = 'orange')
# lines


```

# Grafiken 

## Kontaktplots
### Boxplots

```{r}
plot_1_se <- summarySE(d_all_wide, measurevar =  "all_contact_n_Q79",groupvar = c("wave_two", "qmktsize_16_0"),na.rm = TRUE)
plot_1_se



# Plot: All Contacts - all waves - all areas - log(+1)-scale
aaa <- ggplot(d_all_wide, aes(y=all_contact_n_Q79, x= as.factor(wave_two))) +
  geom_boxplot(aes(fill = qmktsize_16_0)) +
  #geom_errorbar(data = plot_1_se, aes(ymin=all_contact_n_Q79-se, ymax=all_contact_n_Q79+se, fill = qmktsize_16_0),
                 #width=.35,                    # Width of the error bars
                 #position=position_dodge(.75)) +
  geom_point(data=plot_1_se, aes(fill = qmktsize_16_0), col="red", position=position_dodge(.75)) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = c(0,2,3,4,5,200,400,600,800,1200), limits = c(0, 1200)) + 
  #stat_summary(fun=mean, geom="point", aes(group = as.factor(qmktsize_16_0)), position=position_dodge(.75),
               #color="red", size=1.5) +
  labs(x = "Waves") +
  labs(y = "All contacts[logscale]") +
  theme(text = element_text(size=6.5)) +
  labs(fill = "Area") +
  scale_fill_manual(values=c("#E64B35B2", "#00A087B2", "#4DBBD5B2")) +
  theme(legend.position="none") +
  labs(title = "Number of all contacts per wave and area") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.minor = element_blank())


# Plot: All Non-Household contacts - all waves - all areas - log(+1)-scale

plot_2_se <- summarySE(d_all_wide, measurevar =  "non_hh_contact_n_Q79",
                       groupvar = c("wave_two", "qmktsize_16_0"), na.rm = TRUE)
plot_2_se



bbb <- ggplot(d_all_wide, aes(y=non_hh_contact_n_Q79, x= as.factor(wave_two))) +
  geom_boxplot(aes(fill = qmktsize_16_0)) +
  geom_point(data=plot_2_se, aes(fill = qmktsize_16_0), col="red", position=position_dodge(.75)) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = c(0,2,3,4,5,200,400,600,800,1200), limits = c(0, 1200)) + 
  labs(x = "Waves") +
  labs(y = "Non-household contacts[logscale]") +
  theme(text = element_text(size=6.5)) +
  labs(fill = "Area") +
  scale_fill_manual(values=c("#E64B35B2", "#00A087B2", "#4DBBD5B2")) +
  theme(legend.position="none") +
  labs(title = "Number of non-household contacts per wave and area") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.minor = element_blank())


# Plot: All Household contacts - all waves - all areas -log(+1)-scale
plot_3_se <- summarySE(d_all_wide, measurevar =  "hh_contact_n",
                       groupvar = c("wave_two", "qmktsize_16_0"), na.rm = TRUE)
plot_3_se

ccc <- ggplot(d_all_wide, aes(y=hh_contact_n, x= as.factor(wave_two))) +
  geom_boxplot(aes(fill = qmktsize_16_0)) +
  geom_point(data=plot_3_se, aes(fill = qmktsize_16_0), col="red", position=position_dodge(.75)) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = c(0, 1,2,3, 4,5, 6,7, 8,9, 10), limits = c(0, 10)) + 
  #facet_grid(. ~ as.factor(wave), scales="free",  space="free") +
  #scale_y_log10()+
  labs(x = "Waves") +
  labs(y = "Household contacts[logscale]") +
  theme(text = element_text(size=6.5)) +
  labs(fill = "Area") +
  #stat_summary(fun=mean, geom="point",  aes(group=qmktsize_16_0), position=position_dodge(.75),
               #color="red", size=1.5) +
  scale_fill_manual(values=c("#E64B35B2", "#00A087B2", "#4DBBD5B2")) +
  theme(legend.position="none") +
  labs(title = "Number of household-contacts per wave and area") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.minor = element_blank())



# Plot: Pseudo All Contacts (limit to 50) - all waves - all areas - log(+1)-scale
plot_4_se <- summarySE(d_all_wide, measurevar =  "pseudo_all_contact_n_Q79",
                       groupvar = c("wave_two", "qmktsize_16_0"), na.rm = TRUE)
plot_4_se

ddd <- ggplot(d_all_wide, aes(y=pseudo_all_contact_n_Q79, x= as.factor(wave_two))) +
  geom_boxplot(aes(fill = qmktsize_16_0)) +
  #geom_errorbar(data = plot_1_se, aes(ymin=all_contact_n_Q79-se, ymax=all_contact_n_Q79+se, fill = qmktsize_16_0),
  #width=.35,                    # Width of the error bars
  #position=position_dodge(.75)) +
  geom_point(data=plot_4_se, aes(fill = qmktsize_16_0), col="red", position=position_dodge(.75)) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = c(0,2,3,4,5,200,300), limits = c(0, 300)) + 
  #stat_summary(fun=mean, geom="point", aes(group = as.factor(qmktsize_16_0)), position=position_dodge(.75),
  #color="red", size=1.5) +
  labs(x = "Waves") +
  labs(y = "Truncated all contacts[logscale]") +
  theme(text = element_text(size=6.5)) +
  labs(fill = "Area") +
  scale_fill_manual(values=c("#E64B35B2", "#00A087B2", "#4DBBD5B2")) +
  theme(legend.position="none") +
  labs(title = "Number of all contacts (truncated group contacts) per wave and area") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.minor = element_blank())


# Plot: All Non-Household contacts - all waves - all areas - log(+1)-scale

plot_5_se <- summarySE(d_all_wide, measurevar =  "pseudo_non_hh_contact_n_Q79",
                       groupvar = c("wave_two", "qmktsize_16_0"), na.rm = TRUE)
plot_5_se



eee <- ggplot(d_all_wide, aes(y=pseudo_non_hh_contact_n_Q79, x= as.factor(wave_two))) +
  geom_boxplot(aes(fill = qmktsize_16_0)) +
  geom_point(data=plot_5_se, aes(fill = qmktsize_16_0), col="red", position=position_dodge(.75)) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = c(0,2,3,4,5,200,300), limits = c(0, 300)) + 
  labs(x = "Waves") +
  labs(y = "Truncated non-household contacts[logscale]") +
  theme(text = element_text(size=6.5)) +
  labs(fill = "Area") +
  scale_fill_manual(values=c("#E64B35B2", "#00A087B2", "#4DBBD5B2")) +
  theme(legend.position="none") +
  labs(title = "Number of non-household contacts (truncated group contacts) per wave and area") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.minor = element_blank())


example_legend <- ggplot(d_all_wide, aes(y=hh_contact_n, x= as.factor(wave_two))) +
  geom_boxplot(aes(fill = qmktsize_16_0)) +
  geom_point(data=plot_3_se, aes(fill = qmktsize_16_0), col="red", position=position_dodge(.75)) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = c(0,2, 4, 6, 8, 10,12), limits = c(0, 12)) + 
  labs(x = "Waves") +
  labs(y = "All household contacts[logscale]") +
  theme(text = element_text(size=6.5)) +
  labs(fill = "Area") +
  scale_fill_manual(values=c("#E64B35B2", "#00A087B2", "#4DBBD5B2")) +
  theme(legend.position="right") +
  labs(title = "Number of all household contacts per wave and area") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.minor = element_blank()) +
  theme(legend.key.size = unit(2, 'cm'),
        legend.key.width= unit(2, 'cm'),
        legend.title = element_text(size = 20), 
        legend.text = element_text(size = 12))
  


box_legend <- cowplot::get_legend(example_legend)


gridExtra::grid.arrange(aaa, bbb, ccc, ddd, eee, box_legend, nrow =2)


prow <- plot_grid(aaa, bbb, ccc, ddd, eee, box_legend,
  align = 'v',
  axis = "tb",
  labels = c("A", "B", "C", "D", "E"),
  hjust = -1,
  nrow = 2
)
```


## Percentage stacked bar plot 

### Version 1: All contact (including goup contacts)

```{r create data frame for stacked percentage bar plot}
# place other is saved as class factor, has to be integer remove NAs with 0
dt_hh_all$place_oth <- as.integer(dt_hh_all$place_oth)
dt_hh_all$place_oth[is.na(dt_hh_all$place_oth)] <- 0
#dt_outside_all$place_oth <- as.integer(dt_outside_all$place_oth)
#dt_outside_all$place_oth[which(dt_outside_all$place_oth==2)] <- 0
# merge data
# d_self_all and dummy_hh_group, includes group contacts!
dummy_hh_group <- d_self_all %>% select(new_id, wave, qmktsize_16_0,Q79a_age_met_work_u18,
                               Q79a_age_met_work_1864, Q79a_age_met_work_o64, 
                               Q80a_age_met_uni_u18, Q80a_age_met_uni_1864,
                               Q80a_age_met_uni_o64, Q81a_age_met_else_u18,
                               Q81a_age_met_else_1864, Q81a_age_met_else_o64)

dummy_hh_group <- merge(dummy_hh_group, dt_hh_all, by=c("new_id", "wave"), all.y=T)

# drop observations that individual has NOT met this day
#dummy_hh_group <- dummy[!(dummy$hh_met_this_day=="No"),]

# d_self_all and dt_outside_all, no group contacts!
dummy_outside <- d_self_all %>% select(new_id, wave, qmktsize_16_0)
dummy_outside <- merge(dummy_outside, dt_outside_all, by=c("new_id","wave"), all.y=T)


##### preparation for loops
# create empty vectors for places to store sums in loop, needed for loops 
sum_home_own <- c()  
sum_home_else <- c()
sum_work  <- c()
sum_worship  <- c()
sum_transport  <- c()
sum_school  <- c()
sum_shop_essential  <- c()
sum_shop_non_essential  <- c()
sum_entertainment  <- c()
sum_sport  <- c()
sum_park  <- c()
sum_healthcare  <- c()
sum_beauty  <- c()
sum_other  <- c()
sum_else  <- c()

# levels
dummy_hh_group$qmktsize_16_0 <- as.factor(dummy_hh_group$qmktsize_16_0)
dummy_outside$qmktsize_16_0 <- as.factor(dummy_outside$qmktsize_16_0)
levels(dummy_hh_group$qmktsize_16_0) <- c("Densely", "Intermediate","Thinly")
levels(dummy_outside$qmktsize_16_0) <- c("Densely", "Intermediate","Thinly")

# split datasets in thinly, intermediate and densely
dummy_hh_group_thinly <- dummy_hh_group[which(dummy_hh_group$qmktsize_16_0=="Thinly"),]
dummy_hh_group_intermediate <- dummy_hh_group[which(dummy_hh_group$qmktsize_16_0=="Intermediate"),]
dummy_hh_group_densely <- dummy_hh_group[which(dummy_hh_group$qmktsize_16_0=="Densely"),]

dummy_outside_thinly <- dummy_outside[which(dummy_outside$qmktsize_16_0=="Thinly"),]
dummy_outside_intermediate <- dummy_outside[which(dummy_outside$qmktsize_16_0=="Intermediate"),]
dummy_outside_densely <- dummy_outside[which(dummy_outside$qmktsize_16_0=="Densely"),]



```

```{r Version 1 thinly populated}
## plot for thinly
# sums up number of people met in a specific place in a specific wave 
# this is the rows for our dataframe (row = place, column = wave)
for(i in c(1:8)){
  # summing number of people met for each place in a specific wave
  # home own
  home_own<- rbind(sum(dummy_hh_group_thinly$place_home_own[which(dummy_hh_group_thinly$wave==i)])+
                    sum(dummy_outside_thinly$place_home_own[which(dummy_outside_thinly$wave==i)]))
 sum_home_own <- cbind(sum_home_own,home_own)
 # home else
 
 home_else<- rbind(sum(dummy_hh_group_thinly$place_home_else[which(dummy_hh_group_thinly$wave==i)])+
                    sum(dummy_outside_thinly$place_home_else[which(dummy_outside_thinly$wave==i)]))
 sum_home_else <- cbind(sum_home_else,home_else)
 # work (including group contacts)
 
 work_plot<- rbind(sum(dummy_hh_group_thinly$place_home_else[which(dummy_hh_group_thinly$wave==i)])+
                     sum(dummy_outside_thinly$place_home_else[which(dummy_outside_thinly$wave==i)])+
                     sum(dummy_hh_group_thinly$Q79a_age_met_work_u18[which(dummy_hh_group_thinly$wave_0==i)], na.rm = TRUE)+
                     sum(dummy_hh_group_thinly$Q79a_age_met_work_1864[which(dummy_hh_group_thinly$wave_0==i)],na.rm= TRUE)+
                     sum(dummy_hh_group_thinly$Q79a_age_met_work_o64[which(dummy_hh_group_thinly$wave_0==i)],na.rm=TRUE))
 
 sum_work <- cbind(sum_work, work_plot)
 
 # worship
 
worship_plot<- rbind(sum(dummy_hh_group_thinly$place_worship[which(dummy_hh_group_thinly$wave==i)])+
                     sum(dummy_outside_thinly$place_worship[which(dummy_outside_thinly$wave==i)]))
 sum_worship <- cbind(sum_worship,worship_plot)
 
 # transport
 
 transport_plot<- rbind(sum(dummy_hh_group_thinly$place_transport[which(dummy_hh_group_thinly$wave==i)])+
                        sum(dummy_outside_thinly$place_transport[which(dummy_outside_thinly$wave==i)]))
 sum_transport <- cbind(sum_transport,transport_plot)
 
 # school
 
 school_plot<- rbind(sum(dummy_hh_group_thinly$place_school[which(dummy_hh_group_thinly$wave==i)])+
                     sum(dummy_outside_thinly$place_school[which(dummy_outside_thinly$wave==i)])+
                     sum(dummy_hh_group_thinly$Q80a_age_met_uni_u18[which(dummy_hh_group_thinly$wave_0==i)],na.rm=TRUE)+
                     sum(dummy_hh_group_thinly$Q80a_age_met_uni_1864[which(dummy_hh_group_thinly$wave_0==i)],na.rm=TRUE)+
                     sum(dummy_hh_group_thinly$Q80a_age_met_uni_o64[which(dummy_hh_group_thinly$wave_0==i)],na.rm=TRUE))
 
 sum_school <- cbind(sum_school, school_plot)
 
 # shop essential
 
 shop_essential_plot<- rbind(sum(dummy_hh_group_thinly$place_shop_essential[which(dummy_hh_group_thinly$wave==i)])+
                        sum(dummy_outside_thinly$place_shop_essential[which(dummy_outside_thinly$wave==i)]))
 sum_shop_essential <- cbind(sum_shop_essential,shop_essential_plot)
 
 # shop non essential
 
 shop_non_essential_plot<- rbind(sum(dummy_hh_group_thinly$place_shop_non_essential[which(dummy_hh_group_thinly$wave==i)])+
                        sum(dummy_outside_thinly$place_shop_non_essential[which(dummy_outside_thinly$wave==i)]))
 sum_shop_non_essential <- cbind(sum_shop_non_essential,shop_non_essential_plot)
 
 # entertainment
 
 entertainment_plot<- rbind(sum(dummy_hh_group_thinly$place_entertainment[which(dummy_hh_group_thinly$wave==i)])+
                        sum(dummy_outside_thinly$place_entertainment[which(dummy_outside_thinly$wave==i)]))
 sum_entertainment <- cbind(sum_entertainment,entertainment_plot)
 
 # sport
 
 sport_plot<- rbind(sum(dummy_hh_group_thinly$place_sport[which(dummy_hh_group_thinly$wave==i)])+
                        sum(dummy_outside_thinly$place_sport[which(dummy_outside_thinly$wave==i)]))
 sum_sport <- cbind(sum_sport,sport_plot)
 
# park
 
 park_plot<- rbind(sum(dummy_hh_group_thinly$place_park[which(dummy_hh_group_thinly$wave==i)])+
                        sum(dummy_outside_thinly$place_park[which(dummy_outside_thinly$wave==i)]))
 sum_park <- cbind(sum_park,park_plot)
 
 # healthcare
 
 healthcare_plot<- rbind(sum(dummy_hh_group_thinly$place_healthcare[which(dummy_hh_group_thinly$wave==i)])+
                        sum(dummy_outside_thinly$place_healthcare[which(dummy_outside_thinly$wave==i)]))
 sum_healthcare <- cbind(sum_healthcare,healthcare_plot)
 
 # beauty
 
 beauty_plot<- rbind(sum(dummy_hh_group_thinly$place_beauty[which(dummy_hh_group_thinly$wave==i)])+
                        sum(dummy_outside_thinly$place_beauty[which(dummy_outside_thinly$wave==i)]))
 sum_beauty <- cbind(sum_beauty,beauty_plot)
 
 # other
 
 other_plot<- rbind(sum(as.numeric(dummy_hh_group_thinly$place_oth[which(dummy_hh_group_thinly$wave==i)]),na.rm=TRUE)+
                        sum(as.numeric(dummy_outside_thinly$place_oth[which(dummy_outside_thinly$wave==i)]),na.rm=TRUE))
 sum_other <- cbind(sum_other,other_plot)
 
 # else, group contacts met else, interprets NA as 0
 else_plot<- rbind(sum(dummy_hh_group_thinly$Q81a_age_met_else_u18[which(dummy_hh_group_thinly$wave==i)],na.rm=TRUE)+
                     sum(dummy_hh_group_thinly$Q81a_age_met_else_1864[which(dummy_hh_group_thinly$wave==i)],na.rm=TRUE)+
                     sum(dummy_hh_group_thinly$Q81a_age_met_else_o64[which(dummy_hh_group_thinly$wave==i)],na.rm=TRUE))
 
 sum_else <- cbind(sum_else, else_plot)
 
# collect all sums in data.frame 
data_barplot_wide_thinly <- as.data.frame(rbind(sum_home_own,sum_home_else, sum_work, sum_worship, sum_transport,sum_school,
                   sum_shop_essential,sum_shop_non_essential, sum_entertainment, sum_sport, 
                   sum_park, sum_healthcare,sum_beauty, sum_other, sum_else))


}

# drop from global environment for next loop 

rm(sum_beauty,sum_else,sum_entertainment,sum_healthcare,sum_home_else, sum_home_own,
   sum_other, sum_park, sum_school, sum_shop_essential, sum_shop_non_essential,
   sum_sport,sum_transport, sum_work, sum_worship)

# change column names to waves
colnames(data_barplot_wide_thinly) <- c(1:8)

# add place variable as ID
places_plot <- c(places, "place_else")
data_barplot_wide_thinly$places <- places_plot
#rownames(data_barplot_wide_thinly) <- places_plot

# change to long format
data_barplot_long_thinly <- melt(setDT(data_barplot_wide_thinly), id.vars=c("places"), variable.name="wave")


#data_barplot_long_thinly <- data_barplot_wide_thinly %>% 
#  mutate( ind = factor(row_number())) %>%
#  gather(variable, value, -ind) # can't handle character?
#data_barplot_long_thinly$ind <- rep(places_plot,8)



# plot
col1 <- brewer.pal(8, "Dark2") 
col2 <- brewer.pal(7,"Pastel2")
col <- cbind(col1,col2)
plot_version1_thinly <- ggplot(data_barplot_long_thinly, aes(x= wave, y=value, fill=places)) +
  geom_bar(position="fill", stat="identity")+
             scale_y_continuous(labels=scales::percent_format())+ scale_fill_manual(values = col,labels=c("beauty","else","entertainment","healthcare","home_else","home_own","other","park","school", 
  "shop essential","shop non essential","sport","transport","work","worship")) +
  guides(fill=guide_legend(title="Places")) +
  scale_x_discrete(name="Wave", labels=c("03/3-10/3","31/3-07/4","21/4-28/4","12/5-19/5", 
                             "02/6-09/6","23/6-30/6","14/7-21/7","04/8-11/8"))+
  ylab("Percentage")+
  theme(axis.text.x = element_text(angle=90),plot.title = element_text(size=9),legend.position = "none",panel.background = element_blank()) +
  ggtitle("Thinly populated areas")
 


```



```{r intermediate }
## plot for intermediate
# sums up number of people met in a specific place in a specific wave 
# this is the rows for our dataframe (row = place, column = wave)
# create empty vectors for places to store sums in loop, needed for loops 
sum_home_own <- c()  
sum_home_else <- c()
sum_work  <- c()
sum_worship  <- c()
sum_transport  <- c()
sum_school  <- c()
sum_shop_essential  <- c()
sum_shop_non_essential  <- c()
sum_entertainment  <- c()
sum_sport  <- c()
sum_park  <- c()
sum_healthcare  <- c()
sum_beauty  <- c()
sum_other  <- c()
sum_else  <- c()

for(i in c(1:8)){
  # summing number of people met for each place in a specific wave
  # home own
  home_own<- rbind(sum(dummy_hh_group_intermediate$place_home_own[which(dummy_hh_group_intermediate$wave==i)])+
                    sum(dummy_outside_intermediate$place_home_own[which(dummy_outside_intermediate$wave==i)]))
 sum_home_own <- cbind(sum_home_own,home_own)
 # home else
 
 home_else<- rbind(sum(dummy_hh_group_intermediate$place_home_else[which(dummy_hh_group_intermediate$wave==i)])+
                    sum(dummy_outside_intermediate$place_home_else[which(dummy_outside_intermediate$wave==i)]))
 sum_home_else <- cbind(sum_home_else,home_else)
 # work (including group contacts)
 
 work_plot<- rbind(sum(dummy_hh_group_intermediate$place_home_else[which(dummy_hh_group_intermediate$wave==i)])+
                     sum(dummy_outside_intermediate$place_home_else[which(dummy_outside_intermediate$wave==i)])+
                     sum(dummy_hh_group_intermediate$Q79a_age_met_work_u18[which(dummy_hh_group_intermediate$wave_0==i)], na.rm = TRUE)+
                     sum(dummy_hh_group_intermediate$Q79a_age_met_work_1864[which(dummy_hh_group_intermediate$wave_0==i)],na.rm= TRUE)+
                     sum(dummy_hh_group_intermediate$Q79a_age_met_work_o64[which(dummy_hh_group_intermediate$wave_0==i)],na.rm=TRUE))
 
 sum_work <- cbind(sum_work, work_plot)
 
 # worship
 
worship_plot<- rbind(sum(dummy_hh_group_intermediate$place_worship[which(dummy_hh_group_intermediate$wave==i)])+
                     sum(dummy_outside_intermediate$place_worship[which(dummy_outside_intermediate$wave==i)]))
 sum_worship <- cbind(sum_worship,worship_plot)
 
 # transport
 
 transport_plot<- rbind(sum(dummy_hh_group_intermediate$place_transport[which(dummy_hh_group_intermediate$wave==i)])+
                        sum(dummy_outside_intermediate$place_transport[which(dummy_outside_intermediate$wave==i)]))
 sum_transport <- cbind(sum_transport,transport_plot)
 
 # school
 
 school_plot<- rbind(sum(dummy_hh_group_intermediate$place_school[which(dummy_hh_group_intermediate$wave==i)])+
                     sum(dummy_outside_intermediate$place_school[which(dummy_outside_intermediate$wave==i)])+
                     sum(dummy_hh_group_intermediate$Q80a_age_met_uni_u18[which(dummy_hh_group_intermediate$wave_0==i)],na.rm=TRUE)+
                     sum(dummy_hh_group_intermediate$Q80a_age_met_uni_1864[which(dummy_hh_group_intermediate$wave_0==i)],na.rm=TRUE)+
                     sum(dummy_hh_group_intermediate$Q80a_age_met_uni_o64[which(dummy_hh_group_intermediate$wave_0==i)],na.rm=TRUE))
 
 sum_school <- cbind(sum_school, school_plot)
 
 # shop essential
 
 shop_essential_plot<- rbind(sum(dummy_hh_group_intermediate$place_shop_essential[which(dummy_hh_group_intermediate$wave==i)])+
                        sum(dummy_outside_intermediate$place_shop_essential[which(dummy_outside_intermediate$wave==i)]))
 sum_shop_essential <- cbind(sum_shop_essential,shop_essential_plot)
 
 # shop non essential
 
 shop_non_essential_plot<- rbind(sum(dummy_hh_group_intermediate$place_shop_non_essential[which(dummy_hh_group_intermediate$wave==i)])+
                        sum(dummy_outside_intermediate$place_shop_non_essential[which(dummy_outside_intermediate$wave==i)]))
 sum_shop_non_essential <- cbind(sum_shop_non_essential,shop_non_essential_plot)
 
 # entertainment
 
 entertainment_plot<- rbind(sum(dummy_hh_group_intermediate$place_entertainment[which(dummy_hh_group_intermediate$wave==i)])+
                        sum(dummy_outside_intermediate$place_entertainment[which(dummy_outside_intermediate$wave==i)]))
 sum_entertainment <- cbind(sum_entertainment,entertainment_plot)
 
 # sport
 
 sport_plot<- rbind(sum(dummy_hh_group_intermediate$place_sport[which(dummy_hh_group_intermediate$wave==i)])+
                        sum(dummy_outside_intermediate$place_sport[which(dummy_outside_intermediate$wave==i)]))
 sum_sport <- cbind(sum_sport,sport_plot)
 
# park
 
 park_plot<- rbind(sum(dummy_hh_group_intermediate$place_park[which(dummy_hh_group_intermediate$wave==i)])+
                        sum(dummy_outside_intermediate$place_park[which(dummy_outside_intermediate$wave==i)]))
 sum_park <- cbind(sum_park,park_plot)
 
 # healthcare
 
 healthcare_plot<- rbind(sum(dummy_hh_group_intermediate$place_healthcare[which(dummy_hh_group_intermediate$wave==i)])+
                        sum(dummy_outside_intermediate$place_healthcare[which(dummy_outside_intermediate$wave==i)]))
 sum_healthcare <- cbind(sum_healthcare,healthcare_plot)
 
 # beauty
 
 beauty_plot<- rbind(sum(dummy_hh_group_intermediate$place_beauty[which(dummy_hh_group_intermediate$wave==i)])+
                        sum(dummy_outside_intermediate$place_beauty[which(dummy_outside_intermediate$wave==i)]))
 sum_beauty <- cbind(sum_beauty,beauty_plot)
 
 # other
 
 other_plot<- rbind(sum(as.numeric(dummy_hh_group_intermediate$place_oth[which(dummy_hh_group_intermediate$wave==i)]),na.rm=TRUE)+
                        sum(as.numeric(dummy_outside_intermediate$place_oth[which(dummy_outside_intermediate$wave==i)]),na.rm=TRUE))
 sum_other <- cbind(sum_other,other_plot)
 
 # else, group contacts met else, interprets NA as 0
 else_plot<- rbind(sum(dummy_hh_group_intermediate$Q81a_age_met_else_u18[which(dummy_hh_group_intermediate$wave==i)],na.rm=TRUE)+
                     sum(dummy_hh_group_intermediate$Q81a_age_met_else_1864[which(dummy_hh_group_intermediate$wave==i)],na.rm=TRUE)+
                     sum(dummy_hh_group_intermediate$Q81a_age_met_else_o64[which(dummy_hh_group_intermediate$wave==i)],na.rm=TRUE))
 
 sum_else <- cbind(sum_else, else_plot)
 
# collect all sums in data.frame 
data_barplot_wide_intermediate <- as.data.frame(rbind(sum_home_own,sum_home_else, sum_work, sum_worship, sum_transport,sum_school,
                   sum_shop_essential,sum_shop_non_essential, sum_entertainment, sum_sport, 
                   sum_park, sum_healthcare,sum_beauty, sum_other, sum_else))


}

# drop from global environment for next loop 

rm(sum_beauty,sum_else,sum_entertainment,sum_healthcare,sum_home_else, sum_home_own,
   sum_other, sum_park, sum_school, sum_shop_essential, sum_shop_non_essential,
   sum_sport,sum_transport, sum_work, sum_worship)

# change column names to waves
colnames(data_barplot_wide_intermediate) <- c(1:8)

# add place variable as ID
data_barplot_wide_intermediate$places <- places_plot

# change to long format
data_barplot_long_intermediate <- melt(setDT(data_barplot_wide_intermediate), id.vars=c("places"), variable.name="wave")


# plot

plot_version1_intermediate <- ggplot(data_barplot_long_intermediate, aes(x= wave, y=value, fill=places)) +
  geom_bar(position="fill", stat="identity")+
             scale_y_continuous(labels=scales::percent_format())+ scale_fill_manual(values = col,labels=c("beauty","else","entertainment","healthcare","home_else","home_own","other","park","school", 
  "shop essential","shop non essential","sport","transport","work","worship")) +
  guides(fill=guide_legend(title="Places")) +
  scale_x_discrete(name="Wave", labels=c("03/3-10/3","31/3-07/4","21/4-28/4","12/5-19/5", 
                             "02/6-09/6","23/6-30/6","14/7-21/7","04/8-11/8"))+
  ylab("Percentage")+
  theme(axis.text.x = element_text(angle=90),plot.title = element_text(size=9),legend.position = "none", panel.background = element_blank()) +
  ggtitle("Intermediate populated areas")


 

```


```{r desely populated}
## plot for densely
# sums up number of people met in a specific place in a specific wave 
# this is the rows for our dataframe (row = place, column = wave)
# create empty vectors for places to store sums in loop, needed for loops 
sum_home_own <- c()  
sum_home_else <- c()
sum_work  <- c()
sum_worship  <- c()
sum_transport  <- c()
sum_school  <- c()
sum_shop_essential  <- c()
sum_shop_non_essential  <- c()
sum_entertainment  <- c()
sum_sport  <- c()
sum_park  <- c()
sum_healthcare  <- c()
sum_beauty  <- c()
sum_other  <- c()
sum_else  <- c()

for(i in c(1:8)){
  # summing number of people met for each place in a specific wave
  # home own
  home_own<- rbind(sum(dummy_hh_group_densely$place_home_own[which(dummy_hh_group_densely$wave==i)])+
                    sum(dummy_outside_densely$place_home_own[which(dummy_outside_densely$wave==i)]))
 sum_home_own <- cbind(sum_home_own,home_own)
 # home else
 
 home_else<- rbind(sum(dummy_hh_group_densely$place_home_else[which(dummy_hh_group_densely$wave==i)])+
                    sum(dummy_outside_densely$place_home_else[which(dummy_outside_densely$wave==i)]))
 sum_home_else <- cbind(sum_home_else,home_else)
 # work (including group contacts)
 
 work_plot<- rbind(sum(dummy_hh_group_densely$place_home_else[which(dummy_hh_group_densely$wave==i)])+
                     sum(dummy_outside_densely$place_home_else[which(dummy_outside_densely$wave==i)])+
                     sum(dummy_hh_group_densely$Q79a_age_met_work_u18[which(dummy_hh_group_densely$wave_0==i)], na.rm = TRUE)+
                     sum(dummy_hh_group_densely$Q79a_age_met_work_1864[which(dummy_hh_group_densely$wave_0==i)],na.rm= TRUE)+
                     sum(dummy_hh_group_densely$Q79a_age_met_work_o64[which(dummy_hh_group_densely$wave_0==i)],na.rm=TRUE))
 
 sum_work <- cbind(sum_work, work_plot)
 
 # worship
 
worship_plot<- rbind(sum(dummy_hh_group_densely$place_worship[which(dummy_hh_group_densely$wave==i)])+
                     sum(dummy_outside_densely$place_worship[which(dummy_outside_densely$wave==i)]))
 sum_worship <- cbind(sum_worship,worship_plot)
 
 # transport
 
 transport_plot<- rbind(sum(dummy_hh_group_densely$place_transport[which(dummy_hh_group_densely$wave==i)])+
                        sum(dummy_outside_densely$place_transport[which(dummy_outside_densely$wave==i)]))
 sum_transport <- cbind(sum_transport,transport_plot)
 
 # school
 
 school_plot<- rbind(sum(dummy_hh_group_densely$place_school[which(dummy_hh_group_densely$wave==i)])+
                     sum(dummy_outside_densely$place_school[which(dummy_outside_densely$wave==i)])+
                     sum(dummy_hh_group_densely$Q80a_age_met_uni_u18[which(dummy_hh_group_densely$wave_0==i)],na.rm=TRUE)+
                     sum(dummy_hh_group_densely$Q80a_age_met_uni_1864[which(dummy_hh_group_densely$wave_0==i)],na.rm=TRUE)+
                     sum(dummy_hh_group_densely$Q80a_age_met_uni_o64[which(dummy_hh_group_densely$wave_0==i)],na.rm=TRUE))
 
 sum_school <- cbind(sum_school, school_plot)
 
 # shop essential
 
 shop_essential_plot<- rbind(sum(dummy_hh_group_densely$place_shop_essential[which(dummy_hh_group_densely$wave==i)])+
                        sum(dummy_outside_densely$place_shop_essential[which(dummy_outside_densely$wave==i)]))
 sum_shop_essential <- cbind(sum_shop_essential,shop_essential_plot)
 
 # shop non essential
 
 shop_non_essential_plot<- rbind(sum(dummy_hh_group_densely$place_shop_non_essential[which(dummy_hh_group_densely$wave==i)])+
                        sum(dummy_outside_densely$place_shop_non_essential[which(dummy_outside_densely$wave==i)]))
 sum_shop_non_essential <- cbind(sum_shop_non_essential,shop_non_essential_plot)
 
 # entertainment
 
 entertainment_plot<- rbind(sum(dummy_hh_group_densely$place_entertainment[which(dummy_hh_group_densely$wave==i)])+
                        sum(dummy_outside_densely$place_entertainment[which(dummy_outside_densely$wave==i)]))
 sum_entertainment <- cbind(sum_entertainment,entertainment_plot)
 
 # sport
 
 sport_plot<- rbind(sum(dummy_hh_group_densely$place_sport[which(dummy_hh_group_densely$wave==i)])+
                        sum(dummy_outside_densely$place_sport[which(dummy_outside_densely$wave==i)]))
 sum_sport <- cbind(sum_sport,sport_plot)
 
# park
 
 park_plot<- rbind(sum(dummy_hh_group_densely$place_park[which(dummy_hh_group_densely$wave==i)])+
                        sum(dummy_outside_densely$place_park[which(dummy_outside_densely$wave==i)]))
 sum_park <- cbind(sum_park,park_plot)
 
 # healthcare
 
 healthcare_plot<- rbind(sum(dummy_hh_group_densely$place_healthcare[which(dummy_hh_group_densely$wave==i)])+
                        sum(dummy_outside_densely$place_healthcare[which(dummy_outside_densely$wave==i)]))
 sum_healthcare <- cbind(sum_healthcare,healthcare_plot)
 
 # beauty
 
 beauty_plot<- rbind(sum(dummy_hh_group_densely$place_beauty[which(dummy_hh_group_densely$wave==i)])+
                        sum(dummy_outside_densely$place_beauty[which(dummy_outside_densely$wave==i)]))
 sum_beauty <- cbind(sum_beauty,beauty_plot)
 
 # other
 
 other_plot<- rbind(sum(as.numeric(dummy_hh_group_densely$place_oth[which(dummy_hh_group_densely$wave==i)]),na.rm=TRUE)+
                        sum(as.numeric(dummy_outside_densely$place_oth[which(dummy_outside_densely$wave==i)]),na.rm=TRUE))
 sum_other <- cbind(sum_other,other_plot)
 
 # else, group contacts met else, interprets NA as 0
 else_plot<- rbind(sum(dummy_hh_group_densely$Q81a_age_met_else_u18[which(dummy_hh_group_densely$wave==i)],na.rm=TRUE)+
                     sum(dummy_hh_group_densely$Q81a_age_met_else_1864[which(dummy_hh_group_densely$wave==i)],na.rm=TRUE)+
                     sum(dummy_hh_group_densely$Q81a_age_met_else_o64[which(dummy_hh_group_densely$wave==i)],na.rm=TRUE))
 
 sum_else <- cbind(sum_else, else_plot)
 
# collect all sums in data.frame 
data_barplot_wide_densely <- as.data.frame(rbind(sum_home_own,sum_home_else, sum_work, sum_worship, sum_transport,sum_school,
                   sum_shop_essential,sum_shop_non_essential, sum_entertainment, sum_sport, 
                   sum_park, sum_healthcare,sum_beauty, sum_other, sum_else))


}

# drop from global environment for next loop 

rm(sum_beauty,sum_else,sum_entertainment,sum_healthcare,sum_home_else, sum_home_own,
   sum_other, sum_park, sum_school, sum_shop_essential, sum_shop_non_essential,
   sum_sport,sum_transport, sum_work, sum_worship)

# change column names to waves
colnames(data_barplot_wide_densely) <- c(1:8)

# add place variable as ID
data_barplot_wide_densely$places <- places_plot

# change to long format
data_barplot_long_densely <- melt(setDT(data_barplot_wide_densely), id.vars=c("places"), variable.name="wave")


# plot
plot_version1_densely <- ggplot(data_barplot_long_densely, aes(x= wave, y=value, fill=places)) +
  geom_bar(position="fill", stat="identity")+
             scale_y_continuous(labels=scales::percent_format())+ scale_fill_manual(values = col,labels=c("beauty","else","entertainment","healthcare","home_else","home_own","other","park","school", 
  "shop essential","shop non essential","sport","transport","work","worship")) +
  guides(fill=guide_legend(title="Places")) +
  scale_x_discrete(name="Wave", labels=c("03/3-10/3","31/3-07/4","21/4-28/4","12/5-19/5", 
                             "02/6-09/6","23/6-30/6","14/7-21/7","04/8-11/8"))+
  ylab("Percentage")+
  theme(axis.text.x = element_text(angle=90),plot.title = element_text(size=9), legend.position = "none", panel.background = element_blank()) +
  ggtitle("Densely populated areas")


 


```


```{r arranging plots version 1}
# create mosaicplots and arrange them 

plot_0 <- ggplot(data_barplot_long_densely, aes(x= wave, y=value, fill=places)) +
  geom_bar(position="fill", stat="identity")+
             scale_y_continuous(labels=scales::percent_format())+ scale_fill_manual(values = col ) +
  guides(fill=guide_legend(title="Places", ncol = 2)) +
  scale_x_discrete(name="Wave", labels=c("03/3-10/3","31/3-07/4","21/4-28/4","12/5-19/5", 
                             "02/6-09/6","23/6-30/6","14/7-21/7","04/8-11/8"))+
  theme(axis.text.x = element_text(angle=90)) 



legend <- cowplot::get_legend(plot_0)


gridExtra::grid.arrange(plot_version1_densely, plot_version1_intermediate, 
             plot_version1_thinly, legend,
             ncol=2)


```




### Version 2: non household contact without group contacts

```{r thinly non household contacts}

sum_home_own <- c()  
sum_home_else <- c()
sum_work  <- c()
sum_worship  <- c()
sum_transport  <- c()
sum_school  <- c()
sum_shop_essential  <- c()
sum_shop_non_essential  <- c()
sum_entertainment  <- c()
sum_sport  <- c()
sum_park  <- c()
sum_healthcare  <- c()
sum_beauty  <- c()
sum_other  <- c()

## plot for thinly
# sums up number of people met in a specific place in a specific wave 
# this is the rows for our dataframe (row = place, column = wave)
for(i in c(1:8)){
  # summing number of people met for each place in a specific wave
  # home own
  home_own<- rbind(sum(dummy_outside_thinly$place_home_own[which(dummy_outside_thinly$wave==i)]))
  sum_home_own <- cbind(sum_home_own,home_own)
  # home else
  
  home_else<- rbind(sum(dummy_outside_thinly$place_home_else[which(dummy_outside_thinly$wave==i)]))
  sum_home_else <- cbind(sum_home_else,home_else)
  # work (including group contacts)
  
  work_plot<- rbind(sum(dummy_outside_thinly$place_home_else[which(dummy_outside_thinly$wave==i)]))
  
  sum_work <- cbind(sum_work, work_plot)
  
  # worship
  
  worship_plot<- rbind(sum(dummy_outside_thinly$place_worship[which(dummy_outside_thinly$wave==i)]))
  sum_worship <- cbind(sum_worship,worship_plot)
  
  # transport
  
  transport_plot<- rbind(sum(dummy_outside_thinly$place_transport[which(dummy_outside_thinly$wave==i)]))
  sum_transport <- cbind(sum_transport,transport_plot)
  
  # school
  
  school_plot<- rbind(sum(dummy_outside_thinly$place_school[which(dummy_outside_thinly$wave==i)]))
  
  sum_school <- cbind(sum_school, school_plot)
  
  # shop essential
  
  shop_essential_plot<- rbind(sum(dummy_outside_thinly$place_shop_essential[which(dummy_outside_thinly$wave==i)]))
  sum_shop_essential <- cbind(sum_shop_essential,shop_essential_plot)
  
  # shop non essential
  
  shop_non_essential_plot<- rbind(sum(dummy_outside_thinly$place_shop_non_essential[which(dummy_outside_thinly$wave==i)]))
  sum_shop_non_essential <- cbind(sum_shop_non_essential,shop_non_essential_plot)
  
  # entertainment
  
  entertainment_plot<- rbind(sum(dummy_outside_thinly$place_entertainment[which(dummy_outside_thinly$wave==i)]))
  sum_entertainment <- cbind(sum_entertainment,entertainment_plot)
  
  # sport
  
  sport_plot<- rbind(sum(dummy_outside_thinly$place_sport[which(dummy_outside_thinly$wave==i)]))
  sum_sport <- cbind(sum_sport,sport_plot)
  
  # park
  
  park_plot<- rbind(sum(dummy_outside_thinly$place_park[which(dummy_outside_thinly$wave==i)]))
  sum_park <- cbind(sum_park,park_plot)
  
  # healthcare
  
  healthcare_plot<- rbind(sum(dummy_outside_thinly$place_healthcare[which(dummy_outside_thinly$wave==i)]))
  sum_healthcare <- cbind(sum_healthcare,healthcare_plot)
  
  # beauty
  
  beauty_plot<- rbind(sum(dummy_outside_thinly$place_beauty[which(dummy_outside_thinly$wave==i)]))
  sum_beauty <- cbind(sum_beauty,beauty_plot)
  
  # other
  
  other_plot<- rbind(sum(as.numeric(dummy_outside_thinly$place_oth[which(dummy_outside_thinly$wave==i)]),na.rm=TRUE))
  sum_other <- cbind(sum_other,other_plot)
  
 
  
  # collect all sums in data.frame 
  data_barplot_wide_thinly <- as.data.frame(rbind(sum_home_own,sum_home_else, sum_work, sum_worship, sum_transport,sum_school,
                                                  sum_shop_essential,sum_shop_non_essential, sum_entertainment, sum_sport, 
                                                  sum_park, sum_healthcare,sum_beauty, sum_other))
  
  
}

# drop from global environment for next loop 

rm(sum_beauty,sum_entertainment,sum_healthcare,sum_home_else, sum_home_own,
   sum_other, sum_park, sum_school, sum_shop_essential, sum_shop_non_essential,
   sum_sport,sum_transport, sum_work, sum_worship)

# change column names to waves
colnames(data_barplot_wide_thinly) <- c(1:8)

# add place variable as ID
places_plot <- c(places)
data_barplot_wide_thinly$places <- places_plot

# change to long format
data_barplot_long_thinly <- melt(setDT(data_barplot_wide_thinly), 
                                 id.vars=c("places"), variable.name="wave")


# plot
#col1 <- brewer.pal(8, "RdYlBu") 
#col2 <- brewer.pal(6,"PiYG")
#col <- cbind(col1,col2)
plot_version2_thinly_outside <- ggplot(data_barplot_long_thinly, aes(x= wave, y=value, fill=places)) +
  geom_bar(position="fill", stat="identity")+
  scale_y_continuous(labels=scales::percent_format())+ scale_fill_manual(values = col,labels=c("beauty","entertainment","healthcare","home_else","home_own","other","park","school", 
                                                                                               "shop essential","shop non essential","sport","transport","work","worship")) +
  guides(fill=guide_legend(title="Places")) +
  scale_x_discrete(name="Wave", labels=c("03/3-10/3","31/3-07/4","21/4-28/4","12/5-19/5", 
                                         "02/6-09/6","23/6-30/6","14/7-21/7","04/8-11/8"))+
  theme(axis.text.x = element_text(angle=90),plot.title = element_text(size=9),legend.position = "none") +
  ggtitle("Thinly populated areas")



```


```{r intermediate - non-household contacts}

sum_home_own <- c()  
sum_home_else <- c()
sum_work  <- c()
sum_worship  <- c()
sum_transport  <- c()
sum_school  <- c()
sum_shop_essential  <- c()
sum_shop_non_essential  <- c()
sum_entertainment  <- c()
sum_sport  <- c()
sum_park  <- c()
sum_healthcare  <- c()
sum_beauty  <- c()
sum_other  <- c()

## plot for intermediate
# sums up number of people met in a specific place in a specific wave 
# this is the rows for our dataframe (row = place, column = wave)
for(i in c(1:8)){
  # summing number of people met for each place in a specific wave
  # home own
  home_own<- rbind(sum(dummy_outside_intermediate$place_home_own[which(dummy_outside_intermediate$wave==i)]))
  sum_home_own <- cbind(sum_home_own,home_own)
  # home else
  
  home_else<- rbind(sum(dummy_outside_intermediate$place_home_else[which(dummy_outside_intermediate$wave==i)]))
  sum_home_else <- cbind(sum_home_else,home_else)
  # work (including group contacts)
  
  work_plot<- rbind(sum(dummy_outside_intermediate$place_home_else[which(dummy_outside_intermediate$wave==i)]))
  
  sum_work <- cbind(sum_work, work_plot)
  
  # worship
  
  worship_plot<- rbind(sum(dummy_outside_intermediate$place_worship[which(dummy_outside_intermediate$wave==i)]))
  sum_worship <- cbind(sum_worship,worship_plot)
  
  # transport
  
  transport_plot<- rbind(sum(dummy_outside_intermediate$place_transport[which(dummy_outside_intermediate$wave==i)]))
  sum_transport <- cbind(sum_transport,transport_plot)
  
  # school
  
  school_plot<- rbind(sum(dummy_outside_intermediate$place_school[which(dummy_outside_intermediate$wave==i)]))
  
  sum_school <- cbind(sum_school, school_plot)
  
  # shop essential
  
  shop_essential_plot<- rbind(sum(dummy_outside_intermediate$place_shop_essential[which(dummy_outside_intermediate$wave==i)]))
  sum_shop_essential <- cbind(sum_shop_essential,shop_essential_plot)
  
  # shop non essential
  
  shop_non_essential_plot<- rbind(sum(dummy_outside_intermediate$place_shop_non_essential[which(dummy_outside_intermediate$wave==i)]))
  sum_shop_non_essential <- cbind(sum_shop_non_essential,shop_non_essential_plot)
  
  # entertainment
  
  entertainment_plot<- rbind(sum(dummy_outside_intermediate$place_entertainment[which(dummy_outside_intermediate$wave==i)]))
  sum_entertainment <- cbind(sum_entertainment,entertainment_plot)
  
  # sport
  
  sport_plot<- rbind(sum(dummy_outside_intermediate$place_sport[which(dummy_outside_intermediate$wave==i)]))
  sum_sport <- cbind(sum_sport,sport_plot)
  
  # park
  
  park_plot<- rbind(sum(dummy_outside_intermediate$place_park[which(dummy_outside_intermediate$wave==i)]))
  sum_park <- cbind(sum_park,park_plot)
  
  # healthcare
  
  healthcare_plot<- rbind(sum(dummy_outside_intermediate$place_healthcare[which(dummy_outside_intermediate$wave==i)]))
  sum_healthcare <- cbind(sum_healthcare,healthcare_plot)
  
  # beauty
  
  beauty_plot<- rbind(sum(dummy_outside_intermediate$place_beauty[which(dummy_outside_intermediate$wave==i)]))
  sum_beauty <- cbind(sum_beauty,beauty_plot)
  
  # other
  
  other_plot<- rbind(sum(as.numeric(dummy_outside_intermediate$place_oth[which(dummy_outside_intermediate$wave==i)]),na.rm=TRUE))
  sum_other <- cbind(sum_other,other_plot)
  
 
  
  # collect all sums in data.frame 
  data_barplot_wide_intermediate <- as.data.frame(rbind(sum_home_own,sum_home_else, sum_work, sum_worship, sum_transport,sum_school,
                                                  sum_shop_essential,sum_shop_non_essential, sum_entertainment, sum_sport, 
                                                  sum_park, sum_healthcare,sum_beauty, sum_other))
  
  
}

# drop from global environment for next loop 

rm(sum_beauty,sum_entertainment,sum_healthcare,sum_home_else, sum_home_own,
   sum_other, sum_park, sum_school, sum_shop_essential, sum_shop_non_essential,
   sum_sport,sum_transport, sum_work, sum_worship)

# change column names to waves
colnames(data_barplot_wide_intermediate) <- c(1:8)

# add place variable as ID
places_plot <- c(places)
data_barplot_wide_intermediate$places <- places_plot

# change to long format
data_barplot_long_intermediate <- melt(setDT(data_barplot_wide_intermediate), 
                                 id.vars=c("places"), variable.name="wave")


# plot
plot_version2_intermediate_outside <- ggplot(data_barplot_long_intermediate, aes(x= wave, y=value, fill=places)) +
  geom_bar(position="fill", stat="identity")+
  scale_y_continuous(labels=scales::percent_format())+ scale_fill_manual(values = col,labels=c("beauty","entertainment","healthcare","home_else","home_own","other","park","school", 
                                                                                               "shop essential","shop non essential","sport","transport","work","worship")) +
  guides(fill=guide_legend(title="Places")) +
  scale_x_discrete(name="Wave", labels=c("03/3-10/3","31/3-07/4","21/4-28/4","12/5-19/5", 
                                         "02/6-09/6","23/6-30/6","14/7-21/7","04/8-11/8"))+
  theme(axis.text.x = element_text(angle=90),plot.title = element_text(size=9),legend.position = "none", panel.background = element_blank()) +
  ggtitle("Intermediate populated areas")


```


```{r densely - non-household contacts}

sum_home_own <- c()  
sum_home_else <- c()
sum_work  <- c()
sum_worship  <- c()
sum_transport  <- c()
sum_school  <- c()
sum_shop_essential  <- c()
sum_shop_non_essential  <- c()
sum_entertainment  <- c()
sum_sport  <- c()
sum_park  <- c()
sum_healthcare  <- c()
sum_beauty  <- c()
sum_other  <- c()

## plot for densely
# sums up number of people met in a specific place in a specific wave 
# this is the rows for our dataframe (row = place, column = wave)
for(i in c(1:8)){
  # summing number of people met for each place in a specific wave
  # home own
  home_own<- rbind(sum(dummy_outside_densely$place_home_own[which(dummy_outside_densely$wave==i)]))
  sum_home_own <- cbind(sum_home_own,home_own)
  # home else
  
  home_else<- rbind(sum(dummy_outside_densely$place_home_else[which(dummy_outside_densely$wave==i)]))
  sum_home_else <- cbind(sum_home_else,home_else)
  # work (including group contacts)
  
  work_plot<- rbind(sum(dummy_outside_densely$place_home_else[which(dummy_outside_densely$wave==i)]))
  
  sum_work <- cbind(sum_work, work_plot)
  
  # worship
  
  worship_plot<- rbind(sum(dummy_outside_densely$place_worship[which(dummy_outside_densely$wave==i)]))
  sum_worship <- cbind(sum_worship,worship_plot)
  
  # transport
  
  transport_plot<- rbind(sum(dummy_outside_densely$place_transport[which(dummy_outside_densely$wave==i)]))
  sum_transport <- cbind(sum_transport,transport_plot)
  
  # school
  
  school_plot<- rbind(sum(dummy_outside_densely$place_school[which(dummy_outside_densely$wave==i)]))
  
  sum_school <- cbind(sum_school, school_plot)
  
  # shop essential
  
  shop_essential_plot<- rbind(sum(dummy_outside_densely$place_shop_essential[which(dummy_outside_densely$wave==i)]))
  sum_shop_essential <- cbind(sum_shop_essential,shop_essential_plot)
  
  # shop non essential
  
  shop_non_essential_plot<- rbind(sum(dummy_outside_densely$place_shop_non_essential[which(dummy_outside_densely$wave==i)]))
  sum_shop_non_essential <- cbind(sum_shop_non_essential,shop_non_essential_plot)
  
  # entertainment
  
  entertainment_plot<- rbind(sum(dummy_outside_densely$place_entertainment[which(dummy_outside_densely$wave==i)]))
  sum_entertainment <- cbind(sum_entertainment,entertainment_plot)
  
  # sport
  
  sport_plot<- rbind(sum(dummy_outside_densely$place_sport[which(dummy_outside_densely$wave==i)]))
  sum_sport <- cbind(sum_sport,sport_plot)
  
  # park
  
  park_plot<- rbind(sum(dummy_outside_densely$place_park[which(dummy_outside_densely$wave==i)]))
  sum_park <- cbind(sum_park,park_plot)
  
  # healthcare
  
  healthcare_plot<- rbind(sum(dummy_outside_densely$place_healthcare[which(dummy_outside_densely$wave==i)]))
  sum_healthcare <- cbind(sum_healthcare,healthcare_plot)
  
  # beauty
  
  beauty_plot<- rbind(sum(dummy_outside_densely$place_beauty[which(dummy_outside_densely$wave==i)]))
  sum_beauty <- cbind(sum_beauty,beauty_plot)
  
  # other
  
  other_plot<- rbind(sum(as.numeric(dummy_outside_densely$place_oth[which(dummy_outside_densely$wave==i)]),na.rm=TRUE))
  sum_other <- cbind(sum_other,other_plot)
  
 
  
  # collect all sums in data.frame 
  data_barplot_wide_densely <- as.data.frame(rbind(sum_home_own,sum_home_else, sum_work, sum_worship, sum_transport,sum_school,
                                                  sum_shop_essential,sum_shop_non_essential, sum_entertainment, sum_sport, 
                                                  sum_park, sum_healthcare,sum_beauty, sum_other))
  
  
}

# drop from global environment for next loop 

rm(sum_beauty,sum_entertainment,sum_healthcare,sum_home_else, sum_home_own,
   sum_other, sum_park, sum_school, sum_shop_essential, sum_shop_non_essential,
   sum_sport,sum_transport, sum_work, sum_worship)

# change column names to waves
colnames(data_barplot_wide_densely) <- c(1:8)

# add place variable as ID
places_plot <- c(places)
data_barplot_wide_densely$places <- places_plot

# change to long format
data_barplot_long_densely <- melt(setDT(data_barplot_wide_densely), 
                                 id.vars=c("places"), variable.name="wave")


# plot
plot_version2_densely_outside <- ggplot(data_barplot_long_densely, aes(x= wave, y=value, fill=places)) +
  geom_bar(position="fill", stat="identity")+
  scale_y_continuous(labels=scales::percent_format())+ scale_fill_manual(values = col,labels=c("beauty","entertainment","healthcare","home_else","home_own","other","park","school", 
                                                                                               "shop essential","shop non essential","sport","transport","work","worship")) +
  guides(fill=guide_legend(title="Places")) +
  scale_x_discrete(name="Wave", labels=c("03/3-10/3","31/3-07/4","21/4-28/4","12/5-19/5", 
                                         "02/6-09/6","23/6-30/6","14/7-21/7","04/8-11/8"))+
  theme(axis.text.x = element_text(angle=90),plot.title = element_text(size=9),legend.position = "none",panel.background = element_blank()) +
  ggtitle("Densely populated areas")


```

```{r arrange plots version 2 non-household contacts}

# create mosaicplots and arrange them 

plot_0 <- ggplot(data_barplot_long_densely, aes(x= wave, y=value, fill=places)) +
  geom_bar(position="fill", stat="identity")+
             scale_y_continuous(labels=scales::percent_format())+ scale_fill_manual(values = col #,labels=c("beauty","entertainment","healthcare","home_else","home_own","other","park","school", 
#  "shop essential","shop non essential","sport","transport","work","worship")
) +
  guides(fill=guide_legend(title="Places", ncol = 2)) +
  scale_x_discrete(name="Wave", labels=c("03/3-10/3","31/3-07/4","21/4-28/4","12/5-19/5", 
                             "02/6-09/6","23/6-30/6","14/7-21/7","04/8-11/8"))+
  theme(axis.text.x = element_text(angle=90)) 



legend_version2_outside <- cowplot::get_legend(plot_0)


gridExtra::grid.arrange(plot_version2_densely_outside, plot_version2_intermediate_outside, 
             plot_version2_thinly_outside, legend_version2_outside,
             ncol=2,top = "Non-household contacts")

```

### Version 3: non household contacts with group contacts

```{r Version 3 thinly populated}
## plot for thinly

sum_home_own <- c()  
sum_home_else <- c()
sum_work  <- c()
sum_worship  <- c()
sum_transport  <- c()
sum_school  <- c()
sum_shop_essential  <- c()
sum_shop_non_essential  <- c()
sum_entertainment  <- c()
sum_sport  <- c()
sum_park  <- c()
sum_healthcare  <- c()
sum_beauty  <- c()
sum_other  <- c()
sum_else <- c()
# sums up number of people met in a specific place in a specific wave 
# this is the rows for our dataframe (row = place, column = wave)
for(i in c(1:8)){
  # summing number of people met for each place in a specific wave
  # home own
  home_own<- rbind(sum(dummy_outside_thinly$place_home_own[which(dummy_outside_thinly$wave==i)]))
 sum_home_own <- cbind(sum_home_own,home_own)
 # home else
 
 home_else<- rbind(sum(dummy_outside_thinly$place_home_else[which(dummy_outside_thinly$wave==i)]))
 sum_home_else <- cbind(sum_home_else,home_else)
 # work (including group contacts)
 
 work_plot<- rbind(sum(dummy_outside_thinly$place_home_else[which(dummy_outside_thinly$wave==i)])+
                     sum(dummy_hh_group_thinly$Q79a_age_met_work_u18[which(dummy_hh_group_thinly$wave_0==i)], na.rm = TRUE)+
                     sum(dummy_hh_group_thinly$Q79a_age_met_work_1864[which(dummy_hh_group_thinly$wave_0==i)],na.rm= TRUE)+
                     sum(dummy_hh_group_thinly$Q79a_age_met_work_o64[which(dummy_hh_group_thinly$wave_0==i)],na.rm=TRUE))
 
 sum_work <- cbind(sum_work, work_plot)
 
 # worship
 
worship_plot<- rbind(sum(dummy_outside_thinly$place_worship[which(dummy_outside_thinly$wave==i)]))
 sum_worship <- cbind(sum_worship,worship_plot)
 
 # transport
 
 transport_plot<- rbind(sum(dummy_outside_thinly$place_transport[which(dummy_outside_thinly$wave==i)]))
 sum_transport <- cbind(sum_transport,transport_plot)
 
 # school
 
 school_plot<- rbind(sum(dummy_outside_thinly$place_school[which(dummy_outside_thinly$wave==i)])+
                     sum(dummy_hh_group_thinly$Q80a_age_met_uni_u18[which(dummy_hh_group_thinly$wave_0==i)],na.rm=TRUE)+
                     sum(dummy_hh_group_thinly$Q80a_age_met_uni_1864[which(dummy_hh_group_thinly$wave_0==i)],na.rm=TRUE)+
                     sum(dummy_hh_group_thinly$Q80a_age_met_uni_o64[which(dummy_hh_group_thinly$wave_0==i)],na.rm=TRUE))
 
 sum_school <- cbind(sum_school, school_plot)
 
 # shop essential
 
 shop_essential_plot<- rbind(sum(dummy_outside_thinly$place_shop_essential[which(dummy_outside_thinly$wave==i)]))
 sum_shop_essential <- cbind(sum_shop_essential,shop_essential_plot)
 
 # shop non essential
 
 shop_non_essential_plot<- rbind(sum(dummy_outside_thinly$place_shop_non_essential[which(dummy_outside_thinly$wave==i)]))
 sum_shop_non_essential <- cbind(sum_shop_non_essential,shop_non_essential_plot)
 
 # entertainment
 
 entertainment_plot<- rbind(sum(dummy_outside_thinly$place_entertainment[which(dummy_outside_thinly$wave==i)]))
 sum_entertainment <- cbind(sum_entertainment,entertainment_plot)
 
 # sport
 
 sport_plot<- rbind(sum(dummy_outside_thinly$place_sport[which(dummy_outside_thinly$wave==i)]))
 sum_sport <- cbind(sum_sport,sport_plot)
 
# park
 
 park_plot<- rbind(sum(dummy_outside_thinly$place_park[which(dummy_outside_thinly$wave==i)]))
 sum_park <- cbind(sum_park,park_plot)
 
 # healthcare
 
 healthcare_plot<- rbind(sum(dummy_outside_thinly$place_healthcare[which(dummy_outside_thinly$wave==i)]))
 sum_healthcare <- cbind(sum_healthcare,healthcare_plot)
 
 # beauty
 
 beauty_plot<- rbind(sum(dummy_outside_thinly$place_beauty[which(dummy_outside_thinly$wave==i)]))
 sum_beauty <- cbind(sum_beauty,beauty_plot)
 
 # other
 
 other_plot<- rbind(sum(as.numeric(dummy_outside_thinly$place_oth[which(dummy_outside_thinly$wave==i)]),na.rm=TRUE))
 sum_other <- cbind(sum_other,other_plot)
 
 # else, group contacts met else, interprets NA as 0
 else_plot<- rbind(sum(dummy_hh_group_thinly$Q81a_age_met_else_u18[which(dummy_hh_group_thinly$wave==i)],na.rm=TRUE)+
                     sum(dummy_hh_group_thinly$Q81a_age_met_else_1864[which(dummy_hh_group_thinly$wave==i)],na.rm=TRUE)+
                     sum(dummy_hh_group_thinly$Q81a_age_met_else_o64[which(dummy_hh_group_thinly$wave==i)],na.rm=TRUE))
 
 sum_else <- cbind(sum_else, else_plot)
 
# collect all sums in data.frame 
data_barplot_wide_thinly <- as.data.frame(rbind(sum_home_own,sum_home_else, sum_work, sum_worship, sum_transport,sum_school,
                   sum_shop_essential,sum_shop_non_essential, sum_entertainment, sum_sport, 
                   sum_park, sum_healthcare,sum_beauty, sum_other, sum_else))


}

# drop from global environment for next loop 

rm(sum_beauty,sum_else,sum_entertainment,sum_healthcare,sum_home_else, sum_home_own,
   sum_other, sum_park, sum_school, sum_shop_essential, sum_shop_non_essential,
   sum_sport,sum_transport, sum_work, sum_worship)

# change column names to waves
colnames(data_barplot_wide_thinly) <- c(1:8)

# add place variable as ID
places_plot <- c(places, "place_else")
data_barplot_wide_thinly$places <- places_plot

# change to long format
data_barplot_long_thinly <- melt(setDT(data_barplot_wide_thinly), 
                                 id.vars=c("places"), variable.name="wave")


# plot
col1 <- brewer.pal(8, "Dark2") 
col2 <- brewer.pal(7,"Pastel2")
col <- cbind(col1,col2)
plot_version3_thinly <- ggplot(data_barplot_long_thinly, aes(x= wave, y=value, fill=places)) +
  geom_bar(position="fill", stat="identity")+
             scale_y_continuous(labels=scales::percent_format())+ scale_fill_manual(values = col,labels=c("beauty","else","entertainment","healthcare","home_else","home_own","other","park","school", 
  "shop essential","shop non essential","sport","transport","work","worship")) +
  guides(fill=guide_legend(title="Places")) +
  scale_x_discrete(name="Wave", labels=c("03/3-10/3","31/3-07/4","21/4-28/4","12/5-19/5", 
                             "02/6-09/6","23/6-30/6","14/7-21/7","04/8-11/8"))+
  ylab("Percentage")+
  theme(axis.text.x = element_text(angle=90),plot.title = element_text(size=9),legend.position = "none",panel.background = element_blank()) +
  ggtitle("Thinly populated areas")
 

```



```{r intermediate }
## plot for intermediate
# create empty vectors for places to store sums in loop, needed for loops 
sum_home_own <- c()  
sum_home_else <- c()
sum_work  <- c()
sum_worship  <- c()
sum_transport  <- c()
sum_school  <- c()
sum_shop_essential  <- c()
sum_shop_non_essential  <- c()
sum_entertainment  <- c()
sum_sport  <- c()
sum_park  <- c()
sum_healthcare  <- c()
sum_beauty  <- c()
sum_other  <- c()
sum_else  <- c()

for(i in c(1:8)){
  # summing number of people met for each place in a specific wave
  # home own
  home_own<- rbind(sum(dummy_outside_intermediate$place_home_own[which(dummy_outside_intermediate$wave==i)]))
 sum_home_own <- cbind(sum_home_own,home_own)
 # home else
 
 home_else<- rbind(sum(dummy_outside_intermediate$place_home_else[which(dummy_outside_intermediate$wave==i)]))
 sum_home_else <- cbind(sum_home_else,home_else)
 # work (including group contacts)
 
 work_plot<- rbind(sum(dummy_outside_intermediate$place_home_else[which(dummy_outside_intermediate$wave==i)])+
                     sum(dummy_hh_group_intermediate$Q79a_age_met_work_u18[which(dummy_hh_group_intermediate$wave_0==i)], na.rm = TRUE)+
                     sum(dummy_hh_group_intermediate$Q79a_age_met_work_1864[which(dummy_hh_group_intermediate$wave_0==i)],na.rm= TRUE)+
                     sum(dummy_hh_group_intermediate$Q79a_age_met_work_o64[which(dummy_hh_group_intermediate$wave_0==i)],na.rm=TRUE))
 
 sum_work <- cbind(sum_work, work_plot)
 
 # worship
 
worship_plot<- rbind(sum(dummy_outside_intermediate$place_worship[which(dummy_outside_intermediate$wave==i)]))
 sum_worship <- cbind(sum_worship,worship_plot)
 
 # transport
 
 transport_plot<- rbind(sum(dummy_outside_intermediate$place_transport[which(dummy_outside_intermediate$wave==i)]))
 sum_transport <- cbind(sum_transport,transport_plot)
 
 # school
 
 school_plot<- rbind(sum(dummy_outside_intermediate$place_school[which(dummy_outside_intermediate$wave==i)])+
                     sum(dummy_hh_group_intermediate$Q80a_age_met_uni_u18[which(dummy_hh_group_intermediate$wave_0==i)],na.rm=TRUE)+
                     sum(dummy_hh_group_intermediate$Q80a_age_met_uni_1864[which(dummy_hh_group_intermediate$wave_0==i)],na.rm=TRUE)+
                     sum(dummy_hh_group_intermediate$Q80a_age_met_uni_o64[which(dummy_hh_group_intermediate$wave_0==i)],na.rm=TRUE))
 
 sum_school <- cbind(sum_school, school_plot)
 
 # shop essential
 
 shop_essential_plot<- rbind(sum(dummy_outside_intermediate$place_shop_essential[which(dummy_outside_intermediate$wave==i)]))
 sum_shop_essential <- cbind(sum_shop_essential,shop_essential_plot)
 
 # shop non essential
 
 shop_non_essential_plot<- rbind(sum(dummy_outside_intermediate$place_shop_non_essential[which(dummy_outside_intermediate$wave==i)]))
 sum_shop_non_essential <- cbind(sum_shop_non_essential,shop_non_essential_plot)
 
 # entertainment
 
 entertainment_plot<- rbind(sum(dummy_outside_intermediate$place_entertainment[which(dummy_outside_intermediate$wave==i)]))
 sum_entertainment <- cbind(sum_entertainment,entertainment_plot)
 
 # sport
 
 sport_plot<- rbind(sum(dummy_outside_intermediate$place_sport[which(dummy_outside_intermediate$wave==i)]))
 sum_sport <- cbind(sum_sport,sport_plot)
 
# park
 
 park_plot<- rbind(sum(dummy_outside_intermediate$place_park[which(dummy_outside_intermediate$wave==i)]))
 sum_park <- cbind(sum_park,park_plot)
 
 # healthcare
 
 healthcare_plot<- rbind(sum(dummy_outside_intermediate$place_healthcare[which(dummy_outside_intermediate$wave==i)]))
 sum_healthcare <- cbind(sum_healthcare,healthcare_plot)
 
 # beauty
 
 beauty_plot<- rbind(sum(dummy_outside_intermediate$place_beauty[which(dummy_outside_intermediate$wave==i)]))
 sum_beauty <- cbind(sum_beauty,beauty_plot)
 
 # other
 
 other_plot<- rbind(sum(as.numeric(dummy_outside_intermediate$place_oth[which(dummy_outside_intermediate$wave==i)]),na.rm=TRUE))
 sum_other <- cbind(sum_other,other_plot)
 
 # else, group contacts met else, interprets NA as 0
 else_plot<- rbind(sum(dummy_hh_group_intermediate$Q81a_age_met_else_u18[which(dummy_hh_group_intermediate$wave==i)],na.rm=TRUE)+
                     sum(dummy_hh_group_intermediate$Q81a_age_met_else_1864[which(dummy_hh_group_intermediate$wave==i)],na.rm=TRUE)+
                     sum(dummy_hh_group_intermediate$Q81a_age_met_else_o64[which(dummy_hh_group_intermediate$wave==i)],na.rm=TRUE))
 
 sum_else <- cbind(sum_else, else_plot)
 
# collect all sums in data.frame 
data_barplot_wide_intermediate <- as.data.frame(rbind(sum_home_own,sum_home_else, sum_work, sum_worship, sum_transport,sum_school,
                   sum_shop_essential,sum_shop_non_essential, sum_entertainment, sum_sport, 
                   sum_park, sum_healthcare,sum_beauty, sum_other, sum_else))


}

# drop from global environment for next loop 

rm(sum_beauty,sum_else,sum_entertainment,sum_healthcare,sum_home_else, sum_home_own,
   sum_other, sum_park, sum_school, sum_shop_essential, sum_shop_non_essential,
   sum_sport,sum_transport, sum_work, sum_worship)

# change column names to waves
colnames(data_barplot_wide_intermediate) <- c(1:8)

# add place variable as ID
data_barplot_wide_intermediate$places <- places_plot

# change to long format
data_barplot_long_intermediate <- melt(setDT(data_barplot_wide_intermediate), 
                                 id.vars=c("places"), variable.name="wave")


# plot
plot_version3_intermediate <- ggplot(data_barplot_long_intermediate, aes(x= wave, y=value, fill=places)) +
  geom_bar(position="fill", stat="identity")+
             scale_y_continuous(labels=scales::percent_format())+ scale_fill_manual(values = col,labels=c("beauty","else","entertainment","healthcare","home_else","home_own","other","park","school", 
  "shop essential","shop non essential","sport","transport","work","worship")) +
  guides(fill=guide_legend(title="Places")) +
  scale_x_discrete(name="Wave", labels=c("03/3-10/3","31/3-07/4","21/4-28/4","12/5-19/5", 
                             "02/6-09/6","23/6-30/6","14/7-21/7","04/8-11/8"))+
  ylab("Percentage")+
  theme(axis.text.x = element_text(angle=90),plot.title = element_text(size=9),legend.position = "none",panel.background = element_blank()) +
  ggtitle("Intermediate populated areas")


 

```


```{r densely populated}
## plot for densely
# create empty vectors for places to store sums in loop, needed for loops 
sum_home_own <- c()  
sum_home_else <- c()
sum_work  <- c()
sum_worship  <- c()
sum_transport  <- c()
sum_school  <- c()
sum_shop_essential  <- c()
sum_shop_non_essential  <- c()
sum_entertainment  <- c()
sum_sport  <- c()
sum_park  <- c()
sum_healthcare  <- c()
sum_beauty  <- c()
sum_other  <- c()
sum_else  <- c()

for(i in c(1:8)){
  # summing number of people met for each place in a specific wave
  # home own
  home_own<- rbind(sum(dummy_outside_densely$place_home_own[which(dummy_outside_densely$wave==i)]))
 sum_home_own <- cbind(sum_home_own,home_own)
 # home else
 
 home_else<- rbind(sum(dummy_outside_densely$place_home_else[which(dummy_outside_densely$wave==i)]))
 sum_home_else <- cbind(sum_home_else,home_else)
 # work (including group contacts)
 
 work_plot<- rbind(sum(dummy_outside_densely$place_home_else[which(dummy_outside_densely$wave==i)])+
                     sum(dummy_hh_group_densely$Q79a_age_met_work_u18[which(dummy_hh_group_densely$wave_0==i)], na.rm = TRUE)+
                     sum(dummy_hh_group_densely$Q79a_age_met_work_1864[which(dummy_hh_group_densely$wave_0==i)],na.rm= TRUE)+
                     sum(dummy_hh_group_densely$Q79a_age_met_work_o64[which(dummy_hh_group_densely$wave_0==i)],na.rm=TRUE))
 
 sum_work <- cbind(sum_work, work_plot)
 
 # worship
 
worship_plot<- rbind(sum(dummy_outside_densely$place_worship[which(dummy_outside_densely$wave==i)]))
 sum_worship <- cbind(sum_worship,worship_plot)
 
 # transport
 
 transport_plot<- rbind(sum(dummy_outside_densely$place_transport[which(dummy_outside_densely$wave==i)]))
 sum_transport <- cbind(sum_transport,transport_plot)
 
 # school
 
 school_plot<- rbind(sum(dummy_outside_densely$place_school[which(dummy_outside_densely$wave==i)])+
                     sum(dummy_hh_group_densely$Q80a_age_met_uni_u18[which(dummy_hh_group_densely$wave_0==i)],na.rm=TRUE)+
                     sum(dummy_hh_group_densely$Q80a_age_met_uni_1864[which(dummy_hh_group_densely$wave_0==i)],na.rm=TRUE)+
                     sum(dummy_hh_group_densely$Q80a_age_met_uni_o64[which(dummy_hh_group_densely$wave_0==i)],na.rm=TRUE))
 
 sum_school <- cbind(sum_school, school_plot)
 
 # shop essential
 
 shop_essential_plot<- rbind(sum(dummy_outside_densely$place_shop_essential[which(dummy_outside_densely$wave==i)]))
 sum_shop_essential <- cbind(sum_shop_essential,shop_essential_plot)
 
 # shop non essential
 
 shop_non_essential_plot<- rbind(sum(dummy_outside_densely$place_shop_non_essential[which(dummy_outside_densely$wave==i)]))
 sum_shop_non_essential <- cbind(sum_shop_non_essential,shop_non_essential_plot)
 
 # entertainment
 
 entertainment_plot<- rbind(sum(dummy_outside_densely$place_entertainment[which(dummy_outside_densely$wave==i)]))
 sum_entertainment <- cbind(sum_entertainment,entertainment_plot)
 
 # sport
 
 sport_plot<- rbind(sum(dummy_outside_densely$place_sport[which(dummy_outside_densely$wave==i)]))
 sum_sport <- cbind(sum_sport,sport_plot)
 
# park
 
 park_plot<- rbind(sum(dummy_outside_densely$place_park[which(dummy_outside_densely$wave==i)]))
 sum_park <- cbind(sum_park,park_plot)
 
 # healthcare
 
 healthcare_plot<- rbind(sum(dummy_outside_densely$place_healthcare[which(dummy_outside_densely$wave==i)]))
 sum_healthcare <- cbind(sum_healthcare,healthcare_plot)
 
 # beauty
 
 beauty_plot<- rbind(sum(dummy_outside_densely$place_beauty[which(dummy_outside_densely$wave==i)]))
 sum_beauty <- cbind(sum_beauty,beauty_plot)
 
 # other
 
 other_plot<- rbind(sum(as.numeric(dummy_outside_densely$place_oth[which(dummy_outside_densely$wave==i)]),na.rm=TRUE))
 sum_other <- cbind(sum_other,other_plot)
 
 # else, group contacts met else, interprets NA as 0
 else_plot<- rbind(sum(dummy_hh_group_densely$Q81a_age_met_else_u18[which(dummy_hh_group_densely$wave==i)],na.rm=TRUE)+
                     sum(dummy_hh_group_densely$Q81a_age_met_else_1864[which(dummy_hh_group_densely$wave==i)],na.rm=TRUE)+
                     sum(dummy_hh_group_densely$Q81a_age_met_else_o64[which(dummy_hh_group_densely$wave==i)],na.rm=TRUE))
 
 sum_else <- cbind(sum_else, else_plot)
 
# collect all sums in data.frame 
data_barplot_wide_densely <- as.data.frame(rbind(sum_home_own,sum_home_else, sum_work, sum_worship, sum_transport,sum_school,
                   sum_shop_essential,sum_shop_non_essential, sum_entertainment, sum_sport, 
                   sum_park, sum_healthcare,sum_beauty, sum_other, sum_else))


}

# drop from global environment for next loop 

rm(sum_beauty,sum_else,sum_entertainment,sum_healthcare,sum_home_else, sum_home_own,
   sum_other, sum_park, sum_school, sum_shop_essential, sum_shop_non_essential,
   sum_sport,sum_transport, sum_work, sum_worship)

# change column names to waves
colnames(data_barplot_wide_densely) <- c(1:8)

# add place variable as ID
places_plot <- c(places, "place_else")
data_barplot_wide_densely$places <- places_plot

# change to long format
data_barplot_long_densely <- melt(setDT(data_barplot_wide_densely), 
                                 id.vars=c("places"), variable.name="wave")


# plot
plot_version3_densely <- ggplot(data_barplot_long_densely, aes(x= wave, y=value, fill=places)) +
  geom_bar(position="fill", stat="identity")+
             scale_y_continuous(labels=scales::percent_format())+ scale_fill_manual(values = col,labels=c("beauty","else","entertainment","healthcare","home_else","home_own","other","park","school", 
  "shop essential","shop non essential","sport","transport","work","worship")) +
  guides(fill=guide_legend(title="Places")) +
  scale_x_discrete(name="Wave", labels=c("03/3-10/3","31/3-07/4","21/4-28/4","12/5-19/5", 
                             "02/6-09/6","23/6-30/6","14/7-21/7","04/8-11/8"))+
  ylab("Percentage")+
  theme(axis.text.x = element_text(angle=90),plot.title = element_text(size=9), legend.position = "none",panel.background = element_blank()) +
  ggtitle("Densely populated areas")

```



```{r arranging plots version 1}
# create mosaicplots and arrange them 


plot_0 <- ggplot(data_barplot_long_densely, aes(x= wave, y=value, fill=places)) +
  geom_bar(position="fill", stat="identity")+
             scale_y_continuous(labels=scales::percent_format())+ scale_fill_manual(values = col #,labels=c("beauty","else","entertainment","healthcare","home_else","home_own","other","park","school", 
#  "shop essential","shop non essential","sport","transport","work","worship")
) +
  guides(fill=guide_legend(title="Places", ncol = 2)) +
  scale_x_discrete(name="Wave", labels=c("03/3-10/3","31/3-07/4","21/4-28/4","12/5-19/5", 
                             "02/6-09/6","23/6-30/6","14/7-21/7","04/8-11/8"))+
  theme(axis.text.x = element_text(angle=90)) 


legend <- cowplot::get_legend(plot_0)

gridExtra::grid.arrange(plot_version3_densely, plot_version3_intermediate, 
             plot_version3_thinly, legend,
             ncol=2)

```


## Kontaktmatrizen

```{r preparation}
set.seed(12345678)

#cut
d_all_wide$Q79a_age_met_work_u18[d_all_wide$Q79a_age_met_work_u18>50] <- 50
d_all_wide$Q79a_age_met_work_1864[d_all_wide$Q79a_age_met_work_1864>50] <- 50
d_all_wide$Q79a_age_met_work_o64[d_all_wide$Q79a_age_met_work_o64>50] <- 50

d_all_wide$Q80a_age_met_uni_u18[d_all_wide$Q80a_age_met_uni_u18>50] <- 50
d_all_wide$Q80a_age_met_uni_1864[d_all_wide$Q80a_age_met_uni_1864>50] <- 50
d_all_wide$Q80a_age_met_uni_o64[d_all_wide$Q80a_age_met_uni_o64>50] <- 50

d_all_wide$Q81a_age_met_else_u18[d_all_wide$Q81a_age_met_else_u18>50] <- 50
d_all_wide$Q81a_age_met_else_1864[d_all_wide$Q81a_age_met_else_1864>50] <- 50
d_all_wide$Q81a_age_met_else_o64[d_all_wide$Q81a_age_met_else_o64>50] <- 50

# Gruppenkontakte sollen zu non-hh contacts hinzugefügt werden, weil die non-hh
# contacts in der Kontaktmatrize mitbeachtet werden sollen (Gruppenkontakte zeigt Anzahl der Kontakte, jetzt werden daraus einzelne Observations gemacht)
# alle Beobachtungen, cut n=50
df_work_u18 <- d_all_wide %>% 
  filter(Q79a_age_met_work_u18 > 0 & Q79a_age_met_work_u18< 51) %>% 
  select(new_id, wave,age_group, Q79a_age_met_work_u18)
group_contacts_work_u18 <- df_work_u18 %>% uncount(Q79a_age_met_work_u18, TRUE)
group_contacts_work_u18$age_group <- sample(0:17,1306, rep=TRUE)
                   
df_work_1864 <- d_all_wide %>% 
  filter(Q79a_age_met_work_1864> 0 & Q79a_age_met_work_1864< 51) %>% 
  select(new_id, wave,age_group, Q79a_age_met_work_1864) 
group_contacts_work_1864 <- df_work_1864 %>% uncount(Q79a_age_met_work_1864, TRUE)
group_contacts_work_1864$age_group <- sample(18:64,4781, rep=TRUE)

df_work_o64 <- d_all_wide %>% 
  filter(Q79a_age_met_work_o64> 0 & Q79a_age_met_work_o64< 51) %>% 
  select(new_id, wave,age_group, Q79a_age_met_work_o64)
group_contacts_work_o64 <- df_work_o64 %>% uncount(Q79a_age_met_work_o64, TRUE)
group_contacts_work_o64$age_group <- sample(65:90,1585, rep=TRUE)

df_school_u18 <- d_all_wide %>%
  filter(Q80a_age_met_uni_u18  > 0 & Q80a_age_met_uni_u18 < 51) %>% 
  select(new_id, wave,age_group, Q80a_age_met_uni_u18)
group_contacts_school_u18 <- df_school_u18 %>% uncount(Q80a_age_met_uni_u18, TRUE)
group_contacts_school_u18$age_group <- sample(0:17,2201, rep=TRUE)

df_school_1864 <- d_all_wide %>%
  filter(Q80a_age_met_uni_1864  > 0 & Q80a_age_met_uni_1864 < 51) %>% 
  select(new_id, wave,age_group, Q80a_age_met_uni_1864)
group_contacts_school_1864 <- df_school_1864 %>% uncount(Q80a_age_met_uni_1864, TRUE)
group_contacts_school_1864$age_group <- sample(18:64,819, rep=TRUE)

df_school_o64 <- d_all_wide %>%
  filter(Q80a_age_met_uni_o64  > 0 & Q80a_age_met_uni_o64 < 51) %>% 
  select(new_id, wave,age_group, Q80a_age_met_uni_o64)
group_contacts_school_o64 <- df_school_o64 %>% uncount(Q80a_age_met_uni_o64, TRUE)
group_contacts_school_o64$age_group <- sample(64:90,188, rep=TRUE)


df_else_u18 <- d_all_wide %>%
  filter(Q81a_age_met_else_u18 > 0 & Q81a_age_met_else_u18 < 51) %>% 
  select(new_id, wave,age_group, Q81a_age_met_else_u18)
group_contacts_else_u18 <- df_else_u18 %>% uncount(Q81a_age_met_else_u18, TRUE)
group_contacts_else_u18$age_group <- sample(0:17,1210, rep=TRUE)

df_else_1864 <- d_all_wide %>%
  filter(Q81a_age_met_else_1864 > 0 & Q81a_age_met_else_1864 < 51) %>% 
  select(new_id, wave,age_group, Q81a_age_met_else_1864)
group_contacts_else_1864 <- df_else_1864 %>% uncount(Q81a_age_met_else_1864, TRUE)
group_contacts_else_1864$age_group <- sample(18:64,2984, rep=TRUE)

df_else_o64 <- d_all_wide %>%
  filter(Q81a_age_met_else_o64 > 0 & Q81a_age_met_else_o64 < 51) %>% 
select(new_id, wave,age_group, Q81a_age_met_else_o64)
group_contacts_else_o64 <- df_else_o64 %>% uncount(Q81a_age_met_else_o64, TRUE)
group_contacts_else_o64$age_group <- sample(64:90,910, rep=TRUE)


group_list <- list(group_contacts_work_u18, group_contacts_work_1864, group_contacts_work_o64, 
     group_contacts_school_u18, group_contacts_school_1864, group_contacts_school_o64,
     group_contacts_else_u18, group_contacts_else_1864, group_contacts_else_o64)

group_contacts_all <- bind_rows(group_list)

keeps <- c("new_id","wave", "age_group")  
dt_outside_all_2 <- dt_outside_all[ , keeps, drop = FALSE]


# final (mit dieser Variable arbeiten!)
dt_outside_all_c <- rbind(dt_outside_all_2, group_contacts_all)

#rename waves
dt_outside_all_c$wave[dt_outside_all_c$wave== "Wave 1"] <- 1
dt_outside_all_c$wave[dt_outside_all_c$wave== "Wave 2"] <- 2
dt_outside_all_c$wave[dt_outside_all_c$wave== "Wave 3"] <- 3
dt_outside_all_c$wave[dt_outside_all_c$wave== "Wave 4"] <- 4
dt_outside_all_c$wave[dt_outside_all_c$wave== "Wave 5"] <- 5
dt_outside_all_c$wave[dt_outside_all_c$wave== "Wave 6"] <- 6
dt_outside_all_c$wave[dt_outside_all_c$wave== "Wave 7"] <- 7
dt_outside_all_c$wave[dt_outside_all_c$wave== "Wave 8"] <- 8


dt_outside_all_c$age_group[dt_outside_all_c$age_group == 0] <- "<1"

dt_outside_all_c$age_group[dt_outside_all_c$age_group== 1 | dt_outside_all_c$age_group == 2 |
                           dt_outside_all_c$age_group== 3 | dt_outside_all_c$age_group== 4] <- "1-4"

dt_outside_all_c$age_group[dt_outside_all_c$age_group== 5 |dt_outside_all_c$age_group== 6 |
                             dt_outside_all_c$age_group== 7 |dt_outside_all_c$age_group== 8 |
                             dt_outside_all_c$age_group== 9 ] <- "5-9"

dt_outside_all_c$age_group[dt_outside_all_c$age_group== 10 | dt_outside_all_c$age_group== 11 |
                             dt_outside_all_c$age_group== 12 | dt_outside_all_c$age_group== 13 |
                             dt_outside_all_c$age_group== 14] <- "10-14"

dt_outside_all_c$age_group[dt_outside_all_c$age_group== 15 | dt_outside_all_c$age_group== 16 |
                             dt_outside_all_c$age_group== 17 | dt_outside_all_c$age_group== 18 |
                             dt_outside_all_c$age_group== 19 ] <- "15-19"

dt_outside_all_c$age_group[dt_outside_all_c$age_group== 20 | dt_outside_all_c$age_group== 21 |
                             dt_outside_all_c$age_group== 22 | dt_outside_all_c$age_group== 23 |
                             dt_outside_all_c$age_group== 24 ] <- "20-24"

dt_outside_all_c$age_group[dt_outside_all_c$age_group== 25 | dt_outside_all_c$age_group== 26 |
                             dt_outside_all_c$age_group== 27 | dt_outside_all_c$age_group== 28 |
                             dt_outside_all_c$age_group== 29 | dt_outside_all_c$age_group== 30 |
                             dt_outside_all_c$age_group== 31 | dt_outside_all_c$age_group== 32 |
                             dt_outside_all_c$age_group== 33 | dt_outside_all_c$age_group== 34]  <- "25-34"

dt_outside_all_c$age_group[dt_outside_all_c$age_group== 35 | dt_outside_all_c$age_group== 36 |
                             dt_outside_all_c$age_group== 37 | dt_outside_all_c$age_group== 38 |
                             dt_outside_all_c$age_group== 39 | dt_outside_all_c$age_group== 40 |
                             dt_outside_all_c$age_group== 41 | dt_outside_all_c$age_group== 42 |
                             dt_outside_all_c$age_group== 43 | dt_outside_all_c$age_group== 44 ] <- "35-44"

dt_outside_all_c$age_group[dt_outside_all_c$age_group== 45 | dt_outside_all_c$age_group== 46 |
                             dt_outside_all_c$age_group== 47 |dt_outside_all_c$age_group== 48 |
                             dt_outside_all_c$age_group== 49 | dt_outside_all_c$age_group== 50 |
                             dt_outside_all_c$age_group== 51 | dt_outside_all_c$age_group== 52 |
                             dt_outside_all_c$age_group== 53 | dt_outside_all_c$age_group== 54 ] <- "45-54"

dt_outside_all_c$age_group[dt_outside_all_c$age_group== 55 | dt_outside_all_c$age_group== 56 |
                             dt_outside_all_c$age_group== 57 | dt_outside_all_c$age_group== 58 |
                             dt_outside_all_c$age_group== 59 | dt_outside_all_c$age_group== 60 |
                             dt_outside_all_c$age_group== 61 | dt_outside_all_c$age_group== 62 |
                             dt_outside_all_c$age_group== 63 | dt_outside_all_c$age_group== 64 ] <- "55-64"

dt_outside_all_c$age_group[dt_outside_all_c$age_group== 65 | dt_outside_all_c$age_group== 66 |
                             dt_outside_all_c$age_group== 67 | dt_outside_all_c$age_group== 68 |
                             dt_outside_all_c$age_group== 69] <- "65-69"



dt_outside_all_c$age_group[dt_outside_all_c$age_group== 70 | dt_outside_all_c$age_group== 71 | dt_outside_all_c$age_group== 72 |
                             dt_outside_all_c$age_group== 73 | dt_outside_all_c$age_group== 74 ] <- "70-74"

dt_outside_all_c$age_group[dt_outside_all_c$age_group== 75 | dt_outside_all_c$age_group== 76 |
                             dt_outside_all_c$age_group== 77 | dt_outside_all_c$age_group== 78 |
                             dt_outside_all_c$age_group== 79 ] <- "75-79"

dt_outside_all_c$age_group[dt_outside_all_c$age_group== 80 |dt_outside_all_c$age_group== 81 | dt_outside_all_c$age_group== 82 |
                           dt_outside_all_c$age_group== 83 | dt_outside_all_c$age_group== 84] <- "80-84"

dt_outside_all_c$age_group[dt_outside_all_c$age_group== 85 | dt_outside_all_c$age_group== 86 |
                             dt_outside_all_c$age_group== 87 | dt_outside_all_c$age_group== 88 |
                             dt_outside_all_c$age_group== 89 | dt_outside_all_c$age_group== 90] <- "85+"

dt_outside_all_c$age_group[dt_outside_all_c$age_group=="85 years or older"] <- "85+" 
dt_outside_all_c$age_group[dt_outside_all_c$age_group == "Under 1"] <- "<1" 

dt_outside_all_c_between <- dt_outside_all_c

dt_hh_all2 <- dt_hh_all %>% 
  select(new_id, wave, age_group)

dt_hh_all2$age_group[dt_hh_all2$age_group=="85 years or older"] <- "85+" 
dt_hh_all2$age_group[dt_hh_all2$age_group == "Under 1"] <- "<1" 

#check
unique(dt_hh_all2$age_group) 
table1::table1(~age_group  |wave, dt_hh_all2, caption = "Check")
table1::table1(~age_group  |wave, dt_outside_all_c_between, caption = "Check")


dt_outside_all_c <- rbind(dt_outside_all_c_between, dt_hh_all2)

#check
unique(dt_outside_all_c$age_group) 
table1::table1(~age_group  |wave, dt_outside_all_c, caption = "Check")
View(dt_outside_all_c)
```


### Densely

```{r contact matrix for wave 1 densely}
# Variablen, die wir brauchen
part_matrix_den1 <- d_self_all %>% 
  select(new_id, wave, age_group, qmktsize_16_0)%>%
  filter(qmktsize_16_0=="Densely populated area" & wave == 1)

# Variablen, die wir brauchen
out_contacts_den1 <- dt_outside_all_c_between %>% 
  select(new_id, wave, age_group) %>%
  filter(wave==1)

# df bearbeiten
part_matrix_den1 <- part_matrix_den1 %>% mutate(part_age =
                                                  case_when(age_group == "Under 1" ~ 2,
                                                            age_group == "1-4" ~ 2, 
                                                            age_group == "5-9" ~ 2,
                                                            age_group == "10-14" ~ 2,
                                                            age_group == "15-19" ~ 17,
                                                            age_group == "20-24" ~ 22,
                                                            age_group == "25-34" ~ 30,
                                                            age_group == "35-44" ~ 40,
                                                            age_group == "45-54" ~ 50,
                                                            age_group == "55-64" ~ 60,
                                                            age_group == "65-69" ~ 67,
                                                            age_group == "70-74" ~ 67,
                                                            age_group == "75-79" ~ 82,
                                                            age_group == "80-84" ~ 82,
                                                            age_group == "85 years or older" ~ 82))

out_contacts_den1 <- out_contacts_den1 %>% mutate(cnt_age =
                                                    case_when(age_group == "Under 1" ~ 2,
                                                              age_group == "1-4" ~ 2, 
                                                              age_group == "5-9" ~ 2,
                                                              age_group == "10-14" ~ 2,
                                                              age_group == "15-19" ~ 17,
                                                              age_group == "20-24" ~ 22,
                                                              age_group == "25-34" ~ 30,
                                                              age_group == "35-44" ~ 40,
                                                              age_group == "45-54" ~ 50,
                                                              age_group == "55-64" ~ 60,
                                                              age_group == "65-69" ~ 67,
                                                              age_group == "70-74" ~ 67,
                                                              age_group == "75-79" ~ 82,
                                                              age_group == "80-84" ~ 82,
                                                              age_group == "85 years or older" ~ 82))


# für socialmixR Variable in part_id ändern, survey_object erstellen
# part_id und survey_object werden für die Kontaktmatrix benötigt
colnames(part_matrix_den1)[1] <- "part_id"
colnames(out_contacts_den1)[1] <- "part_id" #out_contacts = non_hh-contacts
survey_object_den1  <- survey(part_matrix_den1, out_contacts_den1)

# contact matrix erstellen
cm_den1 <- contact_matrix(survey_object_den1, age.limits = c(0, 14, 19, 24, 34, 44, 54, 64, 75),  missing.participant.age = c("remove"),
                          missing.contact.age = c("remove"), symmetric = TRUE)

# output gespeichert, data_cm wird für heatmap benötigt
data_cm_den1 <- cm_den1$matrix

#Plot
melt_cm_den1 <- melt(data_cm_den1)

melt_cm_den1$Var1[melt_cm_den1$Var1 ==  1] <- "0-14"
melt_cm_den1$Var1[melt_cm_den1$Var1 ==  2] <- "15-19"
melt_cm_den1$Var1[melt_cm_den1$Var1 ==  3] <- "20-24"
melt_cm_den1$Var1[melt_cm_den1$Var1 ==  4] <- "25-34"
melt_cm_den1$Var1[melt_cm_den1$Var1 ==  5] <- "35-44"
melt_cm_den1$Var1[melt_cm_den1$Var1 ==  6] <- "45-54"
melt_cm_den1$Var1[melt_cm_den1$Var1 ==  7] <- "55-64"
melt_cm_den1$Var1[melt_cm_den1$Var1 ==  8] <- "65-74"
melt_cm_den1$Var1[melt_cm_den1$Var1 ==  9] <- "75+"


levels(melt_cm_den1$contact.age.group) <- list("0-14" = "[0,14)", "15-19" = "[14,19)", 
                                               "20-24" = "[19,24)", "25-34" = "[24,34)", 
                                               "35-44" = "[34,44)", "45-54" = "[44,54)",
                                                "55-64" = "[54,64)", "65-74" = "[64,75)",
                                               "75+" = "75+")




aa1<- ggplot(melt_cm_den1, aes(contact.age.group, Var1)) +
  geom_tile(aes(fill = value)) +
  coord_equal() +
  ggtitle("Wave 1 in densely populated areas \n (03/03/21 - 10/03/21)") +
  geom_text(aes(label=sprintf("%0.1f", round(value, digits = 2))), size = 2) +
  scale_fill_gradient2(low = "#2000CD",
                       mid = "white",
                       high = "#D60C00", limits = c(0,4), midpoint = 2) +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20, title = "Contacts")) +
  theme(legend.position = "none")+
  theme(plot.title = element_text(size = 6, face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.y=element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.text.x = element_text(size = 5, angle = 90, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 5))




#library(cowplot)
legend_matrix<- ggplot(melt_cm_den1, aes(contact.age.group, Var1)) +
  geom_tile(aes(fill = value)) +
  coord_equal() +
  ggtitle("Wave 1 in densely populated areas") +
  geom_text(aes(label=sprintf("%0.1f", round(value, digits = 2))), size = 2) +
  scale_fill_gradient2(low = "#2000CD",
                       mid = "white",
                       high = "#D60C00", limits = c(0,4), midpoint = 2) +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 5, title = "Contacts")) +
  theme(legend.position = "right")+
  theme(plot.title = element_text(size = 6, face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank())+
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_text(size = 5)) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.text.x = element_text(size = 5, angle = 90, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 5))



one_legend <- cowplot::get_legend(legend_matrix)
```




```{r contact matrix wave 2 densely}
# Variablen, die wir brauchen
part_matrix_den2 <- d_self_all %>% 
  select(new_id, wave, age_group, qmktsize_16_0)%>%
  filter(qmktsize_16_0=="Densely populated area" & wave == 2)

# Variablen, die wir brauchen
out_contacts_den2 <- dt_outside_all_c_between %>% 
  select(new_id, wave, age_group) %>%
  filter(wave==2)

# df bearbeiten
part_matrix_den2 <- part_matrix_den2 %>% mutate(part_age =
                                                  case_when(age_group == "Under 1" ~ 2,
                                                            age_group == "1-4" ~ 2, 
                                                            age_group == "5-9" ~ 2,
                                                            age_group == "10-14" ~ 2,
                                                            age_group == "15-19" ~ 17,
                                                            age_group == "20-24" ~ 22,
                                                            age_group == "25-34" ~ 30,
                                                            age_group == "35-44" ~ 40,
                                                            age_group == "45-54" ~ 50,
                                                            age_group == "55-64" ~ 60,
                                                            age_group == "65-69" ~ 67,
                                                            age_group == "70-74" ~ 67,
                                                            age_group == "75-79" ~ 82,
                                                            age_group == "80-84" ~ 82,
                                                            age_group == "85 years or older" ~ 82))

out_contacts_den2 <- out_contacts_den2 %>% mutate(cnt_age =
                                                    case_when(age_group == "Under 1" ~ 2,
                                                              age_group == "1-4" ~ 2, 
                                                              age_group == "5-9" ~ 2,
                                                              age_group == "10-14" ~ 2,
                                                              age_group == "15-19" ~ 17,
                                                              age_group == "20-24" ~ 22,
                                                              age_group == "25-34" ~ 30,
                                                              age_group == "35-44" ~ 40,
                                                              age_group == "45-54" ~ 50,
                                                              age_group == "55-64" ~ 60,
                                                              age_group == "65-69" ~ 67,
                                                              age_group == "70-74" ~ 67,
                                                              age_group == "75-79" ~ 82,
                                                              age_group == "80-84" ~ 82,
                                                              age_group == "85 years or older" ~ 82))


# für socialmixR Variable in part_id ändern, survey_object erstellen
# part_id und survey_object werden für die Kontaktmatrix benötigt
colnames(part_matrix_den2)[1] <- "part_id"
colnames(out_contacts_den2)[1] <- "part_id" #out_contacts = non_hh-contacts
survey_object_den2  <- survey(part_matrix_den2, out_contacts_den2)

# contact matrix erstellen
cm_den2 <- contact_matrix(survey_object_den2, age.limits = c(0, 14, 19, 24, 34, 44, 54, 64, 75),  
                          missing.participant.age = c("remove"),
                          missing.contact.age = c("remove"),
                          symmetric = TRUE)

# output gespeichert, data_cm wird für heatmap benötigt
data_cm_den2 <- cm_den2$matrix

#Plot
melt_cm_den2 <- melt(data_cm_den2)

melt_cm_den2$Var1[melt_cm_den2$Var1 ==  1] <- "0-14"
melt_cm_den2$Var1[melt_cm_den2$Var1 ==  2] <- "15-19"
melt_cm_den2$Var1[melt_cm_den2$Var1 ==  3] <- "20-24"
melt_cm_den2$Var1[melt_cm_den2$Var1 ==  4] <- "25-34"
melt_cm_den2$Var1[melt_cm_den2$Var1 ==  5] <- "35-44"
melt_cm_den2$Var1[melt_cm_den2$Var1 ==  6] <- "45-54"
melt_cm_den2$Var1[melt_cm_den2$Var1 ==  7] <- "55-64"
melt_cm_den2$Var1[melt_cm_den2$Var1 ==  8] <- "65-74"
melt_cm_den2$Var1[melt_cm_den2$Var1 ==  9] <- "75+"

levels(melt_cm_den2$contact.age.group) <- list("0-14" = "[0,14)", "15-19" = "[14,19)", 
                                               "20-24" = "[19,24)", "25-34" = "[24,34)", 
                                               "35-44" = "[34,44)", "45-54" = "[44,54)",
                                               "55-64" = "[54,64)", "65-74" = "[64,75)",
                                               "75+" = "75+")

aa2<- ggplot(melt_cm_den2, aes(contact.age.group, Var1)) +
  geom_tile(aes(fill = value)) +
  coord_equal() +
  ggtitle("Wave 2 in densely populated areas \n (31/3/21 - 07/4/21)") +
  geom_text(aes(label=sprintf("%0.1f", round(value, digits = 2))), size = 2) +
  scale_fill_gradient2(low = "#2000CD",
                       mid = "white",
                       high = "#D60C00", limits = c(0,4), midpoint = 2) +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20, title = "Contacts")) +
  theme(legend.position = "none")+
  theme(plot.title = element_text(size = 6, face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.text.x = element_text(size = 5, angle = 90, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 5))

```

```{r contact matrix wave 3 densely}

part_matrix_den3 <- d_self_all %>% 
  select(new_id, wave, age_group, qmktsize_16_0)%>%
  filter(qmktsize_16_0=="Densely populated area" & wave == 3)

# Variablen, die wir brauchen
out_contacts_den3 <- dt_outside_all_c_between %>% 
  select(new_id, wave, age_group) %>%
  filter(wave==3)

# df bearbeiten
part_matrix_den3 <- part_matrix_den3 %>% mutate(part_age =
                                                  case_when(age_group == "Under 1" ~ 2,
                                                            age_group == "1-4" ~ 2, 
                                                            age_group == "5-9" ~ 2,
                                                            age_group == "10-14" ~ 2,
                                                            age_group == "15-19" ~ 17,
                                                            age_group == "20-24" ~ 22,
                                                            age_group == "25-34" ~ 30,
                                                            age_group == "35-44" ~ 40,
                                                            age_group == "45-54" ~ 50,
                                                            age_group == "55-64" ~ 60,
                                                            age_group == "65-69" ~ 67,
                                                            age_group == "70-74" ~ 67,
                                                            age_group == "75-79" ~ 82,
                                                            age_group == "80-84" ~ 82,
                                                            age_group == "85 years or older" ~ 82))
out_contacts_den3 <- out_contacts_den3 %>% mutate(cnt_age =
                                                    case_when(age_group == "Under 1" ~ 2,
                                                              age_group == "1-4" ~ 2, 
                                                              age_group == "5-9" ~ 2,
                                                              age_group == "10-14" ~ 2,
                                                              age_group == "15-19" ~ 17,
                                                              age_group == "20-24" ~ 22,
                                                              age_group == "25-34" ~ 30,
                                                              age_group == "35-44" ~ 40,
                                                              age_group == "45-54" ~ 50,
                                                              age_group == "55-64" ~ 60,
                                                              age_group == "65-69" ~ 67,
                                                              age_group == "70-74" ~ 67,
                                                              age_group == "75-79" ~ 82,
                                                              age_group == "80-84" ~ 82,
                                                              age_group == "85 years or older" ~ 82))

# für socialmixR Variable in part_id ändern, survey_object erstellen
# part_id und survey_object werden für die Kontaktmatrix benötigt
colnames(part_matrix_den3)[1] <- "part_id"
colnames(out_contacts_den3)[1] <- "part_id" #out_contacts = non_hh-contacts
survey_object_den3  <- survey(part_matrix_den3, out_contacts_den3)

# contact matrix erstellen
cm_den3 <- contact_matrix(survey_object_den3, age.limits = c(0, 14, 19, 24, 34, 44, 54, 64, 75),  missing.participant.age = c("remove"),
                          missing.contact.age = c("remove"),
                          symmetric = TRUE)

# output gespeichert, data_cm wird für heatmap benötigt
data_cm_den3 <- cm_den3$matrix

#Plot
melt_cm_den3 <- melt(data_cm_den3)

melt_cm_den3$Var1[melt_cm_den3$Var1 ==  1] <- "0-14"
melt_cm_den3$Var1[melt_cm_den3$Var1 ==  2] <- "15-19"
melt_cm_den3$Var1[melt_cm_den3$Var1 ==  3] <- "20-24"
melt_cm_den3$Var1[melt_cm_den3$Var1 ==  4] <- "25-34"
melt_cm_den3$Var1[melt_cm_den3$Var1 ==  5] <- "35-44"
melt_cm_den3$Var1[melt_cm_den3$Var1 ==  6] <- "45-54"
melt_cm_den3$Var1[melt_cm_den3$Var1 ==  7] <- "55-64"
melt_cm_den3$Var1[melt_cm_den3$Var1 ==  8] <- "65-74"
melt_cm_den3$Var1[melt_cm_den3$Var1 ==  9] <- "75+"

levels(melt_cm_den3$contact.age.group) <- list("0-14" = "[0,14)", "15-19" = "[14,19)", 
                                               "20-24" = "[19,24)", "25-34" = "[24,34)", 
                                               "35-44" = "[34,44)", "45-54" = "[44,54)",
                                               "55-64" = "[54,64)", "65-74" = "[64,75)",
                                               "75+" = "75+")

aa3<- ggplot(melt_cm_den3, aes(contact.age.group, Var1)) +
  geom_tile(aes(fill = value)) +
  coord_equal() +
  ggtitle("Wave 3 in densely populated areas \n (21/4/21 - 28/4/21)") +
  geom_text(aes(label=sprintf("%0.1f", round(value, digits = 2))), size = 2) +
  scale_fill_gradient2(low = "#2000CD",
                       mid = "white",
                       high = "#D60C00", limits = c(0,4), midpoint = 2) +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20, title = "Contacts")) +
  theme(legend.position = "none")+
  theme(plot.title = element_text(size = 6, face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.text.x = element_text(size = 5, angle = 90, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 5))

```

```{r contact matrix wave 4 densely}

# Variablen, die wir brauchen
part_matrix_den4 <- d_self_all %>% 
  select(new_id, wave, age_group, qmktsize_16_0)%>%
  filter(qmktsize_16_0=="Densely populated area" & wave == 4)

# Variablen, die wir brauchen
out_contacts_den4 <- dt_outside_all_c_between %>% 
  select(new_id, wave, age_group) %>%
  filter(wave==4)

# df bearbeiten
part_matrix_den4 <- part_matrix_den4 %>% mutate(part_age =
                                                  case_when(age_group == "Under 1" ~ 2,
                                                            age_group == "1-4" ~ 2, 
                                                            age_group == "5-9" ~ 2,
                                                            age_group == "10-14" ~ 2,
                                                            age_group == "15-19" ~ 17,
                                                            age_group == "20-24" ~ 22,
                                                            age_group == "25-34" ~ 30,
                                                            age_group == "35-44" ~ 40,
                                                            age_group == "45-54" ~ 50,
                                                            age_group == "55-64" ~ 60,
                                                            age_group == "65-69" ~ 67,
                                                            age_group == "70-74" ~ 67,
                                                            age_group == "75-79" ~ 82,
                                                            age_group == "80-84" ~ 82,
                                                            age_group == "85 years or older" ~ 82))

out_contacts_den4 <- out_contacts_den4 %>% mutate(cnt_age =
                                                    case_when(age_group == "Under 1" ~ 2,
                                                              age_group == "1-4" ~ 2, 
                                                              age_group == "5-9" ~ 2,
                                                              age_group == "10-14" ~ 2,
                                                              age_group == "15-19" ~ 17,
                                                              age_group == "20-24" ~ 22,
                                                              age_group == "25-34" ~ 30,
                                                              age_group == "35-44" ~ 40,
                                                              age_group == "45-54" ~ 50,
                                                              age_group == "55-64" ~ 60,
                                                              age_group == "65-69" ~ 67,
                                                              age_group == "70-74" ~ 67,
                                                              age_group == "75-79" ~ 82,
                                                              age_group == "80-84" ~ 82,
                                                              age_group == "85 years or older" ~ 82))

# für socialmixR Variable in part_id ändern, survey_object erstellen
# part_id und survey_object werden für die Kontaktmatrix benötigt
colnames(part_matrix_den4)[1] <- "part_id"
colnames(out_contacts_den4)[1] <- "part_id" #out_contacts = non_hh-contacts
survey_object_den4  <- survey(part_matrix_den4, out_contacts_den4)

# contact matrix erstellen
cm_den4 <- contact_matrix(survey_object_den4, age.limits = c(0, 14, 19, 24, 34, 44, 54, 64, 75),  missing.participant.age = c("remove"),
                          missing.contact.age = c("remove"),
                          symmetric = TRUE)

# output gespeichert, data_cm wird für heatmap benötigt
data_cm_den4 <- cm_den4$matrix

#Plot
melt_cm_den4 <- melt(data_cm_den4)

melt_cm_den4$Var1[melt_cm_den4$Var1 ==  1] <- "0-14"
melt_cm_den4$Var1[melt_cm_den4$Var1 ==  2] <- "15-19"
melt_cm_den4$Var1[melt_cm_den4$Var1 ==  3] <- "20-24"
melt_cm_den4$Var1[melt_cm_den4$Var1 ==  4] <- "25-34"
melt_cm_den4$Var1[melt_cm_den4$Var1 ==  5] <- "35-44"
melt_cm_den4$Var1[melt_cm_den4$Var1 ==  6] <- "45-54"
melt_cm_den4$Var1[melt_cm_den4$Var1 ==  7] <- "55-64"
melt_cm_den4$Var1[melt_cm_den4$Var1 ==  8] <- "65-74"
melt_cm_den4$Var1[melt_cm_den4$Var1 ==  9] <- "75+"

levels(melt_cm_den4$contact.age.group) <- list("0-14" = "[0,14)", "15-19" = "[14,19)", 
                                               "20-24" = "[19,24)", "25-34" = "[24,34)", 
                                               "35-44" = "[34,44)", "45-54" = "[44,54)",
                                               "55-64" = "[54,64)", "65-74" = "[64,75)",
                                               "75+" = "75+")

aa4<- ggplot(melt_cm_den4, aes(contact.age.group, Var1)) +
  geom_tile(aes(fill = value)) +
  coord_equal() +
  ggtitle("Wave 4 in densely populated areas \n (12/5/21 - 19/5/21)") +
  geom_text(aes(label=sprintf("%0.1f", round(value, digits = 2))), size = 2) +
  scale_fill_gradient2(low = "#2000CD",
                       mid = "white",
                       high = "#D60C00", limits = c(0,4), midpoint = 2) +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20, title = "Contacts")) +
  theme(legend.position = "none")+
  theme(plot.title = element_text(size = 6, face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.text.x = element_text(size = 5, angle = 90, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 5))


```

```{r contact matrix wave 5 densely}
# Variablen, die wir brauchen
part_matrix_den5 <- d_self_all %>% 
  select(new_id, wave, age_group, qmktsize_16_0)%>%
  filter(qmktsize_16_0=="Densely populated area" & wave == 5)

# Variablen, die wir brauchen
out_contacts_den5 <- dt_outside_all_c_between %>% 
  select(new_id, wave, age_group) %>%
  filter(wave==5)

# df bearbeiten
part_matrix_den5 <- part_matrix_den5 %>% mutate(part_age =
                                                  case_when(age_group == "Under 1" ~ 2,
                                                            age_group == "1-4" ~ 2, 
                                                            age_group == "5-9" ~ 2,
                                                            age_group == "10-14" ~ 2,
                                                            age_group == "15-19" ~ 17,
                                                            age_group == "20-24" ~ 22,
                                                            age_group == "25-34" ~ 30,
                                                            age_group == "35-44" ~ 40,
                                                            age_group == "45-54" ~ 50,
                                                            age_group == "55-64" ~ 60,
                                                            age_group == "65-69" ~ 67,
                                                            age_group == "70-74" ~ 67,
                                                            age_group == "75-79" ~ 82,
                                                            age_group == "80-84" ~ 82,
                                                            age_group == "85 years or older" ~ 82))

out_contacts_den5 <- out_contacts_den5 %>% mutate(cnt_age =
                                                    case_when(age_group == "Under 1" ~ 2,
                                                              age_group == "1-4" ~ 2, 
                                                              age_group == "5-9" ~ 2,
                                                              age_group == "10-14" ~ 2,
                                                              age_group == "15-19" ~ 17,
                                                              age_group == "20-24" ~ 22,
                                                              age_group == "25-34" ~ 30,
                                                              age_group == "35-44" ~ 40,
                                                              age_group == "45-54" ~ 50,
                                                              age_group == "55-64" ~ 60,
                                                              age_group == "65-69" ~ 67,
                                                              age_group == "70-74" ~ 67,
                                                              age_group == "75-79" ~ 82,
                                                              age_group == "80-84" ~ 82,
                                                              age_group == "85 years or older" ~ 82))


# für socialmixR Variable in part_id ändern, survey_object erstellen
# part_id und survey_object werden für die Kontaktmatrix benötigt
colnames(part_matrix_den5)[1] <- "part_id"
colnames(out_contacts_den5)[1] <- "part_id" #out_contacts = non_hh-contacts
survey_object_den5  <- survey(part_matrix_den5, out_contacts_den5)

# contact matrix erstellen
cm_den5 <- contact_matrix(survey_object_den5, age.limits = c(0, 14, 19, 24, 34, 44, 54, 64, 75),  missing.participant.age = c("remove"),
                          missing.contact.age = c("remove"),
                          symmetric = TRUE)

# output gespeichert, data_cm wird für heatmap benötigt
data_cm_den5 <- cm_den5$matrix

#Plot
melt_cm_den5 <- melt(data_cm_den5)

melt_cm_den5$Var1[melt_cm_den5$Var1 ==  1] <- "0-14"
melt_cm_den5$Var1[melt_cm_den5$Var1 ==  2] <- "15-19"
melt_cm_den5$Var1[melt_cm_den5$Var1 ==  3] <- "20-24"
melt_cm_den5$Var1[melt_cm_den5$Var1 ==  4] <- "25-34"
melt_cm_den5$Var1[melt_cm_den5$Var1 ==  5] <- "35-44"
melt_cm_den5$Var1[melt_cm_den5$Var1 ==  6] <- "45-54"
melt_cm_den5$Var1[melt_cm_den5$Var1 ==  7] <- "55-64"
melt_cm_den5$Var1[melt_cm_den5$Var1 ==  8] <- "65-74"
melt_cm_den5$Var1[melt_cm_den5$Var1 ==  9] <- "75+"


levels(melt_cm_den5$contact.age.group) <- list("0-14" = "[0,14)", "15-19" = "[14,19)", 
                                               "20-24" = "[19,24)", "25-34" = "[24,34)", 
                                               "35-44" = "[34,44)", "45-54" = "[44,54)",
                                               "55-64" = "[54,64)", "65-74" = "[64,75)",
                                               "75+" = "75+")


aa5<- ggplot(melt_cm_den5, aes(contact.age.group, Var1)) +
  geom_tile(aes(fill = value)) +
  coord_equal() +
  ggtitle("Wave 5 in densely populated areas \n (02/6/21 - 09/6/21)") +
  geom_text(aes(label=sprintf("%0.1f", round(value, digits = 2))), size = 2) +
  scale_fill_gradient2(low = "#2000CD",
                       mid = "white",
                       high = "#D60C00", limits = c(0,4), midpoint = 2) +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20, title = "Contacts")) +
  theme(legend.position = "none")+
  theme(plot.title = element_text(size = 6, face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.text.x = element_text(size = 5, angle = 90, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 5))

```

```{r contact matrix wave 6 densely}
part_matrix_den6 <- d_self_all %>% 
  select(new_id, wave, age_group, qmktsize_16_0)%>%
  filter(qmktsize_16_0=="Densely populated area" & wave == 6)

# Variablen, die wir brauchen
out_contacts_den6 <- dt_outside_all_c_between %>% 
  select(new_id, wave, age_group) %>%
  filter(wave==6)

# df bearbeiten
part_matrix_den6 <- part_matrix_den6 %>% mutate(part_age =
                                                  case_when(age_group == "Under 1" ~ 2,
                                                            age_group == "1-4" ~ 2, 
                                                            age_group == "5-9" ~ 2,
                                                            age_group == "10-14" ~ 2,
                                                            age_group == "15-19" ~ 17,
                                                            age_group == "20-24" ~ 22,
                                                            age_group == "25-34" ~ 30,
                                                            age_group == "35-44" ~ 40,
                                                            age_group == "45-54" ~ 50,
                                                            age_group == "55-64" ~ 60,
                                                            age_group == "65-69" ~ 67,
                                                            age_group == "70-74" ~ 67,
                                                            age_group == "75-79" ~ 82,
                                                            age_group == "80-84" ~ 82,
                                                            age_group == "85 years or older" ~ 82))

out_contacts_den6 <- out_contacts_den6 %>% mutate(cnt_age =
                                                    case_when(age_group == "Under 1" ~ 2,
                                                              age_group == "1-4" ~ 2, 
                                                              age_group == "5-9" ~ 2,
                                                              age_group == "10-14" ~ 2,
                                                              age_group == "15-19" ~ 17,
                                                              age_group == "20-24" ~ 22,
                                                              age_group == "25-34" ~ 30,
                                                              age_group == "35-44" ~ 40,
                                                              age_group == "45-54" ~ 50,
                                                              age_group == "55-64" ~ 60,
                                                              age_group == "65-69" ~ 67,
                                                              age_group == "70-74" ~ 67,
                                                              age_group == "75-79" ~ 82,
                                                              age_group == "80-84" ~ 82,
                                                              age_group == "85 years or older" ~ 82))


# für socialmixR Variable in part_id ändern, survey_object erstellen
# part_id und survey_object werden für die Kontaktmatrix benötigt
colnames(part_matrix_den6)[1] <- "part_id"
colnames(out_contacts_den6)[1] <- "part_id" #out_contacts = non_hh-contacts
survey_object_den6  <- survey(part_matrix_den6, out_contacts_den6)

# contact matrix erstellen
cm_den6 <- contact_matrix(survey_object_den6, age.limits = c(0, 14, 19, 24, 34, 44, 54, 64, 75),  missing.participant.age = c("remove"),
                          missing.contact.age = c("remove"),
                          symmetric = TRUE)

# output gespeichert, data_cm wird für heatmap benötigt
data_cm_den6 <- cm_den6$matrix

#Plot
melt_cm_den6 <- melt(data_cm_den6)

melt_cm_den6$Var1[melt_cm_den6$Var1 ==  1] <- "0-14"
melt_cm_den6$Var1[melt_cm_den6$Var1 ==  2] <- "15-19"
melt_cm_den6$Var1[melt_cm_den6$Var1 ==  3] <- "20-24"
melt_cm_den6$Var1[melt_cm_den6$Var1 ==  4] <- "25-34"
melt_cm_den6$Var1[melt_cm_den6$Var1 ==  5] <- "35-44"
melt_cm_den6$Var1[melt_cm_den6$Var1 ==  6] <- "45-54"
melt_cm_den6$Var1[melt_cm_den6$Var1 ==  7] <- "55-64"
melt_cm_den6$Var1[melt_cm_den6$Var1 ==  8] <- "65-74"
melt_cm_den6$Var1[melt_cm_den6$Var1 ==  9] <- "75+"

levels(melt_cm_den6$contact.age.group) <- list("0-14" = "[0,14)", "15-19" = "[14,19)", 
                                               "20-24" = "[19,24)", "25-34" = "[24,34)", 
                                               "35-44" = "[34,44)", "45-54" = "[44,54)",
                                               "55-64" = "[54,64)", "65-74" = "[64,75)",
                                               "75+" = "75+")

aa6<- ggplot(melt_cm_den6, aes(contact.age.group, Var1)) +
  geom_tile(aes(fill = value)) +
  coord_equal() +
  ggtitle("Wave 6 in densely populated areas \n (23/6/21 - 30/6/21)") +
  geom_text(aes(label=sprintf("%0.1f", round(value, digits = 2))), size = 2) +
  scale_fill_gradient2(low = "#2000CD",
                       mid = "white",
                       high = "#D60C00", limits = c(0,4), midpoint = 2) +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20, title = "Contacts")) +
  theme(legend.position = "none")+
  theme(plot.title = element_text(size = 6, face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.text.x = element_text(size = 5, angle = 90, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 5))

```

```{r contact matrix wave 7 densely}
part_matrix_den7 <- d_self_all %>% 
  select(new_id, wave, age_group, qmktsize_16_0)%>%
  filter(qmktsize_16_0=="Densely populated area" & wave == 7)

# Variablen, die wir brauchen
out_contacts_den7 <- dt_outside_all_c_between %>% 
  select(new_id, wave, age_group) %>%
  filter(wave==7)

# df bearbeiten
part_matrix_den7 <- part_matrix_den7 %>% mutate(part_age =
                                                  case_when(age_group == "Under 1" ~ 2,
                                                            age_group == "1-4" ~ 2, 
                                                            age_group == "5-9" ~ 2,
                                                            age_group == "10-14" ~ 2,
                                                            age_group == "15-19" ~ 17,
                                                            age_group == "20-24" ~ 22,
                                                            age_group == "25-34" ~ 30,
                                                            age_group == "35-44" ~ 40,
                                                            age_group == "45-54" ~ 50,
                                                            age_group == "55-64" ~ 60,
                                                            age_group == "65-69" ~ 67,
                                                            age_group == "70-74" ~ 67,
                                                            age_group == "75-79" ~ 82,
                                                            age_group == "80-84" ~ 82,
                                                            age_group == "85 years or older" ~ 82))

out_contacts_den7 <- out_contacts_den7 %>% mutate(cnt_age =
                                                    case_when(age_group == "Under 1" ~ 2,
                                                              age_group == "1-4" ~ 2, 
                                                              age_group == "5-9" ~ 2,
                                                              age_group == "10-14" ~ 2,
                                                              age_group == "15-19" ~ 17,
                                                              age_group == "20-24" ~ 22,
                                                              age_group == "25-34" ~ 30,
                                                              age_group == "35-44" ~ 40,
                                                              age_group == "45-54" ~ 50,
                                                              age_group == "55-64" ~ 60,
                                                              age_group == "65-69" ~ 67,
                                                              age_group == "70-74" ~ 67,
                                                              age_group == "75-79" ~ 82,
                                                              age_group == "80-84" ~ 82,
                                                              age_group == "85 years or older" ~ 82))



# für socialmixR Variable in part_id ändern, survey_object erstellen
# part_id und survey_object werden für die Kontaktmatrix benötigt
colnames(part_matrix_den7)[1] <- "part_id"
colnames(out_contacts_den7)[1] <- "part_id" #out_contacts = non_hh-contacts
survey_object_den7  <- survey(part_matrix_den7, out_contacts_den7)

# contact matrix erstellen
cm_den7 <- contact_matrix(survey_object_den7, age.limits = c(0, 14, 19, 24, 34, 44, 54, 64, 75),  missing.participant.age = c("remove"),
                          missing.contact.age = c("remove"),
                          symmetric = TRUE)

# output gespeichert, data_cm wird für heatmap benötigt
data_cm_den7 <- cm_den7$matrix

#Plot
melt_cm_den7 <- melt(data_cm_den7)

melt_cm_den7$Var1[melt_cm_den7$Var1 ==  1] <- "0-14"
melt_cm_den7$Var1[melt_cm_den7$Var1 ==  2] <- "15-19"
melt_cm_den7$Var1[melt_cm_den7$Var1 ==  3] <- "20-24"
melt_cm_den7$Var1[melt_cm_den7$Var1 ==  4] <- "25-34"
melt_cm_den7$Var1[melt_cm_den7$Var1 ==  5] <- "35-44"
melt_cm_den7$Var1[melt_cm_den7$Var1 ==  6] <- "45-54"
melt_cm_den7$Var1[melt_cm_den7$Var1 ==  7] <- "55-64"
melt_cm_den7$Var1[melt_cm_den7$Var1 ==  8] <- "65-74"
melt_cm_den7$Var1[melt_cm_den7$Var1 ==  9] <- "75+"

levels(melt_cm_den7$contact.age.group) <- list("0-14" = "[0,14)", "15-19" = "[14,19)", 
                                               "20-24" = "[19,24)", "25-34" = "[24,34)", 
                                               "35-44" = "[34,44)", "45-54" = "[44,54)",
                                               "55-64" = "[54,64)", "65-74" = "[64,75)",
                                               "75+" = "75+")


aa7<- ggplot(melt_cm_den7, aes(contact.age.group, Var1)) +
  geom_tile(aes(fill = value)) +
  coord_equal() +
  ggtitle("Wave 7 in densely populated areas \n (14/7/21 - 21/7/21)") +
  geom_text(aes(label=sprintf("%0.1f", round(value, digits = 2))), size = 2) +
  scale_fill_gradient2(low = "#2000CD",
                       mid = "white",
                       high = "#D60C00", limits = c(0,4), midpoint = 2) +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20, title = "Contacts")) +
  theme(legend.position = "none")+
  theme(plot.title = element_text(size = 6, face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.text.x = element_text(size = 5, angle = 90, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 5))

```

```{r contact matrix wave 8 densely}

part_matrix_den8 <- d_self_all %>% 
  select(new_id, wave, age_group, qmktsize_16_0)%>%
  filter(qmktsize_16_0=="Densely populated area" & wave == 8)

# Variablen, die wir brauchen
out_contacts_den8 <- dt_outside_all_c_between %>% 
  select(new_id, wave, age_group) %>%
  filter(wave==8)

# df bearbeiten
part_matrix_den8 <- part_matrix_den8 %>% mutate(part_age =
                                                  case_when(age_group == "Under 1" ~ 2,
                                                            age_group == "1-4" ~ 2, 
                                                            age_group == "5-9" ~ 2,
                                                            age_group == "10-14" ~ 2,
                                                            age_group == "15-19" ~ 17,
                                                            age_group == "20-24" ~ 22,
                                                            age_group == "25-34" ~ 30,
                                                            age_group == "35-44" ~ 40,
                                                            age_group == "45-54" ~ 50,
                                                            age_group == "55-64" ~ 60,
                                                            age_group == "65-69" ~ 67,
                                                            age_group == "70-74" ~ 67,
                                                            age_group == "75-79" ~ 82,
                                                            age_group == "80-84" ~ 82,
                                                            age_group == "85 years or older" ~ 82))

out_contacts_den8 <- out_contacts_den8 %>% mutate(cnt_age =
                                                    case_when(age_group == "Under 1" ~ 2,
                                                              age_group == "1-4" ~ 2, 
                                                              age_group == "5-9" ~ 2,
                                                              age_group == "10-14" ~ 2,
                                                              age_group == "15-19" ~ 17,
                                                              age_group == "20-24" ~ 22,
                                                              age_group == "25-34" ~ 30,
                                                              age_group == "35-44" ~ 40,
                                                              age_group == "45-54" ~ 50,
                                                              age_group == "55-64" ~ 60,
                                                              age_group == "65-69" ~ 67,
                                                              age_group == "70-74" ~ 67,
                                                              age_group == "75-79" ~ 82,
                                                              age_group == "80-84" ~ 82,
                                                              age_group == "85 years or older" ~ 82))

# für socialmixR Variable in part_id ändern, survey_object erstellen
# part_id und survey_object werden für die Kontaktmatrix benötigt
colnames(part_matrix_den8)[1] <- "part_id"
colnames(out_contacts_den8)[1] <- "part_id" #out_contacts = non_hh-contacts
survey_object_den8  <- survey(part_matrix_den8, out_contacts_den8)

# contact matrix erstellen
cm_den8 <- contact_matrix(survey_object_den8, age.limits = c(0, 14, 19, 24, 34, 44, 54, 64, 75),  missing.participant.age = c("remove"),
                          missing.contact.age = c("remove"),
                          symmetric = TRUE)

# output gespeichert, data_cm wird für heatmap benötigt
data_cm_den8 <- cm_den8$matrix

#Plot
melt_cm_den8 <- melt(data_cm_den8)

melt_cm_den8$Var1[melt_cm_den8$Var1 ==  1] <- "0-14"
melt_cm_den8$Var1[melt_cm_den8$Var1 ==  2] <- "15-19"
melt_cm_den8$Var1[melt_cm_den8$Var1 ==  3] <- "20-24"
melt_cm_den8$Var1[melt_cm_den8$Var1 ==  4] <- "25-34"
melt_cm_den8$Var1[melt_cm_den8$Var1 ==  5] <- "35-44"
melt_cm_den8$Var1[melt_cm_den8$Var1 ==  6] <- "45-54"
melt_cm_den8$Var1[melt_cm_den8$Var1 ==  7] <- "55-64"
melt_cm_den8$Var1[melt_cm_den8$Var1 ==  8] <- "65-74"
melt_cm_den8$Var1[melt_cm_den8$Var1 ==  9] <- "75+"

levels(melt_cm_den8$contact.age.group) <- list("0-14" = "[0,14)", "15-19" = "[14,19)", 
                                               "20-24" = "[19,24)", "25-34" = "[24,34)", 
                                               "35-44" = "[34,44)", "45-54" = "[44,54)",
                                               "55-64" = "[54,64)", "65-74" = "[64,75)",
                                               "75+" = "75+")

aa8<- ggplot(melt_cm_den8, aes(contact.age.group, Var1)) +
  geom_tile(aes(fill = value)) +
  coord_equal() +
  ggtitle("Wave 8 in densely populated areas \n (04/8/21 - 11/8/21)") +
  geom_text(aes(label=sprintf("%0.1f", round(value, digits = 2))), size = 2) +
  scale_fill_gradient2(low = "#2000CD",
                       mid = "white",
                       high = "#D60C00", limits = c(0,4), midpoint = 2) +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20, title = "Contacts")) +
  theme(legend.position = "none")+
  theme(plot.title = element_text(size = 6, face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.text.x = element_text(size = 5, angle = 90, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 5))



```


### Intermediate 
```{r contact matrix wave 1 intermediate}

# Variablen, die wir brauchen
part_matrix_int1 <- d_self_all %>% 
  select(new_id, wave, age_group, qmktsize_16_0)%>%
  filter(qmktsize_16_0=="Intermediate density area" & wave == 1)

# Variablen, die wir brauchen
out_contacts_int1 <- dt_outside_all_c_between %>% 
  select(new_id, wave, age_group) %>%
  filter(wave==1)

# df bearbeiten
part_matrix_int1 <- part_matrix_int1 %>% mutate(part_age =
                                                  case_when(age_group == "Under 1" ~ 2,
                                                            age_group == "1-4" ~ 2, 
                                                            age_group == "5-9" ~ 2,
                                                            age_group == "10-14" ~ 2,
                                                            age_group == "15-19" ~ 17,
                                                            age_group == "20-24" ~ 22,
                                                            age_group == "25-34" ~ 30,
                                                            age_group == "35-44" ~ 40,
                                                            age_group == "45-54" ~ 50,
                                                            age_group == "55-64" ~ 60,
                                                            age_group == "65-69" ~ 67,
                                                            age_group == "70-74" ~ 67,
                                                            age_group == "75-79" ~ 82,
                                                            age_group == "80-84" ~ 82,
                                                            age_group == "85 years or older" ~ 82))

out_contacts_int1 <- out_contacts_int1 %>% mutate(cnt_age =
                                                    case_when(age_group == "Under 1" ~ 2,
                                                              age_group == "1-4" ~ 2, 
                                                              age_group == "5-9" ~ 2,
                                                              age_group == "10-14" ~ 2,
                                                              age_group == "15-19" ~ 17,
                                                              age_group == "20-24" ~ 22,
                                                              age_group == "25-34" ~ 30,
                                                              age_group == "35-44" ~ 40,
                                                              age_group == "45-54" ~ 50,
                                                              age_group == "55-64" ~ 60,
                                                              age_group == "65-69" ~ 67,
                                                              age_group == "70-74" ~ 67,
                                                              age_group == "75-79" ~ 82,
                                                              age_group == "80-84" ~ 82,
                                                              age_group == "85 years or older" ~ 82))

# für socialmixR Variable in part_id ändern, survey_object erstellen
# part_id und survey_object werden für die Kontaktmatrix benötigt
colnames(part_matrix_int1)[1] <- "part_id"
colnames(out_contacts_int1)[1] <- "part_id" #out_contacts = non_hh-contacts
survey_object_int1  <- survey(part_matrix_int1, out_contacts_int1)

# contact matrix erstellen
cm_int1 <- contact_matrix(survey_object_int1, age.limits = c(0, 14, 19, 24, 34, 44, 54, 64, 75),  missing.participant.age = c("remove"),
                          missing.contact.age = c("remove"),
                          symmetric = TRUE)

# output gespeichert, data_cm wird für heatmap benötigt
data_cm_int1 <- cm_int1$matrix


#Plot
melt_cm_int1 <- melt(data_cm_int1)

melt_cm_int1$Var1[melt_cm_int1$Var1 ==  1] <- "0-14"
melt_cm_int1$Var1[melt_cm_int1$Var1 ==  2] <- "15-19"
melt_cm_int1$Var1[melt_cm_int1$Var1 ==  3] <- "20-24"
melt_cm_int1$Var1[melt_cm_int1$Var1 ==  4] <- "25-34"
melt_cm_int1$Var1[melt_cm_int1$Var1 ==  5] <- "35-44"
melt_cm_int1$Var1[melt_cm_int1$Var1 ==  6] <- "45-54"
melt_cm_int1$Var1[melt_cm_int1$Var1 ==  7] <- "55-64"
melt_cm_int1$Var1[melt_cm_int1$Var1 ==  8] <- "65-74"
melt_cm_int1$Var1[melt_cm_int1$Var1 ==  9] <- "75+"


levels(melt_cm_int1$contact.age.group) <- list("0-14" = "[0,14)", "15-19" = "[14,19)", 
                                               "20-24" = "[19,24)", "25-34" = "[24,34)", 
                                               "35-44" = "[34,44)", "45-54" = "[44,54)",
                                               "55-64" = "[54,64)", "65-74" = "[64,75)",
                                               "75+" = "75+")

bb1 <- ggplot(melt_cm_int1, aes(contact.age.group, Var1)) +
  geom_tile(aes(fill = value)) +
  coord_equal() +
  ggtitle("Wave 1 in intermediate density areas \n (03/3/21 - 10/3/21)") +
  geom_text(aes(label=sprintf("%0.1f", round(value, digits = 2))), size = 2) +
  scale_fill_gradient2(low = "#2000CD",
                       mid = "white",
                       high = "#D60C00", limits = c(0,4), midpoint = 2) +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20, title = "Contacts")) +
  theme(legend.position = "none")+
  theme(plot.title = element_text(size = 6, face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.text.x = element_text(size = 5, angle = 90, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 5))




```

```{r contact matrix wave 2 intermediate} 
part_matrix_int2 <- d_self_all %>% 
  select(new_id, wave, age_group, qmktsize_16_0)%>%
  filter(qmktsize_16_0=="Intermediate density area" & wave == 2)

# Variablen, die wir brauchen
out_contacts_int2 <- dt_outside_all_c_between %>% 
  select(new_id, wave, age_group) %>%
  filter(wave==2)

# df bearbeiten
part_matrix_int2 <- part_matrix_int2 %>% mutate(part_age =
                                                  case_when(age_group == "Under 1" ~ 2,
                                                            age_group == "1-4" ~ 2, 
                                                            age_group == "5-9" ~ 2,
                                                            age_group == "10-14" ~ 2,
                                                            age_group == "15-19" ~ 17,
                                                            age_group == "20-24" ~ 22,
                                                            age_group == "25-34" ~ 30,
                                                            age_group == "35-44" ~ 40,
                                                            age_group == "45-54" ~ 50,
                                                            age_group == "55-64" ~ 60,
                                                            age_group == "65-69" ~ 67,
                                                            age_group == "70-74" ~ 67,
                                                            age_group == "75-79" ~ 82,
                                                            age_group == "80-84" ~ 82,
                                                            age_group == "85 years or older" ~ 82))

out_contacts_int2 <- out_contacts_int2 %>% mutate(cnt_age =
                                                    case_when(age_group == "Under 1" ~ 2,
                                                              age_group == "1-4" ~ 2, 
                                                              age_group == "5-9" ~ 2,
                                                              age_group == "10-14" ~ 2,
                                                              age_group == "15-19" ~ 17,
                                                              age_group == "20-24" ~ 22,
                                                              age_group == "25-34" ~ 30,
                                                              age_group == "35-44" ~ 40,
                                                              age_group == "45-54" ~ 50,
                                                              age_group == "55-64" ~ 60,
                                                              age_group == "65-69" ~ 67,
                                                              age_group == "70-74" ~ 67,
                                                              age_group == "75-79" ~ 82,
                                                              age_group == "80-84" ~ 82,
                                                              age_group == "85 years or older" ~ 82))


# für socialmixR Variable in part_id ändern, survey_object erstellen
# part_id und survey_object werden für die Kontaktmatrix benötigt
colnames(part_matrix_int2)[1] <- "part_id"
colnames(out_contacts_int2)[1] <- "part_id" #out_contacts = non_hh-contacts
survey_object_int2  <- survey(part_matrix_int2, out_contacts_int2)

# contact matrix erstellen
cm_int2 <- contact_matrix(survey_object_int2, age.limits = c(0, 14, 19, 24, 34, 44, 54, 64, 75),  missing.participant.age = c("remove"),
                          missing.contact.age = c("remove"),
                          symmetric = TRUE)

# output gespeichert, data_cm wird für heatmap benötigt
data_cm_int2 <- cm_int2$matrix

#Plot
melt_cm_int2 <- melt(data_cm_int2)

melt_cm_int2$Var1[melt_cm_int2$Var1 ==  1] <- "0-14"
melt_cm_int2$Var1[melt_cm_int2$Var1==  2] <- "15-19"
melt_cm_int2$Var1[melt_cm_int2$Var1 ==  3] <- "20-24"
melt_cm_int2$Var1[melt_cm_int2$Var1 ==  4] <- "25-34"
melt_cm_int2$Var1[melt_cm_int2$Var1 ==  5] <- "35-44"
melt_cm_int2$Var1[melt_cm_int2$Var1 ==  6] <- "45-54"
melt_cm_int2$Var1[melt_cm_int2$Var1 ==  7] <- "55-64"
melt_cm_int2$Var1[melt_cm_int2$Var1==  8] <- "65-74"
melt_cm_int2$Var1[melt_cm_int2$Var1 ==  9] <- "75+"


levels(melt_cm_int2$contact.age.group) <- list("0-14" = "[0,14)", "15-19" = "[14,19)", 
                                               "20-24" = "[19,24)", "25-34" = "[24,34)", 
                                               "35-44" = "[34,44)", "45-54" = "[44,54)",
                                               "55-64" = "[54,64)", "65-74" = "[64,75)",
                                               "75+" = "75+")

bb2 <- ggplot(melt_cm_int2, aes(contact.age.group, Var1)) +
  geom_tile(aes(fill = value)) +
  coord_equal() +
  ggtitle("Wave 2 in intermediate density areas \n (31/3/21 - 07/4/21)") +
  geom_text(aes(label=sprintf("%0.1f", round(value, digits = 2))), size = 2) +
  scale_fill_gradient2(low = "#2000CD",
                       mid = "white",
                       high = "#D60C00", limits = c(0,4), midpoint = 2) +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20, title = "Contacts")) +
  theme(legend.position = "none")+
  theme(plot.title = element_text(size = 6, face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.text.x = element_text(size = 5, angle = 90, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 5))


```

```{r contact matrix wave 3 intermediate}
# Variablen, die wir brauchen
part_matrix_int3 <- d_self_all %>% 
  select(new_id, wave, age_group, qmktsize_16_0)%>%
  filter(qmktsize_16_0=="Intermediate density area" & wave == 3)

# Variablen, die wir brauchen
out_contacts_int3 <- dt_outside_all_c_between %>% 
  select(new_id, wave, age_group) %>%
  filter(wave==3)

# df bearbeiten
part_matrix_int3 <- part_matrix_int3 %>% mutate(part_age =
                                                  case_when(age_group == "Under 1" ~ 2,
                                                            age_group == "1-4" ~ 2, 
                                                            age_group == "5-9" ~ 2,
                                                            age_group == "10-14" ~ 2,
                                                            age_group == "15-19" ~ 17,
                                                            age_group == "20-24" ~ 22,
                                                            age_group == "25-34" ~ 30,
                                                            age_group == "35-44" ~ 40,
                                                            age_group == "45-54" ~ 50,
                                                            age_group == "55-64" ~ 60,
                                                            age_group == "65-69" ~ 67,
                                                            age_group == "70-74" ~ 67,
                                                            age_group == "75-79" ~ 82,
                                                            age_group == "80-84" ~ 82,
                                                            age_group == "85 years or older" ~ 82))

out_contacts_int3 <- out_contacts_int3 %>% mutate(cnt_age =
                                                    case_when(age_group == "Under 1" ~ 2,
                                                              age_group == "1-4" ~ 2, 
                                                              age_group == "5-9" ~ 2,
                                                              age_group == "10-14" ~ 2,
                                                              age_group == "15-19" ~ 17,
                                                              age_group == "20-24" ~ 22,
                                                              age_group == "25-34" ~ 30,
                                                              age_group == "35-44" ~ 40,
                                                              age_group == "45-54" ~ 50,
                                                              age_group == "55-64" ~ 60,
                                                              age_group == "65-69" ~ 67,
                                                              age_group == "70-74" ~ 67,
                                                              age_group == "75-79" ~ 82,
                                                              age_group == "80-84" ~ 82,
                                                              age_group == "85 years or older" ~ 82))


# für socialmixR Variable in part_id ändern, survey_object erstellen
# part_id und survey_object werden für die Kontaktmatrix benötigt
colnames(part_matrix_int3)[1] <- "part_id"
colnames(out_contacts_int3)[1] <- "part_id" #out_contacts = non_hh-contacts
survey_object_int3  <- survey(part_matrix_int3, out_contacts_int3)

# contact matrix erstellen
cm_int3 <- contact_matrix(survey_object_int3, age.limits = c(0, 14, 19, 24, 34, 44, 54, 64, 75),  missing.participant.age = c("remove"),
                          missing.contact.age = c("remove"),
                          symmetric = TRUE)

# output gespeichert, data_cm wird für heatmap benötigt
data_cm_int3 <- cm_int3$matrix

#Plot
melt_cm_int3 <- melt(data_cm_int3)

melt_cm_int3$Var1[melt_cm_int3$Var1 ==  1] <- "0-14"
melt_cm_int3$Var1[melt_cm_int3$Var1 ==  2] <- "15-19"
melt_cm_int3$Var1[melt_cm_int3$Var1 ==  3] <- "20-24"
melt_cm_int3$Var1[melt_cm_int3$Var1 ==  4] <- "25-34"
melt_cm_int3$Var1[melt_cm_int3$Var1 ==  5] <- "35-44"
melt_cm_int3$Var1[melt_cm_int3$Var1 ==  6] <- "45-54"
melt_cm_int3$Var1[melt_cm_int3$Var1 ==  7] <- "55-64"
melt_cm_int3$Var1[melt_cm_int3$Var1 ==  8] <- "65-74"
melt_cm_int3$Var1[melt_cm_int3$Var1 ==  9] <- "75+"


levels(melt_cm_int3$contact.age.group) <- list("0-14" = "[0,14)", "15-19" = "[14,19)", 
                                               "20-24" = "[19,24)", "25-34" = "[24,34)", 
                                               "35-44" = "[34,44)", "45-54" = "[44,54)",
                                               "55-64" = "[54,64)", "65-74" = "[64,75)",
                                               "75+" = "75+")

bb3 <- ggplot(melt_cm_int3, aes(contact.age.group, Var1)) +
  geom_tile(aes(fill = value)) +
  coord_equal() +
  ggtitle("Wave 3 in intermediate density areas \n (21/4/21 - 28/4/21)") +
  geom_text(aes(label=sprintf("%0.1f", round(value, digits = 2))), size = 2) +
  scale_fill_gradient2(low = "#2000CD",
                       mid = "white",
                       high = "#D60C00", limits = c(0,4), midpoint = 2) +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20, title = "Contacts")) +
  theme(legend.position = "none")+
  theme(plot.title = element_text(size = 6, face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.text.x = element_text(size = 5, angle = 90, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 5))



```

```{r contact matrix wave 4 intermediate}
# Variablen, die wir brauchen
part_matrix_int4 <- d_self_all %>% 
  select(new_id, wave, age_group, qmktsize_16_0)%>%
  filter(qmktsize_16_0=="Intermediate density area" & wave == 4)

# Variablen, die wir brauchen
out_contacts_int4 <- dt_outside_all_c_between %>% 
  select(new_id, wave, age_group) %>%
  filter(wave==4)

# df bearbeiten
part_matrix_int4 <- part_matrix_int4 %>% mutate(part_age =
                                                  case_when(age_group == "Under 1" ~ 2,
                                                            age_group == "1-4" ~ 2, 
                                                            age_group == "5-9" ~ 2,
                                                            age_group == "10-14" ~ 2,
                                                            age_group == "15-19" ~ 17,
                                                            age_group == "20-24" ~ 22,
                                                            age_group == "25-34" ~ 30,
                                                            age_group == "35-44" ~ 40,
                                                            age_group == "45-54" ~ 50,
                                                            age_group == "55-64" ~ 60,
                                                            age_group == "65-69" ~ 67,
                                                            age_group == "70-74" ~ 67,
                                                            age_group == "75-79" ~ 82,
                                                            age_group == "80-84" ~ 82,
                                                            age_group == "85 years or older" ~ 82))

out_contacts_int4 <- out_contacts_int4 %>% mutate(cnt_age =
                                                    case_when(age_group == "Under 1" ~ 2,
                                                              age_group == "1-4" ~ 2, 
                                                              age_group == "5-9" ~ 2,
                                                              age_group == "10-14" ~ 2,
                                                              age_group == "15-19" ~ 17,
                                                              age_group == "20-24" ~ 22,
                                                              age_group == "25-34" ~ 30,
                                                              age_group == "35-44" ~ 40,
                                                              age_group == "45-54" ~ 50,
                                                              age_group == "55-64" ~ 60,
                                                              age_group == "65-69" ~ 67,
                                                              age_group == "70-74" ~ 67,
                                                              age_group == "75-79" ~ 82,
                                                              age_group == "80-84" ~ 82,
                                                              age_group == "85 years or older" ~ 82))


# für socialmixR Variable in part_id ändern, survey_object erstellen
# part_id und survey_object werden für die Kontaktmatrix benötigt
colnames(part_matrix_int4)[1] <- "part_id"
colnames(out_contacts_int4)[1] <- "part_id" #out_contacts = non_hh-contacts
survey_object_int4  <- survey(part_matrix_int4, out_contacts_int4)

# contact matrix erstellen
cm_int4 <- contact_matrix(survey_object_int4, age.limits = c(0, 14, 19, 24, 34, 44, 54, 64, 75),  missing.participant.age = c("remove"),
                          missing.contact.age = c("remove"),
                          symmetric = TRUE)

# output gespeichert, data_cm wird für heatmap benötigt
data_cm_int4 <- cm_int4$matrix

#Plot
melt_cm_int4 <- melt(data_cm_int4)

melt_cm_int4$Var1[melt_cm_int4$Var1 ==  1] <- "0-14"
melt_cm_int4$Var1[melt_cm_int4$Var1 ==  2] <- "15-19"
melt_cm_int4$Var1[melt_cm_int4$Var1 ==  3] <- "20-24"
melt_cm_int4$Var1[melt_cm_int4$Var1 ==  4] <- "25-34"
melt_cm_int4$Var1[melt_cm_int4$Var1 ==  5] <- "35-44"
melt_cm_int4$Var1[melt_cm_int4$Var1 ==  6] <- "45-54"
melt_cm_int4$Var1[melt_cm_int4$Var1 ==  7] <- "55-64"
melt_cm_int4$Var1[melt_cm_int4$Var1 ==  8] <- "65-74"
melt_cm_int4$Var1[melt_cm_int4$Var1 ==  9] <- "75+"


levels(melt_cm_int4$contact.age.group) <- list("0-14" = "[0,14)", "15-19" = "[14,19)", 
                                               "20-24" = "[19,24)", "25-34" = "[24,34)", 
                                               "35-44" = "[34,44)", "45-54" = "[44,54)",
                                               "55-64" = "[54,64)", "65-74" = "[64,75)",
                                               "75+" = "75+")

bb4 <- ggplot(melt_cm_int4, aes(contact.age.group, Var1)) +
  geom_tile(aes(fill = value)) +
  coord_equal() +
  ggtitle("Wave 4 in intermediate density areas \n (12/5/21 - 19/5/21)") +
  geom_text(aes(label=sprintf("%0.1f", round(value, digits = 2))), size = 2) +
  scale_fill_gradient2(low = "#2000CD",
                       mid = "white",
                       high = "#D60C00", limits = c(0,4), midpoint = 2) +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20, title = "Contacts")) +
  theme(legend.position = "none")+
  theme(plot.title = element_text(size = 6, face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.text.x = element_text(size = 5, angle = 90, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 5))


```

```{r contact matrix wave 5 intermediate}
# Variablen, die wir brauchen
part_matrix_int5 <- d_self_all %>% 
  select(new_id, wave, age_group, qmktsize_16_0)%>%
  filter(qmktsize_16_0=="Intermediate density area" & wave == 5)

# Variablen, die wir brauchen
out_contacts_int5 <- dt_outside_all_c_between %>% 
  select(new_id, wave, age_group) %>%
  filter(wave==5)

# df bearbeiten
part_matrix_int5 <- part_matrix_int5 %>% mutate(part_age =
                                                  case_when(age_group == "Under 1" ~ 2,
                                                            age_group == "1-4" ~ 2, 
                                                            age_group == "5-9" ~ 2,
                                                            age_group == "10-14" ~ 2,
                                                            age_group == "15-19" ~ 17,
                                                            age_group == "20-24" ~ 22,
                                                            age_group == "25-34" ~ 30,
                                                            age_group == "35-44" ~ 40,
                                                            age_group == "45-54" ~ 50,
                                                            age_group == "55-64" ~ 60,
                                                            age_group == "65-69" ~ 67,
                                                            age_group == "70-74" ~ 67,
                                                            age_group == "75-79" ~ 82,
                                                            age_group == "80-84" ~ 82,
                                                            age_group == "85 years or older" ~ 82))

out_contacts_int5 <- out_contacts_int5 %>% mutate(cnt_age =
                                                    case_when(age_group == "Under 1" ~ 2,
                                                              age_group == "1-4" ~ 2, 
                                                              age_group == "5-9" ~ 2,
                                                              age_group == "10-14" ~ 2,
                                                              age_group == "15-19" ~ 17,
                                                              age_group == "20-24" ~ 22,
                                                              age_group == "25-34" ~ 30,
                                                              age_group == "35-44" ~ 40,
                                                              age_group == "45-54" ~ 50,
                                                              age_group == "55-64" ~ 60,
                                                              age_group == "65-69" ~ 67,
                                                              age_group == "70-74" ~ 67,
                                                              age_group == "75-79" ~ 82,
                                                              age_group == "80-84" ~ 82,
                                                              age_group == "85 years or older" ~ 82))


# für socialmixR Variable in part_id ändern, survey_object erstellen
# part_id und survey_object werden für die Kontaktmatrix benötigt
colnames(part_matrix_int5)[1] <- "part_id"
colnames(out_contacts_int5)[1] <- "part_id" #out_contacts = non_hh-contacts
survey_object_int5  <- survey(part_matrix_int5, out_contacts_int5)

# contact matrix erstellen
cm_int5 <- contact_matrix(survey_object_int5, age.limits = c(0, 14, 19, 24, 34, 44, 54, 64, 75),  missing.participant.age = c("remove"),
                          missing.contact.age = c("remove"),
                          symmetric = TRUE)

# output gespeichert, data_cm wird für heatmap benötigt
data_cm_int5 <- cm_int5$matrix

#Plot
melt_cm_int5 <- melt(data_cm_int5)

melt_cm_int5$Var1[melt_cm_int5$Var1 ==  1] <- "0-14"
melt_cm_int5$Var1[melt_cm_int5$Var1 ==  2] <- "15-19"
melt_cm_int5$Var1[melt_cm_int5$Var1 ==  3] <- "20-24"
melt_cm_int5$Var1[melt_cm_int5$Var1 ==  4] <- "25-34"
melt_cm_int5$Var1[melt_cm_int5$Var1 ==  5] <- "35-44"
melt_cm_int5$Var1[melt_cm_int5$Var1 ==  6] <- "45-54"
melt_cm_int5$Var1[melt_cm_int5$Var1 ==  7] <- "55-64"
melt_cm_int5$Var1[melt_cm_int5$Var1 ==  8] <- "65-74"
melt_cm_int5$Var1[melt_cm_int5$Var1 ==  9] <- "75+"


levels(melt_cm_int5$contact.age.group) <- list("0-14" = "[0,14)", "15-19" = "[14,19)", 
                                               "20-24" = "[19,24)", "25-34" = "[24,34)", 
                                               "35-44" = "[34,44)", "45-54" = "[44,54)",
                                               "55-64" = "[54,64)", "65-74" = "[64,75)",
                                               "75+" = "75+")

bb5 <- ggplot(melt_cm_int5, aes(contact.age.group, Var1)) +
  geom_tile(aes(fill = value)) +
  coord_equal() +
  ggtitle("Wave 5 in intermediate density areas \n (02/6/21 - 09/6/21)") +
  geom_text(aes(label=sprintf("%0.1f", round(value, digits = 2))), size = 2) +
  scale_fill_gradient2(low = "#2000CD",
                       mid = "white",
                       high = "#D60C00", limits = c(0,4), midpoint = 2) +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20, title = "Contacts")) +
  theme(legend.position = "none")+
  theme(plot.title = element_text(size = 6, face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.text.x = element_text(size = 5, angle = 90, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 5))


############## Contact Matrix for wave6 in intermediate ############## 
# Variablen, die wir brauchen
part_matrix_int6 <- d_self_all %>% 
  select(new_id, wave, age_group, qmktsize_16_0)%>%
  filter(qmktsize_16_0=="Intermediate density area" & wave == 6)

# Variablen, die wir brauchen
out_contacts_int6 <- dt_outside_all_c_between %>% 
  select(new_id, wave, age_group) %>%
  filter(wave==6)

# df bearbeiten
part_matrix_int6 <- part_matrix_int6 %>% mutate(part_age =
                                                  case_when(age_group == "Under 1" ~ 2,
                                                            age_group == "1-4" ~ 2, 
                                                            age_group == "5-9" ~ 2,
                                                            age_group == "10-14" ~ 2,
                                                            age_group == "15-19" ~ 17,
                                                            age_group == "20-24" ~ 22,
                                                            age_group == "25-34" ~ 30,
                                                            age_group == "35-44" ~ 40,
                                                            age_group == "45-54" ~ 50,
                                                            age_group == "55-64" ~ 60,
                                                            age_group == "65-69" ~ 67,
                                                            age_group == "70-74" ~ 67,
                                                            age_group == "75-79" ~ 82,
                                                            age_group == "80-84" ~ 82,
                                                            age_group == "85 years or older" ~ 82))

out_contacts_int6 <- out_contacts_int6 %>% mutate(cnt_age =
                                                    case_when(age_group == "Under 1" ~ 2,
                                                              age_group == "1-4" ~ 2, 
                                                              age_group == "5-9" ~ 2,
                                                              age_group == "10-14" ~ 2,
                                                              age_group == "15-19" ~ 17,
                                                              age_group == "20-24" ~ 22,
                                                              age_group == "25-34" ~ 30,
                                                              age_group == "35-44" ~ 40,
                                                              age_group == "45-54" ~ 50,
                                                              age_group == "55-64" ~ 60,
                                                              age_group == "65-69" ~ 67,
                                                              age_group == "70-74" ~ 67,
                                                              age_group == "75-79" ~ 82,
                                                              age_group == "80-84" ~ 82,
                                                              age_group == "85 years or older" ~ 82))


# für socialmixR Variable in part_id ändern, survey_object erstellen
# part_id und survey_object werden für die Kontaktmatrix benötigt
colnames(part_matrix_int6)[1] <- "part_id"
colnames(out_contacts_int6)[1] <- "part_id" #out_contacts = non_hh-contacts
survey_object_int6  <- survey(part_matrix_int6, out_contacts_int6)

# contact matrix erstellen
cm_int6 <- contact_matrix(survey_object_int6, age.limits = c(0, 14, 19, 24, 34, 44, 54, 64, 75),  missing.participant.age = c("remove"),
                          missing.contact.age = c("remove"),
                          symmetric = TRUE)

# output gespeichert, data_cm wird für heatmap benötigt
data_cm_int6 <- cm_int6$matrix

#Plot
melt_cm_int6 <- melt(data_cm_int6)

melt_cm_int6$Var1[melt_cm_int6$Var1 ==  1] <- "0-14"
melt_cm_int6$Var1[melt_cm_int6$Var1 ==  2] <- "15-19"
melt_cm_int6$Var1[melt_cm_int6$Var1 ==  3] <- "20-24"
melt_cm_int6$Var1[melt_cm_int6$Var1 ==  4] <- "25-34"
melt_cm_int6$Var1[melt_cm_int6$Var1 ==  5] <- "35-44"
melt_cm_int6$Var1[melt_cm_int6$Var1 ==  6] <- "45-54"
melt_cm_int6$Var1[melt_cm_int6$Var1 ==  7] <- "55-64"
melt_cm_int6$Var1[melt_cm_int6$Var1 ==  8] <- "65-74"
melt_cm_int6$Var1[melt_cm_int6$Var1 ==  9] <- "75+"


levels(melt_cm_int6$contact.age.group) <- list("0-14" = "[0,14)", "15-19" = "[14,19)", 
                                               "20-24" = "[19,24)", "25-34" = "[24,34)", 
                                               "35-44" = "[34,44)", "45-54" = "[44,54)",
                                               "55-64" = "[54,64)", "65-74" = "[64,75)",
                                               "75+" = "75+")

bb6 <- ggplot(melt_cm_int6, aes(contact.age.group, Var1)) +
  geom_tile(aes(fill = value)) +
  coord_equal() +
  ggtitle("Wave 6 in intermediate density areas \n (23/6/21 - 30/6/21)") +
  geom_text(aes(label=sprintf("%0.1f", round(value, digits = 2))), size = 2) +
  scale_fill_gradient2(low = "#2000CD",
                       mid = "white",
                       high = "#D60C00", limits = c(0,4), midpoint = 2) +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20, title = "Contacts")) +
  theme(legend.position = "none")+
  theme(plot.title = element_text(size = 6, face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.text.x = element_text(size = 5, angle = 90, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 5))



```

```{r contact matrix wave 7 intermediate}
# Variablen, die wir brauchen
part_matrix_int7 <- d_self_all %>% 
  select(new_id, wave, age_group, qmktsize_16_0)%>%
  filter(qmktsize_16_0=="Intermediate density area" & wave == 7)

# Variablen, die wir brauchen
out_contacts_int7 <- dt_outside_all_c_between %>% 
  select(new_id, wave, age_group) %>%
  filter(wave==7)

# df bearbeiten
part_matrix_int7 <- part_matrix_int7 %>% mutate(part_age =
                                                  case_when(age_group == "Under 1" ~ 2,
                                                            age_group == "1-4" ~ 2, 
                                                            age_group == "5-9" ~ 2,
                                                            age_group == "10-14" ~ 2,
                                                            age_group == "15-19" ~ 17,
                                                            age_group == "20-24" ~ 22,
                                                            age_group == "25-34" ~ 30,
                                                            age_group == "35-44" ~ 40,
                                                            age_group == "45-54" ~ 50,
                                                            age_group == "55-64" ~ 60,
                                                            age_group == "65-69" ~ 67,
                                                            age_group == "70-74" ~ 67,
                                                            age_group == "75-79" ~ 82,
                                                            age_group == "80-84" ~ 82,
                                                            age_group == "85 years or older" ~ 82))

out_contacts_int7 <- out_contacts_int7 %>% mutate(cnt_age =
                                                    case_when(age_group == "Under 1" ~ 2,
                                                              age_group == "1-4" ~ 2, 
                                                              age_group == "5-9" ~ 2,
                                                              age_group == "10-14" ~ 2,
                                                              age_group == "15-19" ~ 17,
                                                              age_group == "20-24" ~ 22,
                                                              age_group == "25-34" ~ 30,
                                                              age_group == "35-44" ~ 40,
                                                              age_group == "45-54" ~ 50,
                                                              age_group == "55-64" ~ 60,
                                                              age_group == "65-69" ~ 67,
                                                              age_group == "70-74" ~ 67,
                                                              age_group == "75-79" ~ 82,
                                                              age_group == "80-84" ~ 82,
                                                              age_group == "85 years or older" ~ 82))


# für socialmixR Variable in part_id ändern, survey_object erstellen
# part_id und survey_object werden für die Kontaktmatrix benötigt
colnames(part_matrix_int7)[1] <- "part_id"
colnames(out_contacts_int7)[1] <- "part_id" #out_contacts = non_hh-contacts
survey_object_int7  <- survey(part_matrix_int7, out_contacts_int7)

# contact matrix erstellen
cm_int7 <- contact_matrix(survey_object_int7, age.limits = c(0, 14, 19, 24, 34, 44, 54, 64, 75),  missing.participant.age = c("remove"),
                          missing.contact.age = c("remove"),
                          symmetric = TRUE)

# output gespeichert, data_cm wird für heatmap benötigt
data_cm_int7 <- cm_int7$matrix

#Plot
melt_cm_int7 <- melt(data_cm_int7)

melt_cm_int7$Var1[melt_cm_int7$Var1 ==  1] <- "0-14"
melt_cm_int7$Var1[melt_cm_int7$Var1 ==  2] <- "15-19"
melt_cm_int7$Var1[melt_cm_int7$Var1 ==  3] <- "20-24"
melt_cm_int7$Var1[melt_cm_int7$Var1 ==  4] <- "25-34"
melt_cm_int7$Var1[melt_cm_int7$Var1 ==  5] <- "35-44"
melt_cm_int7$Var1[melt_cm_int7$Var1 ==  6] <- "45-54"
melt_cm_int7$Var1[melt_cm_int7$Var1 ==  7] <- "55-64"
melt_cm_int7$Var1[melt_cm_int7$Var1 ==  8] <- "65-74"
melt_cm_int7$Var1[melt_cm_int7$Var1 ==  9] <- "75+"


levels(melt_cm_int7$contact.age.group) <- list("0-14" = "[0,14)", "15-19" = "[14,19)", 
                                               "20-24" = "[19,24)", "25-34" = "[24,34)", 
                                               "35-44" = "[34,44)", "45-54" = "[44,54)",
                                               "55-64" = "[54,64)", "65-74" = "[64,75)",
                                               "75+" = "75+")

bb7 <- ggplot(melt_cm_int7, aes(contact.age.group, Var1)) +
  geom_tile(aes(fill = value)) +
  coord_equal() +
  ggtitle("Wave 7 in intermediate density areas \n (14/7/21 - 21/7/21)") +
  geom_text(aes(label=sprintf("%0.1f", round(value, digits = 2))), size = 2) +
  scale_fill_gradient2(low = "#2000CD",
                       mid = "white",
                       high = "#D60C00", limits = c(0,4), midpoint = 2) +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20, title = "Contacts")) +
  theme(legend.position = "none")+
  theme(plot.title = element_text(size = 6, face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.text.x = element_text(size = 5, angle = 90, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 5))

```

```{r contact matrix wave 8 intermediate}
# Variablen, die wir brauchen
part_matrix_int8 <- d_self_all %>% 
  select(new_id, wave, age_group, qmktsize_16_0)%>%
  filter(qmktsize_16_0=="Intermediate density area" & wave == 8)

# Variablen, die wir brauchen
out_contacts_int8 <- dt_outside_all_c_between %>% 
  select(new_id, wave, age_group) %>%
  filter(wave==8)

# df bearbeiten
part_matrix_int8 <- part_matrix_int8 %>% mutate(part_age =
                                                  case_when(age_group == "Under 1" ~ 2,
                                                            age_group == "1-4" ~ 2, 
                                                            age_group == "5-9" ~ 2,
                                                            age_group == "10-14" ~ 2,
                                                            age_group == "15-19" ~ 17,
                                                            age_group == "20-24" ~ 22,
                                                            age_group == "25-34" ~ 30,
                                                            age_group == "35-44" ~ 40,
                                                            age_group == "45-54" ~ 50,
                                                            age_group == "55-64" ~ 60,
                                                            age_group == "65-69" ~ 67,
                                                            age_group == "70-74" ~ 67,
                                                            age_group == "75-79" ~ 82,
                                                            age_group == "80-84" ~ 82,
                                                            age_group == "85 years or older" ~ 82))

out_contacts_int8 <- out_contacts_int8 %>% mutate(cnt_age =
                                                    case_when(age_group == "Under 1" ~ 2,
                                                              age_group == "1-4" ~ 2, 
                                                              age_group == "5-9" ~ 2,
                                                              age_group == "10-14" ~ 2,
                                                              age_group == "15-19" ~ 17,
                                                              age_group == "20-24" ~ 22,
                                                              age_group == "25-34" ~ 30,
                                                              age_group == "35-44" ~ 40,
                                                              age_group == "45-54" ~ 50,
                                                              age_group == "55-64" ~ 60,
                                                              age_group == "65-69" ~ 67,
                                                              age_group == "70-74" ~ 67,
                                                              age_group == "75-79" ~ 82,
                                                              age_group == "80-84" ~ 82,
                                                              age_group == "85 years or older" ~ 82))


# für socialmixR Variable in part_id ändern, survey_object erstellen
# part_id und survey_object werden für die Kontaktmatrix benötigt
colnames(part_matrix_int8)[1] <- "part_id"
colnames(out_contacts_int8)[1] <- "part_id" #out_contacts = non_hh-contacts
survey_object_int8  <- survey(part_matrix_int8, out_contacts_int8)

# contact matrix erstellen
cm_int8 <- contact_matrix(survey_object_int8, age.limits = c(0, 14, 19, 24, 34, 44, 54, 64, 75),  missing.participant.age = c("remove"),
                          missing.contact.age = c("remove"),
                          symmetric = TRUE)

# output gespeichert, data_cm wird für heatmap benötigt
data_cm_int8 <- cm_int8$matrix


#Plot
melt_cm_int8 <- melt(data_cm_int8)

melt_cm_int8$Var1[melt_cm_int8$Var1 ==  1] <- "0-14"
melt_cm_int8$Var1[melt_cm_int8$Var1 ==  2] <- "15-19"
melt_cm_int8$Var1[melt_cm_int8$Var1 ==  3] <- "20-24"
melt_cm_int8$Var1[melt_cm_int8$Var1 ==  4] <- "25-34"
melt_cm_int8$Var1[melt_cm_int8$Var1 ==  5] <- "35-44"
melt_cm_int8$Var1[melt_cm_int8$Var1 ==  6] <- "45-54"
melt_cm_int8$Var1[melt_cm_int8$Var1 ==  7] <- "55-64"
melt_cm_int8$Var1[melt_cm_int8$Var1 ==  8] <- "65-74"
melt_cm_int8$Var1[melt_cm_int8$Var1 ==  9] <- "75+"


levels(melt_cm_int8$contact.age.group) <- list("0-14" = "[0,14)", "15-19" = "[14,19)", 
                                               "20-24" = "[19,24)", "25-34" = "[24,34)", 
                                               "35-44" = "[34,44)", "45-54" = "[44,54)",
                                               "55-64" = "[54,64)", "65-74" = "[64,75)",
                                               "75+" = "75+")

bb8 <- ggplot(melt_cm_int8, aes(contact.age.group, Var1)) +
  geom_tile(aes(fill = value)) +
  coord_equal() +
  ggtitle("Wave 8 in intermediate densityareas \n (04/8/21 - 11/8/21)") +
  geom_text(aes(label=sprintf("%0.1f", round(value, digits = 2))), size = 2) +
  scale_fill_gradient2(low = "#2000CD",
                       mid = "white",
                       high = "#D60C00", limits = c(0,4), midpoint = 2) +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20, title = "Contacts")) +
  theme(legend.position = "none")+
  theme(plot.title = element_text(size = 6, face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.text.x = element_text(size = 5, angle = 90, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 5))


```

### Thinly
```{r contact matrix wave 1 thinly}
# Variablen, die wir brauchen
part_matrix_thin1 <- d_self_all %>% 
  select(new_id, wave, age_group, qmktsize_16_0)%>%
  filter(qmktsize_16_0=="Thinly populated area" & wave == 1)

# Variablen, die wir brauchen
out_contacts_thin1 <- dt_outside_all_c_between %>% 
  select(new_id, wave, age_group) %>%
  filter(wave==1)

# df bearbeiten
part_matrix_thin1 <- part_matrix_thin1 %>% mutate(part_age =
                                                    case_when(age_group == "Under 1" ~ 2,
                                                              age_group == "1-4" ~ 2, 
                                                              age_group == "5-9" ~ 2,
                                                              age_group == "10-14" ~ 2,
                                                              age_group == "15-19" ~ 17,
                                                              age_group == "20-24" ~ 22,
                                                              age_group == "25-34" ~ 30,
                                                              age_group == "35-44" ~ 40,
                                                              age_group == "45-54" ~ 50,
                                                              age_group == "55-64" ~ 60,
                                                              age_group == "65-69" ~ 67,
                                                              age_group == "70-74" ~ 67,
                                                              age_group == "75-79" ~ 82,
                                                              age_group == "80-84" ~ 82,
                                                              age_group == "85 years or older" ~ 82))

out_contacts_thin1 <- out_contacts_thin1 %>% mutate(cnt_age =
                                                      case_when(age_group == "Under 1" ~ 2,
                                                                age_group == "1-4" ~ 2, 
                                                                age_group == "5-9" ~ 2,
                                                                age_group == "10-14" ~ 2,
                                                                age_group == "15-19" ~ 17,
                                                                age_group == "20-24" ~ 22,
                                                                age_group == "25-34" ~ 30,
                                                                age_group == "35-44" ~ 40,
                                                                age_group == "45-54" ~ 50,
                                                                age_group == "55-64" ~ 60,
                                                                age_group == "65-69" ~ 67,
                                                                age_group == "70-74" ~ 67,
                                                                age_group == "75-79" ~ 82,
                                                                age_group == "80-84" ~ 82,
                                                                age_group == "85 years or older" ~ 82))


# für socialmixR Variable in part_id ändern, survey_object erstellen
# part_id und survey_object werden für die Kontaktmatrix benötigt
colnames(part_matrix_thin1)[1] <- "part_id"
colnames(out_contacts_thin1)[1] <- "part_id" #out_contacts = non_hh-contacts
survey_object_thin1  <- survey(part_matrix_thin1, out_contacts_thin1)

# contact matrix erstellen
cm_thin1 <- contact_matrix(survey_object_thin1, age.limits = c(0, 14, 19, 24, 34, 44, 54, 64, 75),  missing.participant.age = c("remove"),
                           missing.contact.age = c("remove"),
                           symmetric = TRUE)

# output gespeichert, data_cm wird für heatmap benötigt
data_cm_thin1 <- cm_thin1$matrix


#Plot
melt_cm_thin1 <- melt(data_cm_thin1)

melt_cm_thin1$Var1[melt_cm_thin1$Var1 ==  1] <- "0-14"
melt_cm_thin1$Var1[melt_cm_thin1$Var1 ==  2] <- "15-19"
melt_cm_thin1$Var1[melt_cm_thin1$Var1 ==  3] <- "20-24"
melt_cm_thin1$Var1[melt_cm_thin1$Var1 ==  4] <- "25-34"
melt_cm_thin1$Var1[melt_cm_thin1$Var1 ==  5] <- "35-44"
melt_cm_thin1$Var1[melt_cm_thin1$Var1 ==  6] <- "45-54"
melt_cm_thin1$Var1[melt_cm_thin1$Var1 ==  7] <- "55-64"
melt_cm_thin1$Var1[melt_cm_thin1$Var1 ==  8] <- "65-74"
melt_cm_thin1$Var1[melt_cm_thin1$Var1 ==  9] <- "75+"


levels(melt_cm_thin1$contact.age.group) <- list("0-14" = "[0,14)", "15-19" = "[14,19)", 
                                               "20-24" = "[19,24)", "25-34" = "[24,34)", 
                                               "35-44" = "[34,44)", "45-54" = "[44,54)",
                                               "55-64" = "[54,64)", "65-74" = "[64,75)",
                                               "75+" = "75+")

cc1 <- ggplot(melt_cm_thin1, aes(contact.age.group, Var1)) +
  geom_tile(aes(fill = value)) +
  coord_equal() +
  ggtitle("Wave 1 in thinly populated areas \n (03/3/21 - 10/3/21)") +
  geom_text(aes(label=sprintf("%0.1f", round(value, digits = 2))), size = 2) +
  scale_fill_gradient2(low = "#2000CD",
                       mid = "white",
                       high = "#D60C00", limits = c(0,4), midpoint = 2) +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20, title = "Contacts")) +
  theme(legend.position = "none")+
  theme(plot.title = element_text(size = 6, face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) + 
  theme(axis.text.x = element_text(size = 5, angle = 90, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 5))


```

```{r contact matrix wave 2 thinly}
# Variablen, die wir brauchen
part_matrix_thin2 <- d_self_all %>% 
  select(new_id, wave, age_group, qmktsize_16_0)%>%
  filter(qmktsize_16_0=="Thinly populated area" & wave == 2)

# Variablen, die wir brauchen
out_contacts_thin2 <- dt_outside_all_c_between %>% 
  select(new_id, wave, age_group) %>%
  filter(wave==2)

# df bearbeiten
part_matrix_thin2 <- part_matrix_thin2 %>% mutate(part_age =
                                                    case_when(age_group == "Under 1" ~ 2,
                                                              age_group == "1-4" ~ 2, 
                                                              age_group == "5-9" ~ 2,
                                                              age_group == "10-14" ~ 2,
                                                              age_group == "15-19" ~ 17,
                                                              age_group == "20-24" ~ 22,
                                                              age_group == "25-34" ~ 30,
                                                              age_group == "35-44" ~ 40,
                                                              age_group == "45-54" ~ 50,
                                                              age_group == "55-64" ~ 60,
                                                              age_group == "65-69" ~ 67,
                                                              age_group == "70-74" ~ 67,
                                                              age_group == "75-79" ~ 82,
                                                              age_group == "80-84" ~ 82,
                                                              age_group == "85 years or older" ~ 82))

out_contacts_thin2 <- out_contacts_thin2 %>% mutate(cnt_age =
                                                      case_when(age_group == "Under 1" ~ 2,
                                                                age_group == "1-4" ~ 2, 
                                                                age_group == "5-9" ~ 2,
                                                                age_group == "10-14" ~ 2,
                                                                age_group == "15-19" ~ 17,
                                                                age_group == "20-24" ~ 22,
                                                                age_group == "25-34" ~ 30,
                                                                age_group == "35-44" ~ 40,
                                                                age_group == "45-54" ~ 50,
                                                                age_group == "55-64" ~ 60,
                                                                age_group == "65-69" ~ 67,
                                                                age_group == "70-74" ~ 67,
                                                                age_group == "75-79" ~ 82,
                                                                age_group == "80-84" ~ 82,
                                                                age_group == "85 years or older" ~ 82))

# für socialmixR Variable in part_id ändern, survey_object erstellen
# part_id und survey_object werden für die Kontaktmatrix benötigt
colnames(part_matrix_thin2)[1] <- "part_id"
colnames(out_contacts_thin2)[1] <- "part_id" #out_contacts = non_hh-contacts
survey_object_thin2  <- survey(part_matrix_thin2, out_contacts_thin2)

# contact matrix erstellen
cm_thin2 <- contact_matrix(survey_object_thin2, age.limits = c(0, 14, 19, 24, 34, 44, 54, 64, 75),  missing.participant.age = c("remove"),
                           missing.contact.age = c("remove"),
                           symmetric = TRUE)

# output gespeichert, data_cm wird für heatmap benötigt
data_cm_thin2 <- cm_thin2$matrix

#Plot
melt_cm_thin2 <- melt(data_cm_thin2)

melt_cm_thin2$Var1[melt_cm_thin2$Var1 ==  1] <- "0-14"
melt_cm_thin2$Var1[melt_cm_thin2$Var1 ==  2] <- "15-19"
melt_cm_thin2$Var1[melt_cm_thin2$Var1 ==  3] <- "20-24"
melt_cm_thin2$Var1[melt_cm_thin2$Var1 ==  4] <- "25-34"
melt_cm_thin2$Var1[melt_cm_thin2$Var1 ==  5] <- "35-44"
melt_cm_thin2$Var1[melt_cm_thin2$Var1 ==  6] <- "45-54"
melt_cm_thin2$Var1[melt_cm_thin2$Var1 ==  7] <- "55-64"
melt_cm_thin2$Var1[melt_cm_thin2$Var1 ==  8] <- "65-74"
melt_cm_thin2$Var1[melt_cm_thin2$Var1==  9] <- "75+"


levels(melt_cm_thin2$contact.age.group) <- list("0-14" = "[0,14)", "15-19" = "[14,19)", 
                                                "20-24" = "[19,24)", "25-34" = "[24,34)", 
                                                "35-44" = "[34,44)", "45-54" = "[44,54)",
                                                "55-64" = "[54,64)", "65-74" = "[64,75)",
                                                "75+" = "75+")

cc2 <- ggplot(melt_cm_thin2, aes(contact.age.group, Var1)) +
  geom_tile(aes(fill = value)) +
  coord_equal() +
  ggtitle("Wave 2 in thinly populated areas \n (31/3/21 - 07/4/21)") +
  geom_text(aes(label=sprintf("%0.1f", round(value, digits = 2))), size = 2) +
  scale_fill_gradient2(low = "#2000CD",
                       mid = "white",
                       high = "#D60C00", limits = c(0,4), midpoint = 2) +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20, title = "Contacts")) +
  theme(legend.position = "none")+
  theme(plot.title = element_text(size = 6, face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.text.x = element_text(size = 5, angle = 90, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 5))

```

```{r contact matrix wave 3 thinly}
# Variablen, die wir brauchen
part_matrix_thin3 <- d_self_all %>% 
  select(new_id, wave, age_group, qmktsize_16_0)%>%
  filter(qmktsize_16_0=="Thinly populated area" & wave == 3)

# Variablen, die wir brauchen
out_contacts_thin3 <- dt_outside_all_c_between %>% 
  select(new_id, wave, age_group) %>%
  filter(wave==3)

# df bearbeiten
part_matrix_thin3 <- part_matrix_thin3 %>% mutate(part_age =
                                                    case_when(age_group == "Under 1" ~ 2,
                                                              age_group == "1-4" ~ 2, 
                                                              age_group == "5-9" ~ 2,
                                                              age_group == "10-14" ~ 2,
                                                              age_group == "15-19" ~ 17,
                                                              age_group == "20-24" ~ 22,
                                                              age_group == "25-34" ~ 30,
                                                              age_group == "35-44" ~ 40,
                                                              age_group == "45-54" ~ 50,
                                                              age_group == "55-64" ~ 60,
                                                              age_group == "65-69" ~ 67,
                                                              age_group == "70-74" ~ 67,
                                                              age_group == "75-79" ~ 82,
                                                              age_group == "80-84" ~ 82,
                                                              age_group == "85 years or older" ~ 82))

out_contacts_thin3 <- out_contacts_thin3 %>% mutate(cnt_age =
                                                      case_when(age_group == "Under 1" ~ 2,
                                                                age_group == "1-4" ~ 2, 
                                                                age_group == "5-9" ~ 2,
                                                                age_group == "10-14" ~ 2,
                                                                age_group == "15-19" ~ 17,
                                                                age_group == "20-24" ~ 22,
                                                                age_group == "25-34" ~ 30,
                                                                age_group == "35-44" ~ 40,
                                                                age_group == "45-54" ~ 50,
                                                                age_group == "55-64" ~ 60,
                                                                age_group == "65-69" ~ 67,
                                                                age_group == "70-74" ~ 67,
                                                                age_group == "75-79" ~ 82,
                                                                age_group == "80-84" ~ 82,
                                                                age_group == "85 years or older" ~ 82))


# für socialmixR Variable in part_id ändern, survey_object erstellen
# part_id und survey_object werden für die Kontaktmatrix benötigt
colnames(part_matrix_thin3)[1] <- "part_id"
colnames(out_contacts_thin3)[1] <- "part_id" #out_contacts = non_hh-contacts
survey_object_thin3  <- survey(part_matrix_thin3, out_contacts_thin3)

# contact matrix erstellen
cm_thin3 <- contact_matrix(survey_object_thin3, age.limits = c(0, 14, 19, 24, 34, 44, 54, 64, 75),  missing.participant.age = c("remove"),
                           missing.contact.age = c("remove"),
                           symmetric = TRUE)

# output gespeichert, data_cm wird für heatmap benötigt
data_cm_thin3 <- cm_thin3$matrix

#Plot
melt_cm_thin3 <- melt(data_cm_thin3)

melt_cm_thin3$Var1[melt_cm_thin3$Var1 ==  1] <- "0-14"
melt_cm_thin3$Var1[melt_cm_thin3$Var1 ==  2] <- "15-19"
melt_cm_thin3$Var1[melt_cm_thin3$Var1 ==  3] <- "20-24"
melt_cm_thin3$Var1[melt_cm_thin3$Var1 ==  4] <- "25-34"
melt_cm_thin3$Var1[melt_cm_thin3$Var1 ==  5] <- "35-44"
melt_cm_thin3$Var1[melt_cm_thin3$Var1 ==  6] <- "45-54"
melt_cm_thin3$Var1[melt_cm_thin3$Var1 ==  7] <- "55-64"
melt_cm_thin3$Var1[melt_cm_thin3$Var1 ==  8] <- "65-74"
melt_cm_thin3$Var1[melt_cm_thin3$Var1==  9] <- "75+"


levels(melt_cm_thin3$contact.age.group) <- list("0-14" = "[0,14)", "15-19" = "[14,19)", 
                                                "20-24" = "[19,24)", "25-34" = "[24,34)", 
                                                "35-44" = "[34,44)", "45-54" = "[44,54)",
                                                "55-64" = "[54,64)", "65-74" = "[64,75)",
                                                "75+" = "75+")

cc3 <- ggplot(melt_cm_thin3, aes(contact.age.group, Var1)) +
  geom_tile(aes(fill = value)) +
  coord_equal() +
  ggtitle("Wave 3 in thinly populated areas \n (21/4/21 - 28/4/21)") +
  geom_text(aes(label=sprintf("%0.1f", round(value, digits = 2))), size = 2) +
  scale_fill_gradient2(low = "#2000CD",
                       mid = "white",
                       high = "#D60C00", limits = c(0,4), midpoint = 2) +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20, title = "Contacts")) +
  theme(legend.position = "none")+
  theme(plot.title = element_text(size = 6, face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.text.x = element_text(size = 5, angle = 90, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 5))



############## Contact Matrix for wave4 in thinly  ############## 
# Variablen, die wir brauchen
part_matrix_thin4 <- d_self_all %>% 
  select(new_id, wave, age_group, qmktsize_16_0)%>%
  filter(qmktsize_16_0=="Thinly populated area" & wave == 4)

# Variablen, die wir brauchen
out_contacts_thin4 <- dt_outside_all_c_between %>% 
  select(new_id, wave, age_group) %>%
  filter(wave==4)

# df bearbeiten
part_matrix_thin4 <- part_matrix_thin4 %>% mutate(part_age =
                                                    case_when(age_group == "Under 1" ~ 2,
                                                              age_group == "1-4" ~ 2, 
                                                              age_group == "5-9" ~ 2,
                                                              age_group == "10-14" ~ 2,
                                                              age_group == "15-19" ~ 17,
                                                              age_group == "20-24" ~ 22,
                                                              age_group == "25-34" ~ 30,
                                                              age_group == "35-44" ~ 40,
                                                              age_group == "45-54" ~ 50,
                                                              age_group == "55-64" ~ 60,
                                                              age_group == "65-69" ~ 67,
                                                              age_group == "70-74" ~ 67,
                                                              age_group == "75-79" ~ 82,
                                                              age_group == "80-84" ~ 82,
                                                              age_group == "85 years or older" ~ 82))

out_contacts_thin4 <- out_contacts_thin4 %>% mutate(cnt_age =
                                                      case_when(age_group == "Under 1" ~ 2,
                                                                age_group == "1-4" ~ 2, 
                                                                age_group == "5-9" ~ 2,
                                                                age_group == "10-14" ~ 2,
                                                                age_group == "15-19" ~ 17,
                                                                age_group == "20-24" ~ 22,
                                                                age_group == "25-34" ~ 30,
                                                                age_group == "35-44" ~ 40,
                                                                age_group == "45-54" ~ 50,
                                                                age_group == "55-64" ~ 60,
                                                                age_group == "65-69" ~ 67,
                                                                age_group == "70-74" ~ 67,
                                                                age_group == "75-79" ~ 82,
                                                                age_group == "80-84" ~ 82,
                                                                age_group == "85 years or older" ~ 82))


# für socialmixR Variable in part_id ändern, survey_object erstellen
# part_id und survey_object werden für die Kontaktmatrix benötigt
colnames(part_matrix_thin4)[1] <- "part_id"
colnames(out_contacts_thin4)[1] <- "part_id" #out_contacts = non_hh-contacts
survey_object_thin4  <- survey(part_matrix_thin4, out_contacts_thin4)

# contact matrix erstellen
cm_thin4 <- contact_matrix(survey_object_thin4, age.limits = c(0, 14, 19, 24, 34, 44, 54, 64, 75),  missing.participant.age = c("remove"),
                           missing.contact.age = c("remove"),
                           symmetric = TRUE)

# output gespeichert, data_cm wird für heatmap benötigt
data_cm_thin4 <- cm_thin4$matrix

#Plot
melt_cm_thin4 <- melt(data_cm_thin4)

melt_cm_thin4$Var1[melt_cm_thin4$Var1 ==  1] <- "0-14"
melt_cm_thin4$Var1[melt_cm_thin4$Var1 ==  2] <- "15-19"
melt_cm_thin4$Var1[melt_cm_thin4$Var1 ==  3] <- "20-24"
melt_cm_thin4$Var1[melt_cm_thin4$Var1 ==  4] <- "25-34"
melt_cm_thin4$Var1[melt_cm_thin4$Var1 ==  5] <- "35-44"
melt_cm_thin4$Var1[melt_cm_thin4$Var1 ==  6] <- "45-54"
melt_cm_thin4$Var1[melt_cm_thin4$Var1 ==  7] <- "55-64"
melt_cm_thin4$Var1[melt_cm_thin4$Var1 ==  8] <- "65-74"
melt_cm_thin4$Var1[melt_cm_thin4$Var1==  9] <- "75+"


levels(melt_cm_thin4$contact.age.group) <- list("0-14" = "[0,14)", "15-19" = "[14,19)", 
                                                "20-24" = "[19,24)", "25-34" = "[24,34)", 
                                                "35-44" = "[34,44)", "45-54" = "[44,54)",
                                                "55-64" = "[54,64)", "65-74" = "[64,75)",
                                                "75+" = "75+")

cc4 <- ggplot(melt_cm_thin4, aes(contact.age.group, Var1)) +
  geom_tile(aes(fill = value)) +
  coord_equal() +
  ggtitle("Wave 4 in thinly populated areas \n (12/5/21 - 19/5/21)") +
  geom_text(aes(label=sprintf("%0.1f", round(value, digits = 2))), size = 2) +
  scale_fill_gradient2(low = "#2000CD",
                       mid = "white",
                       high = "#D60C00", limits = c(0,4), midpoint = 2) +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20, title = "Contacts")) +
  theme(legend.position = "none")+
  theme(plot.title = element_text(size = 6, face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.text.x = element_text(size = 5, angle = 90, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 5))

```

```{r contact matrix wave 51 thinly}
# Variablen, die wir brauchen
part_matrix_thin5 <- d_self_all %>% 
  select(new_id, wave, age_group, qmktsize_16_0)%>%
  filter(qmktsize_16_0=="Thinly populated area" & wave == 5)

# Variablen, die wir brauchen
out_contacts_thin5 <- dt_outside_all_c_between %>% 
  select(new_id, wave, age_group) %>%
  filter(wave==5)

# df bearbeiten
part_matrix_thin5 <- part_matrix_thin5 %>% mutate(part_age =
                                                    case_when(age_group == "Under 1" ~ 2,
                                                              age_group == "1-4" ~ 2, 
                                                              age_group == "5-9" ~ 2,
                                                              age_group == "10-14" ~ 2,
                                                              age_group == "15-19" ~ 17,
                                                              age_group == "20-24" ~ 22,
                                                              age_group == "25-34" ~ 30,
                                                              age_group == "35-44" ~ 40,
                                                              age_group == "45-54" ~ 50,
                                                              age_group == "55-64" ~ 60,
                                                              age_group == "65-69" ~ 67,
                                                              age_group == "70-74" ~ 67,
                                                              age_group == "75-79" ~ 82,
                                                              age_group == "80-84" ~ 82,
                                                              age_group == "85 years or older" ~ 82))

out_contacts_thin5 <- out_contacts_thin5 %>% mutate(cnt_age =
                                                      case_when(age_group == "Under 1" ~ 2,
                                                                age_group == "1-4" ~ 2, 
                                                                age_group == "5-9" ~ 2,
                                                                age_group == "10-14" ~ 2,
                                                                age_group == "15-19" ~ 17,
                                                                age_group == "20-24" ~ 22,
                                                                age_group == "25-34" ~ 30,
                                                                age_group == "35-44" ~ 40,
                                                                age_group == "45-54" ~ 50,
                                                                age_group == "55-64" ~ 60,
                                                                age_group == "65-69" ~ 67,
                                                                age_group == "70-74" ~ 67,
                                                                age_group == "75-79" ~ 82,
                                                                age_group == "80-84" ~ 82,
                                                                age_group == "85 years or older" ~ 82))


# für socialmixR Variable in part_id ändern, survey_object erstellen
# part_id und survey_object werden für die Kontaktmatrix benötigt
colnames(part_matrix_thin5)[1] <- "part_id"
colnames(out_contacts_thin5)[1] <- "part_id" #out_contacts = non_hh-contacts
survey_object_thin5  <- survey(part_matrix_thin5, out_contacts_thin5)

# contact matrix erstellen
cm_thin5 <- contact_matrix(survey_object_thin5, age.limits = c(0, 14, 19, 24, 34, 44, 54, 64, 75),  missing.participant.age = c("remove"),
                           missing.contact.age = c("remove"),
                           symmetric = TRUE)

# output gespeichert, data_cm wird für heatmap benötigt
data_cm_thin5 <- cm_thin5$matrix

#Plot
melt_cm_thin5 <- melt(data_cm_thin5)

melt_cm_thin5$Var1[melt_cm_thin5$Var1 ==  1] <- "0-14"
melt_cm_thin5$Var1[melt_cm_thin5$Var1 ==  2] <- "15-19"
melt_cm_thin5$Var1[melt_cm_thin5$Var1 ==  3] <- "20-24"
melt_cm_thin5$Var1[melt_cm_thin5$Var1 ==  4] <- "25-34"
melt_cm_thin5$Var1[melt_cm_thin5$Var1 ==  5] <- "35-44"
melt_cm_thin5$Var1[melt_cm_thin5$Var1 ==  6] <- "45-54"
melt_cm_thin5$Var1[melt_cm_thin5$Var1 ==  7] <- "55-64"
melt_cm_thin5$Var1[melt_cm_thin5$Var1 ==  8] <- "65-74"
melt_cm_thin5$Var1[melt_cm_thin5$Var1==  9] <- "75+"


levels(melt_cm_thin5$contact.age.group) <- list("0-14" = "[0,14)", "15-19" = "[14,19)", 
                                                "20-24" = "[19,24)", "25-34" = "[24,34)", 
                                                "35-44" = "[34,44)", "45-54" = "[44,54)",
                                                "55-64" = "[54,64)", "65-74" = "[64,75)",
                                                "75+" = "75+")

cc5 <- ggplot(melt_cm_thin5, aes(contact.age.group, Var1)) +
  geom_tile(aes(fill = value)) +
  coord_equal() +
  ggtitle("Wave 5 in thinly populated areas \n (02/6/21 - 09/6/21)") +
  geom_text(aes(label=sprintf("%0.1f", round(value, digits = 2))), size = 2) +
  scale_fill_gradient2(low = "#2000CD",
                       mid = "white",
                       high = "#D60C00", limits = c(0,4), midpoint = 2) +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20, title = "Contacts")) +
  theme(legend.position = "none")+
  theme(plot.title = element_text(size = 6, face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.text.x = element_text(size = 5, angle = 90, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 5))



############## Contact Matrix for wave6 in thinly  ############## 
# Variablen, die wir brauchen
part_matrix_thin6 <- d_self_all %>% 
  select(new_id, wave, age_group, qmktsize_16_0)%>%
  filter(qmktsize_16_0=="Thinly populated area" & wave == 6)

# Variablen, die wir brauchen
out_contacts_thin6 <- dt_outside_all_c_between %>% 
  select(new_id, wave, age_group) %>%
  filter(wave==6)

# df bearbeiten
part_matrix_thin6 <- part_matrix_thin6 %>% mutate(part_age =
                                                    case_when(age_group == "Under 1" ~ 2,
                                                              age_group == "1-4" ~ 2, 
                                                              age_group == "5-9" ~ 2,
                                                              age_group == "10-14" ~ 2,
                                                              age_group == "15-19" ~ 17,
                                                              age_group == "20-24" ~ 22,
                                                              age_group == "25-34" ~ 30,
                                                              age_group == "35-44" ~ 40,
                                                              age_group == "45-54" ~ 50,
                                                              age_group == "55-64" ~ 60,
                                                              age_group == "65-69" ~ 67,
                                                              age_group == "70-74" ~ 67,
                                                              age_group == "75-79" ~ 82,
                                                              age_group == "80-84" ~ 82,
                                                              age_group == "85 years or older" ~ 82))

out_contacts_thin6 <- out_contacts_thin6 %>% mutate(cnt_age =
                                                      case_when(age_group == "Under 1" ~ 2,
                                                                age_group == "1-4" ~ 2, 
                                                                age_group == "5-9" ~ 2,
                                                                age_group == "10-14" ~ 2,
                                                                age_group == "15-19" ~ 17,
                                                                age_group == "20-24" ~ 22,
                                                                age_group == "25-34" ~ 30,
                                                                age_group == "35-44" ~ 40,
                                                                age_group == "45-54" ~ 50,
                                                                age_group == "55-64" ~ 60,
                                                                age_group == "65-69" ~ 67,
                                                                age_group == "70-74" ~ 67,
                                                                age_group == "75-79" ~ 82,
                                                                age_group == "80-84" ~ 82,
                                                                age_group == "85 years or older" ~ 82))

# für socialmixR Variable in part_id ändern, survey_object erstellen
# part_id und survey_object werden für die Kontaktmatrix benötigt
colnames(part_matrix_thin6)[1] <- "part_id"
colnames(out_contacts_thin6)[1] <- "part_id" #out_contacts = non_hh-contacts
survey_object_thin6  <- survey(part_matrix_thin6, out_contacts_thin6)

# contact matrix erstellen
cm_thin6 <- contact_matrix(survey_object_thin6, age.limits = c(0, 14, 19, 24, 34, 44, 54, 64, 75),  missing.participant.age = c("remove"),
                           missing.contact.age = c("remove"),
                           symmetric = TRUE)

# output gespeichert, data_cm wird für heatmap benötigt
data_cm_thin6 <- cm_thin6$matrix

#Plot
melt_cm_thin6 <- melt(data_cm_thin6)

melt_cm_thin6$Var1[melt_cm_thin6$Var1 ==  1] <- "0-14"
melt_cm_thin6$Var1[melt_cm_thin6$Var1 ==  2] <- "15-19"
melt_cm_thin6$Var1[melt_cm_thin6$Var1 ==  3] <- "20-24"
melt_cm_thin6$Var1[melt_cm_thin6$Var1 ==  4] <- "25-34"
melt_cm_thin6$Var1[melt_cm_thin6$Var1 ==  5] <- "35-44"
melt_cm_thin6$Var1[melt_cm_thin6$Var1 ==  6] <- "45-54"
melt_cm_thin6$Var1[melt_cm_thin6$Var1 ==  7] <- "55-64"
melt_cm_thin6$Var1[melt_cm_thin6$Var1 ==  8] <- "65-74"
melt_cm_thin6$Var1[melt_cm_thin6$Var1 ==  9] <- "75+"


levels(melt_cm_thin6$contact.age.group) <- list("0-14" = "[0,14)", "15-19" = "[14,19)", 
                                                "20-24" = "[19,24)", "25-34" = "[24,34)", 
                                                "35-44" = "[34,44)", "45-54" = "[44,54)",
                                                "55-64" = "[54,64)", "65-74" = "[64,75)",
                                                "75+" = "75+")

cc6 <- ggplot(melt_cm_thin6, aes(contact.age.group, Var1)) +
  geom_tile(aes(fill = value)) +
  coord_equal() +
  ggtitle("Wave 6 in thinly populated areas \n (23/6/21 - 30/6/21)") +
  geom_text(aes(label=sprintf("%0.1f", round(value, digits = 2))), size = 2) +
  scale_fill_gradient2(low = "#2000CD",
                       mid = "white",
                       high = "#D60C00", limits = c(0,4), midpoint = 2) +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20, title = "Contacts")) +
  theme(legend.position = "none")+
  theme(plot.title = element_text(size = 6, face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.text.x = element_text(size = 5, angle = 90, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 5))
```

```{r contact matrix wave 7 thinly}
# Variablen, die wir brauchen
part_matrix_thin7 <- d_self_all %>% 
  select(new_id, wave, age_group, qmktsize_16_0)%>%
  filter(qmktsize_16_0=="Thinly populated area" & wave == 7)

# Variablen, die wir brauchen
out_contacts_thin7 <- dt_outside_all_c_between %>% 
  select(new_id, wave, age_group) %>%
  filter(wave==7)

# df bearbeiten
part_matrix_thin7 <- part_matrix_thin7 %>% mutate(part_age =
                                                    case_when(age_group == "Under 1" ~ 2,
                                                              age_group == "1-4" ~ 2, 
                                                              age_group == "5-9" ~ 2,
                                                              age_group == "10-14" ~ 2,
                                                              age_group == "15-19" ~ 17,
                                                              age_group == "20-24" ~ 22,
                                                              age_group == "25-34" ~ 30,
                                                              age_group == "35-44" ~ 40,
                                                              age_group == "45-54" ~ 50,
                                                              age_group == "55-64" ~ 60,
                                                              age_group == "65-69" ~ 67,
                                                              age_group == "70-74" ~ 67,
                                                              age_group == "75-79" ~ 82,
                                                              age_group == "80-84" ~ 82,
                                                              age_group == "85 years or older" ~ 82))

out_contacts_thin7 <- out_contacts_thin7 %>% mutate(cnt_age =
                                                      case_when(age_group == "Under 1" ~ 2,
                                                                age_group == "1-4" ~ 2, 
                                                                age_group == "5-9" ~ 2,
                                                                age_group == "10-14" ~ 2,
                                                                age_group == "15-19" ~ 17,
                                                                age_group == "20-24" ~ 22,
                                                                age_group == "25-34" ~ 30,
                                                                age_group == "35-44" ~ 40,
                                                                age_group == "45-54" ~ 50,
                                                                age_group == "55-64" ~ 60,
                                                                age_group == "65-69" ~ 67,
                                                                age_group == "70-74" ~ 67,
                                                                age_group == "75-79" ~ 82,
                                                                age_group == "80-84" ~ 82,
                                                                age_group == "85 years or older" ~ 82))

# für socialmixR Variable in part_id ändern, survey_object erstellen
# part_id und survey_object werden für die Kontaktmatrix benötigt
colnames(part_matrix_thin7)[1] <- "part_id"
colnames(out_contacts_thin7)[1] <- "part_id" #out_contacts = non_hh-contacts
survey_object_thin7  <- survey(part_matrix_thin7, out_contacts_thin7)

# contact matrix erstellen
cm_thin7 <- contact_matrix(survey_object_thin7, age.limits = c(0, 14, 19, 24, 34, 44, 54, 64, 75),  missing.participant.age = c("remove"),
                           missing.contact.age = c("remove"),
                           symmetric = TRUE)

# output gespeichert, data_cm wird für heatmap benötigt
data_cm_thin7 <- cm_thin7$matrix

#Plot
melt_cm_thin7 <- melt(data_cm_thin7)

melt_cm_thin7$Var1[melt_cm_thin7$Var1 ==  1] <- "0-14"
melt_cm_thin7$Var1[melt_cm_thin7$Var1 ==  2] <- "15-19"
melt_cm_thin7$Var1[melt_cm_thin7$Var1 ==  3] <- "20-24"
melt_cm_thin7$Var1[melt_cm_thin7$Var1 ==  4] <- "25-34"
melt_cm_thin7$Var1[melt_cm_thin7$Var1 ==  5] <- "35-44"
melt_cm_thin7$Var1[melt_cm_thin7$Var1 ==  6] <- "45-54"
melt_cm_thin7$Var1[melt_cm_thin7$Var1 ==  7] <- "55-64"
melt_cm_thin7$Var1[melt_cm_thin7$Var1 ==  8] <- "65-74"
melt_cm_thin7$Var1[melt_cm_thin7$Var1==  9] <- "75+"


levels(melt_cm_thin7$contact.age.group) <- list("0-14" = "[0,14)", "15-19" = "[14,19)", 
                                                "20-24" = "[19,24)", "25-34" = "[24,34)", 
                                                "35-44" = "[34,44)", "45-54" = "[44,54)",
                                                "55-64" = "[54,64)", "65-74" = "[64,75)",
                                                "75+" = "75+")

cc7 <- ggplot(melt_cm_thin7, aes(contact.age.group, Var1)) +
  geom_tile(aes(fill = value)) +
  coord_equal() +
  ggtitle("Wave 7 in thinly populated areas \n (14/7/21 - 21/7/21)") +
  geom_text(aes(label=sprintf("%0.1f", round(value, digits = 2))), size = 2) +
  scale_fill_gradient2(low = "#2000CD",
                       mid = "white",
                       high = "#D60C00", limits = c(0,4), midpoint = 2) +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20, title = "Contacts")) +
  theme(legend.position = "none")+
  theme(plot.title = element_text(size = 6, face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.text.x = element_text(size = 5, angle = 90, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 5))

```

```{r contact matrix wave 8 thinly}
# Variablen, die wir brauchen
part_matrix_thin8 <- d_self_all %>% 
  select(new_id, wave, age_group, qmktsize_16_0)%>%
  filter(qmktsize_16_0=="Thinly populated area" & wave == 8)

# Variablen, die wir brauchen
out_contacts_thin8 <- dt_outside_all_c_between %>% 
  select(new_id, wave, age_group) %>%
  filter(wave==8)

# df bearbeiten
part_matrix_thin8 <- part_matrix_thin8 %>% mutate(part_age =
                                                    case_when(age_group == "Under 1" ~ 2,
                                                              age_group == "1-4" ~ 2, 
                                                              age_group == "5-9" ~ 2,
                                                              age_group == "10-14" ~ 2,
                                                              age_group == "15-19" ~ 17,
                                                              age_group == "20-24" ~ 22,
                                                              age_group == "25-34" ~ 30,
                                                              age_group == "35-44" ~ 40,
                                                              age_group == "45-54" ~ 50,
                                                              age_group == "55-64" ~ 60,
                                                              age_group == "65-69" ~ 67,
                                                              age_group == "70-74" ~ 67,
                                                              age_group == "75-79" ~ 82,
                                                              age_group == "80-84" ~ 82,
                                                              age_group == "85 years or older" ~ 82))

out_contacts_thin8 <- out_contacts_thin8 %>% mutate(cnt_age =
                                                      case_when(age_group == "Under 1" ~ 2,
                                                                age_group == "1-4" ~ 2, 
                                                                age_group == "5-9" ~ 2,
                                                                age_group == "10-14" ~ 2,
                                                                age_group == "15-19" ~ 17,
                                                                age_group == "20-24" ~ 22,
                                                                age_group == "25-34" ~ 30,
                                                                age_group == "35-44" ~ 40,
                                                                age_group == "45-54" ~ 50,
                                                                age_group == "55-64" ~ 60,
                                                                age_group == "65-69" ~ 67,
                                                                age_group == "70-74" ~ 67,
                                                                age_group == "75-79" ~ 82,
                                                                age_group == "80-84" ~ 82,
                                                                age_group == "85 years or older" ~ 82))


# für socialmixR Variable in part_id ändern, survey_object erstellen
# part_id und survey_object werden für die Kontaktmatrix benötigt
colnames(part_matrix_thin8)[1] <- "part_id"
colnames(out_contacts_thin8)[1] <- "part_id" #out_contacts = non_hh-contacts
survey_object_thin8  <- survey(part_matrix_thin8, out_contacts_thin8)

# contact matrix erstellen
cm_thin8 <- contact_matrix(survey_object_thin8, age.limits = c(0, 14, 19, 24, 34, 44, 54, 64, 75),  missing.participant.age = c("remove"),
                           missing.contact.age = c("remove"),
                           symmetric = TRUE)

# output gespeichert, data_cm wird für heatmap benötigt
data_cm_thin8 <- cm_thin8$matrix

#Plot
melt_cm_thin8 <- melt(data_cm_thin8)

melt_cm_thin8$Var1[melt_cm_thin8$Var1 ==  1] <- "0-14"
melt_cm_thin8$Var1[melt_cm_thin8$Var1 ==  2] <- "15-19"
melt_cm_thin8$Var1[melt_cm_thin8$Var1 ==  3] <- "20-24"
melt_cm_thin8$Var1[melt_cm_thin8$Var1 ==  4] <- "25-34"
melt_cm_thin8$Var1[melt_cm_thin8$Var1 ==  5] <- "35-44"
melt_cm_thin8$Var1[melt_cm_thin8$Var1 ==  6] <- "45-54"
melt_cm_thin8$Var1[melt_cm_thin8$Var1 ==  7] <- "55-64"
melt_cm_thin8$Var1[melt_cm_thin8$Var1 ==  8] <- "65-74"
melt_cm_thin8$Var1[melt_cm_thin8$Var1==  9] <- "75+"


levels(melt_cm_thin8$contact.age.group) <- list("0-14" = "[0,14)", "15-19" = "[14,19)", 
                                                "20-24" = "[19,24)", "25-34" = "[24,34)", 
                                                "35-44" = "[34,44)", "45-54" = "[44,54)",
                                                "55-64" = "[54,64)", "65-74" = "[64,75)",
                                                "75+" = "75+")

cc8 <- ggplot(melt_cm_thin8, aes(contact.age.group, Var1)) +
  geom_tile(aes(fill = value)) +
  coord_equal() +
  ggtitle("Wave 8 in thinly populated areas \n (04/8/21 - 11/8/21)") +
  geom_text(aes(label=sprintf("%0.1f", round(value, digits = 2))), size = 2) +
  scale_fill_gradient2(low = "#2000CD",
                       mid = "white",
                       high = "#D60C00", limits = c(0,4), midpoint = 2) +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20, title = "Contacts")) +
  theme(legend.position = "none")+
  theme(plot.title = element_text(size = 6, face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.text.x = element_text(size = 5, angle = 90, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 5))



```


```{r combine plots only for non-hh contacts}
common <- gridExtra::grid.arrange(aa1, aa2, aa3, aa4, aa5, aa6, aa7, aa8, one_legend,
                        bb1, bb2, bb3, bb4, bb5, bb6, bb7, bb8, one_legend,
                        cc1, cc2, cc3, cc4, cc5, cc6, cc7, cc8, one_legend, ncol = 9)

annotate_figure(common, left = "Age of participants (in years)", 
                bottom = "Age of contacts (in years)", fig.lab.size = 2)

```
