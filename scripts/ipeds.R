################################################################################
##
## <PROJ> Updated SREB Data, 2017
## <FILE> ipeds.r
## <AUTH> Will Doyle and Benjamin Skinner
## <INIT> 27 April 2017
##
################################################################################

## 100663 100706 100751

## PURPOSE

## The purpose of this file is automate the process of:
##
## (1) downloading appropriate IPEDS survey data files
## (2) subsetting full datasets to desired variables
## (3) combining across datasets and years
## (4) output in tidy dataset

## CODE

## Code modified from <ipeds_combine.r>:

## https://gist.github.com/btskinner/f42c87507169d0ba773c

##Libraries 
library(tidyverse)
## clear memory
rm(list=ls())

##options
options(stringsAsFactors=FALSE)

## load functions
source('functions.r')

## Output directory

## data dirs
cddir <- '../data/acs/'
rddir <- '../data/ipeds/'
mddir <- '../data/misc/'
addir <- '../data/analysis/'

## =============================================================================
## BUILD DATASETS 
## =============================================================================

## IPEDS institutional characteristics (using HD files)

filenames<-paste0('HD',2002:2017,'.zip')
var <- c('unitid','instnm','city','stabbr','control','sector')
attr_data <- build_dataset_ipeds(filenames=filenames, datadir = rddir, vars = var,years=c(2002:2017))

## IPEDS enrollments (using EFIA files) These are "off" by one year bc of end of year reporting


##EFTEUG: Exists 2004-2016
##

filenames <-paste0('EFIA',2004:2017,'.zip')
var <- c('unitid','efteug')
enroll_data <- build_dataset_ipeds(filenames=filenames, datadir = rddir, vars = var,years=c(2002:2010))

## IPEDS completions
#2000-2007 Varnames
#CRACE15: Grand total men
#CRACE16: Grand total women
#AWLEVEL: Award Level
#UNITID: natch
#CIPCODE
#2016 Varnamnes
#CIPCODE
#AWLEVEL
#CTOTALT (2008-2016 as well)


## For 2002-2007
var <- c('unitid','cipcode','awlevel','crace15','crace16')

filenames<-paste0("C",2002:2007,"_A.zip")

completions_data <- build_dataset_ipeds(filenames=filenames, datadir = rddir,
                                        vars = var,years=2002:2007)


## For 2008-2016
var <- c('unitid','cipcode','awlevel','ctotalt')

filenames<-paste0("C",2008:2016,"_A.zip")

completions_data <- build_dataset_ipeds(filenames=filenames, datadir = rddir,
                                     vars = var,years=2008:2016)

## =============================================================================
## MERGE DATASETS
## =============================================================================

pattern <- '*\\_data\\b'; byvar <- c('unitid', 'year')
inst <- merge_ipeds(pattern = pattern, byvar = byvar)

## Publics only
inst<-filter(inst,control==1)

## =============================================================================
## MAKE TIDY
## =============================================================================

inst<-inst%>%
    gather(key=faminccat,value=netprice,starts_with("np"))

inst$faminccat<-as.character(inst$faminccat)

## change npis categories to useful values
levels <- c('< 30k','30k to 48k','48k to 75k','75k to 110k','> 110k')

inst$faminccat[inst$faminccat == 'npis412'] <- levels[1]
inst$faminccat[inst$faminccat == 'npis422'] <- levels[2]
inst$faminccat[inst$faminccat == 'npis432'] <- levels[3]
inst$faminccat[inst$faminccat == 'npis442'] <- levels[4]
inst$faminccat[inst$faminccat == 'npis452'] <- levels[5]

## convert back to factor
inst$faminccat <- factor(inst$faminccat, levels = levels)

## =============================================================================
## ADD VARS
## =============================================================================

## merge in state and  add full state name for state abbreviation
sl <- read.csv(paste0(mddir, 'statename.csv'))
inst <- left_join(inst, sl, by = 'stabbr')

write.csv(inst,  paste0(addir, 'all_institutions.csv'), row.names = FALSE)

## Create National Averages
inst_national<-inst

## Initialize sectoring
inst_national$new_sector <- NA

## All public four years
inst_national$new_sector[inst_national$sector==1] <- 2

#Reassign public doctoral
inst_national$new_sector[inst_national$new_sector==2 & inst_national$carnegie %in% c(15:16)]<-1

## Public 2 years

inst_national$new_sector[inst_national$sector==4]<-3

## Reassign any asscoiate dominant 2 years from 4 years

inst_national$new_sector[inst_national$new_sector==2 & inst_national$ccipug==2]<-3

## drop if sector is NA since it cannot be used
inst_national<- inst_national[!is.na(inst_national$new_sector),]

## drop cc of the airforce & service academies
inst_national<- inst_national[inst_national$unitid != 100636, ]

## Drop military academies

mil.ids<-c(128328,
           130624,
           197027,
           197036,
           164155,
           164155
           )

inst_national<-filter(inst_national,!(unitid%in%mil.ids))

write.csv(inst_national,file=paste0(addir,"inst_national.csv"))

##==============================================================================
## Subset to SREB states only
##==============================================================================


sreb.abbr<-c("AL","AR","DE","FL","GA","KY","LA","MD","MS","NC","OK","SC", "TN","TX","VA","WV")

inst<-filter(inst,stabbr%in%sreb.abbr)

## REPORT CATEGORIES -----------------------------------------------------------


## list of insts and sectors

sreb.sectors<-read.csv(paste0(mddir,"srebsectoripeds.csv"),as.is=T)

sreb.sectors<-sreb.sectors %>% select(-X)

sreb.sectors<-sreb.sectors %>% rename(sreb.sector=sector)

sreb.sectors<-sreb.sectors %>% select(unitid,sreb.sector)

inst<-left_join(inst,sreb.sectors,by="unitid")

inst$group[inst$sreb.sector%in%c(41,42)]<-1
inst$group[inst$sreb.sector%in%c(43:49)]<-2
inst$group[inst$sreb.sector%in%c(21:29)]<-3
inst$group[inst$sreb.sector%in%c(61:69)]<-4

#Arkansas Problem
inst$group[inst$unitid== 106245]<-1

## Drop if bad merge

inst<-filter(inst,is.na(sreb.sector)==FALSE)

## =============================================================================
## CLEAN
## =============================================================================

## drop if group is NA since it cannot be used
inst <- inst[!is.na(inst$group),]

## resort dataframe; order variables; reset rownames

inst <- inst %>% arrange(stabbr,year,group)

inst<-inst %>% select(stabbr,year,group,instnm,unitid,city,control:sreb.sector)

##Drop rownames
rownames(inst) <- NULL

## =============================================================================
## Some misc cleanup
## =============================================================================

##Drop if no undergrads

inst<-inst[inst$fteug>0,]

## drop cc of the airforce
inst <- inst[inst$unitid != 100636, ]

## Drop military academies

mil.ids<-c(128328,
           130624,
           197027,
           197036,
           164155,
           164155
           )

inst<-filter(inst,!(unitid%in%mil.ids))

## Proportion in each sector
inst<-inst%>%
    group_by(year,stabbr,group,faminccat) %>%
        mutate(all_fte=sum(fteug,na.rm=TRUE))

inst$prop_fte<-inst$fteug/inst$all_fte

## If less than 3% of fte in sector, then drop it

inst<-inst%>%
    group_by(year,stabbr,faminccat) %>%
        mutate(total_state_fte=sum(fteug,na.rm=TRUE))
            
inst$sector_prop<-inst$all_fte/inst$total_state_fte

inst<-inst%>%filter(sector_prop>=.03)

inst_fte<-inst%>%group_by(year,stabbr,group,faminccat)%>%filter(faminccat=="< 30k")%>%
  summarize(fte_sector=mean(sector_prop))%>%select(year,stabbr,fte_sector)

write_csv(inst_fte,paste0(addir,"inst_fte.csv"))

## =============================================================================
## OUTPUT FINAL DATASET AS .CSV
## =============================================================================

write.csv(inst, file = paste0(addir, 'institutions.csv'), row.names = FALSE)

##################################################
## Generate list of institutions
##################################################

inst2<-inst

for (st in sreb.abbr){

    inst<-inst2%>%filter(stabbr==st,year==2014,faminccat=="< 30k")
    
    doc = docx()
    options( "ReporteRs-fontsize" = 12 ,'ReporteRs-default-font'='Calibri')
    addSection(doc,ncol=2,columns.only = TRUE)
    
    for (i in 1:4){
      ## List of insts
        grouplist<-inst%>%ungroup()%>%filter(group==i)%>%select(instnm)
      ## vector
        grouplist<-grouplist$instnm
     ## piece of text
        
        if (length(grouplist)>0){
          pot_group_title<-pot(groups[i],textProperties(font.weight = "bold"))
          addParagraph(doc,pot_group_title)
        }
        
        if (length(grouplist)>0){
        for (j in 1:length(grouplist))
        doc=addParagraph(doc,pot(grouplist[j]))  
        }
        
        ##blank paragraph
        blank.par<-pot(" ")
        
        my.left<-parProperties(text.align="left")
        
        doc = addParagraph( doc ,blank.par,par.properties=my.left)
        
    } #End group loop
    addSection(doc,ncol=1,columns.only = TRUE)
    mypot<-pot("Category 1 four-year institutions award at least 30 doctoral degrees in   five different areas. Category 2 includes all other four-year institutions.")
    addParagraph(doc,mypot)
    writeDoc(doc,paste0(worddir,st,"_inst_list.docx"))
    
} #End state loop

## =============================================================================
## END
################################################################################
