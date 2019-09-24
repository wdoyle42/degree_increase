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

filenames<-paste0('HD',2004:2017,'.zip')
var <- c('unitid','instnm','city','stabbr','control','sector')
attr_data <- build_dataset_ipeds(filenames=filenames, datadir = rddir, vars = var,years=c(2004:2017))

## IPEDS enrollments (using EFIA files) These are "off" by one year bc of end of year reporting


##EFTEUG: Exists 2004-2016
##

filenames <-paste0('EFIA',2004:2017,'.zip')
var <- c('unitid','efteug')
enroll_data <- build_dataset_ipeds(filenames=filenames, datadir = rddir, vars = var,years=c(2004:2017))

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

filenames<-paste0("C",2004:2007,"_A.zip")

completions_data_a <- build_dataset_ipeds(filenames=filenames, datadir = rddir,
                                        vars = var,years=2004:2007)

completions_data_a%>%
  mutate(ctotalt=crace15+crace16)%>%
  select(unitid,year,cipcode,awlevel,ctotalt)->completions_data_a

completions_data_a%>%
  filter(cipcode==99)->completions_data_a

## For 2008-2016
var <- c('unitid','cipcode','awlevel','ctotalt')

filenames<-paste0("C",2008:2016,"_A.zip")

completions_data_b <- build_dataset_ipeds(filenames=filenames, datadir = rddir,
                                     vars = var,years=2008:2016)

completions_data_b%>%
  filter(cipcode==99)->completions_data_b


completions_data<-bind_rows(completions_data_a,completions_data_b)

completions_data%>%
  group_by(unitid,year,awlevel)%>%
  summarize(degrees=sum(ctotalt))%>%
  select(unitid,year,awlevel,degrees)%>%
  arrange(unitid,year)->completions_data


rm(list=c("completions_data_a","completions_data_b"))
## =============================================================================
## MERGE DATASETS
## =============================================================================

pattern <- '*\\_data\\b'
byvar <- c('unitid', 'year')

inst <- merge_ipeds(pattern = pattern, byvar = byvar)

## Drop military academies
mil_ids <- c(100636,
             128328,
             130624,
             197027,
             197036,
             164155,
             164155)

inst%>%filter(!(unitid%in%(mil_ids)))->inst

inst%>%group_by(stabbr,year,awlevel)%>%
  filter(control==1&year==2013)%>%
  summarize(total_degrees=sum(degrees,na.rm = TRUE))%>%
  arrange(stabbr,year,awlevel)%>%View()

save(inst,file=paste0(addir,"inst.Rdata"))
