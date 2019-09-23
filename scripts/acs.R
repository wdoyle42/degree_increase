################################################################################
##
## <PROJ> National Afforability Report: 50 States
## <FILE> acs.r
## <AUTH> Will Doyle and Benjamin Skinner
## <INIT> 27 April 2017
##
################################################################################

## PURPOSE



## The purpose of this file is to clean ACS files. It uses the 3 year rolling
## estimates starting in 2008 and ending in 2014. The years of interest are 2008
## to 2013. These are the last years in each file.

## LARGE FILES NOTE ------------------------------------------------------------
##
## The required ACS files,
##
## ss<09-13>pus<a-d>.csv
##
## are very large. They are from the csv_pus.zip files, which can be downloaded
## from the US census ftp site: ftp://ftp2.census.gov. On OS X, you might
## need to use -unar- rather than -unzip- to open it. The files can be stored
## in data/acs dir or elsewhere and linked with symbolic link.

## WD Note: found it here.
## http://www2.census.gov/programs-surveys/acs/data/pums/2013/3-Year/csv_pus.zip
## Update, 2017, no more three year? using 5 year, found here:
## https://www2.census.gov/programs-surveys/acs/data/pums/2014/5-Year/

## clear memory
rm(list=ls())

## libraries
require(data.table); require(dplyr); require(bit64)

## data dirs
#cddir <- '../../data/acs/'
cddir<-'/Volumes/DOYLE_MAC/Data/acs/'
mddir <- '../../data/misc/'
addir <- '../../data/analysis/'

## =============================================================================
## BUILD DATASETS 
## =============================================================================

## file list (entire US file is split into 4 smaller ones with a-d endings)
files <- letters[1:4]

## years
years <- c('08','09','10','11','12','13','14')

## double loop through reading in data: within each year, combine/set data
for (y in 1:length(years)) {

    ## store year (get middle year so - 1)
    year <- as.integer(years[y])  + 2000

    ## only want certain columns 
        cols <- c('ST','WGTP','FINCP')

    
    for (f in 1:length(files)) {

        ## file name
        fn <- paste0('ss', years[y], 'hus', files[f], '.csv')

        ## read in file with only selected classes; make dataframe; lower names
        message(paste0('\nReading file: ', fn))
        d <- fread(paste0(cddir, fn), select = cols)
        d <- data.frame(d)
        names(d) <- tolower(names(d))
        

        ## rename st to stnum like IPEDS
        names(d)[names(d) == 'st'] <- 'stnum'

        ##Keep only those with family income data
        d<-filter(d, !is.na(fincp))
        
        ## save dataset
        message(paste0('\nAssigning partial dataset to object\n'))
        assign(paste0('d', f), d)

        ## remove d object from memory
        rm('d')
        
    }

    ## bind parts into whole; remove parts (help memory)
    message(paste0('\nBinding partial datasets to year dataset\n'))
    d <- rbind(d1, d2, d3, d4); rm(list = c('d1','d2','d3','d4'))

    ## -------------------------------------------------------------------------
    ## ADD INCOME CUTS
    ## -------------------------------------------------------------------------

    ## create breaks a la IPEDS categories; labels for levels
    breaks <- c(-Inf, 30000, 48000, 75000, 110000, Inf)
    levels <- c('< 30k','30k to 48k','48k to 75k','75k to 110k','> 110k')

    ## group based on income cuts
    message(paste0('\nCreating faminccat cuts\n'))
    d$faminccat <- cut(d$fincp, breaks = breaks, labels = levels)

    ## -------------------------------------------------------------------------
    ## MERGE WITH STATENAMES
    ## -------------------------------------------------------------------------

    ## merge in full state name for state abbreviation
    message(paste0('\nMerge in full state names\n'))
    sl <- read.csv(paste0(mddir, 'statename.csv'))
    d <- merge(d, sl, by = 'stnum', all.x = TRUE)
    
    ## -------------------------------------------------------------------------
    ## MAKE TIDY
    ## -------------------------------------------------------------------------

    ## sort by state
    d <- d[order(d$stabbr),]

    ##Get state population totals

    d<- d%>% group_by(stabbr) %>%
        mutate(state_total_pop=sum(wgtp))
    
    ## aggregate
    message(paste0('\nAggregate weighted income by state and faminccat\n'))
    d <- d %>% group_by(stabbr, faminccat) %>%
        summarize(aveinc = round(weighted.mean(fincp, w = wgtp)), #mean income in group, weighted
                  inc_pop=sum(wgtp), #population in group
                  state_total_pop=min(state_total_pop) #total pop (same across all levels)
                  )

    ## make into dataframe
    d <- data.frame(d)
    
    ## add year column for later merge 
    d$year <- year

    ## save as unique dataset for later bindings
    message(paste0('\nAssigning year dataset to object\n'))
    assign(paste0('d', year), d)
    
    ## remove d object from memory
    rm('d')

}

## =============================================================================
## RBIND YEARLY DATASETS
## =============================================================================

## bind yearly datasets into one
message(paste0('\nBind yearly datasets into one\n'))
d <- data.frame(rbind(d2008, d2009, d2010, d2011, d2012,d2013,d2014))

rm(list = c('d2008','d2009','d2010','d2011','d2012','d2013','d2014'))

## Create pct population variable
d$inc_pct_pop<-(d$inc_pop/d$state_total_pop)*100

## reorder columns: state, year, faminccat, aveinc
message(paste0('\nReorder columns, sort, reset rownames\n'))
d <- d[,c('stabbr','year','faminccat','inc_pct_pop','aveinc')]
d <- d[order(d$stabbr, d$year, d$faminccat),]
rownames(d) <- NULL

## =============================================================================
## OUTPUT FINAL DATASET AS .CSV
## =============================================================================

message(paste0('\nWrite data object to CSV file\n'))
write.csv(d, file = paste0(addir, 'states.csv'), row.names = FALSE)

## =============================================================================
## END
################################################################################
