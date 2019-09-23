################################################################################
##
## <PROJ> National Afforability Report: 50 States
## <FILE> functions.r
## <AUTH> Will Doyle and Benjamin Skinner
## <INIT> 28 May 2015
##
################################################################################

## PURPOSE

## The purpose of this file is hold all user functions in one file. Some code
## has been modified from other sources and is noted as such.

## =============================================================================
## FUNCTIONS
## =============================================================================

## -----------------------------------------------------------------------------
## check and download files function (ipeds)
## -----------------------------------------------------------------------------

check_download_ipeds <- function(filenames, dir,...) {
    
    ## subset to files not found in directory
    dnld <- filenames[!(filenames %in% list.files(dir))]

    ## download files not in directory
    base <- 'https://nces.ed.gov/ipeds/datacenter/data/'
    sapply(dnld, function(x) {

        message(paste0('\n Downloading ', x, '\n'))
        download.file(paste0(base, x), paste0(dir, x), 'curl')
        
    })

    ## return files list
    return(filenames)

}

## -----------------------------------------------------------------------------
## unzip function (modified) from
## http://stackoverflow.com/questions/8986818/automate-zip-file-reading-in-r
## -----------------------------------------------------------------------------

read_zip <- function(zipfile, dir) {

    ## create a name for the dir where we'll unzip
    zipdir <- tempfile()

    ## create the dir using that name
    dir.create(zipdir)

    ## unzip the file into the dir
    unzip(paste0(dir, zipfile), exdir = zipdir)

    ## get the files into the dir
    files <- list.files(zipdir, recursive = TRUE)

    print(files)

    ## chose rv file if more than two 
    if (length(files) > 1) {

        file <- grep('*_rv.csv', files, value = TRUE,invert=TRUE,ignore.case = TRUE)

        print(file)

    } else {

        file <- files[1]

        print(file) 
    }

    ## get the full name of the file
    file <- paste(zipdir, file, sep = '/')

    ## read the file
    message(paste('\n Reading data from zip file',file,'\n'))
    read.csv(file, header = TRUE,row.names=NULL)
}

## -----------------------------------------------------------------------------
## combine IPEDS yearly files into single file
## <see> https://gist.github.com/btskinner/f42c87507169d0ba773c
## -----------------------------------------------------------------------------

build_dataset_ipeds <- function(filenames, datadir,
                                conditions = NULL, vars = NULL, years) {

    ## use check.download.ipeds function to check, download, store files 
    f <- check_download_ipeds(filenames, datadir)

    print(f)
    
    ## loop through files
    for (i in 1:length(f)) {

        ## unzip data with read.zip function
        data <- read_zip(f[i], datadir)

        ## lower variable names in dataset
        names(data) <- tolower(names(data))

        ##KLUDGE
        if(names(data)[1]=="row.names"){
            names(data)[1:length(names(data))-1]<-names(data)[2:length(names(data))]
        }

        ## subset data based on conditions
        if (!is.null(conditions)) {

            message('\n Subsetting data by specified conditions \n')
            cond <- eval(parse(text = (gsub('(\\b[[:alpha:]]+\\b)',
                                        'data$\\1', conditions))))
            print(head(data,n=20))
            data <- data[cond,]
            print(head(data,n=20))
        }

        ## subset data based on rows needed
        if (!is.null(vars)) {

            message('\n Subsetting data by specified variables \n')

            data <- data[,vars]

        }

        ## get year from file name
        year <-years[i]

        ## add year column
        data$year <- year

        ## append dataset to prior data (data0)
        message(paste0('\n Appending dataset: ', f[i], '\n'))
        if(i == 1) {

            ## save a new data name for later rbind
            data0 <- data

        } else if(i == 2) {

            ## first appending
            result <- rbind(data0, data)

        } else {

            ## append the rest
            result <- rbind(result, data)

        }
    }

    ## sort dataset: unitid by year
    message('\n Sorting results \n')
    result <- result[order(result$unitid, result$year),]

    ## reset rownames
    message('\n Resetting rownames \n')
    rownames(result) <- NULL

    ## return dataset
    message('\n Returning dataset \n')
    return(result)
}

## -----------------------------------------------------------------------------
## merge ipeds datasets function
## -----------------------------------------------------------------------------

merge_ipeds <- function(datasets = NULL, pattern = NULL, byvar = 'unitid') {

    ## can either list datasets or use pattern (explicit list overrules)
    if (!is.null(pattern)) {
        
        ds <- ls(envir = .GlobalEnv, pattern = pattern)
        
    } else if (!is.null(datasets)) {
        
        ds <- datasets
        
    } else {
        
        message('\n Need a list of datasets or a pattern \n')
        return
    }

    ## merge datasets
    for(i in 1:length(ds)) {
        if (i == 1) {

            message(paste0('\n Starting with ', ds[i], '\n'))
            final.data <- eval(parse(text = ds[i]))
            
        } else {

            message(paste0('\n merging in ', ds[i], '\n'))
            merge.data <- eval(parse(text = ds[i]))
            final.data <- merge(final.data, merge.data, by = byvar, all.x = TRUE)
        }
    }

    ## reset rownames
    rownames(final.data) <- NULL

    ## return final dataset
    return(final.data)
}

## -----------------------------------------------------------------------------
## check proportion missing (https://gist.github.com/stephenturner/841686)
## -----------------------------------------------------------------------------

propmiss <- function(dataframe) {
	m <- sapply(dataframe, function(x) {
		data.frame(
			nmiss=sum(is.na(x)), 
			n=length(x), 
			propmiss=sum(is.na(x))/length(x)
		)
	})
	d <- data.frame(t(m))
	d <- sapply(d, unlist)
	d <- as.data.frame(d)
	d$variable <- row.names(d)
	row.names(d) <- NULL
	d <- cbind(d[ncol(d)],d[-ncol(d)])
	return(d[order(d$propmiss), ])
}


## =============================================================================
## END
################################################################################
