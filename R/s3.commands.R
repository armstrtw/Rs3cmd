###########################################################################
## Copyright (C) 2015  Whit Armstrong                                    ##
## Contributor   2014  Paulo Miguel Almeida Rodenas                      ##
##                                                                       ##
## This program is free software: you can redistribute it and#or modify  ##
## it under the terms of the GNU General Public License as published by  ##
## the Free Software Foundation, either version 3 of the License, or     ##
## (at your option) any later version.                                   ##
##                                                                       ##
## This program is distributed in the hope that it will be useful,       ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of        ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         ##
## GNU General Public License for more details.                          ##
##                                                                       ##
## You should have received a copy of the GNU General Public License     ##
## along with this program.  If not, see <http:##www.gnu.org#licenses#>. ##
###########################################################################

.check.bucket <- function(bucket) {
    if(!is.character(bucket)) {
        stop("bucket is not character.")
    }
    if(length(bucket)!=1L) {
        stop("bucket has length > 1.")
    }
}

s3.mb <- function(bucket,bucket.location="US",verbose=FALSE,debug=FALSE,access.key='',secret.key='') {
    .check.bucket(bucket)
    s3.cmd <- paste("s3cmd mb",
                    bucket,
                    paste("--bucket-location",bucket.location),
                    "--no-progress",
                    ifelse(verbose,"--verbose",""),
                    ifelse(debug,"--debug",""),
                    ifelse(access.key != '',paste("--access_key",access.key,sep = '='),""),
                    ifelse(secret.key != '',paste("--secret_key",secret.key,sep = '='),"")
                    )
    system(s3.cmd,intern=TRUE)
}

s3.rb <- function(bucket,recursive=FALSE,force=FALSE,bucket.location="US",verbose=FALSE,debug=FALSE,access.key='',secret.key='') {
    .check.bucket(bucket)
    s3.cmd <- paste("s3cmd rb",
                    bucket,
                    "--no-progress",
                    ifelse(verbose,"--verbose",""),
                    ifelse(debug,"--debug",""),
                    ifelse(recursive,"--recursive",""),
                    ifelse(force,"--force",""),
                    ifelse(access.key != '',paste("--access_key",access.key,sep = '='),""),
                    ifelse(secret.key != '',paste("--secret_key",secret.key,sep = '='),"")
                    )
    system(s3.cmd,intern=TRUE)
}

s3.ls <- function(bucket=NULL,bucket.location="US",human.readable.sizes=TRUE,list.md5=FALSE,verbose=FALSE,debug=FALSE,echo=FALSE,access.key='',secret.key='') {
    if(!is.null(bucket)) {
        .check.bucket(bucket)
    }
    s3.cmd <- paste("s3cmd ls",
                    ifelse(!is.null(bucket),bucket,""),
                    paste("--bucket-location",bucket.location),
                    "--no-progress",
                    ifelse(verbose,"--verbose",""),
                    ifelse(debug,"--debug",""),
                    ifelse(human.readable.sizes,"--human-readable-sizes",""),
                    ifelse(list.md5,"--list-md5",""),
                    ifelse(access.key != '',paste("--access_key",access.key,sep = '='),""),
                    ifelse(secret.key != '',paste("--secret_key",secret.key,sep = '='),"")
                    )
    if(echo) { print(s3.cmd) }
    res <- system(s3.cmd,intern=TRUE)
    ans <- read.table(textConnection(res),as.is=TRUE)

    ## this is guessing
    ## directory output has only 3 cols
    ## file output has 4 or 5 (depending on mdsums)
    if(ncol(ans)==3) {
        colnames(ans) <- c("date","time","bucket")
    } else if(ncol(ans)==4 && !list.md5) {
        colnames(ans) <- c("date","time","size","bucket")
    } else if(ncol(ans)==5 && list.md5) {
        colnames(ans) <- c("date","time","size","md5","bucket")
    } else {
        warning("could not determine colnames of output.")
    }
    ans
}

s3.la <- function(bucket.location="US",human.readable.sizes=TRUE,list.md5=FALSE,verbose=FALSE,debug=FALSE,access.key='',secret.key='') {
    s3.cmd <- paste("s3cmd la",
                    paste("--bucket-location",bucket.location),
                    "--no-progress",
                    ifelse(verbose,"--verbose",""),
                    ifelse(debug,"--debug",""),
                    ifelse(human.readable.sizes,"--human-readable-sizes",""),
                    ifelse(list.md5,"--list-md5",""),
                    ifelse(access.key != '',paste("--access_key",access.key,sep = '='),""),
                    ifelse(secret.key != '',paste("--secret_key",secret.key,sep = '='),"")
                    )
    system(s3.cmd,intern=TRUE)
}

s3.put <- function(x,bucket,bucket.location="US",verbose=FALSE,debug=FALSE,encrypt=FALSE,access.key='',secret.key='') {
    .check.bucket(bucket)
    x.serialized <- tempfile()
    saveRDS(x,x.serialized)
    s3.cmd <- paste("s3cmd put",
                    x.serialized,
                    bucket,
                    ifelse(encrypt,"--encrypt",""),
                    paste("--bucket-location",bucket.location),
                    "--no-progress",
                    ifelse(verbose,"--verbose",""),
                    ifelse(debug,"--debug",""),
                    ifelse(access.key != '',paste("--access_key",access.key,sep = '='),""),
                    ifelse(secret.key != '',paste("--secret_key",secret.key,sep = '='),"")
                    )
    res <- system(s3.cmd,intern=TRUE)
    unlink(x.serialized)
    res
}

s3.get <- function(bucket,bucket.location="US",verbose=FALSE,debug=FALSE,access.key='',secret.key='') {
    .check.bucket(bucket)
    x.serialized <- tempfile(fileext=".rds")
    s3.cmd <- paste("s3cmd get",
                    bucket,
                    x.serialized,
                    paste("--bucket-location",bucket.location),
                    "--no-progress",
                    ifelse(verbose,"--verbose",""),
                    ifelse(debug,"--debug",""),
                    ifelse(access.key != '',paste("--access_key",access.key,sep = '='),""),
                    ifelse(secret.key != '',paste("--secret_key",secret.key,sep = '='),"")
                    )
    res <- system(s3.cmd,intern=TRUE)
    ans <- readRDS(x.serialized)
    unlink(x.serialized)
    ans
}


s3.del <- function(bucket,bucket.location="US",human.readable.sizes=TRUE,list.md5=FALSE,verbose=FALSE,debug=FALSE,access.key='',secret.key='') {
    .check.bucket(bucket)
    s3.cmd <- paste("s3cmd del",
                    bucket,
                    paste("--bucket-location",bucket.location),
                    "--no-progress",
                    ifelse(verbose,"--verbose",""),
                    ifelse(debug,"--debug",""),
                    ifelse(human.readable.sizes,"--human-readable-sizes",""),
                    ifelse(list.md5,"--list-md5",""),
                    ifelse(access.key != '',paste("--access_key",access.key,sep = '='),""),
                    ifelse(secret.key != '',paste("--secret_key",secret.key,sep = '='),"")
                    )
    system(s3.cmd,intern=TRUE)
}

s3.info <- function(bucket,bucket.location="US",human.readable.sizes=TRUE,list.md5=FALSE,verbose=FALSE,debug=FALSE,access.key='',secret.key='') {
    .check.bucket(bucket)
    s3.cmd <- paste("s3cmd info",
                    bucket,
                    paste("--bucket-location",bucket.location),
                    "--no-progress",
                    ifelse(verbose,"--verbose",""),
                    ifelse(debug,"--debug",""),
                    ifelse(human.readable.sizes,"--human-readable-sizes",""),
                    ifelse(list.md5,"--list-md5",""),
                    ifelse(access.key != '',paste("--access_key",access.key,sep = '='),""),
                    ifelse(secret.key != '',paste("--secret_key",secret.key,sep = '='),"")
                    )
    system(s3.cmd,intern=TRUE)
}


s3.du <- function(bucket,bucket.location="US",human.readable.sizes=TRUE,list.md5=FALSE,verbose=FALSE,debug=FALSE,access.key='',secret.key='') {
    .check.bucket(bucket)
    s3.cmd <- paste("s3cmd du",
                    bucket,
                    paste("--bucket-location",bucket.location),
                    "--no-progress",
                    ifelse(verbose,"--verbose",""),
                    ifelse(debug,"--debug",""),
                    ifelse(human.readable.sizes,"--human-readable-sizes",""),
                    ifelse(list.md5,"--list-md5",""),
                    ifelse(access.key != '',paste("--access_key",access.key,sep = '='),""),
                    ifelse(secret.key != '',paste("--secret_key",secret.key,sep = '='),"")
                    )
    system(s3.cmd,intern=TRUE)
}


s3.put.file <- function(file, bucket,bucket.location="US", verbose=FALSE,debug=FALSE,encrypt=FALSE,access.key='',secret.key='') {
    .check.bucket(bucket)
    s3.cmd <- paste("s3cmd put",
                    file,
                    bucket,
                    ifelse(encrypt,"--encrypt",""),
                    paste("--bucket-location",bucket.location),
                    "--no-progress",
                    ifelse(verbose,"--verbose",""),
                    ifelse(debug,"--debug",""),
                    ifelse(access.key != '',paste("--access_key",access.key,sep = '='),""),
                    ifelse(secret.key != '',paste("--secret_key",secret.key,sep = '='),"")
                    )
    res <- system(s3.cmd,intern=TRUE)
    res
}
