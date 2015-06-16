\name{s3.cmds}
\alias{s3.mb}
\alias{s3.rb}
\alias{s3.ls}
\alias{s3.la}
\alias{s3.put}
\alias{s3.put.file}
\alias{s3.get}
\alias{s3.del}
\alias{s3.info}
\alias{s3.du}
\title{
  Run S3 commands from R.
}
\description{
  Run most of the commands available in s3cmd.
}
\usage{
s3.mb(bucket, bucket.location = "US", verbose = FALSE,
      debug = FALSE, access.key='', secret.key='')

s3.rb(bucket, recursive = FALSE, force = FALSE,
      bucket.location = "US", verbose = FALSE, debug = FALSE,
      access.key='', secret.key='')

s3.ls(bucket = NULL, bucket.location = "US",
      human.readable.sizes = TRUE,
      list.md5 = FALSE, verbose = FALSE, debug = FALSE,
      echo = FALSE, access.key='', secret.key='')

s3.la(bucket.location = "US", human.readable.sizes = TRUE,
      list.md5 = FALSE, verbose = FALSE, debug = FALSE, 
      access.key='', secret.key='')

s3.put(x, bucket, bucket.location = "US", verbose = FALSE,
       debug = FALSE, encrypt = FALSE, access.key='', 
       secret.key='')

s3.put.file(file, bucket, bucket.location="US", verbose=FALSE,
       debug=FALSE,encrypt=FALSE, access.key='', secret.key='')

s3.get(bucket, bucket.location = "US", verbose = FALSE,
       debug = FALSE, access.key='', secret.key='')

s3.del(bucket, bucket.location = "US",
       human.readable.sizes = TRUE,list.md5 = FALSE,
       verbose = FALSE, debug = FALSE, access.key='', 
       secret.key='')

s3.info(bucket, bucket.location = "US",
        human.readable.sizes = TRUE, list.md5 = FALSE,
        verbose = FALSE, debug = FALSE, access.key='', 
        secret.key='')

s3.du(bucket, bucket.location = "US",
      human.readable.sizes = TRUE, list.md5 = FALSE,
      verbose = FALSE, debug = FALSE, access.key='', 
      secret.key='')
}
\arguments{
  \item{x}{an R object to upload.}
  \item{file}{a file to upload.}
  \item{bucket}{S3 bucket}
  \item{bucket.location}{Specify datacentre where to create the bucket. Possible values are US (default) or EU.}
  \item{human.readable.sizes}{Print sizes in human readable form.}
  \item{list.md5}{Include MD5 sums in bucket listings (only for ls command).}
  \item{verbose}{Enable verbose output.}
  \item{debug}{Enable debug output.}
  \item{recursive}{Recursive upload, download or removal. When used with del it can remove all the files in a bucket.}
  \item{force}{Force overwrite and other dangerous operations. Can be used to remove a non-empty buckets with s3cmd rb --force s3://bkt}
  \item{encrypt}{Use GPG encryption to protect stored objects from
    unauthorized access. See http://s3tools.org/s3cmd-public for
    details aboutencryption.}
  \item{echo}{if TRUE echo command before executing.}
  \item{access.key}{Specify an AWS access key to perform this operation. If not set, s3cmd will lookup for a file name .s3cfg in your home folder}
  \item{secret.key}{Specify an AWS secret key to perform this operation. If not set, s3cmd will lookup for a file name .s3cfg in your home folder}
}
\value{
  Return value depends on command used.
  with the exception of get, most commands return as text the reply from
  the call to the underlying s3cmd.
}
\references{
  http://s3tools.org/s3cmd
}
\author{
  Whit Armstrong, with contributions from Paulo Miguel Almeida Rodenas
}
