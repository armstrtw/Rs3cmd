************
Introduction
************

:Date: June 16, 2015
:Authors: Whit Armstrong
:Contact: armstrong.whit@gmail.com
:Web site: https://github.com/armstrtw/Rs3cmd
:License: GPL-3


Purpose
=======

AWS.tools is an R package for Amazon S3.


Usage
=====

A minimal example::

	library(Rs3cmd)
	x <- rnorm(100)
        s3.put(x,"s3://prod/my.rands.rds")
	x.s3 <- s3.get("s3://prod/my.rands.rds")
        stopifnot(all.equal(x,x.s3))
