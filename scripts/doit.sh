#!/bin/sh
(
date
/usr/bin/time -v ./calibrate.R
date
) &> doit.log
