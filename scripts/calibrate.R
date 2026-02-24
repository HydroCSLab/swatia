#!/bin/env Rscript
source("ispso.R")

par_min <- c(
	     -30,	# cn2 pctchg (HRU)
	     -20,	# awc pctchg (SOL)
	     -50,	# canmx pctchg (HRU)
	     -0.2,	# esco abschg (HRU)
	     0.01,	# alpha absval (AQU)
	     0,		# flo_min absval (AQU)
	     0.001,	# deep_seep absval (AQU)
	     0,		# revap_min absval (AQU)
	     -50,	# lat_ttime pctchg (HRU)
	     0.02,	# revap_co absval (AQU)
	     -0.2,	# epco abschg (HRU)
	     -2,	# snofall_tmp abschg (HRU)
	     -2,	# snomelt_tmp abschg (HRU)
	     -50,	# snomelt_max pctchg (HRU)
	     -50,	# snomelt_min pctchg (HRU)
	     -50	# snomelt_lag pctchg (HRU)
)

par_max <- c(
	     30,	# cn2 pctchg (HRU)
	     20,	# awc pctchg (SOL)
	     50,	# canmx pctchg (HRU)
	     0.2,	# esco abschg (HRU)
	     0.5,	# alpha absval (AQU)
	     10,	# flo_min absval (AQU)
	     0.05,	# deep_seep absval (AQU)
	     20,	# revap_min absval (AQU)
	     50,	# lat_ttime pctchg (HRU)
	     0.2,	# revap_co absval (AQU)
	     0.2,	# epco abschg (HRU)
	     2,		# snofall_tmp abschg (HRU)
	     2,		# snomelt_tmp abschg (HRU)
	     50,	# snomelt_max pctchg (HRU)
	     50,	# snomelt_min pctchg (HRU)
	     50		# snomelt_lag pctchg (HRU)
)


if(length(par_min) != length(par_max)) stop("Lengths of s$xmin and s$xmax different")

obj_txt <- "obj_day.txt"
nse_txt <- "nse_day.txt"

obs_day <- read.table("obs_day.txt")[[1]]
nobs_day <- length(obs_day)
nobs_day_c <- 3288 #define number of days for clibration period out of total data
obs_day_c <- obs_day[1:nobs_day_c]
obs_day_v <- obs_day[(nobs_day_c+1):nobs_day]

#obs_mon <- read.table("obs_mon.txt")[[1]]
#nobs_mon <- length(obs_mon)
#nobs_mon_c <- 180
#obs_mon_c <- obs_mon[1:nobs_mon_c]
#obs_mon_v <- obs_mon[(nobs_mon_c+1):nobs_mon]

calc_nse <- function(obs, sim) 1 - sum((obs - sim)^2) / sum((obs - mean(obs))^2)
run_swat <- function(x, opt){
	if(length(x) != length(par_min))
		stop()
	# USGS gage 13010065
	chaid <- 55
	# USGS gage 13022500
	#chaid <- 196
	# USGS gage? Upper Snake outlet
	#chaid <- 143

	chaid <- sprintf("cha%03d", chaid)

	x[x<0] <- 0
	x[x>1] <- 1
	parval <- par_min + (par_max - par_min) * x

	core <- opt$core
	run <- opt$iter * opt$S + core

        dir <- sprintf("TxtInOut_%d", core)
        system(sprintf("cd %s; (echo \"%s\"; cat calibration.cal) | awk '
		NR == 1 {
			for(i = 1; i <= NF; i++)
				val[i] = $i
			next
		}
		NR <= 4 {
			print
			next
		}
		{
			j = NR - 4
			for(i = 1; i <= 2; i++)
				printf \"%%s \", $i
			printf \"%%s\", val[j]
			for (i++; i <= NF; i++)
				printf \" %%s\", $i
			printf \"\\n\"
		}' > tmp.$$
		mv tmp.$$ calibration.cal", dir, paste(parval, collapse=" ")))
        system(sprintf("cd %s; swatplus", dir))
        system(sprintf("cd %s; awk '
		$7 == \"%s\" {
		       print $48
		}' channel_sd_day.txt > sim_day.txt", dir, chaid))

        sim_day <- read.table(sprintf("%s/sim_day.txt", dir))[[1]]
        sim_day_c <- sim_day[1:nobs_day_c]
        sim_day_v <- sim_day[(nobs_day_c+1):nobs_day]
        nse_day_c <- calc_nse(obs_day_c, sim_day_c)
        nse_day_v <- calc_nse(obs_day_v, sim_day_v)
        obj_day_c <- 1 - nse_day_c
        obj_day_v <- 1 - nse_day_v

#        sim_mon <- read.table(sprintf("%s/sim_mon.txt", dir))[[1]]
#        sim_mon_c <- sim_mon[1:nobs_mon_c]
#        sim_mon_v <- sim_mon[(nobs_mon_c+1):nobs_mon]
#        nse_mon_c <- calc_nse(obs_mon_c, sim_mon_c)
#        nse_mon_v <- calc_nse(obs_mon_v, sim_mon_v)
#        obj_mon_c <- 1 - nse_mon_c
#        obj_mon_v <- 1 - nse_mon_v

        x <- paste(x, collapse=",")
        write.table(sprintf("%d,%f,%f,%s", run, obj_day_c, obj_day_v, x), file=obj_txt, quote=F, row.names=F, col.names=F, append=T)
        write.table(sprintf("%d,%f,%f,%s", run, nse_day_c, nse_day_v, x), file=nse_txt, quote=F, row.names=F, col.names=F, append=T)

        sim_day <- read.table(sprintf("%s/sim_day.txt", dir))[[1]]
        write.table(sim_day, file=sprintf("day/sim_%05d_day.txt", run),
                row.names=F, col.names=F)

#        sim_mon <- read.table(sprintf("%s/sim_mon.txt", dir))[[1]]
#        write.table(sim_mon, file=sprintf("mon/sim_%05d_mon.txt", run),
#                row.names=F, col.names=F)
	obj_day_c
#	obj_mon_c
}


run_swat_per_core <- function(core){
        dir <- sprintf("TxtInOut_%d", core)
        run_swat(dir)
}

set_parameters <- function(s){
        ########################################################################
        # DEBUG
        # Deterministic run?
        s$.deterministic <- FALSE
        #s$.deterministic <- TRUE

        # Stop if all the solutions are found!  This is only for writing a
        # paper, not for real problems because the number of actual solutions
        # is not known in most cases.
        s$.stop_after_solutions <- 0
        #s$.stop_after_solutions <- -1

        # (0, 1]: Fraction of the diagonal span of the search space.
        s$.distance_to_solution <- 0.01

        # Plot method
        s$.plot_method <- "density"
        s$.plot_method <- "movement"
        s$.plot_method <- ""
        #s$.plot_method <- sprintf("%s,species", s$.plot_method)
        s$.plot_delay <- 0
        ########################################################################

        # Swarm size
        s$S <- 10 + floor(2*sqrt(s$D))

        # Maximum particle velocity
        s$vmax <- (s$xmax-s$xmin)*0.1

        # Maximum initial particle velocity
        s$vmax0 <- diagonal(s)*0.001

        # Stopping criteria: Stop if the number of exclusions per particle
        # since the last minimum is greater than exclusion_factor * max sol
        # iter / average sol iter. The more difficult the problem is (i.e.,
        # high max sol iter / average sol iter), the more iterations the
        # algoritm requires to stop.
        s$exclusion_factor <- 3
        # Maximum iteration
        s$maxiter <- 2000
        # Small positive number close to 0
        s$xeps <- 0.001
        s$feps <- 0.0001

        # Search radius for preys: One particle has two memories (i.e., x and
        # pbest).  When two particles collide with each other within prey, one
        # particle takes more desirable x and pbest from the two particles'
        # memories, and the other particle is replaced with a quasi-random
        # particle using scrambled Sobol' sequences (PREY).
        s$rprey <- diagonal(s)*0.0001

        # Nesting criteria for global and local optima using particles' ages
        # (NEST_BY_AGE).
        s$age <- 10

        # Speciation radius: Li (2004) recommends 0.05*L<=rspecies<=0.1*L.
        s$rspecies <- diagonal(s)*0.1

        # Nesting radius
        s$rnest <- diagonal(s)*0.01

        invisible(s)
}

s <- list()
s$f <- run_swat

s$D <- length(par_min)
s$xmin <- rep(0, s$D)
s$xmax <- rep(1, s$D)

s <- set_parameters(s)
s$S <- min(s$S, parallelly::availableCores() - 1)

system(sprintf("rm -rf day TxtInOut_*
	mkdir day
	for i in $(seq 1 %d); do
		cp -a ../TxtInOut TxtInOut_$i
	done", s$S))

s$parallel <- TRUE
s$cl <- makeCluster(s$S)

clusterExport(s$cl, c(
        "par_min", "par_max",
        "obj_txt", "nse_txt",
        "obs_day", "nobs_day", "nobs_day_c", "obs_day_c", "obs_day_v",
        "calc_nse", "run_swat"))

s$maxiter <- 1000

write.table(sprintf("run,obj_day_c,obj_day_v,%s", paste(paste("x", 1:s$D, sep=""), collapse=",")), file=obj_txt, quote=F, row.names=F, col.names=F)
write.table(sprintf("run,nse_day_c,nse_day_v,%s", paste(paste("x", 1:s$D, sep=""), collapse=",")), file=nse_txt, quote=F, row.names=F, col.names=F)

ret <- ispso(s)

stopCluster(s$cl)
