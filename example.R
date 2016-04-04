# Load function definiton
source("load_ensemble.R")

library(PEcAn.all)
library(ncdf4.helpers)

#pecan.id <- 1432    ## DALEC, 1000 member ensemble -- Betsy; original
#pecan.id <- 1436    ## DALEC, 200 member ensemble -- Alexey; more recent
pecan.id <- 1437    ## SIPNET, 500 member ensemble -- Alexey
#pecan.id <- 1347    ## Linkages; Ann(?); Complete ensemble analysis
output.dir <- paste0("/fs/data2/output/PEcAn_100000", pecan.id)

settings <- xmlToList(xmlParse(file.path(output.dir,"pecan.xml")))
runs <- list.files(settings$modeloutdir, full.names = TRUE)

# get the output variable names, probably not the best way to do it but meh
nc <- nc_open(dir(runs[1], full.names=TRUE)[1])
vars <- nc.get.variable.list(nc)
nc_close(nc)
print(vars)

ensemble.out <- load_ensemble(outdir = output.dir, settings = settings, variable = vars)

# Simple plot of all points
plot(GPP ~ temperate.coniferous.Amax, data=ensemble.out)


#----------
# Alexey's Notes

## Load ensemble samples
# ensemble.all.files <- list.files(output.dir, "ensemble")
# ensemble.file <- list.files(output.dir, "ensemble.samples")
# load(file.path(output.dir, ensemble.file))

## "ensemble.output" contains the following:
##    ensemble.output -- List containing a single named value ("AGB")

## "ensemble.samples" contains the following:
##    ens.run.ids -- Run IDs, not paired with anything, but probably in same order as samples
##    ens.ensemble.id -- Just one value; probably in case there are multiple ensembles?
##    ens.samples -- For each PFT, data frame of sampled parameter values. Not linked to run IDs, but presumably in same order?
##    pft.names -- Names of each PFT
##    trait.names -- List of names of traits