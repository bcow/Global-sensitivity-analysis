load_ensemble <- function(outdir, settings, variable){
  require(PEcAn.all)
  require(ncdf4)
  
  # Load the model output 
  ## ANS -- NOTE: There may be a faster/better way to do this using built-in PEcAn functions
  ## ANS -- or...should these be automatically stored somewhere?
  
  ensemble.output.raw <- read.ensemble.output(ensemble.size = as.numeric(settings$ensemble$size),
                                              pecandir = outdir,
                                              outdir = settings$modeloutdir,
                                              start.year = as.numeric(settings$ensemble$start.year),
                                              end.year = as.numeric(settings$ensemble$end.year),
                                              variable = variable)
  ensemble.output <- data.frame(do.call(rbind, ensemble.output.raw))
  
  ## NOTE: read.ensemble.output only returns the mean value at each timestep.
  ## If we want other statistics, they need to be either hard-coded (loop with read.output), 
  ## or read.ensemble.output needs to be modified.
  
  # Load parameter values
  load(file.path(outdir, "samples.Rdata"))
  ## "samples.RData" contains the following:
  ##    ensemble.samples -- For each PFT, data.frame of sampled parameter values. Not linked to run IDs, but presumably in same order
  ##    pft.names -- Names of each PFT
  ##    runs.samples -- Run IDs, not paired with anything, but probably in same order as samples
  ##    sa.samples -- Sensitivity analysis samples? Here it's blank
  ##    trait.names -- Names of parameters (traits) sampled; list by PFT.
  ##    trait.samples -- Samples from meta-analysis? 5004 samples per trait.
  
  ensemble.output$runid <- runs.samples$ensemble$id
  ensemble.samples.cbind <- do.call(cbind, ensemble.samples[pft.names])
  ensemble.output.full <- cbind(ensemble.output, ensemble.samples.cbind)
  
  return(ensemble.output.full)
}

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