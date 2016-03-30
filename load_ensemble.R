library(ncdf4)
library(ncdf4.helpers)
library(stringr)
library(PEcAn.all)
library(ggplot2)

if(file.exists("ensemble.Rdata")){
  load("ensemble.Rdata")
}else{
  
  settings <- xmlToList(xmlParse("/fs/data2/output/PEcAn_1000001432/pecan.xml"))
  runs <- dir(settings$run$host$outdir, full.names=TRUE)
  
  # get the output variable names, probably not the best way to do it but meh
  nc <- nc_open(dir(runs[1], full.names=TRUE)[1])
  vars <- nc.get.variable.list(nc)
  
  ###################################################
  # Load the model output 
  
  out <- list()
  
  for (i in 1:length(runs)){
    print(sprintf("Run %.0f", i))
    out[[i]] <- read.output(runid = basename(runs[i]), 
                            outdir = runs[i], 
                            start.year = year(settings$run$start.date), 
                            end.year = year(settings$run$end.date), 
                            variables = vars)
  }
  
  # data.frame of means from each run, this is just a first pass - we should have additional statistics
  means <- data.frame(matrix(NA, nrow = length(runs), ncol = length(vars)))
  colnames(means) <- vars
  
  for (i in 1:length(vars)){
    means[,i] <- colMeans(sapply(out, "[[", vars[i]))
  }
  
  ###################################################
  # Load parameter initialization values
  
  # DALEC parameter names are kind of screwed up so I'm hardcoding them in for now.
  params <- c("-t2","-t3","-t1","-t8","-t7","-t9","-t7","-t4","-t5","-SLA")
  params <- c("autotrophic_respiration_fraction",
              "leaf_allocation_fraction",
              "litter_decomposition_to_SOM",
              "litter_respiration_rate",
              "wood_turnover_rate",
              "som_respiration_rate",
              "root_turnover_rate",
              "root_allocation_fraction",
              "leaf_turnover_rate",
              "SLA")
  
  init <- data.frame(matrix(NA, nrow = length(runs), ncol = 10))
  colnames(init) <- params
  
  run_files <- dir(settings$run$host$run, full.names=TRUE)
  
  for (i in 1:length(runs)){
    print(sprintf("Run %.0f", i))
    conf <- file.path(run_files[i],paste0("CONFIG.",basename(run_files[i])))
    conf_split <- unlist(strsplit(str_trim(readLines(conf))," "))
    init[i,] <- as.numeric(conf_split[seq(2, length(conf_split), 2)])
  }
  
  save.image("ensemble.Rdata")
}

