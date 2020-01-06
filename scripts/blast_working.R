#!/usr/bin/env Rscript
#Load necessary packages
library(tidyverse)

# Set working directory
setwd(glue::glue("{dirname(rstudioapi::getActiveDocumentContext()$path)}/.."))

# load blast .tsv
blast_results <- data.table::fread('data/sanger/blast_results1.tsv') 

# define caenorhabditis pattern
proc_blasts_1 <- blast_results %>%
  dplyr::mutate(caenorhabditis = str_count(sciname, pattern = fixed("Caenorhabditis")),
                percent_identity = identity/align_len) %>%
  dplyr::group_by(s_plate) %>%
  dplyr::mutate(top3hits_frac_caenorhabditis = sum(caenorhabditis)/n()) %>%
  dplyr::distinct(s_plate, .keep_all = T) 

# output file to show just the caenos
proc_out_caenos <- proc_blasts_1 %>%
  dplyr::filter(caenorhabditis == 1) %>%
  dplyr::select(s_plate, sciname)

# save this temp file to data
#rio::export(proc_out_caenos, 'data/temp_proc_out_caenos.tsv')

# load manual ssu records
manual_blasts <- data.table::fread('data/SSU_caenorhabditis_sp_blast_result.csv') %>%
  dplyr::rename(s_plate = s_label)

# join these data frames
blast_agreement <- full_join(proc_out_caenos, manual_blasts)
