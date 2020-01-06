# install.packages("BiocManager")
library(BiocManager)
# install(c("sangerseqR","annotate","genbankr")) 
# BiocManager::install(c("DECIPHER", "Biostrings", "sangerseqR"))
library(devtools)
# install_github("roblanf/sangeranalyseR")
library(sangerseqR)
library(sangeranalyseR)
library(tidyverse)
# install.packages("microclass")
library(annotate)

# set working directory
setwd(glue::glue("{dirname(rstudioapi::getActiveDocumentContext()$path)}/.."))

# make input folder
input.folders = c("data/sanger/raw/")


# make sanger summary for all the .ab1 files. Takes a long time!
sf = summarise.abi.folder(input.folders, processors = 2)
seq_summary <- sf$summaries

# reading about sf reads structure https://colauttilab.github.io/DNAbarcodes.html
test <- sf$reads[1]

# plot mean qualities summary
raw_mean_qual <- ggplot(sf$summaries) +
  geom_histogram(aes(x = raw.mean.quality), bins = nrow(seq_summary)/3, fill = "blue", alpha = 0.5) +
  geom_histogram(aes(x = trimmed.mean.quality), bins = nrow(seq_summary)/3, fill = "red", alpha = 0.5) +
  xlim(0,60) +
  theme_bw() 
raw_mean_qual
  
######################
# Trying different method https://colauttilab.github.io/DNAbarcodes.html
ITS<-read.abif("data/ssu_pcr_1/S-0415_oECA1271.ab1") # Read
ITSseq <- sangerseq(ITS) # Extract
SeqX<-makeBaseCalls(ITSseq) # Call
SeqXBlastDF<-blastSequences(paste(SeqX@primarySeq),as='data.frame', hitListSize = 3, timeout = 3000)

#####################################
### loop through test  directory  ###
#####################################
# establish test file list
test_file_list <- list.files(test.input.folder)

# make dataframe to hold loop output
test_seq_blast_df <- NULL

# write loop to process .ab1 files
for(i in unique(test_file_list)){
  ITS<-read.abif(glue::glue("{test.input.folder}{i}")) # Read
  ITSseq <- sangerseq(ITS) # Extract
  SeqX<-makeBaseCalls(ITSseq) # Call
  SeqXBlastDF<-blastSequences(paste(SeqX@primarySeq),as='data.frame', hitListSize = 3, timeout = 10000) # blast
  
  test_seq_blast_df <- rbind(test_seq_blast_df, SeqXBlastDF) # bind blast data into dataframe named test_seq_blast_df
}

 
