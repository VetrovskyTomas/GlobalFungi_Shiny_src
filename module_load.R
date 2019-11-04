if(!exists("global_samples")) {
  #################################################################################
  # python process sample FASTA script path
  global_vars_to_fasta_py <- "/srv/shiny-server/seqs_variants_to_fasta.py"
  
  # nucleotide database for blast
  global_blast_db <- "/home/fungal/databases/blast_database/fm_sequences_vol1.fa"

  # output path 
  global_out_path <- "/home/fungal/databases/user_outputs/"  
    
  # tables
  #global_tables_path <- "/home/fungal/databases/tables/"  
  global_tables_path <- "C:/fm_database_root/tables/"  
  
  #################################################################################
  # load samples table...
  global_samples <- fread(paste0(global_tables_path, "fm_samples_v8_with_counts.txt"))
  
  # construct papers table...
  global_papers <- global_samples[,c("paper_id", "title_year", "authors", "journal", "doi", "contact")]
  global_papers <- distinct(global_papers, paper_id, .keep_all= TRUE) # remove duplicate rows based on variable
  # write.table(global_papers,file = "fm_papers.txt", sep = "\t", quote = F, row.names = F)
  # split title and year...
  splited_title_year <- do.call('rbind', strsplit(as.character(global_papers$title_year), '_', fixed=TRUE))
  colnames(splited_title_year) <- c("title", "year")
  global_papers <- cbind(global_papers, splited_title_year)
  global_papers = subset(global_papers, select = -c(title_year) ) #drop column...
  
  # filter sample table...
  global_samples <- global_samples[,c("id", "paper_id", "sample_type", "latitude", "longitude", "continent", 
                                      "year_of_sampling", "Biome", "sequencing_platform", "target_gene", "primers", 
                                      "elevation", "MAT", "MAP", "MAT_study", "MAP_study",
                                      "country", "Plants", "area_sampled", "number_of_subsamples", "sample_depth", 
                                      "total_C_content", "total_N_content", "organic_matter_content", 
                                      "pH", "pH_method", "total_Ca", "total_P", "total_K", "ITS1_extracted", "ITS2_extracted", "ITS_total")]
  
  # load SH table...
  global_SH <- fread(paste0(global_tables_path, "fm_sh_07FU.txt"))
  global_SH <- global_SH[,c("SH", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")]
  
  # load sequence variants with SH...
  global_variants <- fread(paste0(global_tables_path, "fm_sequences_vol1_test_corrected.txt"))
  global_variants <- global_variants[,c("hash", "marker", "samples", "abundances", "SH")]
  
  # remove SH not existing in the dataset...
  SH_list <- unique(global_variants$SH)
  global_SH <- global_SH %>% filter(SH %in% SH_list)
  
  # options
  global_SH_list <- sort(global_SH$SH)
  global_species_list <- sort(unique(global_SH$Species))
  global_species_list <- global_species_list[!global_species_list %in% grep(" sp.", global_species_list, value = T)]
  global_genus_list <- sort(unique(global_SH$Genus))
  
  # test blast out
  #global_blast_out <- read.delim(file = "C:/fm_database_root/tables/results.out", header = F)
  # test retrieve FASTA (test file is for Genus "Russula")
  #global_fasta_out <- scan(file = "C:/fm_database_root/tables/results.fa", character(), quote = "")
}