# MySQL configuration
options(mysql = list(
  "host" = "127.0.0.1",
  "port" = 3306,
  "user" = "root", # "user" = "ubuntu",
  "password" = "", # "password" = "ubuntu",
  "db" = "fm", # "db" = "fmd",
  # samples table
  "samples" = "samples",
  
  # main table with all variants
  "variants_table" = "variants",
  
  # tables for taxonomical search
  "genus_table" = "genus",
  "species_table" = "species",
  "sh_table" = "sh",
  
  # table for geosearch
  "samples_to_sh_table" = "samples_to_sh",
  
  # table with taxonomy for SH
  "taxonomy" = "taxonomy",
  
  # table info
  "info" = "info",
  
  # table info
  "traffic" = "traffic",
  
  # table for F&Q
  "messages" = "messages",
  
  # inserted studies
  "study" = "study",
  
  # inserted metadata
  "metadata" = "metadata",
  
  # inserted metadata
  "maillist" = "maillist",
  
  # clustered sequences
  "clusters_table" = "clusters"
))

safeSqlQueryVal <- function (query_val) {
  #query_val <- gsub("[\r\n]", "", query_val)
  query_val <- gsub('"', '/', query_val)
  query_val <- gsub("'", '/', query_val)
  #print(query_val)
  return(query_val)
}

sendEmail <- function (sender, recipients,subjtext, bodytext) {
  send.mail(from = sender,
            to = recipients,
            subject=subjtext,
            body = bodytext,
            smtp = list(host.name = "email09.active24.com", port = 587, # port = 465, 
                        user.name="info@globalfungi.com", passwd="ea4XRNz0XT", ssl=FALSE),
            authenticate = TRUE,
            send = TRUE)
}

sqlQuery <- function (query) {
  #print(query)
  # creating DB connection object with RMysql package
  db <- dbConnect(MySQL(), dbname = options()$mysql$db, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  # set charset
  #rs <- dbSendQuery(db, 'set character set "latin1"')
  rs <- dbSendQuery(db, 'set character set "utf8"')
  # Construct the fetching query
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  # return the dataframe
  return(data)
}

killDbConnections <- function () {
  all_cons <- dbListConnections(MySQL())
  print(all_cons)
  for(con in all_cons)
    +  dbDisconnect(con)
  print(paste(length(all_cons), " connections killed."))
}

#################################################################################
# python process sample FASTA script path
global_vars_to_fasta_py <- "/srv/shiny-server/seqs_variants_to_fasta.py"
  
# nucleotide database for blast
#global_blast_db <- "/home/fungal/databases/blast_database/VARIANTS_PROCESSED.fa"
#global_blast_db_double <- "/home/fungal/databases/blast_database/VARIANTS_PROCESSED_DOUBLE.fa"

#global_blast_FUN_ANNOT <- "/home/fungal/databases/blast_database/VARIANTS_FUNGAL_ANNOTATED.fa"
#global_blast_FUN_POOR <- "/home/fungal/databases/blast_database/VARIANTS_FUNGAL_POOR.fa"
#global_blast_FUN <- "/home/fungal/databases/blast_database/VARIANTS_FUNGAL.fa"
#global_blast_NO_HIT <- "/home/fungal/databases/blast_database/VARIANTS_NO_HIT.fa"
global_blast_db_names <- c("Fungal annotated", "Fungal eval<e-50","Fungal eval>e-50", "Unknown")
global_blast_db_paths <- c("/home/fungal/databases/blast_database/VARIANTS_FUNGAL_ANNOTATED.fa", "/home/fungal/databases/blast_database/VARIANTS_FUNGAL.fa", "/home/fungal/databases/blast_database/VARIANTS_FUNGAL_POOR.fa", "/home/fungal/databases/blast_database/VARIANTS_NO_HIT.fa")


global_clusters_db_names <- c("ITS1", "ITS2")
global_clusters_db_paths <- c("/home/fungal/databases/blast_database/CLUSTERS_ITS1.fas","/home/fungal/databases/blast_database/CLUSTERS_ITS2.fas")

# sample sequences path 
global_samples_path <- "/home/fungal/databases/samples_fasta/"

# variant sequences path 
global_variants_path <- "/home/fungal/databases/variants_fasta/"

# output path 
global_out_path <- "D:/EXAMPLES/" #"/home/fungal/databases/user_outputs/"

# blast cpus
global_blast_nproc <- "12"

#################################################################################
# tracking get last session count
query <- sprintf(paste0("SELECT `session` FROM ",options()$mysql$traffic," ORDER BY id DESC LIMIT 1;"))
session <- data.table(sqlQuery(query))
if (nrow(session)>0){
  global_session <- as.numeric(session)
} else {
  global_session <- 1
}
print(paste0("Current session number ",global_session))


# database info
query <- sprintf(paste0("SELECT `name`,`version`,`release`,`unite_version`,`its_variants_count`,`its1_raw_count`,`its2_raw_count`,`info`,`citation`,`date` FROM ",options()$mysql$info," ORDER BY id DESC LIMIT 1;"))
global_info <- data.table(sqlQuery(query))

#query <- sprintf(paste0("SELECT TABLE_ROWS from information_schema.Tables where TABLE_SCHEMA= '",options()$mysql$db,"' && TABLE_NAME = '",options()$mysql$variants_table,"'"))
#global_variants_count <- sqlQuery(query)
# load samples table...
query <- sprintf(paste0("SELECT * FROM ",options()$mysql$samples))
global_samples <- data.table(sqlQuery(query))

# construct papers table...
global_papers <- global_samples[,c("add_date","paper_id", "title", "year", "authors", "journal", "doi", "contact", "manipulated")]
# this will be changed in future...

global_papers$submitted_by <- rep(global_info[,"name"], nrow(global_papers))
global_papers <- distinct(global_papers, paper_id, .keep_all= TRUE) # remove duplicate rows based on variable

# split title and year...
#print(global_papers$title_year)
#Sys.setlocale('LC_CTYPE','C')
#splited_title_year <- do.call('rbind', strsplit(as.character(global_papers$title_year), '_', fixed=TRUE))
#colnames(splited_title_year) <- c("title", "year")
#global_papers <- cbind(global_papers, splited_title_year)
#global_papers = subset(global_papers, select = -c(title_year) ) #drop column...

# sort
rowidx <- order(global_papers[, "add_date"], global_papers[, "authors"])
global_papers <- global_papers[rowidx, , drop = FALSE]


# store minimal amount of information
global_samples <- global_samples[,c("id","paper_id", "primers", "longitude", "latitude","continent", "sample_type", "ITS1_extracted", "ITS2_extracted","ITS_total", "Biome", "MAT", "MAP", "pH", "year_of_sampling", "manipulated")] 

 
# load SH table...
query <- sprintf(paste0("SELECT * FROM ",options()$mysql$taxonomy))
global_SH <- sqlQuery(query)
global_SH <- global_SH[,c("SH", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species","genus_id","species_id","SH_id")]
print(nrow(global_SH))

# options
global_SH_list <- sort(global_SH$SH)
global_species_list <- sort(unique(global_SH$Species))
global_species_list <- global_species_list[!global_species_list %in% grep(" sp.", global_species_list, value = T)]
global_genus_list <- sort(unique(global_SH$Genus))
  
