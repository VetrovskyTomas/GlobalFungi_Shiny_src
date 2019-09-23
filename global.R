# Persistent database

requiredPackages = c('shiny','leaflet','data.table')
for(p in requiredPackages){
  library(p,character.only = TRUE)
}

toTrainMetadata <- fread(input = "C:/fm_database_root/tables/FUNGAL_METASTUDY_Filtered_metadata.txt")
SNV_counts <- fread(input = "C:/fm_database_root/tables/Samples_to_SH.txt", header = F)

# Prepare vectors of unique SHs
# SHs <- unique(SNV_counts$V4)
SHs <- unique(SNV_counts$V3)
