#load("list.RData")
setwd("D:/OneDrive/00 - ARTIGOS LIDERADOS/00 - DECOLONIZING AMAZON")

# Load data and packages:
library(dplyr)
library(data.table)

# species list
load("data/amazonia_list.RData")
species_target <- df_list_amazonia %>%
  filter(YearOfDescription > 2000) %>%
  distinct(scientificName) %>%
  pull()
# extraction from zootaxa
load("data/zootaxa_extraction_amphibia.RData")
load("data/zootaxa_extraction_reptiles.RData")
data <- rbind(zootaxa_authors_full_amp, zootaxa_authors_full_rep)

# Remove data wrongly captured via webscrapping:
selected_entries <- which(data$title %like% "References")
data[selected_entries, ]$title <- NA
selected_entries <- which(data$Abstract %like% "References")
data[selected_entries, ]$Abstract <- NA
selected_entries <- which(data$title %like% "References")
data[selected_entries, ]$title <- NA

# Terms indicating Amazon + species list
key_terms <- c(species_target,
               "Amazon", "Amazônia", "Amazonia",
               "Amazonian", "AMAZONIAN",
               "AMAZON", "AMAZONIA", "AMAZÔNIA",
               "Suriname", "Guiana Francesa", "French Guiana",
               "SURINAME", "GUIANA FRANCESA", "FRENCH GUIANA",
               "GUYANA", "Guyana", "Guiana", "GUIANA",
               "PERU", "Peru",
               "Colombia", "Colômbia", "COLOMBIA", "COLÔMBIA",
               "Venezuela", "VENEZUELA",
               "Bolívia", "BOLÍVIA", "Bolivia", "BOLIVIA",
               "Ecuador", "Equador", "ECUADOR", "EQUADOR")

# Identify which rows have the key_terms mentioned in their values:
selected_titles <- list()
selected_abstract <- list()
selected_keywords <- list()
for(i in 1:length(key_terms)){
  
  # Select rows based on the mention of key terms:
  selected_titles[[i]] <- which(data$title %like% key_terms[i])
  selected_abstract[[i]] <- which(data$Abstract %like% key_terms[i])
  selected_keywords[[i]] <- which(data$KeyWords %like% key_terms[i])
  
}

# Bind all results:
selected_titles <- unlist(selected_titles)
selected_abstract <- unlist(selected_abstract)
selected_keywords <- unlist(selected_keywords)

# Get the list of papers:
selected_papers <- unique(c(selected_titles, selected_abstract, selected_keywords))
filtered_data <- data[selected_papers,]

# Export filtered papers:
data.table::fwrite(filtered_data, file="data/filtered_data_species.csv")

# Load filiation ----
load(
  file = "data/reptilia_zootaxa_filiation.RData"
)
load(
  file = "data/amphibia_zootaxa_filiation.RData"
)

data_filiation <- rbind(
  amphibia_zootaxa_filiation,
  reptilia_zootaxa_filiation
)

df_target <- left_join(
  filtered_data,
  data_filiation,
  by = "URL"
)

visdat::vis_miss(df_target)
View(table(df_target$filiation))
     