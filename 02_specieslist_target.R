library(tidyverse)

# listas obtidas via QGis sobrepondo poligonos de distribuicao de especialistas
# e a delimitacao da floresta Amazonica seguindo Eva et al., 2005
# https://forobs.jrc.ec.europa.eu/amazon
# Load data ----
# IUCN polygons
amphibia_list1 <- read.delim("data/amphibia_list1.csv", sep = ',')
amphibia_list2 <- read.delim("data/amphibia_list2.csv", sep = ',')

amphibia_list <- bind_rows(
  amphibia_list1,
  amphibia_list2) %>%
  distinct() 

# GARD polygons
reptilia_list <- read.delim("data/reptiles_list.csv", sep = ',') %>%
  distinct()

# assessment IUCN
amphibia_assessment <- read.delim(
  "data/iucn_assessment/redlist_species_data_amphibia/taxonomy.csv", sep = ',')

reptilia_assessment <- read.delim(
  "data/iucn_assessment/redlist_species_data_reptiles/taxonomy.csv", sep = ',')

# Join two groups in one dataframe ----
# Join taxonomic information from assessment IUCN
# Amphibia
amphibia_assessment_year <- amphibia_assessment %>%
  mutate(YearOfDescription = str_extract(authority, "\\d{4}")) %>%
  select(all_of(c("scientificName", "YearOfDescription")))

df_amphibia <- left_join(amphibia_list,
          amphibia_assessment_year,
          by = c("sci_name" = "scientificName")) %>%
  rename(
    eva_boundarie = name,
    scientificName = sci_name,
    order = order_,
  ) %>%
  filter(
    origin == 1 # apenas especies nativas
  ) %>% 
  select(eva_boundarie, class, order, family, scientificName,
         YearOfDescription) %>%
  mutate(YearOfDescription = as.numeric(YearOfDescription))

# Reptilia
reptilia_assessment_year <- reptilia_assessment %>%
  mutate(YearOfDescription = str_extract(authority, "\\d{4}")) %>%
  select(all_of(c("className","familyName",
                  "scientificName", "YearOfDescription")))

df_reptilia <- left_join(reptilia_list,
                         reptilia_assessment_year,
                         by = c("binomial" = "scientificName")) %>%
  select(-familyName) %>%
  rename(
    eva_boundarie = name, 
    suborder = group,
    scientificName = binomial,
    class = className,
  ) %>%
  select(eva_boundarie, class, suborder, family, scientificName,
         YearOfDescription) %>%
  mutate(YearOfDescription = as.numeric(YearOfDescription))

# Adicionar mais dados do tetrapodtraits
TetraData <- data.table::fread("data/TetrapodTraits_1.0.0.csv") %>%
  select(all_of(c("Scientific.Name","Class","YearOfDescription"))) %>%
  rename("class" = "Class")

df_reptilia_fill <- df_reptilia %>%
  filter(is.na(YearOfDescription)) %>%
  distinct(scientificName)

df_reptilia_filled <- left_join(
  df_reptilia_fill,
  TetraData,
  by = c("scientificName" = "Scientific.Name")) %>%
  remove_missing()

# unindo os preenchidos com dados gerais
df_reptilia_complement <- left_join(
  df_reptilia_filled,
  df_reptilia %>% select(-class, -YearOfDescription),
  by = "scientificName"
)

# adicionando os dados complementados no TetrapodTraits
filled_data <- data.table::fread("data/missing_data_species.csv") 

df_reptilia_complement_2 <- left_join(
  filled_data,
  df_reptilia %>% select(-class, -YearOfDescription),
  by = "scientificName"
) 

df_reptilia_complete <- df_reptilia %>%
  filter(!is.na(YearOfDescription)) %>%
  bind_rows(df_reptilia_complement,
            df_reptilia_complement_2)

# Ainda faltam preencher dados para 
length(unique(df_reptilia$scientificName)) - length(unique(df_reptilia_complete$scientificName))
# 1 especie removida pq era sinonimo apostolepis roncadori

# Missing data to fill
#df_reptilia_missing <- left_join(
#  df_reptilia_fill,
#  TetraData,
#  by = c("scientificName" = "Scientific.Name")) %>%
#  filter(is.na(YearOfDescription)) %>%
#  distinct()
#length(df_reptilia_missing$scientificName) # TODO 85 spp to fill

# Export filtered papers:
#data.table::fwrite(df_reptilia_missing,
#                   file="data/missing_data_species.csv")

# Join groups ----
df_list_amazonia <- bind_rows(
  df_amphibia,
  df_reptilia_complete,
) %>%
  mutate(order = coalesce(order, suborder)) %>%
  mutate(across(c(class, order, family), ~ str_to_title(.))) %>%
  select(-suborder)

# Save
save(df_list_amazonia,
     file = "data/amazonia_list.RData")
