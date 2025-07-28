library(rvest)
library(dplyr)
library(stringr)
library(tidyverse)

# Webscrapping ----
# Iterar sobre paginas ate nao encontrar mais artigos
# Aqui vamos obter o link de todos os artigos filtrando para anfibios
base_url <- "https://mapress.com/zt/publishedarticles/page/"

#parametros <- "?taxon%5B%5D=Amphibia&fromDate=&toDate=&openAccess=" # filtro
parametros <- "?taxon%5B%5D=Reptilia&fromDate=&toDate="

extrair_links_pagina <- function(page_number) {
  # Construir URL correta
  if (page_number == 1) {
    url_pagina <- paste0("https://mapress.com/zt/publishedarticles/", parametros)
  } else {
    url_pagina <- paste0(base_url, page_number, "/", parametros)
  }
  
  # Carregar a página com headers (simula navegador)
  pagina <- tryCatch(
    {
      read_html(url_pagina)
    },
    error = function(e) {
      cat("Erro ao carregar página", page_number, "\n")
      return(NULL)
    }
  )
  
  if (is.null(pagina)) return(character(0))
  
  # XPath para links dos artigos (ignora PDFs)
  links <- pagina %>%
    html_nodes(xpath = "//div[contains(@class, 'article-item')]//a[contains(@href, '/zt/article/view/') and not(contains(@href, '/zt/article/view/zootaxa'))]") %>%
    html_attr("href") %>%
    #paste0("https://mapress.com", .) %>%
    str_subset("/zt/article/view/\\d+$")  # Filtra links de artigos
  
  return(links)
}

# Iterar por todas as páginas até não encontrar mais links
manuscript_link <- list()
page <- 1

repeat {
  cat("Processando página", page, "\n")
  links_pagina <- extrair_links_pagina(page)
  
  if (length(links_pagina) == 0) {
    cat("Nenhum link encontrado na página", page, ". Parando...\n")
    break
  }
  
  manuscript_link <- c(manuscript_link, links_pagina)
  page <- page + 1
  
  # Delay para evitar bloqueio
  Sys.sleep(2)  # Aumente para 5+ segundos se necessário
}

# Remover duplicatas e salvar
manuscript_link_unique <- unique(unlist(manuscript_link))
# Resultado final
cat("Total de artigos encontrados:", length(manuscript_link_unique), "\n")
# 1170 artigos, 1169 baixados de anfibios
# 1281 artigos, 1281 baixados de repteis

# lista parece receber os dados
zootaxa_authors <- list()
# links da zootaxa, agora extrair informacoes para cada um deles
for (url in manuscript_link_unique) {
  tryCatch({
    pagina <- read_html(url)
    print(url)
    # Extrair título
    titulo <- pagina %>%
      html_nodes(xpath = "/html/body/main/div/div[1]/h1/strong") %>%
      html_text() %>%
      str_trim()
    if (length(titulo) == 0) titulo <- NA_character_
    # Extrair DOI
    doi <- pagina %>%
      html_nodes(xpath = "/html/body/main/div/div[1]/div[2]/div[4]/span[2]") %>%
      html_text() %>%
      str_trim() %>%
      str_squish()
    if (length(doi) == 0) doi <- NA_character_
    # Extrair Abstract
    abstract <- pagina %>%
      html_nodes(xpath = "/html/body/main/div/div[2]/div[2]/div/p[1]") %>%
      html_text() %>%
      str_trim() %>%
      str_squish()
    if (length(abstract) == 0) abstract <- NA_character_
    # Extrair key-words
    keywords <- pagina %>%
      html_nodes(xpath = "/html/body/main/div/div[1]/div[4]/div") %>%
      html_text() %>%
      str_trim() %>%
      str_squish()
    if (length(keywords) == 0) keywords <- NA_character_
    # Extrair autores
    autores <- pagina %>%
      html_nodes(xpath = "/html/body/main/div/div[1]/ul/li/a[1]/span") %>%
      html_text() %>%
      str_trim()
    if (length(autores) == 0) stop("Nenhum autor encontrado")
    # Extrair afiliações
    num_autores <- length(autores)
    afiliacoes_texto <- character(num_autores)
    for (j in 1:num_autores) {
      #xpath_afiliacao <- paste0("/html/body/main/div/div[1]/div[2]/div[", j, "]")
      xpath_afiliacao <- paste0("/html/body/main/div/div[1]/div[3]/div[", j, "]")
      afiliacao <- pagina %>%
        html_nodes(xpath = xpath_afiliacao) %>%
        html_text() %>%
        str_replace_all("\\n", " ") %>%
        str_squish()
      afiliacoes_texto[j] <- ifelse(length(afiliacao) == 0, NA_character_, afiliacao)
    }
    # Verificar correspondência de comprimentos
    if (length(autores) != length(afiliacoes_texto)) {
      stop(paste("Número de autores (", length(autores), ") e afiliações (", length(afiliacoes_texto), ") não corresponde", sep = ""))
    }
    # Criar data.frame
    dados_atuais <- data.frame(
      author_names = autores,
      filiation = afiliacoes_texto,
      manuscript_title = titulo,
      Abstract = abstract, 
      KeyWords = keywords,
      DOI = doi,
      URL = url,
      stringsAsFactors = FALSE
    )
    # Acumular dados
    zootaxa_authors <- bind_rows(zootaxa_authors, dados_atuais)
  }, error = function(e) {
    cat("Erro em:", url, "-", conditionMessage(e), "\n")
    return(NULL)
  })
}
length(unique(zootaxa_authors$URL)) 
# 1169 artigos anfibios
# 1281 artigos repteis

# Save raw data
#save(zootaxa_authors,
#     manuscript_link_unique,
#     file = "data/zootaxa_amphibia_raw.RData") # change name if needed

#save(zootaxa_authors,
#     manuscript_link_unique,
#     file = "data/zootaxa_reptilia_raw.RData") # change name if needed

# Extract 'Article' missing data ----
# iremos separar o webscrapping em dois dataframes:

# df_zootaxa_articles: cada linha é um artigo (url), com o respectivo
# nome do artigo, resumo, e palavras-chave.

# df_zootaxa_authors: cada linha é um autor, com o nome, filiacao
#e o artigo (url), com o respectivo, nesse caso, o mesmo URL pode aparecer
# mais de uma vez. 
load("data/zootaxa_reptilia_raw.RData")
load("data/zootaxa_amphibia_raw.RData")

# extrair o nome dos artigos unicos 
df_zootaxa_articles <- zootaxa_authors %>%
  select(DOI, URL, manuscript_title, Abstract, KeyWords) %>%
  distinct(URL, .keep_all = TRUE)
visdat::vis_miss(df_zootaxa_articles)

View(df_zootaxa_articles)

nrow(df_zootaxa_articles)

df_to_fill <- df_zootaxa_articles %>%
  filter(is.na(manuscript_title) | 
           is.na(Abstract) |
           is.na(KeyWords)) 
  
df_complete <- df_zootaxa_articles %>%
  filter(!is.na(manuscript_title) & 
           !is.na(Abstract) &
           !is.na(KeyWords)) 

# Se separar certinho, tem que dar TRUE
nrow(df_to_fill) + nrow(df_complete) == nrow(df_zootaxa_articles)

# to fill title
url_without_title <- df_to_fill %>%
  filter(is.na(manuscript_title)) %>%
  pull(URL)

# Cria um data.frame vazio para armazenar os resultados
resultado_titulos <- data.frame(
  url = character(),
  titulo = character(),
  stringsAsFactors = FALSE
)
# Loop para processar cada URL e capturar os titulos
for (url in url_without_title) {
  tryCatch({
    # Lê a página
    pagina <- read_html(url)
    
    # Extrai o título (ajuste o seletor se precisar)
    titulo <- pagina %>%
      html_element("strong") %>%
      html_text(trim = TRUE)
    
    # Adiciona ao resultado
    resultado_titulos <- resultado_titulos %>%
      add_row(url = url, titulo = titulo)
    
    # Print de controle
    cat("✔️  OK:", url, "\n")
    
  }, error = function(e) {
    # Em caso de erro, adiciona NA
    resultado_titulos <- resultado_titulos %>%
      add_row(url = url, titulo = NA)
    
    cat("❌ Erro:", url, "\n")
  })
}
View(resultado_titulos)
# adiciona titulos faltantes
df_to_fill_2 <- left_join(df_to_fill, resultado_titulos, by = c("URL"="url"))
df_to_fill_2$title <- coalesce(df_to_fill_2$manuscript_title, df_to_fill_2$titulo)
df_to_fill_2 <- df_to_fill_2 %>%
  relocate("title", .after = "URL") %>%
  select(-manuscript_title, -titulo)

visdat::vis_miss(df_to_fill_2)

abstract_to_fill <- df_to_fill_2 %>%
  filter(is.na(Abstract)) %>%
  pull(URL)

length(abstract_to_fill) 

# Extrair Abstract
resultado_abstract <- data.frame(
  url = character(),
  abstract = character(),
  stringsAsFactors = FALSE
)
# Loop para processar cada URL e capturar os titulos
for (url in abstract_to_fill) {
  tryCatch({
    # Lê a página
    pagina <- read_html(url)
    
    # Extrai o título (ajuste o seletor se precisar)
    abstract <- pagina %>%
      html_element(xpath = "/html/body/main/div/div[2]/div[2]/div") %>%
      html_text() %>%
      str_trim() %>%
      str_squish()
    
    # Adiciona ao resultado
    resultado_abstract <- resultado_abstract %>%
      add_row(url = url, abstract = abstract)
    
    # Print de controle
    cat("✔️  OK:", url, "\n")
    
  }, error = function(e) {
    # Em caso de erro, adiciona NA
    resultado_abstract <- resultado_abstract %>%
      add_row(url = url, abstract = NA)
    
    cat("❌ Erro:", url, "\n")
  })
}
length(resultado_abstract$url) # 426
View(resultado_abstract)
# Adiciona dados faltantes
names(df_to_fill_2)
names(resultado_abstract)

df_to_fill_3 <- left_join(df_to_fill_2,
                          resultado_abstract,
                          by = c("URL"="url"))

df_to_fill_3$Abstract <- coalesce(df_to_fill_3$Abstract,
                                  df_to_fill_3$abstract)
df_to_fill_3 <- df_to_fill_3 %>%
  relocate("Abstract", .after = "title") %>%
  select(-abstract)

visdat::vis_miss(df_to_fill_3)

# keywords to fill
keywords_to_fill <- df_to_fill_3 %>%
  filter(is.na(KeyWords)) %>%
  pull(URL)
length(keywords_to_fill)

# Extrair Abstract
resultado_keyword <- data.frame(
  url = character(),
  keyword = character(),
  stringsAsFactors = FALSE
)

# Loop para processar cada URL e capturar os titulos
for (url in keywords_to_fill) {
  tryCatch({
    # Lê a página
    pagina <- read_html(url)
    
    # Extrai o título (ajuste o seletor se precisar)
    keyword <- pagina %>%
      html_element(xpath = "/html/body/main/div/div[1]/div[3]/div") %>%
      html_text() %>%
      str_trim() %>%
      str_squish()
    
    # Adiciona ao resultado
    resultado_keyword <- resultado_keyword %>%
      add_row(url = url, keyword = keyword)
    
    # Print de controle
    cat("✔️  OK:", url, "\n")
    
  }, error = function(e) {
    # Em caso de erro, adiciona NA
    resultado_keyword <- resultado_keyword %>%
      add_row(url = url, keyword = NA)
    
    cat("❌ Erro:", url, "\n")
  })
}
length(resultado_keyword$url) 

# Adiciona dados faltantes
names(resultado_keyword)

df_to_fill_4 <- left_join(df_to_fill_3,
                          resultado_keyword,
                          by = c("URL"="url"))

df_to_fill_4$KeyWords <- coalesce(df_to_fill_4$KeyWords,
                                  df_to_fill_4$keyword)

df_to_fill_4 <- df_to_fill_4 %>%
  select(-keyword)

visdat::vis_miss(df_to_fill_4)

save(
  df_to_fill_4,
  file = "data/zootaxa_extraction_reptiles.RData"
)

zootaxa_authors_full <- df_to_fill_4 %>%
  select(-DOI) %>%
  mutate(across(everything(), ~na_if(.x, "")))
visdat::vis_miss(zootaxa_authors_full)
View(zootaxa_authors_full)

# Amphibia 
df_complete_adj <- df_complete %>%
  rename(title = manuscript_title) %>%
  select(-DOI)

zootaxa_authors_full_amp <- bind_rows(zootaxa_authors_full,
                                      df_complete_adj) %>%
  mutate(across(everything(), ~na_if(.x, "")))

visdat::vis_miss(zootaxa_authors_full_amp)
View(zootaxa_authors_full_amp)

# Reptilia
df_complete_adj <- df_complete %>%
  rename(title = manuscript_title) %>%
  select(-DOI)

zootaxa_authors_full_rep <- bind_rows(zootaxa_authors_full,
                                      df_complete_adj)
visdat::vis_miss(zootaxa_authors_full_rep)
length(unique(zootaxa_authors_full_rep$URL)) # 1281 artigos 

# capturar manualmente os vazios
zootaxa_authors_full %>%
  filter(is.na(KeyWords))%>%
  pull(URL)

# Anfibios adjust 
# preencher titulos faltantes
zootaxa_authors_full$title[zootaxa_authors_full$URL == "https://mapress.com/zt/article/view/46667"] <- "Erratum: SHENGBO ZHOU (周圣博), LI HE (何力), SIYU MA (马思雨), SHUJUN XU (徐树军), QIANG ZHAI (翟强), PING GUAN (关萍), HUI WANG (王惠)& JINGSONG SHI (史静耸) (2022)Taxonomic status of Rana nigromaculata mongolia and the validity of Pelophylax tenggerensis (Anura, Ranidae) Zootaxa 5165 (4): 486–500"
zootaxa_authors_full$title[zootaxa_authors_full$URL == "https://mapress.com/zt/article/view/30451"] <- "Re-description of the tadpole of Pleurodema somuncurensis (Cei, 1969) (Amphibia: Anura)"
zootaxa_authors_full$title[zootaxa_authors_full$URL == "https://mapress.com/zt/article/view/30340"] <- "A new species of Leptolalax (Anura: Megophryidae) from Gunung Mulu National Park, Sarawak, East Malaysia (Borneo)"
zootaxa_authors_full$title[zootaxa_authors_full$URL == "https://mapress.com/zt/article/view/30214"] <- "Review of the systematics, distribution, biogeography and natural history of Moroccan amphibians"
zootaxa_authors_full$title[zootaxa_authors_full$URL == "https://mapress.com/zt/article/view/30092"] <- "The identity of Rana margaritifera Laurenti, 1768 (Anura, Bufonidae)"
zootaxa_authors_full$title[zootaxa_authors_full$URL =="https://mapress.com/zt/article/view/12739"] <- "The phylogenetic relationships of Paramesotriton (Caudata: Salamandridae) based on partial mitochondrial DNA gene sequences"
zootaxa_authors_full$title[zootaxa_authors_full$URL == "https://mapress.com/zt/article/view/2285"] <- "Distribution and morphological variation of Eleutherodactylus mercedesae Lynch & McDiarmid, 1987 (Amphibia, Anura, Leptodactylidae) with first record for Peru"
# preencher keywords faltantes
zootaxa_authors_full$KeyWords[zootaxa_authors_full$URL == "https://mapress.com/zt/article/view/5782"] <- "Amphibia Amazonian lowlands Pristimantis new species Region Loreto Region San Martín"
# preencher abstract faltantes
zootaxa_authors_full$Abstract[zootaxa_authors_full$URL == "https://mapress.com/zt/article/view/14303"] <- "The external, oral and buccopharyngeal morphologies of Nasikabatrachus sahyadrensis tadpoles were studied using ste-reoscopic and scanning electron microscopy. Using DNA barcodes, taxonomic identity of the tadpoles was establishedand tadoples of N. sahyadrensis were reared in semi-natural conditions. Development in the species from hatching tometamorph was prolonged and it took about 100 days for the freshly hatched larvae to metamorphose. The tadpoles areexotrophic, torrent dwelling with a large ventral suctorial oral disc, broadly rounded snout, cylindrical body, and a funnelshaped vent tube opening medially. During development external and buccopharyngeal characters show extensive changes. During metamorphosis developmental asynchrony was observed."
zootaxa_authors_full$Abstract[zootaxa_authors_full$URL == "https://mapress.com/zt/article/view/14302"] <- "Novel and significant data on the breeding biology and tadpole morphology of Nasikabatrachus sahyadrensis expands ourunderstanding of this unusual frog and clarifies some data in other reports. Nonpigmented eggs are laid in arrays or clumpsin small shaded rocky pools in the bedrock of torrential streams, as they are charged by early monsoon rains. The suctorialtadpole adapted to rheophilic habitats, has a strongly depressed body, dorsal eyes, complete marginal papillae, a labialtooth row formula of 2/3 or 2/3(1), and a medial vent with unusual flaps subtending the vent and limb buds. Tadpoles meta-morphose in about 100 days. Additional site records and issues relating to the conservation of this frog and its habitat in the southern Western Ghats of India are discussed."
# corrigindo titulos errados
zootaxa_authors_full$title[zootaxa_authors_full$URL == "https://mapress.com/zt/article/view/30004"] <- "Amietia angolensis and A. fuscigula (Anura: Pyxicephalidae) in southern Africa: A cold case reheated"
zootaxa_authors_full$title[zootaxa_authors_full$URL == "https://mapress.com/zt/article/view/29919"] <- "A new Stumpffia (Amphibia: Anura: Microhylidae) from the Ranomafana region, south-eastern Madagascar"

visdat::vis_miss(zootaxa_authors_full)

# Salvar o webscraping em xlsx
save(
  zootaxa_authors_full_amp,
  file = "data/zootaxa_extraction_amphibia.RData"
)

save(
  zootaxa_authors_full_rep,
  file = "data/zootaxa_extraction_reptiles.RData"
)

#openxlsx::write.xlsx(zootaxa_authors_full,
#                     "data/zootaxa_extraction_amphibia.xlsx",
#                     rowNames = FALSE)

# Extract 'Filiation" missing data after select target studies ----
#load("data/zootaxa_amphibia_raw.RData")
load("data/zootaxa_reptilia_raw.RData")

df_to_fill_filiation <- zootaxa_authors %>%
  filter(is.na(filiation)) %>%
  distinct(URL) %>%
  pull()
length(df_to_fill_filiation) # URL unico para preencher filiacoes

# lista parece receber os dados
zootaxa_authors <- list()
# links da zootaxa, agora extrair informacoes para cada um deles
for (url in df_to_fill_filiation) {
  tryCatch({
    pagina <- read_html(url)
    print(url)
    # Extrair autores
    autores <- pagina %>%
      html_nodes(xpath = "/html/body/main/div/div[1]/ul/li/a[1]/span") %>%
      html_text() %>%
      str_trim()
    if (length(autores) == 0) stop("Nenhum autor encontrado")
    # Extrair afiliações
    num_autores <- length(autores)
    afiliacoes_texto <- character(num_autores)
    for (j in 1:num_autores) {
      xpath_afiliacao <- paste0("/html/body/main/div/div[1]/div[2]/div[", j, "]")
      #xpath_afiliacao <- paste0("/html/body/main/div/div[1]/div[3]/div[", j, "]")
      afiliacao <- pagina %>%
        html_nodes(xpath = xpath_afiliacao) %>%
        html_text() %>%
        str_replace_all("\\n", " ") %>%
        str_squish()
      afiliacoes_texto[j] <- ifelse(length(afiliacao) == 0, NA_character_, afiliacao)
    }
    # Verificar correspondência de comprimentos
    if (length(autores) != length(afiliacoes_texto)) {
      stop(paste("Número de autores (", length(autores), ") e afiliações (", length(afiliacoes_texto), ") não corresponde", sep = ""))
    }
    # Criar data.frame
    dados_atuais <- data.frame(
      author_names = autores,
      filiation = afiliacoes_texto,
      URL = url,
      stringsAsFactors = FALSE
    )
    # Acumular dados
    zootaxa_authors <- bind_rows(zootaxa_authors, dados_atuais)
  }, error = function(e) {
    cat("Erro em:", url, "-", conditionMessage(e), "\n")
    return(NULL)
  })
}
missing_filiation <- zootaxa_authors 
View(missing_filiation)

# Load zootaxa_authors object
#load("data/zootaxa_amphibia_raw.RData")
load("data/zootaxa_reptilia_raw.RData")

# Aqui me retorna apenas aqueles que tem NA na filiacao
df_zootaxa_authors <- zootaxa_authors %>%
  select(URL, author_names, filiation) %>%
  filter(!is.na(filiation))

# alguns casos, um dos autores em cada URL tiveram a coluna
# filiation carregada. por isso, aqui criei um vetor que remove 
# todos aqueles url que aparecem no missing_filiation (dados completos) 
# e df_zootaxa_authors (dados incompletos), com isso conseguimos filtrar
# todos os estudos que tiveram problemas de extração via URL
url_dup <- intersect(df_zootaxa_authors$URL,
        missing_filiation$URL)
# removendo URL com imprecisao na filiacao
df_zootaxa_authors <- zootaxa_authors %>%
  filter(!URL %in% url_dup) 

# Gerando data.frame preenchido de filiacoes

#amphibia_zootaxa_filiation <- bind_rows(df_zootaxa_authors,
#          missing_filiation) %>%
#  select(URL, author_names, filiation)
# Deu 1169 URL's unicos, ok!

#save(
#  amphibia_zootaxa_filiation,
#  file = "data/amphibia_zootaxa_filiation.RData"
#)

reptilia_zootaxa_filiation <- bind_rows(df_zootaxa_authors,
                                        missing_filiation) %>%
  select(URL, author_names, filiation)
visdat::vis_miss(reptilia_zootaxa_filiation)
length(unique(reptilia_zootaxa_filiation$URL))

save(
  reptilia_zootaxa_filiation,
  file = "data/reptilia_zootaxa_filiation.RData"
)
