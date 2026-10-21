# ==============================================================================
# PROJETO DE MQ1 (2016.2) - REGRESSÃO LOGÍSTICA: 
# TAXA DE VARIAÇÃO ORÇAMENTÁRIA EM SEGURANÇA PÚBLICA ~ REELEIÇÃO DE GOVERNADOR
# ELEIÇÕES: 2010, 2014, 2018, 2022
# ==============================================================================

library(pdftools)
library(dplyr)
library(stringr)
library(stringi)
library(purrr)

# ------------------------------------------------------------------------------
# BASE DE DADOS OFICIAL DE ESTADOS E UFS
# ------------------------------------------------------------------------------
df_base_estados <- data.frame(
  Estados = c("Acre", "Alagoas", "Amapá", "Amazonas", "Bahia", "Ceará", 
              "Distrito Federal", "Espírito Santo", "Goiás", "Maranhão", 
              "Mato Grosso", "Mato Grosso do Sul", "Minas Gerais", "Pará", 
              "Paraíba", "Paraná", "Pernambuco", "Piauí", "Rio de Janeiro", 
              "Rio Grande do Norte", "Rio Grande do Sul", "Rondônia", "Roraima", 
              "Santa Catarina", "São Paulo", "Sergipe", "Tocantins"),
  UF = c("AC", "AL", "AP", "AM", "BA", "CE", 
         "DF", "ES", "GO", "MA", 
         "MT", "MS", "MG", "PA", 
         "PB", "PR", "PE", "PI", "RJ", 
         "RN", "RS", "RO", "RR", 
         "SC", "SP", "SE", "TO"),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# FUNÇÕES DE EXTRAÇÃO POR ANO
# ------------------------------------------------------------------------------

# FUNÇÃO CORRIGIDA PARA EXTRAIR 2006 (PENÚLTIMO VALOR FINANCEIRO DA PÁG 37)
extrair_2006 <- function(caminho_pdf) {
  pag37 <- pdf_text(caminho_pdf)[37]
  linhas37 <- unlist(str_split(pag37, "\r?\n"))
  
  # Filtra apenas linhas que contêm valores monetários grandes com pontos
  linhas_dados <- linhas37[sapply(linhas37, function(l) {
    str_detect(l, "\\b\\d{1,3}(?:\\.\\d{3})+\\b")
  })]
  
  map_df(1:nrow(df_base_estados), function(i) {
    sigla_uf <- df_base_estados$UF[i]
    idx_linha <- i + 2 # Pula a linha 1 (Total Brasil) e a linha 2 (União)
    
    if (idx_linha <= length(linhas_dados)) {
      linha_alvo <- linhas_dados[idx_linha]
      
      # Captura apenas números formatados com milhar (exclui porcentagens de 1 a 3 dígitos)
      numeros <- str_extract_all(linha_alvo, "\\b\\d{1,3}(?:\\.\\d{3})+(?:,\\d{2})?\\b")[[1]]
      
      if (length(numeros) >= 2) {
        val_str <- numeros[length(numeros) - 1] # O PENÚLTIMO valor é SEMPRE o Total 2006
        val_num <- as.numeric(str_replace_all(str_replace_all(val_str, "\\.", ""), ",", "."))
        return(data.frame(UF = sigla_uf, Despesa_2006 = val_num, stringsAsFactors = FALSE))
      }
    }
    return(data.frame(UF = sigla_uf, Despesa_2006 = NA_real_, stringsAsFactors = FALSE))
  })
}

# 2010: Pág 51 / Extrai o 2º valor financeiro grande
extrair_2010 <- function(caminho_pdf) {
  pag51 <- pdf_text(caminho_pdf)[51]
  linhas <- unlist(str_split(pag51, "\r?\n"))
  
  map_df(1:nrow(df_base_estados), function(i) {
    nome_estado <- df_base_estados$Estados[i]
    sigla_uf    <- df_base_estados$UF[i]
    nome_norm   <- stri_trans_general(nome_estado, "Latin-ASCII") %>% str_to_lower()
    
    idx <- which(str_detect(
      stri_trans_general(linhas, "Latin-ASCII") %>% str_to_lower(),
      paste0("\\b", nome_norm, "\\b")
    ))
    
    if (length(idx) > 0) {
      linha_alvo <- linhas[idx[1]]
      numeros <- str_extract_all(linha_alvo, "\\b\\d{1,3}(?:\\.\\d{3}){2,}(?:,\\d{2})?\\b")[[1]]
      
      if (length(numeros) >= 2) {
        val_num <- as.numeric(str_replace_all(str_replace_all(numeros[2], "\\.", ""), ",", "."))
        return(data.frame(UF = sigla_uf, Valor = val_num, stringsAsFactors = FALSE))
      }
    }
    return(data.frame(UF = sigla_uf, Valor = NA_real_, stringsAsFactors = FALSE))
  })
}

# 2014: Pág 53 / Descarte de notas laterais no lado direito
extrair_2014 <- function(caminho_pdf) {
  pag53 <- pdf_text(caminho_pdf)[53]
  linhas <- unlist(str_split(pag53, "\r?\n"))
  
  map_df(1:nrow(df_base_estados), function(i) {
    nome_estado <- df_base_estados$Estados[i]
    sigla_uf    <- df_base_estados$UF[i]
    nome_norm   <- stri_trans_general(nome_estado, "Latin-ASCII") %>% str_to_lower()
    
    idx <- which(sapply(linhas, function(l) {
      l_tabela <- str_split(l, "(?i)(\\(\\d+\\)|fonte:|nota:|voltar)")[[1]][1]
      l_norm   <- stri_trans_general(l_tabela, "Latin-ASCII") %>% str_to_lower()
      pos      <- regexpr(paste0("\\b", nome_norm, "\\b"), l_norm)
      return(pos > 0 && pos <= 30 && str_detect(l_tabela, "\\b\\d{1,3}(?:\\.\\d{3})+\\b"))
    }))
    
    if (length(idx) > 0) {
      linha_alvo <- linhas[idx[1]]
      linha_tabela <- str_split(linha_alvo, "(?i)(\\(\\d+\\)|fonte:|nota:|voltar)")[[1]][1]
      numeros_grandes <- str_extract_all(linha_tabela, "\\b\\d{1,3}(?:\\.\\d{3})+(?:,\\d{2})?\\b")[[1]]
      
      if (length(numeros_grandes) >= 1) {
        val_str <- tail(numeros_grandes, 1)
        val_num <- as.numeric(str_replace_all(str_replace_all(val_str, "\\.", ""), ",", "."))
        return(data.frame(UF = sigla_uf, Valor = val_num, stringsAsFactors = FALSE))
      }
    }
    return(data.frame(UF = sigla_uf, Valor = NA_real_, stringsAsFactors = FALSE))
  })
}

# 2018: Pág 163 / Descarte de notas laterais no lado direito
extrair_2018 <- function(caminho_pdf) {
  pag163 <- pdf_text(caminho_pdf)[163]
  linhas <- unlist(str_split(pag163, "\r?\n"))
  
  map_df(1:nrow(df_base_estados), function(i) {
    nome_estado <- df_base_estados$Estados[i]
    sigla_uf    <- df_base_estados$UF[i]
    nome_norm   <- stri_trans_general(nome_estado, "Latin-ASCII") %>% str_to_lower()
    
    idx <- which(sapply(linhas, function(l) {
      l_tabela <- str_split(l, "(?i)(fonte:|nota|voltar)")[[1]][1]
      l_norm   <- stri_trans_general(l_tabela, "Latin-ASCII") %>% str_to_lower()
      pos      <- regexpr(paste0("\\b", nome_norm, "\\b"), l_norm)
      return(pos > 0 && pos <= 30 && str_detect(l_tabela, "\\b\\d{1,3}(?:\\.\\d{3})+\\b"))
    }))
    
    if (length(idx) > 0) {
      linha_alvo <- linhas[idx[1]]
      linha_tabela <- str_split(linha_alvo, "(?i)(fonte:|nota|voltar)")[[1]][1]
      numeros_grandes <- str_extract_all(linha_tabela, "\\b\\d{1,3}(?:\\.\\d{3})+(?:,\\d{2})?\\b")[[1]]
      
      if (length(numeros_grandes) >= 1) {
        val_str <- tail(numeros_grandes, 1)
        val_num <- as.numeric(str_replace_all(str_replace_all(val_str, "\\.", ""), ",", "."))
        return(data.frame(UF = sigla_uf, Valor = val_num, stringsAsFactors = FALSE))
      }
    }
    return(data.frame(UF = sigla_uf, Valor = NA_real_, stringsAsFactors = FALSE))
  })
}

# 2022: Pág 241 / Coluna 4 (Ano 2022)
extrair_2022 <- function(caminho_pdf) {
  pag241 <- pdf_text(caminho_pdf)[241]
  linhas <- unlist(str_split(pag241, "\r?\n"))
  
  map_df(1:nrow(df_base_estados), function(i) {
    nome_estado <- df_base_estados$Estados[i]
    sigla_uf    <- df_base_estados$UF[i]
    nome_norm   <- stri_trans_general(nome_estado, "Latin-ASCII") %>% str_to_lower()
    
    idx <- which(str_detect(
      stri_trans_general(linhas, "Latin-ASCII") %>% str_to_lower(),
      paste0("\\b", nome_norm, "\\b")
    ))
    
    if (length(idx) > 0) {
      linha_alvo <- linhas[idx[1]]
      pos_est <- regexpr(paste0("(?i)\\b", nome_norm, "\\b"), stri_trans_general(linha_alvo, "Latin-ASCII"))
      linha_valores <- substr(linha_alvo, pos_est + attr(pos_est, "match.length"), nchar(linha_alvo))
      tokens <- str_extract_all(linha_valores, "\\b\\d{1,3}(?:\\.\\d{3})*(?:,\\d{2})?\\b|(?<=^|\\s)-(?=\\s|$)")[[1]]
      
      if (length(tokens) >= 4) {
        val_str <- tokens[4]
        val_num <- as.numeric(str_replace_all(str_replace_all(val_str, "\\.", ""), ",", "."))
        return(data.frame(UF = sigla_uf, Valor = val_num, stringsAsFactors = FALSE))
      }
    }
    return(data.frame(UF = sigla_uf, valor = NA_real_, stringsAsFactors = FALSE))
  })
}

# ------------------------------------------------------------------------------
# EXTRAÇÕES DE TODOS OS ANOS
# ------------------------------------------------------------------------------

df_2006 <- extrair_2006("C:/Dados_ASP/2008_anuario_brasileiro_seguranca_publica.pdf")
df_2010 <- extrair_2010("C:/Dados_ASP/2011_anuario_brasileiro_seguranca_publica.pdf") %>% rename(Despesa_2010 = Valor)
df_2014 <- extrair_2014("C:/Dados_ASP/2015_anuario_brasileiro_seguranca_publica.pdf") %>% rename(Despesa_2014 = Valor)
df_2018 <- extrair_2018("C:/Dados_ASP/2019_anuario_brasileiro_seguranca_publica.pdf") %>% rename(Despesa_2018 = Valor)
df_2022 <- extrair_2022("C:/Dados_ASP/2023_anuario_brasileiro_seguranca_publica.pdf") %>% rename(Despesa_2022 = Valor)

# Preenchimento manual das células que não puderam ser lidas pela função extrair_2014().
# Os valores foram extraídos diretamente da tabela 20, página 53, linhas "Mato Grosso do
# Sul e "Minas Gerais", coluna "Total>2014".
df_2014[10,"Despesa_2014"] <- 949126078 
df_2014[13,"Despesa_2014"] <- 10077099998

# ------------------------------------------------------------------------------
# 4. CONSOLIDAÇÃO DA TABELA FINAL NOMINAL E REAIS DE 2022 (IPCA)
# ------------------------------------------------------------------------------

df_final_nominal <- df_base_estados %>%
  left_join(df_2006, by = "UF") %>%
  left_join(df_2010, by = "UF") %>%
  left_join(df_2014, by = "UF") %>%
  left_join(df_2018, by = "UF") %>%
  left_join(df_2022, by = "UF") %>%
  select(Estados, UF, Despesa_2006, Despesa_2010, Despesa_2014, Despesa_2018, Despesa_2022) %>%
  arrange(Estados)

df_final_real_2022 <- df_final_nominal %>%
  mutate(
    Despesa_2006 = round(Despesa_2006 * 2.4757, 2),
    Despesa_2010 = round(Despesa_2010 * 2.0258, 2),
    Despesa_2014 = round(Despesa_2014 * 1.5947, 2),
    Despesa_2018 = round(Despesa_2018 * 1.2693, 2),
    Despesa_2022 = round(Despesa_2022 * 1.0000, 2)
    )

# Exibe o resultado consolidado e deflacionado
print(df_final_real_2022)