# ==============================================================================
# PROJETO DE MQ1 (2016.2) - REGRESSÃO LOGÍSTICA: 
# TAXA DE VARIAÇÃO ORÇAMENTÁRIA EM SEGURANÇA PÚBLICA ~ REELEIÇÃO DE GOVERNADOR
# ELEIÇÕES: 2010, 2014, 2018, 2022
# VERSÃO: 8
# ==============================================================================

library(tidyverse)
library(tidyr)
library(stringr)
library(broom)
library(ggplot2)
# ------------------------------------------------------------------------------
# BASE DE DADOS OFICIAL COM AS 27 UFs (ORDEM ALFABÉTICA)
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
# CONSOLIDAÇÃO DOS VALORES NOMINAIS EXTRAÍDOS DAS TABELAS
# ------------------------------------------------------------------------------
df_final_nominal <- df_base_estados %>%
  mutate(
    # Anuário 2008 - Pág 37 (Total 2006)
    Despesa_2006 = c(
      152127568.45, 386030251.49, 150840881.46, 430738582.48, 1429220495.91,
      450389925.00,  92236299.12, 472818285.82, 714884471.11,  363286310.30,
      508233935.30, 415084203.64, 3911127686.93, 632472050.66, 362317230.03,
      1010463463.44, 780179117.93, 205831227.30, 4200236163.31, 295295137.83,
      1447229713.34, 352730994.87,  89165121.32, 908407159.12, 7116372156.81,
      279546235.14, 217199259.64
    ),
    # Anuário 2011 - Pág 51 (Despesa 2010)
    Despesa_2010 = c(
      279985016.85, 744115418.11, 244464877.48, 692917979.00, 1962408345.87,
      997917628.53, 283451450.68, 760757857.48, 1174130154.21, 784936223.92,
      979993100.87, 637523717.76, 5010254004.20, 1031278000.78, 576917165.11,
      1399003475.40, 1594131173.88, 292002220.20, 3914303880.11, 521111782.58,
      2625354406.08, 634200252.45, 146994729.27, 1352340569.14, 7373658387.05,
      706316011.34, 425457355.16
    ),
    # Anuário 2015 - Pág 53 (Total 2014)
    Despesa_2014 = c(
      449476165.99, 1063247125.37,  60124787.12, 1356403847.60, 3429608027.22,
      1699603997.50, 811397786.55, 1207412058.13, 2170416316.15, 1090910232.28,
      1119653186.52, 949126078.88, 10077099998.81, 1880019533.50, 861084733.45,
      2507554590.23, 2184447091.24,  59023864.49, 7717784433.78, 747136281.78,
      3003943900.69, 931295327.17, 244267184.56, 1973516032.89, 10386741965.77,
      740953856.49, 582988300.89
    ),
    # Anuário 2019 - Pág 163 (Ano 2018)
    Despesa_2018 = c(
      585955229.11, 1168666586.11, 456946368.27, 1739761071.13, 4297429365.90,
      2581001023.13, 845289739.13, 1471697256.77, 3402767593.18, 1849647394.56,
      2058639851.20, 1152447553.21, 9252331157.85, 2711633109.99, 1263695976.69,
      3885210795.35, 2775175397.30, 746278530.59, 9547928238.19, 976733756.97,
      4516732567.87, 843652832.25, 378092544.35, 2320874904.98, 11450839792.24,
      892986186.11, 839354309.58
    ),
    # Anuário 2023 - Pág 241 (Ano 2022)
    Despesa_2022 = c(
      974366892.28, 1623759179.88, 907088653.91, 2662045997.71, 5277719893.83,
      4186299676.85, 1146946782.02, 2247668152.57, 3519465640.61, 2062110314.84,
      3645305970.67, 1991889298.44, 11139603561.08, 4163685252.74, 1780596385.62,
      5104810384.26, 3320671005.89, 999571783.20, 13891833966.32, 1393230138.76,
      7000418171.24, 1601303839.05, 634471576.06, 3369120457.15, 14840589983.94,
      1284898903.55, 1149665495.72
    )
  )

# ------------------------------------------------------------------------------
# ATUALIZAÇÃO MONETÁRIA PARA REAIS CONSTANTES DE 2022 (IPCA/IBGE)
# ------------------------------------------------------------------------------
df_final_real_2022 <- df_final_nominal %>%
  mutate(
    Despesa_2006 = round(Despesa_2006 * 2.4757, 2),
    Despesa_2010 = round(Despesa_2010 * 2.0258, 2),
    Despesa_2014 = round(Despesa_2014 * 1.5947, 2),
    Despesa_2018 = round(Despesa_2018 * 1.2693, 2),
    Despesa_2022 = round(Despesa_2022 * 1.0000, 2)
  )

# ------------------------------------------------------------------------------
# 4. CÁLCULO DAS VARIAÇÕES PERCENTUAIS POR QUADRIÊNIO
# ------------------------------------------------------------------------------
df_variacao_wide <- df_final_real_2022 %>%
  mutate(
    Variacao_2007_2010 = round(((Despesa_2010 - Despesa_2006) / Despesa_2006) * 100, 2),
    Variacao_2011_2014 = round(((Despesa_2014 - Despesa_2010) / Despesa_2010) * 100, 2),
    Variacao_2015_2018 = round(((Despesa_2018 - Despesa_2014) / Despesa_2014) * 100, 2),
    Variacao_2019_2022 = round(((Despesa_2022 - Despesa_2018) / Despesa_2018) * 100, 2)
  ) %>%
  select(Estados, UF, Variacao_2007_2010, Variacao_2011_2014, Variacao_2015_2018, Variacao_2019_2022)

# Formato Long (UF | quadrienio | variacao_de_investimento_em_seguranca)
df_variacao_long <- df_variacao_wide %>%
  pivot_longer(
    cols = starts_with("Variacao_"),
    names_to = "quadrienio",
    values_to = "variacao_de_investimento_em_seguranca"
  ) %>%
  mutate(
    quadrienio = case_when(
      quadrienio == "Variacao_2007_2010" ~ "2007-2010",
      quadrienio == "Variacao_2011_2014" ~ "2011-2014",
      quadrienio == "Variacao_2015_2018" ~ "2015-2018",
      quadrienio == "Variacao_2019_2022" ~ "2019-2022"
    )
  ) %>%
  select(UF, quadrienio, variacao_de_investimento_em_seguranca)

# ------------------------------------------------------------------------------
# 5. EXIBIÇÃO DOS RESULTADOS
# ------------------------------------------------------------------------------
print("PAINEL FINAL EM REAIS DE 2022")
print(df_final_real_2022)

print("TABELA DE VARIAÇÃO PERCENTUAL REAL")
print(df_variacao_long, n = 108)

# Tratamento de Observações Discrepantes (Outliers)
#Com o objetivo de evitar distorções nas análises descritivas e inferenciais, 
#identificou-se e tratou-se a presença de observações atípicas (outliers) 
#nas taxas de variação quadrienal. Variações percentuais extremas (como no caso 
#do Piauí em 2015-2018 e do Amapá em 2015-2018) decorrem do "efeito base" 
#provocado pela subnotificação de gastos nos anos iniciais ou por readequações 
#metodológicas de registro contábil junto à Secretaria do Tesouro Nacional (STN), 
#e não de alterações efetivas nas diretrizes de gasto público.Para preservar a 
#consistência analítica e evitar a contaminação das médias estatísticas por 
#pontos de alta alavancagem, aplicou-se a remoção das observações que 
#ultrapassaram o limite de [inserir o critério escolhido, 
#ex.: 3 desvios-padrão / variação superior a 200%]. Os dados originais 
#permanecem documentados no repositório para fins de transparência e 
#reprodutibilidade.


# ------------------------------------------------------------------------------
# Função de Leitura e Padronização dos Arquivos do TSE (2006-2022)
# ------------------------------------------------------------------------------
ler_dados_tse <- function(ano) {
  arquivo <- file.path(caminho_tse, paste0("consulta_cand_", ano, "_BRASIL.csv"))
  
  if(!file.exists(arquivo)) {
    stop(paste("Arquivo não encontrado:", arquivo))
  }
  
  df <- read_delim(
    arquivo, 
    delim = ";", 
    locale = locale(encoding = "latin1"), 
    col_types = cols(.default = "c"),
    show_col_types = FALSE
  )
  
  colnames(df) <- toupper(colnames(df))
  
  # Identificação dinâmica das colunas
  col_cd_cargo <- intersect(c("CD_CARGO", "CODIGO_CARGO"), colnames(df))[1]
  col_ds_cargo <- intersect(c("DS_CARGO", "DESCRICAO_CARGO"), colnames(df))[1]
  col_sit_tot  <- intersect(c("DS_SIT_TOT_TURNO", "DS_SITUACAO_TOTALIZACAO", "DS_SIT_TOTALIZACAO"), colnames(df))[1]
  col_cpf      <- intersect(c("NR_CPF_CANDIDATO", "CPF_CANDIDATO"), colnames(df))[1]
  col_turno    <- intersect(c("NR_TURNO", "NUM_TURNO"), colnames(df))[1]
  
  # Filtragem do Cargo de Governador (Código 3 ou "GOVERNADOR")
  if(!is.na(col_cd_cargo)) {
    df <- df %>% filter(.data[[col_cd_cargo]] %in% c("3", "03"))
  } else {
    df <- df %>% filter(str_detect(toupper(.data[[col_ds_cargo]]), "GOVERNADOR") & 
                          !str_detect(toupper(.data[[col_ds_cargo]]), "VICE"))
  }
  
  # Ordenação por turno para garantir a situação final do pleito
  if(!is.na(col_turno)) {
    df <- df %>% arrange(as.numeric(.data[[col_turno]]))
  }
  
  df %>%
    mutate(
      ANO_ELEICAO = as.numeric(ANO_ELEICAO),
      SG_UF = toupper(SG_UF),
      NM_CANDIDATO = toupper(NM_CANDIDATO),
      
      # Limpeza de CPF
      CPF_RAW = str_remove_all(.data[[col_cpf]], "\\D"),
      NR_CPF_CANDIDATO = if_else(nchar(CPF_RAW) == 11 & CPF_RAW != "00000000000", CPF_RAW, NA_character_),
      
      SG_PARTIDO = toupper(SG_PARTIDO),
      SITUACAO_TURNO = toupper(.data[[col_sit_tot]]),
      ELEITO = if_else(
        str_detect(SITUACAO_TURNO, "ELEITO") & !str_detect(SITUACAO_TURNO, "NÃO ELEITO|SUPLENTE"), 
        1, 0
      )
    ) %>%
    group_by(SG_UF, NM_CANDIDATO) %>%
    slice_tail(n = 1) %>% 
    ungroup() %>%
    select(
      ANO_ELEICAO, SG_UF, NM_CANDIDATO, NR_CPF_CANDIDATO,
      SG_PARTIDO, SITUACAO_TURNO, ELEITO
    )
}

# ------------------------------------------------------------------------------
# Carregamento dos Dados das Eleições
# ------------------------------------------------------------------------------
caminho_tse <- "C:/Dados_TSE/"
anos_eleicao <- c(2006, 2010, 2014, 2018, 2022)
dados_tse_lista <- lapply(anos_eleicao, ler_dados_tse)
names(dados_tse_lista) <- paste0("ano_", anos_eleicao)

# ------------------------------------------------------------------------------
# Classificação de Reeleição Direta do Governador Incumbente
# ------------------------------------------------------------------------------
classificar_reeleicao_governador <- function(ano_atual, ano_anterior) {
  df_atual <- dados_tse_lista[[paste0("ano_", ano_atual)]]
  df_anterior <- dados_tse_lista[[paste0("ano_", ano_anterior)]]
  
  # Resgata o Governador que VENCEU no pleito anterior
  govs_anteriores <- df_anterior %>%
    filter(ELEITO == 1) %>%
    group_by(SG_UF) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(
      SG_UF, 
      NOME_GOV_ANT = NM_CANDIDATO,
      CPF_GOV_ANT = NR_CPF_CANDIDATO, 
      PARTIDO_GOV_ANT = SG_PARTIDO
    )
  
  # Verifica se o Governador anterior disputou a eleição atual
  reeleicao_panel <- govs_anteriores %>%
    left_join(df_atual, by = "SG_UF", relationship = "many-to-many") %>%
    mutate(
      # Casamento por CPF e por Nome (para cobrir eventuais CPFs ausentes em 2006/2010)
      eh_mesmo_candidato = case_when(
        !is.na(CPF_GOV_ANT) & !is.na(NR_CPF_CANDIDATO) ~ (CPF_GOV_ANT == NR_CPF_CANDIDATO),
        TRUE ~ (NOME_GOV_ANT == NM_CANDIDATO)
      )
    ) %>%
    group_by(SG_UF) %>%
    mutate(
      concorreu = any(eh_mesmo_candidato, na.rm = TRUE)
    ) %>%
    filter(if (any(concorreu)) eh_mesmo_candidato else row_number() == 1) %>%
    ungroup() %>%
    mutate(
      status_da_eleicao = case_when(
        !concorreu  ~ "não concorreu",
        ELEITO == 1 ~ "reeleito",
        ELEITO == 0 ~ "não reeleito"
      ),
      status_binario = if_else(status_da_eleicao == "reeleito", 1L, 0L),
      quadrienio = paste0(ano_anterior + 1, "-", ano_atual)
    ) %>%
    select(
      UF = SG_UF, 
      quadrienio, 
      governador_incumbente = NOME_GOV_ANT,
      status_da_eleicao, 
      status_binario
    )
  
  return(reeleicao_panel)
}

# ------------------------------------------------------------------------------
# Consolidação do Painel Eleitoral Final (108 observações: 27 UFs x 4 Quadriênios)
# ------------------------------------------------------------------------------
dados_reeleicao_painel <- bind_rows(
  classificar_reeleicao_governador(2010, 2006),
  classificar_reeleicao_governador(2014, 2010),
  classificar_reeleicao_governador(2018, 2014),
  classificar_reeleicao_governador(2022, 2018)
)

print(dados_reeleicao_painel, n = 108)

# ------------------------------------------------------------------------------
# JUNÇÃO, RENOMEAÇÃO E REORDENAÇÃO FINAL DO PAINEL
# ------------------------------------------------------------------------------
df_painel_final <- df_variacao_long %>%
  left_join(
    dados_reeleicao_painel %>% 
      select(UF, quadrienio, governador_incumbente, status_da_eleicao, status_binario),
    by = c("UF", "quadrienio")
  ) %>%
  rename(
    variacao_de_investimento_em_seguranca_em_percentual = variacao_de_investimento_em_seguranca
  ) %>%
  select(
    UF,
    quadrienio,
    governador_incumbente,
    status_da_eleicao,
    status_binario,
    variacao_de_investimento_em_seguranca_em_percentual
  )

# Exibe a nova estrutura consolidada
print(df_painel_final, n = 108)

# Exporta a base final no formato CSV
write.csv2(df_painel_final, "painel_seguranca_reeleicao_2006_2022.csv", row.names = FALSE)


# ------------------------------------------------------------------------------
# CARREGAMENTO E CRIAÇÃO DOS FILTROS DE OUTLIERS
# ------------------------------------------------------------------------------
# Carrega a base consolidada final (supondo que esteja na memória como df_painel_final)
# Ou via CSV: df_painel_final <- read_csv2("painel_seguranca_reeleicao_2006_2022.csv")

# Cálculo dos limites pela Regra do Amplitude Interquartil (IQR)
q1  <- quantile(df_painel_final$variacao_de_investimento_em_seguranca_em_percentual, 0.25, na.rm = TRUE)
q3  <- quantile(df_painel_final$variacao_de_investimento_em_seguranca_em_percentual, 0.75, na.rm = TRUE)
iqr <- q3 - q1

limite_inferior <- q1 - 1.5 * iqr
limite_superior <- q3 + 1.5 * iqr

# Adiciona flag de outlier no dataframe
df_analise <- df_painel_final %>%
  mutate(
    eh_outlier = variacao_de_investimento_em_seguranca_em_percentual < limite_inferior | 
      variacao_de_investimento_em_seguranca_em_percentual > limite_superior
  )

# Criar versão sem outliers para testes de robustez
df_sem_outliers <- df_analise %>% filter(!eh_outlier)


# ------------------------------------------------------------------------------
# ANÁLISE DO PAINEL COMPLETO (N = 108)
# ------------------------------------------------------------------------------
cat("\n ETAPA 1.1: ESTATÍSTICA DESCRITIVA DO PAINEL COMPLETO (GERAL) \n")

# Função auxiliar para tabela descritiva completa
gerar_descritivas <- function(df, grupo_var = NULL) {
  df %>%
    { if (!is.null(grupo_var)) group_by(., across(all_of(grupo_var))) else . } %>%
    summarise(
      N          = n(),
      Média      = round(mean(variacao_de_investimento_em_seguranca_em_percentual, na.rm = TRUE), 2),
      Mediana    = round(median(variacao_de_investimento_em_seguranca_em_percentual, na.rm = TRUE), 2),
      Desv_Pad   = round(sd(variacao_de_investimento_em_seguranca_em_percentual, na.rm = TRUE), 2),
      IQR        = round(IQR(variacao_de_investimento_em_seguranca_em_percentual, na.rm = TRUE), 2),
      Mínimo     = round(min(variacao_de_investimento_em_seguranca_em_percentual, na.rm = TRUE), 2),
      Máximo     = round(max(variacao_de_investimento_em_seguranca_em_percentual, na.rm = TRUE), 2),
      .groups    = "drop"
    )
}

# Descritiva Geral: Com vs Sem Outliers
desc_geral_com <- gerar_descritivas(df_analise) %>% mutate(Amostra = "Painel Completo (Com Outliers)")
desc_geral_sem <- gerar_descritivas(df_sem_outliers) %>% mutate(Amostra = "Painel Ajustado (Sem Outliers)")

bind_rows(desc_geral_com, desc_geral_sem) %>%
  select(Amostra, N, Média, Mediana, Desv_Pad, IQR, Mínimo, Máximo) %>%
  print()

cat("\n ETAPA 1.2: DESCRITIVA POR STATUS DA ELEIÇÃO (PAINEL COMPLETO n")
# Comparação entre as 3 categorias: reeleito, não reeleito, não concorreu
desc_status_com <- gerar_descritivas(df_analise, "status_da_eleicao") %>% mutate(Tipo = "Com Outliers")
desc_status_sem <- gerar_descritivas(df_sem_outliers, "status_da_eleicao") %>% mutate(Tipo = "Sem Outliers")

bind_rows(desc_status_com, desc_status_sem) %>%
  select(Tipo, status_da_eleicao, N, Média, Mediana, Desv_Pad, IQR, Mínimo, Máximo) %>%
  arrange(status_da_eleicao, Tipo) %>%
  print()


# ------------------------------------------------------------------------------
# SUBAMOSTRA APENAS DOS GOVERNADORES CONCORRENTES (REELEIÇÃO)
# ------------------------------------------------------------------------------
cat("\n ETAPA 2.1: DESCRITIVA DA SUBAMOSTRA DE CONCORRENTES (REELEITO VS NÃO REELEITO) \n")

# Filtragem: isola quem de fato disputou o pleito
df_concorrentes <- df_analise %>%
  filter(status_da_eleicao %in% c("reeleito", "não reeleito"))

df_concorrentes_sem_outliers <- df_sem_outliers %>%
  filter(status_da_eleicao %in% c("reeleito", "não reeleito"))

# Tabela descritiva comparativa para os postulantes à reeleição
desc_conc_com <- gerar_descritivas(df_concorrentes, "status_da_eleicao") %>% mutate(Amostra = "Concorrentes (Com Outliers)")
desc_conc_sem <- gerar_descritivas(df_concorrentes_sem_outliers, "status_da_eleicao") %>% mutate(Amostra = "Concorrentes (Sem Outliers)")

bind_rows(desc_conc_com, desc_conc_sem) %>%
  select(Amostra, status_da_eleicao, N, Média, Mediana, Desv_Pad, Mínimo, Máximo) %>%
  print()


# ------------------------------------------------------------------------------
# TESTES DE HIPÓTESES (DIFERENÇA DE MÉDIAS E MEDIANAS ENTRE CONCORRENTES)
# ------------------------------------------------------------------------------
cat("\n ETAPA 2.2: TESTES DE HIPÓTESES (REELEITO VS NÃO REELEITO) \n")

executar_testes <- function(df_sub, rotulo) {
  cat(paste0("\n--- Resultados para: ", rotulo, " ---\n"))
  
  # 1. Teste t de Student (Paramétrico - Compara Médias)
  teste_t <- t.test(
    variacao_de_investimento_em_seguranca_em_percentual ~ status_binario,
    data = df_sub,
    var.equal = FALSE
  )
  
  # 2. Teste de Mann-Whitney-Wilcoxon (Não Paramétrico - Compara Postos/Medianas)
  teste_mww <- wilcox.test(
    variacao_de_investimento_em_seguranca_em_percentual ~ status_binario,
    data = df_sub
  )
  
  resumo_testes <- data.frame(
    Cenário            = rotulo,
    N_Total            = nrow(df_sub),
    `Dif. Médias (1-0)`= round(diff(rev(tapply(df_sub$variacao_de_investimento_em_seguranca_em_percentual, df_sub$status_binario, mean))), 2),
    `p-valor (Teste t)`= round(teste_t$p.value, 4),
    `p-valor (Mann-Whitney)` = round(teste_mww$p.value, 4)
  )
  return(resumo_testes)
}

# Execução dos testes nas duas versões (Com e Sem Outliers)
testes_com <- executar_testes(df_concorrentes, "Com Outliers")
testes_sem <- executar_testes(df_concorrentes_sem_outliers, "Sem Outliers (Robustez)")

# Consolidação dos Testes
bind_rows(testes_com, testes_sem) %>%
  print(row.names = FALSE)

# ------------------------------------------------------------------------------
# PREPARAÇÃO DA SUBAMOSTRA (APENAS CONCORRENTES)
# ------------------------------------------------------------------------------
# Subamostra geral de concorrentes (Com Outliers: N = 53)
df_logit_com <- df_analise %>%
  filter(status_da_eleicao %in% c("reeleito", "não reeleito"))

# Subamostra ajustada de concorrentes (Sem Outliers / Robusto: N = 48)
df_logit_sem <- df_sem_outliers %>%
  filter(status_da_eleicao %in% c("reeleito", "não reeleito"))


# ------------------------------------------------------------------------------
# ESTIMAÇÃO DOS MODELOS LOGÍSTICOS
# ------------------------------------------------------------------------------
# Modelo 1: Com Outliers
modelo_logit_com <- glm(
  status_binario ~ variacao_de_investimento_em_seguranca_em_percentual,
  data = df_logit_com,
  family = binomial(link = "logit")
)

# Modelo 2: Sem Outliers (Modelo Principal de Robustez)
modelo_logit_sem <- glm(
  status_binario ~ variacao_de_investimento_em_seguranca_em_percentual,
  data = df_logit_sem,
  family = binomial(link = "logit")
)


# ------------------------------------------------------------------------------
# EXTRAÇÃO E ORGANIZAÇÃO DOS RESULTADOS (ODDS RATIO E SIGNIFICÂNCIA)
# ------------------------------------------------------------------------------
formatar_resultados_logit <- function(modelo, nome_modelo) {
  tidy(modelo, exponentiate = FALSE) %>%
    mutate(
      Odds_Ratio = exp(estimate),
      OR_IC_Inferior = exp(estimate - 1.96 * std.error),
      OR_IC_Superior = exp(estimate + 1.96 * std.error),
      p_valor_fmt = round(p.value, 4),
      Modelo = nome_modelo
    ) %>%
    select(
      Modelo,
      Termo = term,
      Coeficiente = estimate,
      Erro_Padrao = std.error,
      Estatistica_z = statistic,
      p_valor = p_valor_fmt,
      Odds_Ratio,
      OR_IC_Inferior,
      OR_IC_Superior
    )
}

res_com <- formatar_resultados_logit(modelo_logit_com, "Com Outliers (N=53)")
res_sem <- formatar_resultados_logit(modelo_logit_sem, "Sem Outliers (N=48)")

# Tabela consolidada dos coeficientes
tabela_coeficientes <- bind_rows(res_com, res_sem)

cat("\n=== RESULTADOS DA REGRESSÃO LOGÍSTICA ===\n")
print(tabela_coeficientes)


# ------------------------------------------------------------------------------
# QUALIDADE DO AJUSTE DO MODELO (FIT STATISTICS)
# ------------------------------------------------------------------------------
calcular_medidas_ajuste <- function(modelo, df_sub, nome) {
  null_deviance <- modelo$null.deviance
  resid_deviance <- modelo$deviance
  pseudo_r2 <- round(1 - (resid_deviance / null_deviance), 4) # McFadden's R2
  
  data.frame(
    Modelo = nome,
    N = nrow(df_sub),
    Deviance_Residua = round(resid_deviance, 2),
    AIC = round(AIC(modelo), 2),
    `McFadden_Pseudo_R2` = pseudo_r2
  )
}

fit_com <- calcular_medidas_ajuste(modelo_logit_com, df_logit_com, "Com Outliers")
fit_sem <- calcular_medidas_ajuste(modelo_logit_sem, df_logit_sem, "Sem Outliers")

cat("\n ESTATÍSTICAS DE AJUSTE DO MODELO (FIT) \n")
print(bind_rows(fit_com, fit_sem))

# ------------------------------------------------------------------------------
# GRÁFICO DA REGRESSÃO LOGÍSTICA (PROBABILIDADE PREVISTA VS. GASTO)
# ------------------------------------------------------------------------------
# Este gráfico plota os pontos reais (0 ou 1) e a curva de ajuste do Logit com IC 95%
grafico_logit <- ggplot(
  df_logit_sem, 
  aes(x = variacao_de_investimento_em_seguranca_em_percentual, y = status_binario)
) +
  # Pontos reais com leve deslocamento vertical (jitter) para evitar sobreposição
  geom_jitter(
    aes(color = factor(status_binario)), 
    height = 0.03, 
    width = 0, 
    size = 3, 
    alpha = 0.7
  ) +
  # Curva de probabilidade estimada pelo modelo Logit
  geom_smooth(
    method = "glm", 
    method.args = list(family = "binomial"), 
    color = "#2b5c8f", 
    fill = "#2b5c8f", 
    alpha = 0.15,
    linewidth = 1.2
  ) +
  # Linha de referência no ponto de indiferença (Probabilidade = 0.5)
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50", alpha = 0.7) +
  scale_color_manual(
    values = c("0" = "#d95f02", "1" = "#1b9e77"),
    labels = c("Não Reeleito (0)", "Reeleito (1)")
  ) +
  scale_y_continuous(
    breaks = c(0, 0.25, 0.50, 0.75, 1.0),
    labels = c("0% (Derrota)", "25%", "50%", "75%", "100% (Vitória)")
  ) +
  labs(
    title = "Modelo de Regressão Logística: Probabilidade de Reeleição",
    subtitle = "Amostra de Concorrentes sem Outliers (N = 48) | Coeficiente p-valor = 0,685 (Insignificante)",
    x = "Variação Real nos Gastos em Segurança Pública (%)",
    y = "Probabilidade Prevista de Reeleição",
    color = "Resultado Eleitoral"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray30", size = 10),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

# Exibir gráfico da regressão
print(grafico_logit)


# ------------------------------------------------------------------------------
# GRÁFICO DESCRITIVO: BOXPLOT + JITTER DOS CONCORRENTES
# ------------------------------------------------------------------------------
# Este gráfico evidencia a sobreposição quase perfeita dos dois grupos
grafico_boxplot <- ggplot(
  df_logit_sem, 
  aes(x = status_da_eleicao, y = variacao_de_investimento_em_seguranca_em_percentual, fill = status_da_eleicao)
) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA, width = 0.4) +
  geom_jitter(width = 0.1, size = 2.5, alpha = 0.8, aes(color = status_da_eleicao)) +
  scale_fill_manual(values = c("não reeleito" = "#d95f02", "reeleito" = "#1b9e77")) +
  scale_color_manual(values = c("não reeleito" = "#a63603", "reeleito" = "#006d2c")) +
  labs(
    title = "Distribuição da Variação de Gastos por Resultado Eleitoral",
    subtitle = "Subamostra de Governadores Postulantes à Reeleição (Sem Outliers)",
    x = "Status na Eleição",
    y = "Variação Real de Investimento (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

# Exibir gráfico de comparação de grupos
print(grafico_boxplot)