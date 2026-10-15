library(basedosdados)
library(tidyverse)

set_billing_id("humancapitalindex")

# pasta output: funciona rodando de code/ ou da raiz do projeto
dir_output <- if (dir.exists("../output")) "../output" else "output"

# criterios por escola (tratada, controle) salvos em 1_tabela_escolas_2013.R
criterios <- readRDS(file.path(dir_output, "criterios_tratamento.rds"))

# 1a, 2a e 3a series do Ensino Medio (regular / integrado / normal-magisterio), 2013-2015
censo <- read_sql("
  SELECT
    ano, id_municipio, id_escola, etapa_ensino,
    numero_duracao_turma, hora_inicial, minuto_inicial, quantidade_matriculas
  FROM `basedosdados.br_inep_censo_escolar.turma`
  WHERE ano BETWEEN 2013 AND 2015
    AND sigla_uf = 'MG'
    AND etapa_ensino IN ('25', '30', '35', '26', '31', '36', '27', '32', '37')
    AND rede = 'estadual'
")

# zona (urbana / rural) da escola e nome do municipio
diretorio <- read_sql("
  SELECT id_escola, localizacao
  FROM `basedosdados.br_bd_diretorios_brasil.escola`
  WHERE sigla_uf = 'MG'
")
municipios <- read_sql("
  SELECT id_municipio, nome AS municipio
  FROM `basedosdados.br_bd_diretorios_brasil.municipio`
  WHERE sigla_uf = 'MG'
")

# =====================================================================
# Objetivo: entender as escolas controle. Em 2014 o Reinventando o Ensino
# Medio levou o EM a 3.000 horas (6o horario) em toda a rede, mas nas
# escolas controle o horario de saida da 1a serie nao mudou. Hipoteses:
#   1. a 1a serie ja tinha 6 horarios (300+ min) em 2013
#   2. o 6o horario entrou antes da aula: entrada 40+ min mais cedo e
#      duracao 40+ min maior, mesma saida
#   3. o 6o horario ficou fora da grade declarada (duracao igual)
# Compara tratadas, controles e o resto da base de 1_tabela_escolas_2013.R:
#   A. horario da 1a serie de manha em 2013, 2014 e 2015
#   B. caracteristicas da escola em 2013
# =====================================================================
grupos <- criterios %>%
  transmute(id_escola,
            grupo = case_when(tratada  ~ "Treated",
                              controle ~ "Control",
                              TRUE     ~ "Rest of base"),
            grupo = factor(grupo, levels = c("Treated", "Control", "Rest of base")))

turmas <- censo %>%
  mutate(across(c(ano, numero_duracao_turma, hora_inicial, minuto_inicial,
                  quantidade_matriculas), as.integer)) %>%
  filter(id_escola %in% grupos$id_escola) %>%
  mutate(
    serie = case_when(
      etapa_ensino %in% c("25", "30", "35") ~ "1a",
      etapa_ensino %in% c("26", "31", "36") ~ "2a",
      etapa_ensino %in% c("27", "32", "37") ~ "3a"
    ),
    tipo = case_when(
      etapa_ensino %in% c("25", "26", "27") ~ "regular",
      etapa_ensino %in% c("30", "31", "32") ~ "integrado",
      etapa_ensino %in% c("35", "36", "37") ~ "normal"
    ),
    entrada_min = hora_inicial * 60L + minuto_inicial,
    saida_min   = entrada_min + numero_duracao_turma,
    # mesmos turnos de 1_tabela_escolas_2013.R
    turno = case_when(
      hora_inicial < 12      ~ "manha",
      entrada_min <= 14 * 60 ~ "tarde",
      TRUE                   ~ "outro"
    )
  )

# -------------------------------------------------------------------
# A. horario da 1a serie de manha: medias por escola e ano
# -------------------------------------------------------------------
horario_1a <- turmas %>%
  filter(serie == "1a", turno == "manha") %>%
  group_by(id_escola, ano) %>%
  summarise(entrada = mean(entrada_min, na.rm = TRUE),
            duracao = mean(numero_duracao_turma, na.rm = TRUE),
            saida   = mean(saida_min, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = ano, values_from = c(entrada, duracao, saida)) %>%
  mutate(dif_duracao_14 = duracao_2014 - duracao_2013,
         dif_entrada_14 = entrada_2014 - entrada_2013,
         dif_saida_14   = saida_2014 - saida_2013,
         dif_duracao_15 = duracao_2015 - duracao_2013)

pct <- function(x) 100 * mean(x, na.rm = TRUE)

resumo_horario <- grupos %>%
  left_join(horario_1a, by = "id_escola") %>%
  group_by(grupo) %>%
  summarise(
    escolas                    = n(),
    duracao_media_2013         = mean(duracao_2013, na.rm = TRUE),
    duracao_media_2014         = mean(duracao_2014, na.rm = TRUE),
    duracao_media_2015         = mean(duracao_2015, na.rm = TRUE),
    pct_6_horarios_2013        = pct(duracao_2013 >= 300),
    pct_duracao_igual_2014     = pct(abs(dif_duracao_14) < 1),
    pct_duracao_mais_40_2014   = pct(dif_duracao_14 >= 40),
    pct_entrada_40_antes_2014  = pct(dif_entrada_14 <= -40),
    pct_6o_horario_antes_aula  = pct(dif_duracao_14 >= 40 & dif_entrada_14 <= -40),
    mudanca_media_saida_2014   = mean(dif_saida_14, na.rm = TRUE),
    pct_duracao_2015_igual_2013 = pct(abs(dif_duracao_15) < 1),
    .groups = "drop"
  ) %>%
  mutate(across(-c(grupo, escolas), ~ round(.x, 1)))

cat("\n===== A. horario da 1a serie de manha =====\n")
print(as.data.frame(resumo_horario))

# duracao da 1a serie em 2013 e 2014: combinacoes mais comuns
dist_duracao <- grupos %>%
  filter(grupo %in% c("Treated", "Control")) %>%
  left_join(horario_1a, by = "id_escola") %>%
  mutate(duracao_2013 = round(duracao_2013), duracao_2014 = round(duracao_2014)) %>%
  count(grupo, duracao_2013, duracao_2014, name = "escolas") %>%
  group_by(grupo) %>%
  mutate(pct = round(100 * escolas / sum(escolas), 1)) %>%
  slice_max(escolas, n = 8, with_ties = FALSE) %>%
  ungroup()

cat("\n===== duracao da 1a serie (min), 2013 -> 2014 =====\n")
print(as.data.frame(dist_duracao))

# -------------------------------------------------------------------
# B. caracteristicas da escola em 2013
# -------------------------------------------------------------------
carac <- turmas %>%
  filter(ano == 2013) %>%
  group_by(id_escola) %>%
  summarise(id_municipio    = first(id_municipio),
            turmas_hs       = n(),
            matriculas_hs   = sum(quantidade_matriculas, na.rm = TRUE),
            turmas_1a_manha = sum(serie == "1a" & turno == "manha"),
            noturno         = any(!is.na(entrada_min) & entrada_min > 14 * 60),
            integrado       = any(tipo == "integrado"),
            normal          = any(tipo == "normal"),
            .groups = "drop") %>%
  left_join(diretorio, by = "id_escola") %>%
  left_join(municipios, by = "id_municipio")

resumo_carac <- grupos %>%
  left_join(carac, by = "id_escola") %>%
  group_by(grupo) %>%
  summarise(
    escolas              = n(),
    turmas_hs_media      = mean(turmas_hs),
    matriculas_hs_media  = mean(matriculas_hs),
    turmas_1a_manha      = mean(turmas_1a_manha),
    pct_com_noturno      = pct(noturno),
    pct_com_integrado    = pct(integrado),
    pct_com_normal       = pct(normal),
    pct_rural            = pct(localizacao == "Rural"),
    pct_belo_horizonte   = pct(id_municipio == "3106200"),
    municipios           = n_distinct(id_municipio),
    .groups = "drop"
  ) %>%
  mutate(across(-c(grupo, escolas, municipios), ~ round(.x, 1)))

cat("\n===== B. caracteristicas em 2013 =====\n")
print(as.data.frame(resumo_carac))

# municipios com mais escolas controle (e quantas tratadas cada um tem)
top_municipios <- grupos %>%
  left_join(carac, by = "id_escola") %>%
  count(municipio, grupo) %>%
  pivot_wider(names_from = grupo, values_from = n, values_fill = 0) %>%
  arrange(desc(Control)) %>%
  slice_head(n = 12)

cat("\n===== municipios com mais controles =====\n")
print(as.data.frame(top_municipios))

# -------------------------------------------------------------------
# C. as escolas controle so repetem o horario declarado no Censo?
# Para cada escola e ano (2009-2017), a "assinatura" da manha: o conjunto
# de combinacoes serie x entrada x duracao das turmas de EM de manha.
# Mudou = assinatura diferente da do ano anterior. Se as controles so
# repetem o horario declarado, mudam menos tambem nos anos sem o REM.
# -------------------------------------------------------------------
censo_painel <- read_sql("
  SELECT ano, id_escola, etapa_ensino, numero_duracao_turma, hora_inicial, minuto_inicial
  FROM `basedosdados.br_inep_censo_escolar.turma`
  WHERE ano BETWEEN 2009 AND 2017
    AND sigla_uf = 'MG'
    AND etapa_ensino IN ('25', '30', '35', '26', '31', '36', '27', '32', '37')
    AND rede = 'estadual'
")

assinatura <- censo_painel %>%
  mutate(across(c(ano, numero_duracao_turma, hora_inicial, minuto_inicial), as.integer)) %>%
  filter(id_escola %in% grupos$id_escola, hora_inicial < 12) %>%
  mutate(serie = case_when(
    etapa_ensino %in% c("25", "30", "35") ~ "1a",
    etapa_ensino %in% c("26", "31", "36") ~ "2a",
    etapa_ensino %in% c("27", "32", "37") ~ "3a"
  )) %>%
  distinct(id_escola, ano, serie, hora_inicial, minuto_inicial, numero_duracao_turma) %>%
  arrange(id_escola, ano, serie, hora_inicial, minuto_inicial, numero_duracao_turma) %>%
  group_by(id_escola, ano) %>%
  summarise(assinatura = paste(serie, hora_inicial, minuto_inicial, numero_duracao_turma,
                               collapse = ";"),
            .groups = "drop")

# so pares de anos consecutivos
mudancas <- assinatura %>%
  arrange(id_escola, ano) %>%
  group_by(id_escola) %>%
  mutate(ano_anterior = lag(ano), mudou = assinatura != lag(assinatura)) %>%
  ungroup() %>%
  filter(!is.na(ano_anterior), ano == ano_anterior + 1) %>%
  left_join(grupos, by = "id_escola")

# % das escolas com mudanca na assinatura, por par de anos
resumo_mudancas <- mudancas %>%
  mutate(transicao = paste0(ano_anterior, "-", ano)) %>%
  group_by(transicao, grupo) %>%
  summarise(pct_mudou = round(100 * mean(mudou), 1), .groups = "drop") %>%
  pivot_wider(names_from = grupo, values_from = pct_mudou)

# escolas que nunca mudaram, fora das transicoes do REM (2013-2014 e 2014-2015)
resumo_nunca <- mudancas %>%
  filter(!ano %in% c(2014, 2015)) %>%
  group_by(grupo, id_escola) %>%
  summarise(alguma_mudanca = any(mudou), .groups = "drop") %>%
  group_by(grupo) %>%
  summarise(escolas = n(), pct_nunca_mudou = round(100 * mean(!alguma_mudanca), 1),
            .groups = "drop")

cat("\n===== C. % de escolas com mudanca no horario declarado da manha =====\n")
print(as.data.frame(resumo_mudancas))
print(as.data.frame(resumo_nunca))

writexl::write_xlsx(
  list(horario_1a = resumo_horario, duracao_2013_2014 = dist_duracao,
       caracteristicas = resumo_carac, municipios = top_municipios,
       mudancas_ano = resumo_mudancas, nunca_mudou = resumo_nunca,
       escolas = grupos %>% left_join(horario_1a, by = "id_escola") %>%
         left_join(carac, by = "id_escola")),
  file.path(dir_output, "estudo_controles.xlsx")
)
