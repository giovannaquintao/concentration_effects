library(basedosdados)
library(tidyverse)

set_billing_id("humancapitalindex")

# pasta output: funciona rodando de code/ ou da raiz do projeto
dir_output <- if (dir.exists("../output")) "../output" else "output"

# 1a, 2a e 3a series do Ensino Medio (regular / integrado / normal-magisterio), 2013-2015
censo <- read_sql("
  SELECT
    ano, id_escola, id_turma, etapa_ensino,
    numero_duracao_turma, hora_inicial, minuto_inicial
  FROM `basedosdados.br_inep_censo_escolar.turma`
  WHERE ano BETWEEN 2013 AND 2015
    AND sigla_uf = 'MG'
    AND etapa_ensino IN ('25', '30', '35', '26', '31', '36', '27', '32', '37')
    AND rede = 'estadual'
")

# escolas do piloto do Reinventando (2012 e 2013), salvas em 0_escolas_reinventando.R
id_escolas_reinventando <- readRDS(file.path(dir_output, "id_escolas_reinventando.rds"))

# aumento minimo (em minutos) da duracao da 1a serie de 2013 para 2014:
# limiar principal e um mais frouxo
limiar_min  <- 50
limiar2_min <- 40

# diferenca maxima (em minutos) para dizer que a 1a serie sai junto com a 2a e a 3a
janela_min <- 5

# =====================================================================
# Objetivo: tabela descritiva das escolas estaduais de MG com Ensino
# Medio em 2013.
#   - todas as escolas estaduais com turma de 1a, 2a ou 3a serie do EM
#   - amostra: as mesmas, sem as escolas do piloto
# So na amostra (% em relacao a amostra):
#   - duracao da turma de 1a serie subiu 50 min ou mais (e 40 min ou mais)
#     de 2013 para 2014, em pelo menos um turno da escola
#   - duracao da turma de 1a serie nao mudou em nenhum turno da escola
#   - escola com turma de EM de manha e de tarde
#   - mais de um horario de saida dentro da mesma serie e turno
#     (so manha e tarde; o noturno fica de fora)
#   - numero medio de turmas por serie (media entre escola x serie)
# Segunda tabela, tambem so na amostra: em 2013, 2014 e 2015, quantas
# escolas soltam a 1a serie junto (ate 5 min) com a 2a e a 3a series.
# As duas tabelas vao para output/ em .xlsx e em .tex (main.tex le o .tex).
# =====================================================================
turmas <- censo %>%
  mutate(across(c(ano, numero_duracao_turma, hora_inicial, minuto_inicial), as.integer)) %>%
  mutate(
    serie = case_when(
      etapa_ensino %in% c("25", "30", "35") ~ "1a",
      etapa_ensino %in% c("26", "31", "36") ~ "2a",
      etapa_ensino %in% c("27", "32", "37") ~ "3a"
    ),
    entrada_min = hora_inicial * 60L + minuto_inicial,
    saida_min   = entrada_min + numero_duracao_turma,
    # mesmos turnos de 01_grafico_linha_horario.R: manha = entrada antes das 12:00;
    # tarde = entrada entre 12:00 e 14:00
    turno = case_when(
      hora_inicial < 12      ~ "manha",
      entrada_min <= 14 * 60 ~ "tarde",
      TRUE                   ~ "outro"
    )
  )

turmas_2013 <- turmas %>% filter(ano == 2013)

escolas_mg      <- unique(turmas_2013$id_escola)
escolas_amostra <- setdiff(escolas_mg, id_escolas_reinventando)
n_amostra       <- length(escolas_amostra)
turmas_amostra  <- turmas_2013 %>% filter(id_escola %in% escolas_amostra)

cat("escolas do piloto com turma de EM em 2013:",
    length(intersect(escolas_mg, id_escolas_reinventando)), "\n")

# -------------------------------------------------------------------
# duracao da 1a serie: 2013 -> 2014, por escola x turno
# -------------------------------------------------------------------
duracao_1a <- turmas %>%
  filter(serie == "1a", ano %in% c(2013, 2014), id_escola %in% escolas_amostra,
         !is.na(numero_duracao_turma)) %>%
  group_by(id_escola, turno, ano) %>%
  summarise(duracao_media = mean(numero_duracao_turma), .groups = "drop") %>%
  pivot_wider(names_from = ano, values_from = duracao_media, names_prefix = "duracao_") %>%
  # so turnos com turma de 1a serie nos dois anos
  filter(!is.na(duracao_2013), !is.na(duracao_2014)) %>%
  mutate(aumento = duracao_2014 - duracao_2013)

mudou_duracao <- duracao_1a %>%
  group_by(id_escola) %>%
  summarise(mudou     = any(aumento >= limiar_min),
            mudou2    = any(aumento >= limiar2_min),
            # nao muda = duracao media igual em todos os turnos com 1a serie nos dois anos
            nao_mudou = all(aumento == 0),
            .groups = "drop")
n_mudou     <- sum(mudou_duracao$mudou)
n_mudou2    <- sum(mudou_duracao$mudou2)
n_nao_mudou <- sum(mudou_duracao$nao_mudou)

cat("escolas da amostra com turma de 1a serie no mesmo turno em 2013 e 2014:",
    nrow(mudou_duracao), "\n")

# -------------------------------------------------------------------
# turnos de manha e de tarde
# -------------------------------------------------------------------
turnos_escola <- turmas_amostra %>%
  group_by(id_escola) %>%
  summarise(manha = any(turno == "manha"), tarde = any(turno == "tarde"), .groups = "drop")
n_manha_tarde <- sum(turnos_escola$manha & turnos_escola$tarde)

# -------------------------------------------------------------------
# mais de um horario de saida dentro da mesma serie e turno
# -------------------------------------------------------------------
saidas_escola <- turmas_amostra %>%
  filter(turno %in% c("manha", "tarde"), !is.na(saida_min)) %>%
  group_by(id_escola, serie, turno) %>%
  summarise(n_saidas = n_distinct(saida_min), .groups = "drop") %>%
  group_by(id_escola) %>%
  summarise(saida_diferente = any(n_saidas > 1), .groups = "drop")
n_saida_diferente <- sum(saidas_escola$saida_diferente)

# -------------------------------------------------------------------
# numero de turmas por serie
# -------------------------------------------------------------------
turmas_serie <- turmas_amostra %>% count(id_escola, serie, name = "turmas")
media_turmas <- mean(turmas_serie$turmas)

# media por serie, so no console
turmas_serie %>%
  group_by(serie) %>%
  summarise(escolas = n(), media_turmas = round(mean(turmas), 2), .groups = "drop") %>%
  print()

# -------------------------------------------------------------------
# tabela
# -------------------------------------------------------------------
pct_amostra <- function(n) round(100 * n / n_amostra, 1)

tabela <- tibble(
  `State schools with high school, 2013` = c(
    "All, Minas Gerais",
    "Sample (excluding pilot schools)",
    paste0("1st-year class duration up ", limiar_min, "+ min in 2014"),
    paste0("1st-year class duration up ", limiar2_min, "+ min in 2014"),
    "1st-year class duration unchanged in 2014",
    "Morning and afternoon shifts",
    "Different release times within grade and shift",
    "Average classes per grade"
  ),
  N   = c(length(escolas_mg), n_amostra, n_mudou, n_mudou2, n_nao_mudou,
          n_manha_tarde, n_saida_diferente, round(media_turmas, 1)),
  `%` = c(NA, 100, pct_amostra(n_mudou), pct_amostra(n_mudou2), pct_amostra(n_nao_mudou),
          pct_amostra(n_manha_tarde), pct_amostra(n_saida_diferente), NA)
)

print(tabela, width = Inf)

# -------------------------------------------------------------------
# tabela 2: 1a serie sai junto (ate 5 min) com a 2a e a 3a, 2013-2015
# Por escola x ano x turno (manha e tarde): diferenca entre o horario
# medio de saida da 1a serie e o da 2a e 3a series. A escola sai junto
# no ano se a diferenca for de ate 5 min em pelo menos um turno.
# -------------------------------------------------------------------
saida_junto <- turmas %>%
  filter(id_escola %in% escolas_amostra, turno %in% c("manha", "tarde"), !is.na(saida_min)) %>%
  mutate(grupo = if_else(serie == "1a", "primeiro", "segundo_terceiro")) %>%
  group_by(id_escola, ano, turno, grupo) %>%
  summarise(saida_media = mean(saida_min), .groups = "drop") %>%
  pivot_wider(names_from = grupo, values_from = saida_media) %>%
  # so turnos com turma de 1a e de 2a/3a series no ano
  filter(!is.na(primeiro), !is.na(segundo_terceiro)) %>%
  mutate(junto = abs(primeiro - segundo_terceiro) <= janela_min) %>%
  group_by(id_escola, ano) %>%
  summarise(junto = any(junto), .groups = "drop")

tabela_junto <- saida_junto %>%
  group_by(ano) %>%
  summarise(comparaveis = n(), junto = sum(junto), .groups = "drop") %>%
  transmute(
    Year = ano,
    # comparaveis: com turma de 1a e de 2a/3a series no mesmo turno no ano
    `Comparable schools` = comparaveis,
    !!paste0("Release within ", janela_min, " min") := junto,
    `%` = pct_amostra(junto)
  )

print(tabela_junto, width = Inf)

writexl::write_xlsx(list(escolas_2013 = tabela, saida_junto = tabela_junto),
                    file.path(dir_output, "tabela_escolas_2013.xlsx"))

# -------------------------------------------------------------------
# .tex das duas tabelas (so o tabular; legenda e notas ficam no main.tex)
# -------------------------------------------------------------------
# numero -> texto (milhar com virgula; NA vira celula vazia)
fmt <- function(x, digitos = 0) {
  ifelse(is.na(x), "", formatC(x, format = "f", digits = digitos, big.mark = ","))
}

tabela_tex <- tabela %>%
  mutate(N   = c(fmt(head(N, -1)), fmt(tail(N, 1), 1)),
         `%` = fmt(`%`, 1))

tabela_junto_tex <- tabela_junto %>%
  mutate(across(2:3, fmt), `%` = fmt(`%`, 1), Year = as.character(Year))

writeLines(knitr::kable(tabela_tex, format = "latex", booktabs = TRUE, align = "lrr",
                        linesep = ""),
           file.path(dir_output, "tabela_escolas_2013.tex"))
writeLines(knitr::kable(tabela_junto_tex, format = "latex", booktabs = TRUE, align = "lrrr",
                        linesep = ""),
           file.path(dir_output, "tabela_saida_junto.tex"))
