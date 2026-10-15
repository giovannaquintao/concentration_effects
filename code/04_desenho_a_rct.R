library(basedosdados)
library(tidyverse)

set_billing_id("humancapitalindex")

# pasta output: funciona rodando de code/ ou da raiz do projeto
dir_output <- if (dir.exists("../output")) "../output" else "output"

censo <- read_sql("
  SELECT
    ano, sigla_uf, id_municipio, id_escola, id_turma,
    rede, etapa_ensino, numero_duracao_turma, hora_inicial, minuto_inicial,quantidade_matriculas
  FROM `basedosdados.br_inep_censo_escolar.turma`
  WHERE ano BETWEEN 2007 AND 2017
    AND sigla_uf = 'MG'
    AND etapa_ensino IN ('11', '41', '25', '30', '35')
")

# so as escolas da amostra salva em 01_amostra.R (amostra_id_escola.rds)
amostra_id_escola <- readRDS(file.path(dir_output, "amostra_id_escola.rds"))
censo <- censo %>% filter(id_escola %in% amostra_id_escola)

# =====================================================================
# Objetivo: quantas escolas da amostra podem entrar no "RCT" (desenho a).
#
# Celula = escola x turno x ano x duracao da turma. Tratada e controle tem
# que ter a mesma duracao de aula, senao o tempo na escola muda junto com
# o tratamento. A celula entra no RCT se:
#   - tem pelo menos 2 turmas de 9o ano
#   - pelo menos 1 turma de 9o ano sai no MESMO horario de alguma turma
#     de 10o ano (EM) da escola, naquele ano          -> tratada
#   - pelo menos 1 turma de 9o ano sai em horario que NAO bate com
#     nenhuma turma de 10o ano                        -> controle
# =====================================================================
turmas <- censo %>%
  filter(etapa_ensino %in% c("11", "41", "25", "30", "35")) %>%
  mutate(across(c(ano, numero_duracao_turma, hora_inicial, minuto_inicial), as.integer)) %>%
  mutate(
    grade = case_when(
      etapa_ensino %in% c("11", "41")       ~ 9,    # 8a serie (EF8) / 9o ano (EF9)
      etapa_ensino %in% c("25", "30", "35") ~ 10,   # 1a serie do Ensino Medio
      TRUE                                  ~ NA_real_
    ),
    entrada_min = hora_inicial * 60L + minuto_inicial,
    saida_min   = entrada_min + numero_duracao_turma,
    # turno pelo horario de entrada (nao tem coluna de turno na query)
    turno = case_when(
      hora_inicial < 12 ~ "manha",
      hora_inicial < 18 ~ "tarde",
      TRUE              ~ "noite"
    )
  ) %>%
  filter(!is.na(saida_min))

# horarios de saida do 10o ano, por escola-ano
# (mesmo horario de saida ja implica, na pratica, mesmo turno)
saidas_10ano <- turmas %>%
  filter(grade == 10) %>%
  distinct(ano, id_escola, saida_min) %>%
  mutate(compartilhada = TRUE)

# turma de 9o ano: tratada (saida compartilhada com 10o ano) ou controle
turmas_9ano <- turmas %>%
  filter(grade == 9) %>%
  left_join(saidas_10ano, by = c("ano", "id_escola", "saida_min")) %>%
  mutate(compartilhada = coalesce(compartilhada, FALSE))

# celula escola x turno x ano x duracao
celulas <- turmas_9ano %>%
  group_by(ano, id_escola, turno, numero_duracao_turma) %>%
  summarise(
    n_turmas           = n(),
    n_tratadas         = sum(compartilhada),
    n_controle         = sum(!compartilhada),
    alunos_tratados    = sum(quantidade_matriculas[compartilhada]),
    alunos_controle    = sum(quantidade_matriculas[!compartilhada]),
    .groups = "drop"
  ) %>%
  mutate(
    situacao = case_when(
      n_turmas < 2    ~ "1 turma so",
      n_controle == 0 ~ "todas compartilhadas",
      n_tratadas == 0 ~ "nenhuma compartilhada",
      TRUE            ~ "entra no RCT"
    ),
    entra_rct = situacao == "entra no RCT"
  )

# por que as celulas entram / nao entram
celulas %>%
  count(situacao) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

# por ano: escolas, celulas, turmas e alunos que entram no RCT
resumo_ano <- celulas %>%
  group_by(ano) %>%
  summarise(
    escolas_amostra   = n_distinct(id_escola),
    escolas_rct       = n_distinct(id_escola[entra_rct]),
    pct_escolas_rct   = round(100 * escolas_rct / escolas_amostra, 1),
    celulas_rct       = sum(entra_rct),
    turmas_tratadas   = sum(n_tratadas[entra_rct]),
    turmas_controle   = sum(n_controle[entra_rct]),
    alunos_tratados   = sum(alunos_tratados[entra_rct]),
    alunos_controle   = sum(alunos_controle[entra_rct]),
    .groups = "drop"
  )

print(resumo_ano, width = Inf)

# por turno (so as celulas que entram)
celulas %>%
  filter(entra_rct) %>%
  count(turno) %>%
  print()

# escolas que entram no RCT em pelo menos 1 ano, e em quantos anos
escolas_rct <- celulas %>%
  filter(entra_rct) %>%
  group_by(id_escola) %>%
  summarise(n_anos_rct = n_distinct(ano), .groups = "drop")

length(amostra_id_escola)          # escolas na amostra
nrow(escolas_rct)                  # escolas que entram no RCT em algum ano
table(escolas_rct$n_anos_rct)      # em quantos anos cada uma entra

# =====================================================================
# Teste com janela: a turma de 9o ano e tratada se alguma turma de 10o ano
# da escola, naquele ano, sai a ate w minutos dela (w = 0 e o caso exato).
# =====================================================================
# distancia (em minutos) ate a saida de 10o ano mais proxima
dist_10ano <- turmas_9ano %>%
  select(ano, id_escola, id_turma, saida_min) %>%
  inner_join(
    saidas_10ano %>% select(ano, id_escola, saida_10 = saida_min),
    by = c("ano", "id_escola"), relationship = "many-to-many"
  ) %>%
  group_by(ano, id_escola, id_turma) %>%
  summarise(dist_min = min(abs(saida_min - saida_10)), .groups = "drop")

# escola-ano sem turma de 10o ano: distancia infinita (nunca tratada)
turmas_9ano <- turmas_9ano %>%
  left_join(dist_10ano, by = c("ano", "id_escola", "id_turma")) %>%
  mutate(dist_min = coalesce(as.numeric(dist_min), Inf))

# quao longe da saida do 10o ano sai cada turma de 9o ano
turmas_9ano %>%
  mutate(faixa = cut(dist_min, c(-Inf, 0, 5, 10, 15, 30, 60, Inf),
                     labels = c("0", "1-5", "6-10", "11-15", "16-30", "31-60", "60+"))) %>%
  count(faixa) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

# celulas que entram no RCT para a janela w, com e sem exigir mesma duracao
conta_celulas <- function(w, mesma_duracao) {
  grupos <- c("ano", "id_escola", "turno", if (mesma_duracao) "numero_duracao_turma")
  turmas_9ano %>%
    mutate(tratada = dist_min <= w) %>%
    group_by(across(all_of(grupos))) %>%
    summarise(
      n_tratadas = sum(tratada),
      n_controle = sum(!tratada),
      alunos     = sum(as.numeric(quantidade_matriculas)),
      .groups = "drop"
    ) %>%
    filter(n_tratadas > 0, n_controle > 0) %>%
    summarise(
      janela          = w,
      mesma_duracao   = mesma_duracao,
      celulas_rct     = n(),
      escolas_rct     = n_distinct(id_escola),
      turmas_tratadas = sum(n_tratadas),
      turmas_controle = sum(n_controle),
      alunos          = sum(alunos)
    )
}

resumo_janela <- expand_grid(w = c(0, 5, 10, 15, 30), mesma_duracao = c(TRUE, FALSE)) %>%
  pmap_dfr(conta_celulas)

print(resumo_janela, width = Inf)

writexl::write_xlsx(
  list(resumo_ano = resumo_ano, celulas = celulas, escolas_rct = escolas_rct,
       resumo_janela = resumo_janela),
  file.path(dir_output, "rct_elegibilidade.xlsx")
)