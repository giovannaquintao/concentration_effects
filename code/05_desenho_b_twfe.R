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
# Objetivo: viabilidade do TWFE (desenho b), no nivel da escola.
#
# Escola-ano "compartilha" = pelo menos 1 turma de 9o ano sai no MESMO
# horario de alguma turma de 10o ano (EM) da escola, naquele ano.
#   - Tratamento: a escola passa de "compartilha" para ZERO saida
#     compartilhada (compartilha -> zero).
#   - Controle: a escola compartilha nos dois periodos (ainda nao tratada).
# =====================================================================
turmas <- censo %>%
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
saidas_10ano <- turmas %>%
  filter(grade == 10) %>%
  distinct(ano, id_escola, saida_10 = saida_min)

# 10o ano por escola-ano: quantas turmas e em quais turnos
em_escola_ano <- turmas %>%
  filter(grade == 10) %>%
  group_by(ano, id_escola) %>%
  summarise(
    n_turmas_em = n(),
    turnos_em   = paste(sort(unique(turno)), collapse = "|"),
    .groups = "drop"
  )

# distancia (em minutos) de cada turma de 9o ano ate a saida de 10o ano mais proxima
dist_10ano <- turmas %>%
  filter(grade == 9) %>%
  select(ano, id_escola, id_turma, saida_min) %>%
  inner_join(saidas_10ano, by = c("ano", "id_escola"), relationship = "many-to-many") %>%
  group_by(ano, id_escola, id_turma) %>%
  summarise(dist_min = min(abs(saida_min - saida_10)), .groups = "drop")

# escola-ano sem turma de 10o ano: distancia NA (nao compartilha)
turmas_9ano <- turmas %>%
  filter(grade == 9) %>%
  left_join(dist_10ano, by = c("ano", "id_escola", "id_turma")) %>%
  mutate(compartilhada = coalesce(dist_min == 0, FALSE))

# painel escola x ano
painel <- turmas_9ano %>%
  group_by(ano, id_escola) %>%
  summarise(
    n_turmas_9         = n(),
    alunos_9           = sum(as.numeric(quantidade_matriculas)),
    n_compartilhadas   = sum(compartilhada),
    pct_compartilhadas = round(100 * mean(compartilhada), 1),
    # saida de 9o ano mais proxima de uma saida de 10o ano (NA = sem EM no ano)
    dist_min_escola    = suppressWarnings(min(dist_min, na.rm = TRUE)),
    turnos_9           = paste(sort(unique(turno)), collapse = "|"),
    .groups = "drop"
  ) %>%
  left_join(em_escola_ano, by = c("ano", "id_escola")) %>%
  mutate(
    dist_min_escola = na_if(dist_min_escola, Inf),
    n_turmas_em     = coalesce(n_turmas_em, 0L),
    turnos_em       = coalesce(turnos_em, "sem EM"),
    tem_em          = n_turmas_em > 0,
    compartilha     = n_compartilhadas > 0
  ) %>%
  arrange(id_escola, ano)

nrow(painel)                       # escola-anos
n_distinct(painel$id_escola)       # escolas
table(table(painel$id_escola))     # em quantos anos cada escola aparece

# por ano: quantas escolas compartilham / nao compartilham
painel %>%
  group_by(ano) %>%
  summarise(
    escolas         = n(),
    n_compartilha   = sum(compartilha),
    n_zero          = sum(!compartilha),
    pct_zero        = round(100 * mean(!compartilha), 1),
    zero_sem_em     = sum(!compartilha & !tem_em),
    .groups = "drop"
  ) %>%
  print()

# =====================================================================
# Transicoes: cada escola-ano contra o ano anterior OBSERVADO da escola
# =====================================================================
monta_transicoes <- function(p) {
  p %>%
    group_by(id_escola) %>%
    arrange(ano, .by_group = TRUE) %>%
    mutate(
      ano_anterior    = lag(ano),
      gap_anos        = ano - ano_anterior,
      compartilha_ant = lag(compartilha),
      n_turmas_9_ant  = lag(n_turmas_9),
      n_turmas_em_ant = lag(n_turmas_em),
      turnos_9_ant    = lag(turnos_9),
      turnos_em_ant   = lag(turnos_em),
      transicao = case_when(
        is.na(ano_anterior)            ~ NA_character_,
        compartilha_ant & compartilha  ~ "compartilha -> compartilha (controle)",
        compartilha_ant & !compartilha ~ "compartilha -> zero (tratamento)",
        !compartilha_ant & compartilha ~ "zero -> compartilha (reversao)",
        TRUE                           ~ "zero -> zero"
      )
    ) %>%
    ungroup()
}

transicoes <- monta_transicoes(painel)

transicoes %>%
  filter(!is.na(transicao)) %>%
  count(transicao) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

# por ano: tratadas e controles disponiveis em cada comparacao t-1 -> t
transicoes_ano <- transicoes %>%
  filter(!is.na(transicao)) %>%
  count(ano, transicao) %>%
  pivot_wider(names_from = transicao, values_from = n, values_fill = 0)

print(transicoes_ano, width = Inf)

# =====================================================================
# Padrao de cada escola no periodo (C = compartilha, Z = zero, . = sem dado)
# =====================================================================
anos <- sort(unique(painel$ano))

sequencias <- painel %>%
  select(id_escola, ano, compartilha) %>%
  complete(id_escola, ano = anos) %>%
  arrange(id_escola, ano) %>%
  group_by(id_escola) %>%
  summarise(
    sequencia = paste(case_when(is.na(compartilha) ~ ".", compartilha ~ "C", TRUE ~ "Z"),
                      collapse = ""),
    .groups = "drop"
  )

classifica_padrao <- function(p) {
  p %>%
    group_by(id_escola) %>%
    arrange(ano, .by_group = TRUE) %>%
    summarise(
      n_anos           = n(),
      anos_compartilha = sum(compartilha),
      anos_zero        = sum(!compartilha),
      comeca_compart   = first(compartilha),
      n_trocas         = sum(compartilha != lag(compartilha), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      padrao = case_when(
        anos_zero == 0                 ~ "sempre compartilha (nunca tratada)",
        anos_compartilha == 0          ~ "nunca compartilha (sempre tratada)",
        n_trocas == 1 & comeca_compart ~ "troca 1 vez: compartilha -> zero",
        n_trocas == 1                  ~ "troca 1 vez: zero -> compartilha",
        TRUE                           ~ "troca mais de 1 vez (liga/desliga)"
      )
    )
}

padrao_escola <- classifica_padrao(painel) %>%
  left_join(sequencias, by = "id_escola")

padrao_escola %>%
  count(padrao) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

# =====================================================================
# Eventos de tratamento: inicio de um bloco de anos "zero" que vem logo
# depois de um bloco de anos "compartilha"
# =====================================================================
monta_eventos <- function(p) {
  p %>%
    group_by(id_escola) %>%
    arrange(ano, .by_group = TRUE) %>%
    mutate(bloco = cumsum(compartilha != lag(compartilha, default = first(compartilha)))) %>%
    group_by(id_escola, bloco) %>%
    summarise(
      compartilha = first(compartilha),
      ano_inicio  = min(ano),
      ano_fim     = max(ano),
      n_anos      = n(),
      .groups = "drop"
    ) %>%
    group_by(id_escola) %>%
    arrange(bloco, .by_group = TRUE) %>%
    mutate(anos_pre = lag(n_anos)) %>%       # anos seguidos compartilhando antes
    ungroup() %>%
    filter(!compartilha, bloco > 0) %>%
    transmute(
      id_escola,
      ano_evento  = ano_inicio,
      anos_pre,
      anos_pos    = n_anos,                  # anos seguidos com zero, contando o do evento
      # fica com zero ate o ultimo ano do painel (nao reverte)
      fica_ate_fim = ano_fim == max(anos)
    )
}

resume_eventos <- function(e) {
  e %>%
    summarise(
      eventos             = n(),
      escolas             = n_distinct(id_escola),
      n_fica_ate_fim      = sum(fica_ate_fim),
      n_reverte           = sum(!fica_ate_fim),
      dura_1_ano_e_volta  = sum(anos_pos == 1 & !fica_ate_fim),
      # evento "limpo": 2+ anos compartilhando antes e 2+ anos com zero depois
      limpos              = sum(anos_pre >= 2 & anos_pos >= 2),
      escolas_limpas      = n_distinct(id_escola[anos_pre >= 2 & anos_pos >= 2])
    )
}

eventos <- monta_eventos(painel) %>%
  # o que mais muda na escola no ano do evento
  left_join(
    transicoes %>%
      select(id_escola, ano, gap_anos, tem_em, dist_min_escola,
             n_turmas_9, n_turmas_9_ant, n_turmas_em, n_turmas_em_ant,
             turnos_9, turnos_9_ant, turnos_em, turnos_em_ant),
    by = c("id_escola", "ano_evento" = "ano")
  ) %>%
  mutate(
    mudou_n_turmas_9 = n_turmas_9 != n_turmas_9_ant,
    o_que_mudou = case_when(
      !tem_em                                               ~ "escola sem turma de EM no ano",
      dist_min_escola <= 15                                 ~ "saida a ate 15 min do EM (possivel ruido)",
      turnos_9 != turnos_9_ant | turnos_em != turnos_em_ant ~ "turno do 9o ano ou do EM mudou",
      TRUE                                                  ~ "mesmos turnos, horario de saida mudou"
    )
  )

nrow(eventos)                      # eventos compartilha -> zero
n_distinct(eventos$id_escola)      # escolas com pelo menos 1 evento

# coortes: eventos por ano
eventos %>% count(ano_evento) %>% print()

# quantos anos antes (compartilhando) e depois (zero) de cada evento
table(anos_pre = eventos$anos_pre, anos_pos = eventos$anos_pos)

# o tratamento persiste?
resume_eventos(eventos) %>% print(width = Inf)

# o que muda junto com o tratamento
eventos %>%
  count(o_que_mudou) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

eventos %>%
  summarise(
    pct_mudou_n_turmas_9 = round(100 * mean(mudou_n_turmas_9), 1),
    pct_gap_maior_1_ano  = round(100 * mean(gap_anos > 1), 1)
  ) %>%
  print()

# eventos limpos, por o que mudou
eventos %>%
  filter(anos_pre >= 2, anos_pos >= 2) %>%
  count(o_que_mudou) %>%
  print()

# =====================================================================
# 2014: a duracao das turmas de EM sobe ~50 min so nesse ano, entao quase
# toda escola deixa de compartilhar em 2014 e volta em 2015. Refaz as
# contas sem 2014 (2013 passa a ser comparado direto com 2015).
# =====================================================================
turmas %>%
  group_by(ano, grade) %>%
  summarise(
    turmas        = n(),
    duracao_media = round(mean(numero_duracao_turma), 1),
    saida_mediana = median(saida_min),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = grade, values_from = c(turmas, duracao_media, saida_mediana)) %>%
  print(width = Inf)

painel_sem_2014 <- painel %>% filter(ano != 2014)

padrao_sem_2014 <- classifica_padrao(painel_sem_2014)

padrao_sem_2014 %>%
  count(padrao) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

eventos_sem_2014 <- monta_eventos(painel_sem_2014)

eventos_sem_2014 %>% count(ano_evento) %>% print()
resume_eventos(eventos_sem_2014) %>% print(width = Inf)

# =====================================================================
# Restricao: o turno do 9o ano NAO pode mudar entre t-1 e t (o do EM pode).
# turnos_9 = conjunto de turnos com turma de 9o ano na escola-ano (ex.:
# "manha|tarde"). Vale para tratadas e para controles.
# =====================================================================
analisa_turno_9_fixo <- function(p) {
  tr <- monta_transicoes(p) %>%
    filter(!is.na(transicao)) %>%
    mutate(mesmo_turno_9 = turnos_9 == turnos_9_ant)

  # quantas comparacoes t-1 -> t sobram
  tr %>%
    group_by(transicao) %>%
    summarise(todas = n(), mesmo_turno_9 = sum(mesmo_turno_9), .groups = "drop") %>%
    print()

  # por ano: tratadas e controles com o turno do 9o ano fixo
  tr %>%
    filter(mesmo_turno_9) %>%
    count(ano, transicao) %>%
    pivot_wider(names_from = transicao, values_from = n, values_fill = 0) %>%
    select(ano, starts_with("compartilha")) %>%
    print(width = Inf)

  ev <- monta_eventos(p) %>%
    left_join(
      tr %>% select(id_escola, ano, mesmo_turno_9, tem_em, dist_min_escola,
                    n_turmas_9, n_turmas_9_ant, turnos_9, turnos_em, turnos_em_ant),
      by = c("id_escola", "ano_evento" = "ano")
    ) %>%
    filter(mesmo_turno_9) %>%
    mutate(
      mudou_n_turmas_9 = n_turmas_9 != n_turmas_9_ant,
      o_que_mudou = case_when(
        !tem_em                    ~ "escola sem turma de EM no ano",
        dist_min_escola <= 15      ~ "saida a ate 15 min do EM (possivel ruido)",
        turnos_em != turnos_em_ant ~ "turno do EM mudou",
        TRUE                       ~ "mesmos turnos, horario de saida mudou"
      )
    )

  resume_eventos(ev) %>% print(width = Inf)

  ev %>%
    count(o_que_mudou) %>%
    mutate(pct = round(100 * n / sum(n), 1)) %>%
    print()

  cat("pct de eventos em que o n. de turmas de 9o ano muda:",
      round(100 * mean(ev$mudou_n_turmas_9), 1), "\n")

  # mais restrito: escolas em que o turno do 9o ano nunca muda no periodo
  escolas_fixas <- p %>%
    group_by(id_escola) %>%
    summarise(fixo = n_distinct(turnos_9) == 1, .groups = "drop") %>%
    filter(fixo) %>%
    pull(id_escola)

  cat("escolas com turno do 9o ano fixo no periodo todo:", length(escolas_fixas), "\n")
  p %>% filter(id_escola %in% escolas_fixas) %>% monta_eventos() %>%
    resume_eventos() %>% print(width = Inf)
  p %>% filter(id_escola %in% escolas_fixas) %>% classifica_padrao() %>%
    count(padrao) %>% print()

  ev
}

eventos_turno_9_fixo          <- analisa_turno_9_fixo(painel)
eventos_turno_9_fixo_sem_2014 <- analisa_turno_9_fixo(painel_sem_2014)

writexl::write_xlsx(
  list(painel = painel, transicoes_ano = transicoes_ano,
       padrao_escola = padrao_escola, eventos = eventos,
       padrao_sem_2014 = padrao_sem_2014, eventos_sem_2014 = eventos_sem_2014,
       eventos_turno_9_fixo = eventos_turno_9_fixo,
       ev_turno_9_fixo_sem_2014 = eventos_turno_9_fixo_sem_2014),
  file.path(dir_output, "twfe_viabilidade.xlsx")
)
