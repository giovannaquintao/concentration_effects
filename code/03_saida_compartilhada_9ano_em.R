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
")

# so as escolas da amostra salva em 01_amostra.R (amostra_id_escola.rds)
amostra_id_escola <- readRDS(file.path(dir_output, "amostra_id_escola.rds"))
censo <- censo %>% filter(id_escola %in% amostra_id_escola)

censo <- censo %>%
  # agora precisa das duas: 9o ano (EF9/EF8) E Ensino Medio ("10 ano"),
  # pra poder comparar horario de saida entre elas
  filter(etapa_ensino %in% c("11", "41", "25", "30", "35")) %>%
  mutate(across(c(ano, numero_duracao_turma, hora_inicial, minuto_inicial), as.integer)) %>%
  mutate(grade = case_when(
    etapa_ensino %in% c("11", "41")       ~ 9,    # 8a serie (EF8) / 9o ano (EF9)
    etapa_ensino %in% c("25", "30", "35") ~ 10,    # Ensino Medio
    TRUE                                  ~ NA_real_
  ))

# =====================================================================
# Objetivo (raciocinio invertido): indicador por escola-ano -- a escola
# tem ZERO turmas de 9o ano com horario de SAIDA igual ao de qualquer
# turma de Ensino Medio ("10o ano"), na mesma escola e no mesmo ano?
# (ou seja, NENHUM horario de saida do 9o bate com NENHUM do EM)
# =====================================================================
turmas <- censo %>%
  mutate(
    entrada_min = hora_inicial * 60L + minuto_inicial,
    saida_min   = entrada_min + numero_duracao_turma
  )

saidas_9ano <- turmas %>%
  filter(grade == 9) %>%
  distinct(ano, id_escola, saida_min)

saidas_em <- turmas %>%
  filter(grade == 10) %>%
  distinct(ano, id_escola, saida_min)

# string tipo "2*685+1*690": quantas turmas terminam em cada horario de
# saida (saida_min), ordenado do horario mais cedo pro mais tarde
perfil_horarios <- function(saida_vec) {
  tb  <- table(saida_vec)
  ord <- order(as.numeric(names(tb)))
  paste(sprintf("%d*%d", as.integer(tb[ord]), as.integer(names(tb)[ord])), collapse = "+")
}

horarios_9ano <- turmas %>%
  filter(grade == 9) %>%
  group_by(ano, id_escola) %>%
  summarise(horarios_9ano = perfil_horarios(saida_min), .groups = "drop")

horarios_em <- turmas %>%
  filter(grade == 10) %>%
  group_by(ano, id_escola) %>%
  summarise(horarios_em = perfil_horarios(saida_min), .groups = "drop")

# escola-ano em que pelo menos 1 saida do 9o ano bate com 1 saida do EM
# (na escola-ano grain, nao na saida_min grain -- ver nota abaixo)
escolas_saida_compartilhada <- saidas_9ano %>%
  inner_join(saidas_em, by = c("ano", "id_escola", "saida_min")) %>%
  distinct(ano, id_escola)

# base: todas as escola-anos que tem turma de 9o ano
base_escola_ano_9ano <- turmas %>%
  filter(grade == 9) %>%
  distinct(ano, id_escola)

# escola-ano com ZERO saida em comum = base MENOS quem tem pelo menos 1
# (anti_join no nivel escola-ano; nao dá pra fazer isso saida a saida,
# porque uma escola pode ter 1 saida que bate e outra que nao bate --
# o que importa aqui e' o total por escola, nao cada saida isolada)
escolas_sem_saida_compartilhada <- base_escola_ano_9ano %>%
  anti_join(escolas_saida_compartilhada, by = c("ano", "id_escola"))

assinatura_escola_ano <- turmas %>%
  filter(grade == 9) %>%
  group_by(ano, id_escola) %>%
  summarise(
    n_turmas_9ano  = n(),
    conjunto_saida = paste(sort(unique(saida_min)), collapse = "|"),   # ex.: "740|790"
    .groups = "drop"
  ) %>%
  left_join(
    escolas_saida_compartilhada %>% mutate(saida_compartilhada_9_em = TRUE),
    by = c("ano", "id_escola")
  ) %>%
  left_join(horarios_9ano, by = c("ano", "id_escola")) %>%
  left_join(horarios_em, by = c("ano", "id_escola")) %>%
  mutate(
    saida_compartilhada_9_em     = coalesce(saida_compartilhada_9_em, FALSE),
    sem_saida_compartilhada_9_em = !saida_compartilhada_9_em,   # a nova variavel de interesse
    horarios_em                  = coalesce(horarios_em, "sem turma de EM")
  ) %>%
  arrange(id_escola, ano)

nrow(assinatura_escola_ano)
n_distinct(assinatura_escola_ano$id_escola)
table(assinatura_escola_ano$sem_saida_compartilhada_9_em)

assinatura_escola_ano %>%
  group_by(ano) %>%
  summarise(
    n_escolas                   = n(),
    pct_sem_saida_compartilhada = round(100 * mean(sem_saida_compartilhada_9_em), 1),
    .groups = "drop"
  ) %>%
  print()

writexl::write_xlsx(assinatura_escola_ano, file.path(dir_output, "assinatura_escola_ano.xlsx"))

# =====================================================================
# Escolas que, em algum momento do periodo, PASSARAM A TER saida
# compartilhada 9o-EM: FALSE (sem_saida_compartilhada=TRUE) no ano
# anterior observado -> TRUE (sem_saida_compartilhada=FALSE) no ano
# seguinte observado (so essa direcao; nao o contrario)
# =====================================================================
transicao_saida <- assinatura_escola_ano %>%
  group_by(id_escola) %>%
  arrange(ano, .by_group = TRUE) %>%
  mutate(
    ano_anterior            = lag(ano),
    saida_compartilhada_ant = lag(saida_compartilhada_9_em),
    passou_a_compartilhar   = !is.na(ano_anterior) &
                               !saida_compartilhada_ant & saida_compartilhada_9_em
  ) %>%
  ungroup()

# escolas com pelo menos 1 transicao FALSE -> TRUE no periodo
escolas_passou_a_compartilhar <- transicao_saida %>%
  filter(passou_a_compartilhar) %>%
  distinct(id_escola) %>%
  pull(id_escola)

length(escolas_passou_a_compartilhar)

# trajetoria completa (todos os anos) so dessas escolas
trajetoria_passou_a_compartilhar <- transicao_saida %>%
  filter(id_escola %in% escolas_passou_a_compartilhar) %>%
  arrange(id_escola, ano)

trajetoria_passou_a_compartilhar
writexl::write_xlsx(trajetoria_passou_a_compartilhar, file.path(dir_output, "trajetoria_passou_a_compartilhar.xlsx"))
# tem q ser no mesmo turno