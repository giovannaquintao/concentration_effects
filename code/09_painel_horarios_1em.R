library(basedosdados)
library(tidyverse)

set_billing_id("humancapitalindex")

# pasta output: funciona rodando de code/ ou da raiz do projeto
dir_output <- if (dir.exists("../output")) "../output" else "output"

# 1a serie do Ensino Medio (regular / integrado / normal-magisterio), 2011-2014
censo <- read_sql("
  SELECT
    ano, id_municipio, id_escola, id_turma,
    rede, etapa_ensino, numero_duracao_turma, hora_inicial, minuto_inicial, quantidade_matriculas
  FROM `basedosdados.br_inep_censo_escolar.turma`
  WHERE ano BETWEEN 2011 AND 2014
    AND sigla_uf = 'MG'
    AND etapa_ensino IN ('25', '30', '35')
    AND rede = 'estadual'
")

# so as escolas da amostra salva em 01_amostra.R (amostra_id_escola.rds)
amostra_id_escola <- readRDS(file.path(dir_output, "amostra_id_escola.rds"))
censo <- censo %>% filter(id_escola %in% amostra_id_escola)

# =====================================================================
# Objetivo: painel de ESCOLAS (2011-2014) com
#   (1) o horario de SAIDA das turmas de 1a serie do EM no turno da manha
#   (2) o horario de ENTRADA das turmas de 1a serie do EM no turno da tarde
# Se a escola tem mais de um horario no ano, marca e lista todos os
# horarios na coluna (com o numero de turmas em cada um).
# =====================================================================
turmas <- censo %>%
  mutate(across(c(ano, numero_duracao_turma, hora_inicial, minuto_inicial,
                  quantidade_matriculas), as.integer)) %>%
  mutate(
    entrada_min = hora_inicial * 60L + minuto_inicial,
    saida_min   = entrada_min + numero_duracao_turma,
    # mesmos turnos de 08_horarios_1em_2em.R: manha = entrada antes das 12:00;
    # tarde = entrada entre 12:00 e 14:00
    turno = case_when(
      hora_inicial < 12      ~ "manha",
      entrada_min <= 14 * 60 ~ "tarde",
      TRUE                   ~ "outro"
    )
  )

# minutos depois da meia-noite -> "HH:MM"
rotulo_hora <- function(m) sprintf("%02d:%02d", m %/% 60, m %% 60)

# monta o painel escola x ano para um turno e uma variavel de horario
#   turno_sel : "manha" ou "tarde"
#   hora_var  : "saida_min" ou "entrada_min"
painel_escola <- function(turno_sel, hora_var) {

  turmas_turno <- turmas %>%
    filter(turno == turno_sel) %>%
    mutate(hora_min = .data[[hora_var]]) %>%
    filter(!is.na(hora_min))

  # turmas e alunos em cada horario, por escola e ano
  por_horario <- turmas_turno %>%
    group_by(id_municipio, id_escola, ano, hora_min) %>%
    summarise(turmas = n(),
              alunos = sum(quantidade_matriculas, na.rm = TRUE),
              .groups = "drop") %>%
    arrange(id_escola, ano, hora_min)

  painel <- por_horario %>%
    group_by(id_municipio, id_escola, ano) %>%
    summarise(
      n_turmas     = sum(turmas),
      n_alunos     = sum(alunos),
      n_horarios   = n(),
      # horario mais comum (empate: o mais cedo)
      horario_principal = rotulo_hora(hora_min[which.max(turmas)]),
      # lista de todos os horarios, com o numero de turmas: "11:20 (3); 11:30 (1)"
      lista_horarios = paste0(rotulo_hora(hora_min), " (", turmas, ")", collapse = "; "),
      .groups = "drop"
    ) %>%
    mutate(mais_de_um_horario = n_horarios > 1) %>%
    # escolas da amostra sem turma de 1a serie nesse turno e ano ficam com NA
    complete(nesting(id_municipio, id_escola), ano = 2011:2014) %>%
    arrange(id_escola, ano)

  # versao larga: uma linha por escola, uma coluna por ano
  largo <- painel %>%
    mutate(horario = if_else(mais_de_um_horario, lista_horarios, horario_principal)) %>%
    select(id_municipio, id_escola, ano, horario, mais_de_um_horario) %>%
    pivot_wider(names_from = ano, values_from = c(horario, mais_de_um_horario)) %>%
    mutate(algum_ano_mais_de_um = if_any(starts_with("mais_de_um_horario_"), ~ .x %in% TRUE))

  list(longo = painel, largo = largo, por_horario = por_horario)
}

saida_manha   <- painel_escola("manha", "saida_min")
entrada_tarde <- painel_escola("tarde", "entrada_min")

# -------------------------------------------------------------------
# resumo: quantas escolas tem turma no turno e quantas tem mais de um horario
# -------------------------------------------------------------------
resumo <- function(p, nome) {
  cat("\n=====", nome, "=====\n")
  p$longo %>%
    group_by(ano) %>%
    summarise(escolas_com_turma = sum(!is.na(n_turmas)),
              mais_de_um_horario = sum(mais_de_um_horario, na.rm = TRUE),
              .groups = "drop") %>%
    print()
}
resumo(saida_manha,   "saida da 1a serie EM, manha")
resumo(entrada_tarde, "entrada da 1a serie EM, tarde")

# -------------------------------------------------------------------
# salva
# -------------------------------------------------------------------
saveRDS(saida_manha$longo, file.path(dir_output, "painel_saida_1em_manha.rds"))
saveRDS(entrada_tarde$longo, file.path(dir_output, "painel_entrada_1em_tarde.rds"))

writexl::write_xlsx(
  list(longo = saida_manha$longo, largo = saida_manha$largo,
       por_horario = saida_manha$por_horario %>% mutate(horario = rotulo_hora(hora_min))),
  file.path(dir_output, "painel_saida_1em_manha.xlsx")
)
writexl::write_xlsx(
  list(longo = entrada_tarde$longo, largo = entrada_tarde$largo,
       por_horario = entrada_tarde$por_horario %>% mutate(horario = rotulo_hora(hora_min))),
  file.path(dir_output, "painel_entrada_1em_tarde.xlsx")
)
