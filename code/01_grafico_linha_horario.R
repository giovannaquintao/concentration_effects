library(basedosdados)
library(tidyverse)

set_billing_id("humancapitalindex")

# pasta output: funciona rodando de code/ ou da raiz do projeto
dir_output <- if (dir.exists("../output")) "../output" else "output"

# 1a, 2a e 3a series do Ensino Medio (regular / integrado / normal-magisterio)
censo <- read_sql("
  SELECT
    ano, sigla_uf, id_municipio, id_escola, id_turma,
    rede, etapa_ensino, numero_duracao_turma, hora_inicial, minuto_inicial,quantidade_matriculas
  FROM `basedosdados.br_inep_censo_escolar.turma`
  WHERE ano BETWEEN 2007 AND 2017
    AND sigla_uf = 'MG'
    AND etapa_ensino IN ('25', '30', '35', '26', '31', '36', '27', '32', '37')
    AND rede = 'estadual'
")

# amostra: todas as escolas estaduais de MG (nao filtra por amostra_id_escola.rds)

# =====================================================================
# Objetivo: como os horarios de SAIDA mais frequentes das turmas de
# 1a, 2a e 3a series do Ensino Medio mudam de um ano para o outro, no turno
# da manha e no turno da tarde. Seis graficos de linhas: um por serie em
# cada turno. Dentro do turno, os tres graficos usam os mesmos horarios,
# as mesmas cores e o mesmo eixo y, para comparar as series.
# =====================================================================
turmas_em <- censo %>%
  mutate(across(c(ano, numero_duracao_turma, hora_inicial, minuto_inicial), as.integer)) %>%
  mutate(
    serie = case_when(
      etapa_ensino %in% c("25", "30", "35") ~ "1st year of high school",
      etapa_ensino %in% c("26", "31", "36") ~ "2nd year of high school",
      etapa_ensino %in% c("27", "32", "37") ~ "3rd year of high school"
    ),
    entrada_min = hora_inicial * 60L + minuto_inicial,
    saida_min   = entrada_min + numero_duracao_turma,
    # turno pelo horario de entrada; tarde = entrada entre 12:00 e 14:00
    # (quem entra depois das 14:00 fica de fora: sao turmas que saem a noite)
    turno = case_when(
      hora_inicial < 12      ~ "manha",
      entrada_min <= 14 * 60 ~ "tarde",
      TRUE                   ~ "outro"
    )
  ) %>%
  filter(!is.na(saida_min))

# serie -> sufixo do nome do arquivo
arquivo_serie <- c("1st year of high school" = "1ano",
                   "2nd year of high school" = "2ano",
                   "3rd year of high school" = "3ano")

# minutos depois da meia-noite -> "HH:MM"
rotulo_hora <- function(m) sprintf("%02d:%02d", m %/% 60, m %% 60)

# mesma cor por horario usada nos outros graficos (06_horarios_1em_manha.R); horario
# que nao aparece la pega uma das cores que sobraram; "Other" em cinza
cores_em <- c("11:20" = "#2a78d6", "11:25" = "#eb6834", "11:30" = "#1baf7a",
              "12:15" = "#eda100", "12:20" = "#e87ba4")
paleta   <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4",
              "#008300", "#4a3aa7", "#e34948")

tema <- theme_minimal(base_size = 12) +
  theme(
    plot.background    = element_rect(fill = "#fcfcfb", colour = NA),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#e1e0d9", linewidth = 0.3),
    axis.text          = element_text(colour = "#52514e"),
    axis.title         = element_text(colour = "#52514e"),
    plot.title         = element_text(colour = "#0b0b0b", face = "bold"),
    plot.subtitle      = element_text(colour = "#52514e"),
    plot.title.position = "plot",
    legend.position    = "right"
  )

# faz a tabela e os tres graficos de linhas (um por serie) de um turno
#   turno_sel : "manha" ou "tarde"
#   turno_en  : nome do turno nos titulos (em ingles)
#   subtitulo : subtitulo dos graficos (em ingles)
horarios_turno <- function(turno_sel, turno_en, subtitulo) {

  turmas_turno <- turmas_em %>%
    filter(turno == turno_sel)

  # % das turmas do turno em cada horario de saida, por serie e ano
  freq <- turmas_turno %>%
    count(serie, ano, hora_min = saida_min, name = "turmas") %>%
    group_by(serie, ano) %>%
    mutate(pct = 100 * turmas / sum(turmas)) %>%
    ungroup()

  # horarios mais frequentes: os 3 mais comuns de cada serie-ano (no maximo 8 no total)
  principais <- freq %>%
    group_by(serie, ano) %>%
    slice_max(turmas, n = 3, with_ties = FALSE) %>%
    ungroup() %>%
    distinct(hora_min) %>%
    pull(hora_min)

  if (length(principais) > 8) {
    principais <- freq %>%
      filter(hora_min %in% principais) %>%
      group_by(hora_min) %>%
      summarise(pct_max = max(pct), .groups = "drop") %>%
      slice_max(pct_max, n = 8, with_ties = FALSE) %>%
      pull(hora_min)
  }
  principais <- sort(principais)

  # tabela: serie x ano x horario (os que nao sao "principais" viram "Other")
  freq_principais <- freq %>%
    mutate(horario = if_else(hora_min %in% principais, rotulo_hora(hora_min), "Other")) %>%
    group_by(serie, ano, horario) %>%
    summarise(turmas = sum(turmas), pct = sum(pct), .groups = "drop") %>%
    complete(serie, ano, horario, fill = list(turmas = 0, pct = 0)) %>%
    mutate(horario = factor(horario, levels = c(rotulo_hora(principais), "Other")))

  cat("\n=====", turno_sel, "=====\n")

  freq_principais %>%
    mutate(pct = round(pct, 1)) %>%
    select(-turmas) %>%
    pivot_wider(names_from = horario, values_from = pct, names_sort = TRUE) %>%
    print(n = Inf, width = Inf)

  turmas_turno %>%
    count(serie, ano) %>%
    pivot_wider(names_from = serie, values_from = n) %>%
    print()

  # -------------------------------------------------------------------
  # Graficos (textos em ingles): um grafico de linhas por serie
  # -------------------------------------------------------------------
  rotulos <- rotulo_hora(principais)
  fixos   <- cores_em[intersect(rotulos, names(cores_em))]
  novos   <- setdiff(rotulos, names(cores_em))
  cores_horario <- c(fixos, setNames(setdiff(paleta, fixos)[seq_along(novos)], novos))[rotulos]
  cores_horario <- c(cores_horario, Other = "#898781")

  # mesmo eixo y nas tres series do turno
  y_max <- max(freq_principais$pct)

  for (s in names(arquivo_serie)) {
    g_linhas <- ggplot(filter(freq_principais, serie == s),
                       aes(ano, pct, colour = horario, group = horario)) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 2.2) +
      scale_colour_manual(values = cores_horario, name = "Release time", drop = FALSE) +
      scale_x_continuous(breaks = sort(unique(freq_principais$ano))) +
      scale_y_continuous(limits = c(0, y_max), expand = expansion(mult = c(0, 0.05))) +
      labs(
        title    = paste0(s, ", ", turno_en, " shift"),
        subtitle = subtitulo,
        x = NULL, y = "% of classes"
      ) +
      tema

    ggsave(file.path(dir_output,
                     paste0("horarios_em_series_", turno_sel, "_", arquivo_serie[[s]], ".png")),
           g_linhas, width = 9, height = 3.6, dpi = 200)
  }

  writexl::write_xlsx(
    list(principais = freq_principais,
         todos = freq %>% mutate(horario = rotulo_hora(hora_min))),
    file.path(dir_output, paste0("horarios_em_series_", turno_sel, ".xlsx"))
  )

  invisible(freq)
}

horarios_turno("manha", "morning",
               "Release time, % of classes starting before 12:00, state schools in Minas Gerais")
horarios_turno("tarde", "afternoon",
               "Release time, % of classes starting between 12:00 and 14:00, state schools in Minas Gerais")
