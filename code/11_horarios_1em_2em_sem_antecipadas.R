library(basedosdados)
library(tidyverse)

set_billing_id("humancapitalindex")

# 1a e 2a serie do Ensino Medio (regular / integrado / normal-magisterio)
censo <- read_sql("
  SELECT
    ano, sigla_uf, id_municipio, id_escola, id_turma,
    rede, etapa_ensino, numero_duracao_turma, hora_inicial, minuto_inicial,quantidade_matriculas
  FROM `basedosdados.br_inep_censo_escolar.turma`
  WHERE ano BETWEEN 2007 AND 2017
    AND sigla_uf = 'MG'
    AND etapa_ensino IN ('25', '30', '35', '26', '31', '36')
    AND rede = 'estadual'
")

# =====================================================================
# Versao de 08_horarios_1em_2em.R sem as escolas que ja tinham atrasado a saida
# do 1o ano em 50 min ou mais antes de 2014 (8 em 2011->2012 e 11 em
# 2012->2013; vetores salvos por 10_mudancas_horario_1em.R). Restrito as
# escolas da amostra (01_amostra.R), onde essas escolas foram encontradas.
# =====================================================================

# pasta output: funciona rodando de code/ ou da raiz do projeto
dir_output <- if (dir.exists("../output")) "../output" else "output"

# so as escolas da amostra salva em 01_amostra.R (amostra_id_escola.rds)
amostra_id_escola <- readRDS(file.path(dir_output, "amostra_id_escola.rds"))

# escolas tratadas antes de 2014
antecipadas <- c(readRDS(file.path(dir_output, "id_mudou_50_2011_2012.rds")),
                 readRDS(file.path(dir_output, "id_mudou_50_2012_2013.rds")))

censo <- censo %>%
  filter(id_escola %in% amostra_id_escola, !id_escola %in% antecipadas)

cat("escolas da amostra:", length(amostra_id_escola),
    "| retiradas (tratadas antes de 2014):", length(unique(antecipadas)),
    "| escolas no grafico:", n_distinct(censo$id_escola), "\n")


# =====================================================================
# Objetivo: como os horarios de SAIDA mais frequentes das turmas de
# 1a e de 2a serie do Ensino Medio mudam de um ano para o outro, no turno
# da manha e no turno da tarde. Um grafico por turno; em cada grafico,
# um painel por serie, lado a lado.
# =====================================================================
turmas_em <- censo %>%
  mutate(across(c(ano, numero_duracao_turma, hora_inicial, minuto_inicial), as.integer)) %>%
  mutate(
    serie = case_when(
      etapa_ensino %in% c("25", "30", "35") ~ "1st year of high school",
      etapa_ensino %in% c("26", "31", "36") ~ "2nd year of high school"
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
    panel.spacing.x    = unit(1.5, "lines"),
    axis.text          = element_text(colour = "#52514e"),
    axis.title         = element_text(colour = "#52514e"),
    strip.text         = element_text(colour = "#0b0b0b", face = "bold", hjust = 0),
    plot.title         = element_text(colour = "#0b0b0b", face = "bold"),
    plot.subtitle      = element_text(colour = "#52514e"),
    plot.title.position = "plot",
    legend.position    = "right"
  )

# faz as tabelas e os dois graficos de um turno
#   turno_sel : "manha" ou "tarde"
#   turno_en  : nome do turno nos titulos (em ingles)
#   subtitulo : subtitulo dos graficos (em ingles)
#   hora_var  : "saida_min" (horario de saida) ou "entrada_min" (horario de entrada)
horarios_turno <- function(turno_sel, turno_en, subtitulo, hora_var = "saida_min") {

  entrada <- hora_var == "entrada_min"
  hora_en <- if (entrada) "Start time" else "Release time"
  arquivo <- file.path(dir_output,
                       paste0("horarios_em2_sem_antecipadas_", turno_sel,
                              if (entrada) "_entrada" else ""))

  turmas_turno <- turmas_em %>%
    filter(turno == turno_sel) %>%
    mutate(hora_min = .data[[hora_var]])

  # % das turmas do turno em cada horario (de saida ou de entrada), por serie e ano
  freq <- turmas_turno %>%
    count(serie, ano, hora_min, name = "turmas") %>%
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
  # Graficos (textos em ingles)
  # -------------------------------------------------------------------
  rotulos <- rotulo_hora(principais)
  fixos   <- cores_em[intersect(rotulos, names(cores_em))]
  novos   <- setdiff(rotulos, names(cores_em))
  cores_horario <- c(fixos, setNames(setdiff(paleta, fixos)[seq_along(novos)], novos))[rotulos]
  cores_horario <- c(cores_horario, Other = "#898781")

  # 1) linhas: % das turmas em cada horario principal, por ano, uma serie ao lado da outra
  g_linhas <- ggplot(freq_principais, aes(ano, pct, colour = horario, group = horario)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.2) +
    facet_wrap(~serie, nrow = 1) +
    scale_colour_manual(values = cores_horario, name = hora_en) +
    scale_x_continuous(breaks = sort(unique(freq_principais$ano))) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    labs(
      title    = paste0("Most frequent ", tolower(hora_en), "s of high school classes, ", turno_en, " shift"),
      subtitle = subtitulo,
      x = NULL, y = "% of classes"
    ) +
    tema

  ggsave(paste0(arquivo, "_linhas.png"), g_linhas,
         width = 14, height = 5.5, dpi = 200)

  # 2) mapa de calor: todos os horarios com pelo menos 2% das turmas em alguma serie-ano
  horarios_mapa <- freq %>%
    group_by(hora_min) %>%
    filter(max(pct) >= 2) %>%
    ungroup() %>%
    complete(serie, ano, hora_min, fill = list(turmas = 0, pct = 0)) %>%
    mutate(horario = fct_reorder(rotulo_hora(hora_min), hora_min))

  g_mapa <- ggplot(horarios_mapa, aes(factor(ano), horario, fill = pct)) +
    geom_tile(colour = "#fcfcfb", linewidth = 1) +
    # rotulo so nas celulas com 5% ou mais
    geom_text(
      data = ~ filter(.x, pct >= 5),
      aes(label = round(pct), colour = pct >= 35),
      size = 3.3, show.legend = FALSE
    ) +
    facet_wrap(~serie, nrow = 1) +
    scale_colour_manual(values = c(`FALSE` = "#0b0b0b", `TRUE` = "#ffffff")) +
    scale_fill_gradient(low = "#eef4fc", high = "#0d366b", name = "% of classes",
                        limits = c(0, NA)) +
    labs(
      title    = paste0(hora_en, " of high school classes, ", turno_en, " shift, by year"),
      subtitle = subtitulo,
      x = NULL, y = hora_en
    ) +
    tema +
    theme(panel.grid.major.y = element_blank())

  ggsave(paste0(arquivo, "_mapa.png"), g_mapa,
         width = 14, height = 6, dpi = 200)

  writexl::write_xlsx(
    list(principais = freq_principais,
         todos = freq %>% mutate(horario = rotulo_hora(hora_min))),
    paste0(arquivo, ".xlsx")
  )

  invisible(freq)
}

# manha: horario de SAIDA
horarios_turno("manha", "morning",
               paste0("% of classes starting before 12:00. Sample state schools (RMBH), ",
                      "excluding the 19 schools that delayed 1st-year release before 2014"))

# tarde: horario de ENTRADA
horarios_turno("tarde", "afternoon",
               paste0("% of classes starting between 12:00 and 14:00. Sample state schools (RMBH), ",
                      "excluding the 19 schools that delayed 1st-year release before 2014"),
               hora_var = "entrada_min")
