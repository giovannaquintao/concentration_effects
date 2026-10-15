library(basedosdados)
library(tidyverse)

set_billing_id("humancapitalindex")

# pasta output: funciona rodando de code/ ou da raiz do projeto
dir_output <- if (dir.exists("../output")) "../output" else "output"

# so Ensino Medio (1a serie: regular / integrado / normal-magisterio)
censo <- read_sql("
  SELECT
    ano, sigla_uf, id_municipio, id_escola, id_turma,
    rede, etapa_ensino, numero_duracao_turma, hora_inicial, minuto_inicial,quantidade_matriculas
  FROM `basedosdados.br_inep_censo_escolar.turma`
  WHERE ano BETWEEN 2007 AND 2017
    AND sigla_uf = 'MG'
    AND etapa_ensino IN ('25', '30', '35')
")

# so as escolas da amostra salva em 01_amostra.R (amostra_id_escola.rds)
amostra_id_escola <- readRDS(file.path(dir_output, "amostra_id_escola.rds"))
censo <- censo %>% filter(id_escola %in% amostra_id_escola)

# =====================================================================
# Objetivo: como os horarios de SAIDA mais frequentes das turmas de
# Ensino Medio do turno da manha mudam de um ano para o outro.
# =====================================================================
turmas_em <- censo %>%
  mutate(across(c(ano, numero_duracao_turma, hora_inicial, minuto_inicial), as.integer)) %>%
  mutate(
    entrada_min = hora_inicial * 60L + minuto_inicial,
    saida_min   = entrada_min + numero_duracao_turma
  ) %>%
  filter(!is.na(saida_min), hora_inicial < 12)   # turno da manha (entrada antes de 12h)

# minutos depois da meia-noite -> "HH:MM"
rotulo_hora <- function(m) sprintf("%02d:%02d", m %/% 60, m %% 60)

# % das turmas de EM da manha em cada horario de saida, por ano
freq <- turmas_em %>%
  count(ano, saida_min, name = "turmas") %>%
  group_by(ano) %>%
  mutate(pct = 100 * turmas / sum(turmas)) %>%
  ungroup()

# horarios mais frequentes: os 3 mais comuns de cada ano (no maximo 8 no total)
principais <- freq %>%
  group_by(ano) %>%
  slice_max(turmas, n = 3, with_ties = FALSE) %>%
  ungroup() %>%
  distinct(saida_min) %>%
  pull(saida_min)

if (length(principais) > 8) {
  principais <- freq %>%
    filter(saida_min %in% principais) %>%
    group_by(saida_min) %>%
    summarise(pct_max = max(pct), .groups = "drop") %>%
    slice_max(pct_max, n = 8, with_ties = FALSE) %>%
    pull(saida_min)
}
principais <- sort(principais)

# tabela: ano x horario (os que nao sao "principais" viram "Outros")
freq_principais <- freq %>%
  mutate(horario = if_else(saida_min %in% principais, rotulo_hora(saida_min), "Outros")) %>%
  group_by(ano, horario) %>%
  summarise(turmas = sum(turmas), pct = sum(pct), .groups = "drop") %>%
  complete(ano, horario, fill = list(turmas = 0, pct = 0)) %>%
  mutate(horario = factor(horario, levels = c(rotulo_hora(principais), "Outros")))

freq_principais %>%
  mutate(pct = round(pct, 1)) %>%
  select(-turmas) %>%
  pivot_wider(names_from = horario, values_from = pct, names_sort = TRUE) %>%
  print(width = Inf)

turmas_em %>% count(ano, name = "turmas_em_manha") %>% print()

# ---------------------------------------------------------------------
# Graficos
# ---------------------------------------------------------------------
# uma cor fixa por horario (na ordem do horario); "Outros" em cinza
cores <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
           "#e87ba4", "#008300", "#4a3aa7", "#e34948")
cores_horario <- c(setNames(cores[seq_along(principais)], rotulo_hora(principais)),
                   Outros = "#898781")

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

subtitulo <- "% das turmas de 1ª série do EM com entrada antes de 12h, escolas da amostra (RMBH, estaduais)"

# 1) linhas: % das turmas em cada horario principal, por ano
g_linhas <- ggplot(freq_principais, aes(ano, pct, colour = horario, group = horario)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.2) +
  scale_colour_manual(values = cores_horario, name = "Horário de saída") +
  scale_x_continuous(breaks = sort(unique(freq_principais$ano))) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "Horários de saída mais frequentes do Ensino Médio, turno da manhã",
    subtitle = subtitulo,
    x = NULL, y = "% das turmas"
  ) +
  tema

ggsave(file.path(dir_output, "horarios_em_manha_linhas.png"), g_linhas, width = 10, height = 5.5, dpi = 200)

# 2) mapa de calor: todos os horarios com pelo menos 2% das turmas em algum ano
horarios_mapa <- freq %>%
  group_by(saida_min) %>%
  filter(max(pct) >= 2) %>%
  ungroup() %>%
  complete(ano, saida_min, fill = list(turmas = 0, pct = 0)) %>%
  mutate(horario = fct_reorder(rotulo_hora(saida_min), saida_min))

g_mapa <- ggplot(horarios_mapa, aes(factor(ano), horario, fill = pct)) +
  geom_tile(colour = "#fcfcfb", linewidth = 1) +
  # rotulo so nas celulas com 5% ou mais
  geom_text(
    data = ~ filter(.x, pct >= 5),
    aes(label = round(pct), colour = pct >= 35),
    size = 3.3, show.legend = FALSE
  ) +
  scale_colour_manual(values = c(`FALSE` = "#0b0b0b", `TRUE` = "#ffffff")) +
  scale_fill_gradient(low = "#eef4fc", high = "#0d366b", name = "% das turmas",
                      limits = c(0, NA)) +
  labs(
    title    = "Horário de saída do Ensino Médio, turno da manhã, por ano",
    subtitle = subtitulo,
    x = NULL, y = "Horário de saída"
  ) +
  tema +
  theme(panel.grid.major.y = element_blank())

ggsave(file.path(dir_output, "horarios_em_manha_mapa.png"), g_mapa, width = 10, height = 6, dpi = 200)

writexl::write_xlsx(
  list(principais = freq_principais,
       todos = freq %>% mutate(horario = rotulo_hora(saida_min))),
  file.path(dir_output, "horarios_em_manha.xlsx")
)
