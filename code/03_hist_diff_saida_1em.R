library(basedosdados)
library(tidyverse)

set_billing_id("humancapitalindex")

# 1a, 2a e 3a series do Ensino Medio (regular / integrado / normal-magisterio)
censo <- read_sql("
  SELECT
    ano, id_municipio, id_escola, id_turma,
    rede, etapa_ensino, numero_duracao_turma, hora_inicial, minuto_inicial
  FROM `basedosdados.br_inep_censo_escolar.turma`
  WHERE ano BETWEEN 2013 AND 2015
    AND sigla_uf = 'MG'
    AND etapa_ensino IN ('25', '30', '35', '26', '31', '36', '27', '32', '37')
    AND rede = 'estadual'
")

# =====================================================================
# Objetivo: para cada escola, a diferenca entre o horario medio de SAIDA
# das turmas de 1a serie do EM e o horario medio de saida das turmas de
# 2a e 3a series, no turno da manha. Um histograma por ano (2013, 2014,
# 2015). Escolas da amostra (01_amostra.R), sem as 19 escolas que ja
# tinham atrasado a saida do 1o ano antes de 2014.
# =====================================================================

# pasta output: funciona rodando de code/ ou da raiz do projeto
dir_output <- if (dir.exists("../output")) "../output" else "output"

amostra_id_escola <- readRDS(file.path(dir_output, "amostra_id_escola.rds"))
antecipadas <- c(readRDS(file.path(dir_output, "id_mudou_50_2011_2012.rds")),
                 readRDS(file.path(dir_output, "id_mudou_50_2012_2013.rds")))

turmas <- censo %>%
  filter(id_escola %in% amostra_id_escola, !id_escola %in% antecipadas) %>%
  mutate(across(c(ano, numero_duracao_turma, hora_inicial, minuto_inicial), as.integer)) %>%
  mutate(
    serie = case_when(
      etapa_ensino %in% c("25", "30", "35") ~ "1a",
      etapa_ensino %in% c("26", "31", "36") ~ "2a",
      etapa_ensino %in% c("27", "32", "37") ~ "3a"
    ),
    entrada_min = hora_inicial * 60L + minuto_inicial,
    saida_min   = entrada_min + numero_duracao_turma
  ) %>%
  # turno da manha = entrada antes das 12:00 (mesma regra de 08_horarios_1em_2em.R)
  filter(!is.na(saida_min), hora_inicial < 12)

# horario medio de saida por escola x ano: 1a serie vs 2a e 3a series
diferencas <- turmas %>%
  mutate(grupo = if_else(serie == "1a", "primeiro", "segundo_terceiro")) %>%
  group_by(id_escola, ano, grupo) %>%
  summarise(saida_media = mean(saida_min), turmas = n(), .groups = "drop") %>%
  pivot_wider(names_from = grupo, values_from = c(saida_media, turmas)) %>%
  # so escolas com turma de 1a e de 2a/3a series de manha no ano
  filter(!is.na(saida_media_primeiro), !is.na(saida_media_segundo_terceiro)) %>%
  mutate(diferenca_min = saida_media_primeiro - saida_media_segundo_terceiro)

# resumo por ano
diferencas %>%
  group_by(ano) %>%
  summarise(escolas    = n(),
            media      = round(mean(diferenca_min), 1),
            mediana    = median(diferenca_min),
            zero       = sum(abs(diferenca_min) < 1),
            ate_15_min = sum(abs(diferenca_min) <= 15),
            mais_40    = sum(diferenca_min >= 40),
            .groups = "drop") %>%
  print()

# -------------------------------------------------------------------
# histogramas (um painel por ano)
# -------------------------------------------------------------------
n_escolas <- diferencas %>% count(ano)
rotulos_ano <- setNames(paste0(n_escolas$ano, " (", n_escolas$n, " schools)"),
                        n_escolas$ano)

g_hist <- ggplot(diferencas, aes(diferenca_min)) +
  geom_histogram(binwidth = 5, boundary = -2.5, fill = "#2a78d6",
                 colour = "#fcfcfb", linewidth = 0.4) +
  geom_vline(xintercept = 0, colour = "#52514e", linewidth = 0.4, linetype = "dashed") +
  facet_wrap(~ano, ncol = 1, labeller = as_labeller(rotulos_ano)) +
  scale_x_continuous(breaks = seq(-30, 80, 10)) +
  # eixo cortado em -30 a 80 min: 3 escolas em 2013 ficam de fora do
  # grafico (diferencas de -120, -170 e -225 min), mas continuam na base
  coord_cartesian(xlim = c(-30, 80)) +
  labs(
    title    = "Release time of 1st-year classes minus release time of 2nd- and 3rd-year classes",
    subtitle = paste0("Morning shift, difference in the school's mean release time (minutes). ",
                      "Positive: 1st year leaves later."),
    x = "Minutes", y = "Number of schools"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background    = element_rect(fill = "#fcfcfb", colour = NA),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#e1e0d9", linewidth = 0.3),
    axis.text          = element_text(colour = "#52514e"),
    axis.title         = element_text(colour = "#52514e"),
    strip.text         = element_text(colour = "#0b0b0b", face = "bold", hjust = 0),
    plot.title         = element_text(colour = "#0b0b0b", face = "bold"),
    plot.subtitle      = element_text(colour = "#52514e"),
    plot.title.position = "plot"
  )

ggsave(file.path(dir_output, "hist_diff_saida_1em_manha.png"), g_hist,
       width = 10, height = 8, dpi = 200)

writexl::write_xlsx(diferencas, file.path(dir_output, "hist_diff_saida_1em_manha.xlsx"))
