library(tidyverse)

# pasta output: funciona rodando de code/ ou da raiz do projeto
dir_output <- if (dir.exists("../output")) "../output" else "output"
dir.create(dir_output, showWarnings = FALSE)

# =====================================================================
# Objetivo: quantas escolas mudaram o horario principal da 1a serie do EM
# de um ano para o seguinte (2011->2012, 2012->2013, 2013->2014) e qual
# foi a mudanca. Usa os paineis salvos em 09_painel_horarios_1em.R:
#   - saida da 1a serie, turno da manha
#   - entrada da 1a serie, turno da tarde
# =====================================================================
saida_manha   <- readRDS(file.path(dir_output, "painel_saida_1em_manha.rds"))
names(saida_manha)

table(saida_manha$mais_de_um_horario)

entrada_tarde <- readRDS(file.path(dir_output, "painel_entrada_1em_tarde.rds"))
table(entrada_tarde$mais_de_um_horario)

# "HH:MM" -> minutos depois da meia-noite (para calcular a diferenca)
para_min <- function(h) as.integer(substr(h, 1, 2)) * 60L + as.integer(substr(h, 4, 5))

# pares de anos consecutivos, so escolas com turma nos dois anos
transicoes <- function(painel) {
  painel %>%
    select(id_municipio, id_escola, ano, horario_principal, mais_de_um_horario) %>%
    arrange(id_escola, ano) %>%
    group_by(id_escola) %>%
    mutate(
      ano_anterior      = lag(ano),
      horario_anterior  = lag(horario_principal),
      mais_de_um_anterior = lag(mais_de_um_horario)
    ) %>%
    ungroup() %>%
    filter(ano_anterior == ano - 1,
           !is.na(horario_principal), !is.na(horario_anterior)) %>%
    mutate(
      periodo   = paste0(ano_anterior, "->", ano),
      mudou     = horario_principal != horario_anterior,
      diferenca_min = para_min(horario_principal) - para_min(horario_anterior),
      mudanca   = paste(horario_anterior, "->", horario_principal)
    )
}

# resumo: quantas escolas mudaram em cada periodo e quais foram as mudancas
resumo_mudancas <- function(painel, nome) {
  tr <- transicoes(painel)

  por_periodo <- tr %>%
    group_by(periodo) %>%
    summarise(
      escolas_nos_dois_anos = n(),
      mudaram               = sum(mudou),
      pct_mudaram           = round(100 * mean(mudou), 1),
      mudaram_mais_cedo     = sum(diferenca_min < 0),
      mudaram_mais_tarde    = sum(diferenca_min > 0),
      .groups = "drop"
    )

  quais_mudancas <- tr %>%
    filter(mudou) %>%
    count(periodo, mudanca, diferenca_min, name = "escolas") %>%
    arrange(periodo, desc(escolas))

  cat("\n=====", nome, "=====\n")
  print(por_periodo)
  print(quais_mudancas, n = Inf)

  list(por_periodo = por_periodo,
       quais_mudancas = quais_mudancas,
       escolas = tr %>% filter(mudou) %>%
         select(periodo, id_municipio, id_escola, horario_anterior,
                horario_principal, diferenca_min,
                mais_de_um_anterior, mais_de_um_horario))
}

manha <- resumo_mudancas(saida_manha,   "saida da 1a serie EM, manha")
tarde <- resumo_mudancas(entrada_tarde, "entrada da 1a serie EM, tarde")

# -------------------------------------------------------------------
# escolas que atrasaram a saida da manha em 50 min ou mais antes de 2014
# (possivel adocao antecipada da mudanca de 2014). A tarde nao tem
# nenhuma mudanca de horario em 2011->2012 nem em 2012->2013.
# -------------------------------------------------------------------
tr_manha <- transicoes(saida_manha)

id_mudou_50_2011_2012 <- tr_manha %>%
  filter(periodo == "2011->2012", diferenca_min >= 50) %>%
  pull(id_escola)

id_mudou_50_2012_2013 <- tr_manha %>%
  filter(periodo == "2012->2013", diferenca_min >= 50) %>%
  pull(id_escola)

saveRDS(id_mudou_50_2011_2012, file.path(dir_output, "id_mudou_50_2011_2012.rds"))
saveRDS(id_mudou_50_2012_2013, file.path(dir_output, "id_mudou_50_2012_2013.rds"))

writexl::write_xlsx(
  list(manha_resumo  = manha$por_periodo,
       manha_mudancas = manha$quais_mudancas,
       manha_escolas = manha$escolas,
       tarde_resumo  = tarde$por_periodo,
       tarde_mudancas = tarde$quais_mudancas,
       tarde_escolas = tarde$escolas),
  file.path(dir_output, "mudancas_horario_1em.xlsx")
)

# -------------------------------------------------------------------
# grafico de tratados: % das escolas (com turma de 1a serie de manha nos
# dois anos) que atrasaram a saida em 50 min ou mais em relacao ao ano
# anterior. Numero absoluto de escolas dentro da barra.
# -------------------------------------------------------------------
tratados <- tr_manha %>%
  group_by(periodo) %>%
  summarise(escolas   = n(),
            tratados  = sum(diferenca_min >= 50),
            .groups = "drop") %>%
  mutate(pct     = 100 * tratados / escolas,
         # rotulo = ano da mudanca (em relacao ao ano anterior)
         periodo = str_sub(periodo, -4),
         # barras baixas nao cabem o numero: rotulo fica logo acima da barra
         dentro  = pct >= 8)

print(tratados)

g_tratados <- ggplot(tratados, aes(periodo, pct)) +
  geom_col(width = 0.6, fill = "#2a78d6") +
  geom_text(data = ~ filter(.x, dentro),
            aes(label = tratados), vjust = 1.6, colour = "#ffffff",
            size = 4.5, fontface = "bold") +
  geom_text(data = ~ filter(.x, !dentro),
            aes(label = tratados), vjust = -0.6, colour = "#0b0b0b",
            size = 4.5, fontface = "bold") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 100), expand = expansion(mult = c(0, 0.02))) +
  labs(
    title    = "Schools that delayed the release of 1st-year high school classes by 50 minutes or more",
    subtitle = paste0("Change from the previous year. Morning shift, state schools in the sample. Bar height: % of\n",
                      "schools with 1st-year morning classes in both years. Number on the bar: schools that delayed release."),
    x = NULL, y = "% of schools"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background    = element_rect(fill = "#fcfcfb", colour = NA),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#e1e0d9", linewidth = 0.3),
    axis.text          = element_text(colour = "#52514e"),
    axis.title         = element_text(colour = "#52514e"),
    plot.title         = element_text(colour = "#0b0b0b", face = "bold"),
    plot.subtitle      = element_text(colour = "#52514e"),
    plot.title.position = "plot"
  )

ggsave(file.path(dir_output, "tratados_saida_1em_manha.png"), g_tratados,
       width = 9, height = 5.5, dpi = 200)
