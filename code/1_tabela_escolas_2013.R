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

# aumento minimo (em minutos) da duracao e da saida da 1a serie de 2013 para 2014
limiar_min <- 40

# diferenca maxima (em minutos) para dizer que a 1a serie sai junto com a 2a e a 3a
janela_min <- 5

# =====================================================================
# Objetivo: tabelas descritivas das escolas estaduais de MG com Ensino
# Medio em 2013.
# Tabela 1: da lista de escolas a amostra, com exclusoes em sequencia:
#   1. todas as escolas estaduais com turma de 1a, 2a ou 3a serie do EM
#   2. menos as escolas do piloto do Reinventando
#   3. menos as escolas com turma de EM a tarde
#   4. menos as escolas com mais de um horario de saida dentro da mesma
#      serie, no turno da manha
#   amostra = 1 - 2 - 3 - 4 (pode ter turmas no noturno)
# Tabela de turnos: escolas fora do piloto pelos turnos das turmas de EM.
# Tabela de saida junto: em 2013, 2014 e 2015, quantas escolas da amostra
# soltam a 1a serie junto (ate 5 min) com a 2a e a 3a series.
# Tabela de tratadas e controles: criterios (i)-(iii) na amostra.
# As tabelas vao para output/ em .xlsx e em .tex (main.tex le o .tex).
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
    # mesmos turnos de 2_grafico_linha_horario.R: manha = entrada antes das 12:00;
    # tarde = entrada entre 12:00 e 14:00
    turno = case_when(
      hora_inicial < 12      ~ "manha",
      entrada_min <= 14 * 60 ~ "tarde",
      TRUE                   ~ "outro"
    )
  )

turmas_2013 <- turmas %>% filter(ano == 2013)

# -------------------------------------------------------------------
# amostra: exclusoes em sequencia (cada passo so entre as que sobraram)
# -------------------------------------------------------------------
escolas_mg <- unique(turmas_2013$id_escola)

escolas_piloto     <- intersect(escolas_mg, id_escolas_reinventando)
escolas_sem_piloto <- setdiff(escolas_mg, escolas_piloto)

escolas_tarde <- turmas_2013 %>%
  filter(id_escola %in% escolas_sem_piloto, turno == "tarde") %>%
  distinct(id_escola) %>%
  pull(id_escola)
escolas_sem_tarde <- setdiff(escolas_sem_piloto, escolas_tarde)

escolas_saida_diferente <- turmas_2013 %>%
  filter(id_escola %in% escolas_sem_tarde, turno == "manha", !is.na(saida_min)) %>%
  group_by(id_escola, serie) %>%
  summarise(n_saidas = n_distinct(saida_min), .groups = "drop") %>%
  filter(n_saidas > 1) %>%
  distinct(id_escola) %>%
  pull(id_escola)

escolas_amostra <- setdiff(escolas_sem_tarde, escolas_saida_diferente)
n_amostra       <- length(escolas_amostra)

pct_todas   <- function(n) round(100 * n / length(escolas_mg), 1)
pct_amostra <- function(n) round(100 * n / n_amostra, 1)

# -------------------------------------------------------------------
# tabela 1: da lista de escolas a amostra (% em relacao a todas)
# -------------------------------------------------------------------
tabela <- tibble(
  `State schools with high school, 2013` = c(
    "1. All schools",
    "2. Pilot schools",
    "3. Schools with afternoon EM classes",
    "4. Schools with different release times within grade",
    "Sample (1 - 2 - 3 - 4)"
  ),
  N   = c(length(escolas_mg), length(escolas_piloto), length(escolas_tarde),
          length(escolas_saida_diferente), n_amostra),
  `%` = pct_todas(N)
)

print(tabela, width = Inf)

# -------------------------------------------------------------------
# tabela de turnos: escolas fora do piloto pelos turnos das turmas de EM, 2013
#   so manha: tem turma de manha e nenhuma de tarde (pode ter noturno)
#   so tarde: tem turma de tarde e nenhuma de manha (pode ter noturno)
#   manha e tarde: tem turma nos dois turnos
#   sem manha e sem tarde: so noturno (entrada depois das 14:00) ou sem horario
# -------------------------------------------------------------------
turnos_2013 <- turmas_2013 %>%
  filter(id_escola %in% escolas_sem_piloto) %>%
  group_by(id_escola) %>%
  summarise(manha = any(turno == "manha"),
            tarde = any(turno == "tarde"),
            noite = any(turno == "outro" & !is.na(entrada_min)),
            .groups = "drop") %>%
  mutate(grupo = case_when(
    manha & !tarde ~ "Only morning",
    tarde & !manha ~ "Only afternoon",
    manha & tarde  ~ "Morning and afternoon",
    TRUE           ~ "Neither (night only or no time)"
  ))

ordem_grupos <- c("Only morning", "Only afternoon", "Morning and afternoon",
                  "Neither (night only or no time)")

tabela_turnos <- turnos_2013 %>%
  group_by(grupo) %>%
  summarise(N = n(), noite = sum(noite), .groups = "drop") %>%
  arrange(match(grupo, ordem_grupos)) %>%
  bind_rows(tibble(grupo = "Non-pilot schools", N = length(escolas_sem_piloto),
                   noite = sum(turnos_2013$noite)), .) %>%
  transmute(`Non-pilot schools by EM shift, 2013` = grupo,
            N,
            `%` = round(100 * N / length(escolas_sem_piloto), 1),
            `Also night EM` = noite)

print(tabela_turnos, width = Inf)

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

# -------------------------------------------------------------------
# tabela de tratadas e controles
# Amostra da tabela 1. Os criterios usam so as turmas da manha. Base:
# escolas com turma de 1a, 2a e 3a series de manha em 2013 e em 2014.
# Horarios = media das turmas da serie na escola.
#   (i)   2013: a 1a serie sai ate 5 min da 2a e ate 5 min da 3a
#   (ii)  2014: a 1a serie sai 40+ min depois da 2a e da 3a, e a duracao
#         da 1a serie subiu 40+ min de 2013 para 2014
#   (iii) 2a e 3a series: mesma entrada e mesma duracao em 2013 e 2014
#   tratada:  (i), (ii) e (iii)
#   controle: (i) e (iii), e em 2014 a 1a serie ainda sai ate 5 min da 2a e da 3a
# % em relacao a amostra
# -------------------------------------------------------------------
# entrada, duracao e saida medias por escola x ano x serie, so turmas da manha
por_serie <- turmas %>%
  filter(ano %in% c(2013, 2014), id_escola %in% escolas_amostra, turno == "manha") %>%
  group_by(id_escola, ano, serie) %>%
  summarise(entrada = mean(entrada_min), duracao = mean(numero_duracao_turma),
            saida = mean(saida_min), .groups = "drop")

# base: escolas com as tres series de manha nos dois anos (3 series x 2 anos = 6 linhas)
escolas_base <- por_serie %>% count(id_escola) %>% filter(n == 6) %>% pull(id_escola)

criterios <- por_serie %>%
  filter(id_escola %in% escolas_base) %>%
  pivot_wider(names_from = c(serie, ano), values_from = c(entrada, duracao, saida)) %>%
  mutate(
    c1 = abs(saida_1a_2013 - saida_2a_2013) <= janela_min &
         abs(saida_1a_2013 - saida_3a_2013) <= janela_min,
    c2 = pmin(saida_1a_2014 - saida_2a_2014, saida_1a_2014 - saida_3a_2014) >= limiar_min &
         duracao_1a_2014 - duracao_1a_2013 >= limiar_min,
    c3 = abs(entrada_2a_2014 - entrada_2a_2013) < 1 & abs(duracao_2a_2014 - duracao_2a_2013) < 1 &
         abs(entrada_3a_2014 - entrada_3a_2013) < 1 & abs(duracao_3a_2014 - duracao_3a_2013) < 1,
    junto_2014 = abs(saida_1a_2014 - saida_2a_2014) <= janela_min &
                 abs(saida_1a_2014 - saida_3a_2014) <= janela_min,
    # duracao sem informacao (NA) conta como criterio nao cumprido
    across(c(c1, c2, c3, junto_2014), ~ coalesce(.x, FALSE)),
    tratada  = c1 & c2 & c3,
    controle = c1 & c3 & junto_2014
  )

n_trat <- c(
  base     = nrow(criterios),
  c1       = sum(criterios$c1),
  c2       = sum(criterios$c2),
  c3       = sum(criterios$c3),
  tratada  = sum(criterios$tratada),
  controle = sum(criterios$controle),
  outra    = sum(criterios$c1 & criterios$c3 & !criterios$tratada & !criterios$controle)
)

tabela_trat <- tibble(
  Schools = c(
    "Sample (Table 1)",
    "1st, 2nd and 3rd years in the morning, 2013 and 2014",
    "(i) Shared release in 2013",
    paste0("(ii) 1st year leaves ", limiar_min, "+ min later in 2014"),
    "(iii) 2nd and 3rd years unchanged",
    "Treated: (i), (ii) and (iii)",
    "Control: (i) and (iii), still shared in 2014",
    "Other: (i) and (iii), neither"
  ),
  N   = c(n_amostra, n_trat),
  `%` = c(100, pct_amostra(n_trat))
)

print(tabela_trat, width = Inf)

writexl::write_xlsx(list(escolas_2013 = tabela, saida_junto = tabela_junto,
                         tratamento = tabela_trat, criterios_escola = criterios,
                         turnos = tabela_turnos),
                    file.path(dir_output, "tabela_escolas_2013.xlsx"))

# -------------------------------------------------------------------
# .tex das tabelas (so o tabular; legenda e notas ficam no main.tex)
# -------------------------------------------------------------------
# numero -> texto (milhar com virgula; NA vira celula vazia)
fmt <- function(x, digitos = 0) {
  ifelse(is.na(x), "", formatC(x, format = "f", digits = digitos, big.mark = ","))
}

tabela_tex <- tabela %>%
  mutate(N = fmt(N), `%` = fmt(`%`, 1))

tabela_junto_tex <- tabela_junto %>%
  mutate(across(2:3, fmt), `%` = fmt(`%`, 1), Year = as.character(Year))

writeLines(knitr::kable(tabela_tex, format = "latex", booktabs = TRUE, align = "lrr",
                        linesep = ""),
           file.path(dir_output, "tabela_escolas_2013.tex"))
writeLines(knitr::kable(tabela_junto_tex, format = "latex", booktabs = TRUE, align = "lrrr",
                        linesep = ""),
           file.path(dir_output, "tabela_saida_junto.tex"))

tabela_turnos_tex <- tabela_turnos %>%
  mutate(N = fmt(N), `%` = fmt(`%`, 1), `Also night EM` = fmt(`Also night EM`))

writeLines(knitr::kable(tabela_turnos_tex, format = "latex", booktabs = TRUE, align = "lrrr",
                        linesep = ""),
           file.path(dir_output, "tabela_turnos.tex"))

tabela_trat_tex <- tabela_trat %>%
  mutate(N = fmt(N), `%` = fmt(`%`, 1))

writeLines(knitr::kable(tabela_trat_tex, format = "latex", booktabs = TRUE, align = "lrr",
                        linesep = ""),
           file.path(dir_output, "tabela_tratamento.tex"))
