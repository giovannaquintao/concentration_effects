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
  filter(etapa_ensino %in% c("11", "41")) %>%   # so 9o ano (EF9) / 8a serie (EF8)
  mutate(across(c(ano, numero_duracao_turma, hora_inicial, minuto_inicial), as.integer))

# =====================================================================
# Objetivo: como o horario de SAIDA (fim da aula) das turmas de 9o ano
# muda de um ano para o outro, DENTRO da mesma escola.
#
# Nao da pra seguir a mesma turma ano a ano (id_turma nao e' estavel),
# entao comparamos a "assinatura" da escola no ano: o conjunto de
# horarios de saida entre as turmas de 9o ano, e quantas turmas existem.
#   - se todas as turmas terminavam 12:40 e, no ano seguinte, uma passa
#     a terminar 12:10 -> o CONJUNTO de saidas mudou.
#   - se aparece uma turma a mais (ou a menos) -> o N. DE TURMAS mudou,
#     mesmo que os horarios de saida continuem os mesmos.
# As duas coisas contam como "mudanca", e sao independentes uma da outra.
# =====================================================================
turmas <- censo %>%
  mutate(
    entrada_min = hora_inicial * 60L + minuto_inicial,
    saida_min   = entrada_min + numero_duracao_turma
  )

# assinatura da escola em cada ano
assinatura_escola_ano <- turmas %>%
  group_by(ano, id_escola) %>%
  summarise(
    n_turmas         = n(),
    saidas_distintas = n_distinct(saida_min),
    conjunto_saida   = paste(sort(unique(saida_min)), collapse = "|"),   # ex.: "740|790"
    .groups = "drop"
  ) %>%
  arrange(id_escola, ano)

# compara cada escola-ano com o ANO ANTERIOR OBSERVADO da mesma escola
# (nao necessariamente ano-1: se faltar um ano no meio, compara com o
# ultimo ano que a escola de fato apareceu -- gap_anos mostra o salto)
mudancas <- assinatura_escola_ano %>%
  group_by(id_escola) %>%
  arrange(ano, .by_group = TRUE) %>%
  mutate(
    ano_anterior         = lag(ano),
    gap_anos             = ano - ano_anterior,
    n_turmas_ant         = lag(n_turmas),
    conjunto_saida_ant   = lag(conjunto_saida),
    mudou_n_turmas       = !is.na(ano_anterior) & n_turmas       != n_turmas_ant,
    mudou_conjunto_saida = !is.na(ano_anterior) & conjunto_saida != conjunto_saida_ant,
    houve_mudanca        = mudou_n_turmas | mudou_conjunto_saida
  ) %>%
  ungroup()

# resumo: % de escola-ano (com ano anterior observado) com cada tipo de mudanca
mudancas %>%
  filter(!is.na(ano_anterior)) %>%
  summarise(
    n_comparacoes           = n(),
    pct_mudou_n_turmas      = round(100 * mean(mudou_n_turmas), 1),
    pct_mudou_conjunto      = round(100 * mean(mudou_conjunto_saida), 1),
    pct_houve_mudanca       = round(100 * mean(houve_mudanca), 1)
  ) %>%
  print()

nrow(censo)
n_distinct(censo$id_escola)
mudancas

# View() so funciona em sessao interativa com GUI (RStudio, ou VS Code rodando
# num "R Terminal" da extensao) -- em Rscript/source() nao tem onde abrir.
# alternativas que sempre funcionam:
glimpse(mudancas)                              # resumo das colunas no console
print(head(mudancas, 20))                      # primeiras linhas no console
writexl::write_xlsx(mudancas, file.path(dir_output, "mudancas.xlsx")) # abre no Excel

# escolas que tiveram mudou_conjunto_saida == TRUE em pelo menos 1 ano
escolas_mudaram_conjunto <- mudancas %>%
  filter(mudou_conjunto_saida) %>%
  distinct(id_escola) %>%
  pull(id_escola)

length(escolas_mudaram_conjunto)

# trajetoria completa (todos os anos) so dessas escolas, pra ver o antes/depois
mudancas_conjunto <- mudancas %>%
  filter(id_escola %in% escolas_mudaram_conjunto) %>%
  arrange(id_escola, ano)

mudancas_conjunto

sample<-