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
  WHERE ano = 2015
    AND sigla_uf = 'MG'
    AND etapa_ensino IN ('11', '41', '25', '30', '35')   -- 11 = 8a serie (EF 8 anos); 41 = 9o ano (EF 9 anos)
    AND rede != 'privada'               -- rede e' texto: federal/estadual/municipal/privada
")

names(censo)

# --- Regiao Metropolitana de BH: pega os codigos id_municipio (censo so tem o
# codigo, nao o nome) e filtra DEPOIS de extrair o censo, como pedido ---
municipios_rmbh <- read_sql("
  SELECT *
  FROM `basedosdados.br_bd_diretorios_brasil.municipio`
  WHERE sigla_uf = 'MG'
")
municipios_rmbh <-municipios_rmbh%>%
filter(nome%in%c(
  'Baldim','Belo Horizonte','Betim','Brumadinho','Caeté','Capim Branco',
  'Confins','Contagem','Esmeraldas','Florestal','Ibirité','Igarapé',
  'Itaguara','Itatiaiuçu','Jaboticatubas','Juatuba','Lagoa Santa',
  'Mário Campos','Mateus Leme','Matozinhos','Nova Lima','Nova União',
  'Pedro Leopoldo','Raposos','Ribeirão das Neves','Rio Acima','Rio Manso',
  'Sabará','Santa Luzia','São Joaquim de Bicas','São José da Lapa',
  'Sarzedo','Taquaraçu de Minas','Vespasiano'
))

nrow(municipios_rmbh)
censo2 <- censo %>% filter(id_municipio %in% municipios_rmbh$id_municipio)

censo2 <- censo2 %>%
  mutate(grade = case_when(
    etapa_ensino %in% c("11", "41")       ~ 9,         # 8a serie (EF 8 anos) / 9o ano (EF 9 anos)
    etapa_ensino %in% c("25", "30", "35") ~ 10,
    TRUE                                  ~ NA
  ))

# escolas que tem pelo menos 1 turma de 9o ano
escolas_com_9ano <- censo2 %>%
  filter(grade == 9) %>%
  distinct(ano, id_escola)

censo2<-censo2%>%
  filter(id_escola%in%c(escolas_com_9ano$id_escola))

escolas <- censo2 %>%
  filter(grade == 9) %>%
  group_by(ano, id_escola) %>%
  summarise(
    # ATENCAO: quantidade_matriculas nao esta no SELECT da query de `censo`
    # (que so traz ano, sigla_uf, id_municipio, id_escola, id_turma, rede,
    # etapa_ensino, numero_duracao_turma, hora_inicial, minuto_inicial).
    # Adicione a coluna na query (ou junte com a tabela de matricula) antes de rodar.
    quantidade_matriculas = sum(quantidade_matriculas),
    rede       = first(rede),   # 1 rede por escola-ano; first() em vez de coluna crua
    n_turmas_9 = n(),
    .groups = "drop"
  )

# indicador: a escola tem turma de "10 ano" (grade==10, ou seja, Ensino Medio)?
# usa censo2 de ANTES do filter(grade==9) do `escolas`, que ainda tem as duas grades
indicador_10ano <- censo2 %>%
  group_by(ano, id_escola) %>%
  summarise(tem_10_ano = any(grade == 10), .groups = "drop")

escolas <- escolas %>%
  left_join(indicador_10ano, by = c("ano", "id_escola"))

k <- escolas %>%
  group_by(ano, rede) %>%
  summarise(matriculas = sum(quantidade_matriculas), .groups = "drop") %>%
  mutate(pct_matriculas = round(100 * matriculas / sum(matriculas), 1))


print(k)

# pct de matriculas: estadual com 10 ano (EM) vs estadual sem 10 ano
k2 <- escolas %>%
  group_by(ano, rede, tem_10_ano) %>%
  summarise(matriculas = sum(quantidade_matriculas), .groups = "drop") %>%
  mutate(pct_matriculas = round(100 * matriculas / sum(matriculas), 1))

k2
print(k2)
print(k2 %>% filter(rede == "estadual"))

names(escolas)
escolas<-escolas%>%
filter(tem_10_ano==1)%>%
filter(rede=="estadual")

nrow(escolas)
sum(escolas$n_turmas_9)
sum(escolas$quantidade_matriculas)
names(censo2)

# vetor de id_escola da amostra final (9o ano, rede estadual, com turma de EM)
amostra_id_escola <- escolas$id_escola
saveRDS(amostra_id_escola, file.path(dir_output, "amostra_id_escola.rds"))
length(amostra_id_escola)
class<-censo2%>%
filter(id_escola%in%escolas$id_escola)

names(class)

