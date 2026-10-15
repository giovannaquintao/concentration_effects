library(tidyverse)

# pasta output: funciona rodando de code/ ou da raiz do projeto
dir_output <- if (dir.exists("../output")) "../output" else "output"

# =====================================================================
# Objetivo: linha do tempo do calendario escolar provavel de 2014 na rede
# estadual de MG (Resolucao SEE n 2.368, de 09/08/2013; imagens em
# reinventando_em/calendario_2014_1.png e calendario_2014_2.png).
# Um quadrado por dia, uma linha por mes, colorido pelo tipo de dia.
# =====================================================================
inicio_letivo <- as.Date("2014-02-03")
fim_letivo    <- as.Date("2014-12-19")

# datas da resolucao
d <- function(...) as.Date(c(...))
recesso <- d("2014-03-03", "2014-03-05", "2014-04-17", "2014-05-02",
             "2014-06-12", "2014-06-13", paste0("2014-06-", 16:18), "2014-06-20",
             paste0("2014-06-", 23:27), "2014-06-30",
             paste0("2014-07-0", 1:4), paste0("2014-07-", sprintf("%02d", 7:11)),
             "2014-12-24", "2014-12-26", paste0("2014-12-", 29:31))
feriado <- d("2014-01-01", "2014-03-04", "2014-04-18", "2014-04-21", "2014-05-01",
             "2014-06-19", "2014-09-07", "2014-10-12", "2014-11-02", "2014-11-15",
             "2014-12-25")
# Seminario de Percurso Curricular (escolas do REM) e Dia D
dia_especial <- d("2014-02-18", "2014-02-19", "2014-02-20", "2014-06-04", "2014-06-07")
# dias escolares de avaliacao e encerramento (sem aula)
dia_escolar <- d("2014-12-22", "2014-12-23")

niveis <- c("School day", "Recess", "Holiday", "Special school day",
            "Staff day (no classes)", "Weekend", "Outside the school year")

dias <- tibble(data = seq(as.Date("2014-01-01"), as.Date("2014-12-31"), by = "day")) %>%
  mutate(
    fim_de_semana = wday(data, week_start = 1) >= 6,
    tipo = case_when(
      data %in% feriado                                   ~ "Holiday",
      data %in% dia_especial                              ~ "Special school day",
      data %in% dia_escolar                               ~ "Staff day (no classes)",
      data %in% recesso                                   ~ "Recess",
      fim_de_semana                                       ~ "Weekend",
      data < inicio_letivo | data > fim_letivo            ~ "Outside the school year",
      TRUE                                                ~ "School day"
    ),
    tipo = factor(tipo, levels = niveis),
    mes  = factor(month(data, label = TRUE, abbr = TRUE, locale = "C"),
                  levels = rev(month.abb)),
    dia  = mday(data)
  )

# dias letivos: dias de aula mais os dias especiais, de segunda a sexta
# (o Dia D de 7 de junho cai num sabado)
n_letivos <- sum(dias$tipo %in% c("School day", "Special school day") & !dias$fim_de_semana)
cat("dias letivos de segunda a sexta (aula + dias especiais):", n_letivos, "\n")
print(count(dias, tipo))

# anotacoes por mes (coluna a direita)
notas <- tribble(
  ~mes,  ~texto,
  "Jan", "No classes",
  "Feb", "3: classes start  |  18-20: REM curriculum seminar",
  "Mar", "3-5: Carnival",
  "Apr", "17: recess  |  18: Good Friday  |  21: Tiradentes",
  "May", "1: Labour Day  |  2: recess",
  "Jun", "4, 7: Dia D  |  from 12: World Cup recess  |  19: Corpus Christi",
  "Jul", "1-11: World Cup recess  |  14: classes resume",
  "Sep", "7: Independence Day",
  "Oct", "12: holiday",
  "Nov", "2, 15: holidays",
  "Dec", "19: classes end  |  22-23: staff days  |  24-31: recess"
) %>%
  mutate(mes = factor(mes, levels = rev(month.abb)))

# cores: paleta de referencia (dataviz), primeiras cinco na ordem fixa;
# fim de semana e fora do ano letivo em cinza neutro
cores <- c("School day"              = "#2a78d6",
           "Recess"                  = "#eb6834",
           "Holiday"                 = "#1baf7a",
           "Special school day"      = "#eda100",
           "Staff day (no classes)"  = "#e87ba4",
           "Weekend"                 = "#d5d4cd",
           "Outside the school year" = "#efeee9")

g <- ggplot(dias, aes(dia, mes)) +
  geom_tile(aes(fill = tipo), width = 0.88, height = 0.8) +
  geom_text(data = notas, aes(x = 32.5, y = mes, label = texto),
            hjust = 0, size = 3, colour = "#52514e") +
  scale_fill_manual(values = cores, name = NULL, drop = FALSE) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25, 31), limits = c(0.5, 62),
                     expand = expansion(0), position = "top") +
  labs(
    title    = "State school calendar, Minas Gerais, 2014",
    subtitle = paste0("Resolution SEE 2,368/2013. One square per day. ",
                      n_letivos, " school days on weekdays, plus Dia D on Saturday 7 June."),
    x = "Day of the month", y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background     = element_rect(fill = "#fcfcfb", colour = NA),
    panel.grid          = element_blank(),
    axis.text           = element_text(colour = "#52514e"),
    axis.title.x        = element_text(colour = "#52514e", hjust = 0.25),
    plot.title          = element_text(colour = "#0b0b0b", face = "bold"),
    plot.subtitle       = element_text(colour = "#52514e"),
    plot.title.position = "plot",
    legend.position     = "bottom",
    legend.text         = element_text(colour = "#52514e")
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

ggsave(file.path(dir_output, "calendario_2014.png"), g, width = 11, height = 5.5, dpi = 200)
