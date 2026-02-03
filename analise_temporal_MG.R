# =========================================================
# Projeto: Procedimentos odontológicos no SUS
# Script: 01_importacao_dados_ficticio.R
# Objetivo: Criar e importar dados fictícios para MG
# Autora: Aline R. Mota
# Período: 2023 - 2025
# =========================================================

# -------------------------------
# 1. Pacotes
# -------------------------------
library(tidyverse)

# -------------------------------
# 2. Criar dados fictícios
# -------------------------------
set.seed(123) # garante que os números aleatórios sejam sempre os mesmos

odontologia <- tibble(
  Ano = rep(2023:2025, each = 12*5),      # 3 anos, 12 meses, 5 procedimentos
  Mes = rep(rep(1:12, each = 5), times = 3),
  Procedimento = rep(c(
    "Restauração", 
    "Profilaxia", 
    "Extração", 
    "Curetagem", 
    "Aplicação de Flúor"
  ), times = 36),  # 36 meses
  Quantidade = sample(50:300, 36*5, replace = TRUE),
  Valor = sample(1000:5000, 36*5, replace = TRUE),
  UF = ("MG")
)

# -------------------------------
# 3. Conferência inicial
# -------------------------------
glimpse(odontologia)
head(odontologia)

odontologia <- odontologia %>%
  mutate(Data = as.Date(paste(Ano, Mes, "01", sep = "-")))

# Total de procedimentos por ano
odontologia %>%
  group_by(Ano) %>%
  summarise(
    total_proc = sum(Quantidade),
    total_valor = sum(Valor)
  )

# Total de procedimentos por tipo
odontologia %>%
  group_by(Procedimento) %>%
  summarise(total = sum(Quantidade)) %>%
  arrange(desc(total))

odontologia %>%
  group_by(Data) %>%
  summarise(total_proc = sum(Quantidade)) %>%
  ggplot(aes(x = Data, y = total_proc)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red") +
  labs(
    title = "Tendência de Procedimentos Odontológicos em Minas Gerais (Fictício)",
    x = "Mês",
    y = "Quantidade de Procedimentos"
  ) +
  theme_minimal()

if (!dir.exists("C:/procedimentos_odontologicos_sus/dados/limpos")) dir.create("dados/limpos")

saveRDS(odontologia, "C:/procedimentos_odontologicos_sus/dados/limpos/odontologia_ficticio_MG_2023_2025.rds")

# =========================================================
# Projeto: Procedimentos odontológicos no SUS
# Script: 02_tratamento_dados_MG.R
# Objetivo: Agrupar, sumarizar e gerar gráficos de procedimentos odontológicos em MG
# Autora: Aline R. Mota
# Período: 2023 - 2025
# =========================================================

# -------------------------------
# 1. Pacotes
# -------------------------------
library(tidyverse)
library(ggplot2)

# -------------------------------
# 2. Carregar base limpa
# -------------------------------
odontologia <- readRDS("dados/limpos/odontologia_ficticio_MG_2023_2025.rds")

# -------------------------------
# 3. Conferência
# -------------------------------
glimpse(odontologia)

# -------------------------------
# 4. Agrupamento por ano e procedimento
# -------------------------------
resumo_ano_proc <- odontologia %>%
  group_by(Ano, Procedimento) %>%
  summarise(
    total_quantidade = sum(Quantidade),
    total_valor = sum(Valor)
  ) %>%
  arrange(Ano, desc(total_quantidade))

resumo_ano_proc

#5. Gráfico de Barras por procedimento e ano

ggplot(resumo_ano_proc, aes(x = Procedimento, y = total_quantidade, fill = factor(Ano))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Procedimentos Odontológicos em Minas Gerais por Ano",
    x = "Procedimento",
    y = "Quantidade",
    fill = "Ano"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Total mensal de procedimentos
resumo_mensal <- odontologia %>%
  group_by(Data) %>%
  summarise(total_quantidade = sum(Quantidade))

ggplot(resumo_mensal, aes(x = Data, y = total_quantidade)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red") +
  labs(
    title = "Tendência Mensal de Procedimentos Odontológicos em MG",
    x = "Mês",
    y = "Quantidade"
  ) +
  theme_minimal()

# =========================================================
# Projeto: Procedimentos odontológicos no SUS
# Script: 03_analise_graficos_MG.R
# Objetivo: Criar gráficos, ranking e tendências de procedimentos odontológicos em MG
# Autora: Aline R. Mota
# Período: 2023 - 2025
# =========================================================

# -------------------------------
# 1. Pacotes
# -------------------------------
library(tidyverse)
library(ggplot2)
library(scales)  # para formatar números nos gráficos

# -------------------------------
# 2. Carregar base limpa
# -------------------------------
odontologia <- readRDS("dados/limpos/odontologia_ficticio_MG_2023_2025.rds")

# -------------------------------
# 3. Ranking de procedimentos (total 2023-2025)
# -------------------------------
ranking_proc <- odontologia %>%
  group_by(Procedimento) %>%
  summarise(total_quantidade = sum(Quantidade)) %>%
  arrange(desc(total_quantidade))

ranking_proc

ggplot(ranking_proc, aes(x = reorder(Procedimento, total_quantidade), y = total_quantidade, fill = Procedimento)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # barras horizontais
  labs(
    title = "Ranking de Procedimentos Odontológicos em Minas Gerais (2023-2025)",
    x = "Procedimento",
    y = "Quantidade Total"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

resumo_ano_proc <- odontologia %>%
  group_by(Ano, Procedimento) %>%
  summarise(total_quantidade = sum(Quantidade)) %>%
  arrange(Ano, desc(total_quantidade))

ggplot(resumo_ano_proc, aes(x = Procedimento, y = total_quantidade, fill = factor(Ano))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Procedimentos Odontológicos por Ano em Minas Gerais",
    x = "Procedimento",
    y = "Quantidade",
    fill = "Ano"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(resumo_ano_proc, aes(x = factor(Ano), y = total_quantidade, fill = Procedimento)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Distribuição de Procedimentos Odontológicos por Ano",
    x = "Ano",
    y = "Quantidade",
    fill = "Procedimento"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = comma)

if (!dir.exists("dados/resumos")) dir.create("dados/resumos")

# Tabelas
write_csv(odontologia, "C:/procedimentos_odontologicos_sus/dados/tabelas/odontologia.csv")
write_csv(ranking_proc, "C:/procedimentos_odontologicos_sus/dados/tabelas/ranking_proc_MG.csv")
write_csv(resumo_ano_proc, "C:/procedimentos_odontologicos_sus/dados/tabelas/resumo_ano_proc_MG.csv")
write_csv(resumo_mensal, "C:/procedimentos_odontologicos_sus/dados/tabelas/resumo_mensal_MG.csv")
