

library(tidyverse)
library(haven)
library(knitr)

# Abrindo a base de dados
library(readxl)
base <- read_excel("C:/Users/Felipe/Desktop/SGs_db.xlsx")
View(base)

# Rodando os dadospara o Quadro 1
table(base$SG_nacionalidade)