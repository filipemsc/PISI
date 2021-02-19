
# Script do artigo de Potências Intermediárias 2020.2
# Prof. Dr. Rafael Mesquita
# Discente: Felipe Lira Afonso Ferreira Paiva


# Dados disponíveis em: https://github.com/felipelirapaiva/PISI



#### PARTE 1: Organizando e analisando apenas o WPI.

# Abrindo o banco de dados do WPI.
library(readxl)
  WPI <- read_excel("C:/Users/Felipe/Desktop/WPI.xlsx")
    View(WPI)


    remove.packages("rlang")
    
# Transformando num banco "longo":
library(tidyr)
    WPI_1 <- WPI %>%
    pivot_longer(
      `1975.0`:`2017.0`,
        names_to = "ano")

    str(WPI_1)

# Transformando a variável de ano em numérica
  WPI_1$ano <- as.numeric(as.character(WPI_1$ano))

  
# Renomeando as variáveis
library(tidyverse)  
  WPI_2 <- WPI_1 %>% 
    rename(poder = value,
           país = WPI)


# Descritivos
  WPI_2 %>%
    summarise(media = mean(poder, na.rm = TRUE), 
              mediana = median(poder, na.rm = TRUE), 
              minimo = min(poder, na.rm = TRUE),
              maximo = max(poder, na.rm = TRUE))

  WPI_2 %>%
    group_by(país)%>%
    summarise(media = mean(poder, na.rm = TRUE), 
              mediana = median(poder, na.rm = TRUE), 
              minimo = min(poder, na.rm = TRUE),
              maximo = max(poder, na.rm = TRUE))


# Fazendo o gráfico de linha
  library(ggplot2)

  ggplot(WPI_2, aes(ano, poder)) +
    geom_line(aes (group=país)) +
    theme_classic()
  
  
  
# Filtrando apenas os países que tiveram uma OI
  library(dplyr)
  
    WPI_3 <- WPI_2 %>%
    filter(país %in% c("Estados Unidos","França","Japão","Suécia",
           "Egito","Brasil","Nigéria","Índia","Austrália","Senegal",
           "Canadá","Suíça","Gana","Reino Unido", "Argélia","Chile",
           "Dinamarca","Itália","Alemanha","Espanha","Filipinas",
           "Líbano","Portugal","China","Coréia do Sul","Arábia Saudita",
           "Argentina","Paquistão","Quênia","Bélgica","Fiji","Áustria",
           "Finlândia","Jordânia","Nova Zelândia","Sudão","Tailândia",
           "Turquia","África do Sul","México","Peru","Sri Lanka",
           "Tanzânia","Bulgária","Grécia","Holanda","Kuwait","Mali",
           "Marrocos","Serra Leoa","Tunísia","Irlanda","Noruega",
           "Rússia","Camarões","Equador","Irã","El Salvador"))
  

# Descritivos (apenas países com OI)
  WPI_3 %>%
    summarise(media = mean(poder, na.rm = TRUE), 
              mediana = median(poder, na.rm = TRUE), 
              minimo = min(poder, na.rm = TRUE),
              maximo = max(poder, na.rm = TRUE))
  
  WPI_3 %>%
    group_by(país)%>%
    summarise(media = mean(poder, na.rm = TRUE), 
              mediana = median(poder, na.rm = TRUE), 
              minimo = min(poder, na.rm = TRUE),
              maximo = max(poder, na.rm = TRUE))
  
  
# Fazendo o gráfico de linha (apenas países com OI)
  library(ggplot2)
  
  ggplot(WPI_3, aes(ano, poder)) +
    geom_line(aes (group=país)) +
    theme_classic()
  
  
# Para ver cada um dos grupos:

# Mundiais
  mundiais <- WPI_3 %>%
    filter(poder >= 800)
  
  ggplot(WPI_4, aes(ano, poder)) +
    geom_line(aes (group=país)) +
    theme_classic()
  
# Médias
  medias <- WPI_3 %>%
    filter(poder >= 650 & poder < 800)
  
  ggplot(medias, aes(ano, poder)) +
    geom_line(aes (group=país)) +
    theme_classic()
  
# Pequenas
  pequenas <- WPI_3 %>%
    filter(poder < 650)
  
  ggplot(pequenas, aes(ano, poder)) +
    geom_line(aes (group=país)) +
    theme_classic()
  

# Recodificando a variável do WPI para aparecer a categoria da potência
  WPI_4 <-  WPI_3 %>%
    mutate(classificação = case_when(poder < 650 ~ "3. Potência menor",
                           poder >= 650 & poder < 800 ~ "2. Potência intermediária",
                           poder >= 800 ~ "1. Potência mundial"))
  
  

  
#### PARTE 2: Analisando as Organizações Internacionais.

# Abrindo a segunda base de dados
    SGs_db <- read_excel("C:/Users/Felipe/Desktop/SGs_db.xlsx")


  
##### PARTE 3: Juntando os bancos de dados
    
# Mergindo
    PISI <- left_join(SGs_db, WPI_4,
                      by = c("país" = "país",
                             "ano" = "ano"))

   
# Fazendo uma db nova agrupada pela classificação
  db <- PISI %>%
      group_by(ano, classificação) %>% 
      count()%>%
      filter(classificação != "NA")

# Fazendo um gráfico de linha
    ggplot(db, aes(ano, n)) +
      geom_line(aes (group=classificação, color=classificação)) +
      theme_classic()
      
    
    
    
    db1 <- PISI %>%
      group_by(ano, país, classificação) %>%
      count()%>%
      filter(classificação != "NA")%>%
      filter(ano <= 1991)
    
    ggplot(db1, aes(ano, n)) +
      geom_bar(aes (group=país, color=país)) +
      theme_classic()

    ggplot(db, aes(fill=classificação, y=n, x=ano)) + 
      geom_bar(position="fill", stat="identity")+
      theme_classic()

    
    
    library(treemap)
    library(scales)
    update.packages(ask = FALSE)
    

# Analisando
  
    
    #China <- WPI_3 %>%
     # filter(país %in% c("Estados Unidos","China","Rússia"))
        
    #ggplot(China, aes(ano, poder)) +
     # geom_line(aes (group=país)) +
      #theme_classic()