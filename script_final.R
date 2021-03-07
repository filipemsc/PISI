
# Script do artigo de Potências Intermediárias 2020.2
# Prof. Dr. Rafael Mesquita
# Discente: Felipe Lira Afonso Ferreira Paiva


# Dados disponíveis em: https://github.com/felipelirapaiva/PISI



#### PARTE 1: Organizando e analisando apenas o WPI.

# Abrindo o banco de dados do WPI.
library(readxl)
  WPI <- read_excel("C:/Users/Felipe/Desktop/WPI.xlsx")
    View(WPI)


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


library(ggplot2)


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
  

# Para ver cada um dos grupos:

# Mundiais
  mundiais <- WPI_3 %>%
    filter(poder >= 800)
  
  ggplot(mundiais, aes(ano, poder)) +
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
  
# Não mexer no WPI_4. Será usado para mergir. Se modificar algo, faça no 5.
# Descobrindo quantas classificações cada país possui.
  
  WPI_5 <- WPI_4 %>%
    filter(ano != 2017)
      
  WPI_6 <- WPI_5 %>%
    group_by(país, classificação) %>% 
    count()%>%
    filter(classificação == "1. Potência mundial")

  View(WPI_6)
  
#    filter(classificação == "1. Potência mundial")
#    filter(classificação == "2. Potência intermediária")
#    filter(classificação == "3. Potência menor")  
  
  
# Gráfico de linha para vários países. Figura 2 do artigo.
  
  mudaram <- c("Canadá", "Itália", "Rússia", "China", "Austrália",
               "México", "Grécia", "Coréia do Sul", "Argentina",
               "Arábia Saudita", "Portugal", "Irlanda", "Turquia",
               "Índia", "Nova Zelândia", "Irã", "Tailândia",
               "África do Sul")
  
  
  WPI_6 <- WPI_4 %>%
    filter(país %in% mudaram)
  
  ggplot(WPI_6, aes(ano, poder)) +
    geom_line(aes (group=país, color=classificação)) +
    facet_wrap(~país) +
    theme_classic()+
  scale_x_continuous(breaks=seq(1975,2015,10))+
  scale_y_continuous(breaks=c(650,800))+
  labs(title = "Figura 2",
         subtitle = "Países que mudaram de classificação",
         x = "", 
         y = "WPI",
         caption = "Fonte: Elaboração própria")

  

#### PARTE 2: Analisando as Organizações Internacionais.

# Abrindo a segunda base de dados
    SGs_db <- read_excel("C:/Users/Felipe/Desktop/SGs_db.xlsx")
    
    lideres <- SGs_db %>%
      group_by(país)%>%
      count()
     
    summary(lideres$n)
    
    lideres %>%
      summarise(media = mean(lideres$n, na.rm = TRUE), 
                mediana = median(lideres$n, na.rm = TRUE), 
                minimo = min(lideres$n, na.rm = TRUE),
                maximo = max(lideres$n, na.rm = TRUE),
                dp = sd(lideres$n, na.rm = TRUE))
    
    
##### PARTE 3: Juntando os bancos de dados
    library(scales)

# Mergindo
    PISI <- left_join(SGs_db, WPI_4,
                      by = c("país" = "país",
                             "ano" = "ano"))

   
# Fazendo uma db nova agrupada pela classificação - Para colocar os valores na tabela.
  db <- PISI %>%
      group_by(ano, classificação) %>% 
      count()%>%
      filter(classificação != "NA")

  dbb <- db %>%
    group_by(ano)%>%
    summarise(soma = sum(n))

  dbb2 <- left_join(db, dbb,
                    by = c("ano" = "ano"))
  
  dbb2 <- dbb2 %>%
    mutate (percentual = n / soma)%>%
    filter(classificação == "2. Potência intermediária")
  
  dbb3 <- PISI %>%
    group_by(país, classificação)%>%
    count()%>%
    filter(classificação == "2. Potência intermediária")
  
  dbb3 <- PISI %>%
    group_by(país, classificação)%>%
    count()%>%
    filter(classificação == "3. Potência menor")
  
# Fazendo um gráfico de linha
    ggplot(db, aes(ano, n)) +
      geom_line(aes (group=classificação, color=classificação)) +
      theme_classic()+
      scale_x_continuous(breaks=seq(1975,2015,5))+
      scale_y_continuous(breaks=seq(4,19,2))+
      labs(title = "Figura 3",
           subtitle = "Total de OIs lideradas pelas potências",
           x = "", 
           y = "OIs",
           caption = "Fonte: Elaboração própria")

# Gráfico barras por categoria
     ggplot(db, aes(fill=classificação, y=n, x=ano)) + 
      geom_bar(position="fill", stat="identity") +
      theme_classic()+
      scale_x_continuous(breaks=seq(1975,2015,5))+
      scale_y_continuous(labels = scales::percent)+
      labs(title = "Figura 4",
           subtitle = "Porcentagem de OIs lideradas por grupos de potências",
           x = "", 
           y = "",
           caption = "Fonte: Elaboração própria")


    
# Selecionando país e classificação.
    db1 <- PISI %>%
      group_by(ano, país, classificação) %>%
      count()%>%
      filter(classificação != "NA")%>%
      mutate(sigla = case_when(país == "Estados Unidos" ~ "Estados Unidos",
            país == "França" ~ "França", país == "Japão" ~ "Japão",
            país == "Suécia" ~ "Suécia", país == "Egito" ~ "Egito",
            país == "Brasil" ~ "Brasil", país == "Nigéria" ~ "Nigéria",
            país == "Índia" ~ "Índia", país == "Austrália" ~ "AU",
            país == "Senegal" ~ "Senegal", país == "Canadá" ~ "Canadá",
            país == "Suíça" ~ "Suíça", país == "Gana" ~ "Gana",
            país == "Reino Unido" ~ "Reino Unido", país == "Argélia" ~ "Argélia",
            país == "Chile" ~ "Chile", país == "Dinamarca" ~ "Dinamarca",
            país == "Itália" ~ "Itália", país == "Alemanha" ~ "Ale- manha",
            país == "Espanha" ~ "Espanha", país == "Filipinas" ~ "Filipinas",
            país == "Líbano" ~ "Lí- bano", país == "Portugal" ~ "PT",
            país == "China" ~ "CN", país == "Coréia do Sul" ~ "Coréia do Sul",
            país == "Arábia Saudita" ~ "SA", país == "Argentina" ~ "Argen-  tina",
            país == "Paquistão" ~ "Paquistão", país == "Quênia" ~ "Quênia",
            país == "Bélgica" ~ "Bélgica", país == "Fiji" ~ "Fiji",
            país == "Áustria" ~ "Áustria", país == "Finlândia" ~ "Finlândia",
            país == "Jordânia" ~ "Jor- dânia", país == "Nova Zelândia" ~ "NZ",
            país == "Sudão" ~ "Sudão", país == "Tailândia" ~ "Tai- lândia",
            país == "Turquia" ~ "Turquia", país == "África do Sul" ~ "Á. do Sul",
            país == "México" ~ "México", país == "Peru" ~ "Peru",
            país == "Sri Lanka" ~ "Sri Lanka", país == "Tanzânia" ~ "Tanzânia",
            país == "Bulgária" ~ "Bul- gária", país == "Grécia" ~ "Grécia",
            país == "Holanda" ~ "NL", país == "Kuwait" ~ "Kuwait",
            país == "Mali" ~ "Mali", país == "Marrocos" ~ "Mar- rocos",
            país == "Serra Leoa" ~ "Serra Leoa", país == "Tunísia" ~ "Tunísia",
            país == "Irlanda" ~ "Irlanda", país == "Noruega" ~ "NO",
            país == "Rússia" ~ "Rússia", país == "Camarões" ~ "Cama- rões",
            país == "Equador" ~ "EC", país == "Irã" ~ "Irã",
            país == "El Salvador" ~ "SV"))
        
# Treemap
       library(scales)
       library(treemap)
       library(RColorBrewer)
    
    
    treemap(db1,
            index=c("classificação","sigla"),
            vSize="n",
            type="index",
            palette = "Set1",
            title="Figura 1 - Distribuição de OIs/ano por classificação e país",

            border.col=c("black","black"),
            border.lwds=c(1,1),
            
            fontsize.labels=c(10,9),
            fontface.labels=c(1,1),
            bg.labels=c("transparent"),
            overlap.labels=0,
            align.labels = list(
              c("left", "top"),
                c("center", "center")))
