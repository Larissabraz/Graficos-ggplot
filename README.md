# Graficos-ggplot
Criação de gráficos com o comando ggplot2
#### pesquisa 2 ####
windowsFonts(Times=windowsFont("Times New Roman"))

## carregando pacotes necessarios 
install.packages("extrafont")
library(tidyverse)
library(openxlsx)
library(openssl)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(forcats)

library(extrafont)
font_import()
loadfonts(device="win")   
fonts() 

## chamando o banco de dados ##

library(tidyverse)
library(readxl)


pesquisa <- read_excel("pesquisa1.xlsx")
attach(pesquisa)
names(pesquisa)



### G?nero ####
## transfomando em fator ##

pesquisa$genero <- as.factor(pesquisa$genero)

pesquisa %>%
  group_by(genero) %>%
  count(genero) %>%
  ggplot(., aes(x = genero , y = n, fill = genero )) +
  geom_col()



pesquisa %>%
  group_by(genero) %>%
  count(genero) %>%
  ggplot(aes(x = reorder(genero , n), y = n, fill =genero,label= n)) +
  geom_label(alpha=0.5)+
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette = "Greens", direction = 1) +
  theme_classic()+
  theme(text=element_text(family="Times", face="bold", size=12))

warnings()


### faixa etaria 

pesquisa$Faixaetaria  <- as.factor(pesquisa$Faixaetaria)

pesquisa %>%
  group_by(Faixaetaria ) %>%
  count(Faixaetaria ) %>%
  ggplot(., aes(x = Faixaetaria , y = n, fill = Faixaetaria )) +
  geom_col()



pesquisa %>%
  group_by(Faixaetaria ) %>%
  count(Faixaetaria) %>%
  ggplot(., aes(x = reorder(Faixaetaria , n), y = n, fill =Faixaetaria,label= n)) +
  geom_label(alpha=1)+
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c(`Entre 18 e 28 anos` = "#00441B",
               `Entre 28 e 38 anos` = "#66C2A4",
               `Entre 48 e 58 anos` = "#238B45",
               `Entre 38 e 48 anos` = "#7AC3D2",
               `Acima de 59 anos` = "#CCECE6")
  ) +
  theme_minimal() +
  labs(
    x = "Faixa et?ria", y="Contagem",
    title = "", fill = "Faixa et?ria") +
  theme_classic()+
  theme(text=element_text(family="Times", size=12))


##### Local que reside ### 

pesquisa$Reside <- as.factor(pesquisa$Reside)

pesquisa %>%
  group_by(Reside ) %>%
  count(Reside ) %>%
  ggplot(., aes(x = Reside , y = n, fill = Reside )) +
  geom_col()



pesquisa %>%
  group_by(Reside) %>%
  count(Reside) %>%
  ggplot(., aes(x = reorder(Reside , n), y = n, fill =Reside,label= n)) +
  geom_label(alpha=1)+
  geom_bar(stat = "identity")+
    scale_fill_brewer(palette = "Greens", direction = 1) +
    theme_classic()


### beneficio da obra 

pesquisa$beneficio <- as.factor(pesquisa$beneficio)

pesquisa %>%
  group_by(beneficio) %>%
  count(beneficio) %>%
  ggplot(., aes(x =beneficio , y = n, fill =beneficio)) +
  geom_col()



pesquisa %>%
  group_by(beneficio) %>%
  count(beneficio) %>%
  ggplot(., aes(x = reorder(beneficio , n), y = n, fill =beneficio,label= n)) +
  geom_label(alpha=1)+
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette = "Greens", direction = 1) +
  theme_classic() +
  theme(legend.position = "none")+
  labs(
    x = "Notas", y="Contagem",
    title = "")

#### vc sabia da obra 

pesquisa$vcsabia <- as.factor(pesquisa$vcsabia)

pesquisa %>%
  group_by(vcsabia) %>%
  count(vcsabia) %>%
  ggplot(., aes(x =vcsabia , y = n, fill =vcsabia)) +
  geom_col()



pesquisa %>%
  group_by(vcsabia) %>%
  count(vcsabia) %>%
  ggplot(., aes(x = reorder(vcsabia , n), y = n, fill =vcsabia,label= n)) +
  geom_label(alpha=1)+
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette = "Greens", direction = 1) +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none")
labs(
  x = "", y="Contagem",
  title = "", fill = "Legenda")

### conhecimento sobre a obra

pesquisa$conheceobra <- as.factor(pesquisa$conheceobra)

pesquisa %>%
  group_by(conheceobra) %>%
  count(conheceobra) %>%
  ggplot(., aes(x = conheceobra , y = n, fill = conheceobra)) +
  geom_col()



pesquisa %>%
  group_by(conheceobra) %>%
  count(conheceobra) %>%
  ggplot(., aes(x = reorder(conheceobra, n), y = n, fill =conheceobra,label= n)) +
  geom_label(alpha=1)+
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette = "Greens", direction = 1) +
  theme_classic()




#### Reclama??o sobre a obra 

pesquisa$reclamacao <- as.factor(pesquisa$reclamacao)

pesquisa %>%
  group_by(reclamacao) %>%
  count(reclamacao) %>%
  ggplot(., aes(x = reclamacao , y = n, fill = reclamacao)) +
  geom_col()



pesquisa %>%
  group_by(reclamacao) %>%
  count(reclamacao) %>%
  ggplot(., aes(x = reorder(reclamacao, n), y = n, fill =reclamacao,label= n)) +
  geom_label(alpha=1)+
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette = "Greens", direction = 1) +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none")

#### supervisao sobre a obra 

pesquisa$supervisao <- as.factor(pesquisa$supervisao)

pesquisa %>%
  group_by(supervisao) %>%
  count(supervisao) %>%
  ggplot(., aes(x =supervisao , y = n, fill =supervisao )) +
  geom_col()



pesquisa %>%
  group_by(supervisao) %>%
  count(supervisao) %>%
  ggplot(., aes(x = reorder(supervisao , n), y = n, fill =supervisao,label= n)) +
  geom_label(alpha=1)+
  geom_bar(stat = "identity")+
  scale_fill_manual(
    values = c(N?o = "#B7DDA8",
               Sim = "#75B28D")
  ) +
  labs(fill = "Legenda") +
  theme_classic()

#### Abordado na obra 

pesquisa$abordado <- as.factor(pesquisa$abordado)

pesquisa %>%
  group_by(abordado) %>%
  count(abordado) %>%
  ggplot(., aes(x =abordado , y = n, fill =abordado )) +
  geom_col()



pesquisa %>%
  group_by(abordado) %>%
  count(abordado) %>%
  ggplot(., aes(x = reorder(abordado , n), y = n, fill =abordado,label= n)) +
  geom_label(alpha=1)+
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette = "Greens", direction = 1)+
  theme_classic()+
labs(
  x = "", y="Contagem",
  title = "", fill = "Legenda")

#### Abordado na obra 

pesquisa$abordado <- as.factor(pesquisa$abordado)

pesquisa %>%
  group_by(abordado) %>%
  count(abordado) %>%
  ggplot(., aes(x =abordado , y = n, fill =abordado )) +
  geom_col()



pesquisa %>%
  group_by(abordado) %>%
  count(abordado) %>%
  ggplot(., aes(x = reorder(abordado , n), y = n, fill =abordado,label= n)) +
  geom_label(alpha=1)+
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette = "Greens", direction = 1)+
  theme_classic()+
  labs(
    x = "", y="Contagem",
    title = "", fill = "Legenda")


  

pesquisa$beneficio <- as.factor(pesquisa$beneficio)
  
pesquisa %>%
  group_by(beneficio) %>%
  count(beneficio) %>%
  ggplot(., aes(x =beneficio , y = n, fill =beneficio, label= n )) +
  geom_text(aes(label = n), nudge_y = 0.9)+
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette = "Greens", direction = 1)+
  theme_classic()+
  labs(
    x = "Notas", y="Contagem",
    title = "", fill = "Legenda") +
  theme(text=element_text(family="Times", size=12))+
  theme(axis.text.x = element_text( hjust = 1))

  geom_col()+
    
    
    
    
    geom_text(aes(label = n), nudge_y = 0.5)
  
  
  
  
  library(tidyverse)
  pesquisa %>%
    group_by(beneficio) %>%
    count(beneficio) %>%
    ggplot(., aes(x =beneficio , y = n, fill =beneficio, label= n ))
    geom_col() +
    geom_text(aes(label = n), nudge_y = 0.5)+
      geom_bar(stat = "identity")+
    scale_fill_brewer(palette = "Greens", direction = 1) +
    theme_classic()+
    theme(legend.position = "none")+
    labs(
      x = "", y="Contagem",
      title = "", fill = "Legenda") +
    theme(text=element_text(family="Times", size=12))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
