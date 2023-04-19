# DashBoard-com-ShinyR
>Projeto 2VA de Estatística Exploratória I <br>
A ideia é criar um dashboard através do ShinyR. Sendo assim, foi dado uma planilha contendo dados de um corredor, onde temos informações de sua localização, velocidade e horário que foi registrado a informação.<br>
Apartir dessas informações, foi dado alguns questionários para praticarmos os assuntos abordados em aula utilizando R, como por exemplo: Teste de hipótese, Regressão linear e Intervalo de confiança.

## Rodando o projeto

- Clone o Projeto
- Utilize RStudio para importar o projeto
- Abra o arquivo main.R
- Click em Run App

## Entendendo o projeto

>Sumário:

- [Imports](#imports)
- [Estrutura interface](#estrutura-interface)
- [Criação do mapa](#criação-do-mapa)
- [Intervalo de Confiança](#intervalo-de-confiança)
- [Regressão](#regressão)
- [Testes de hipóteses](#testes-de-hipóteses)

## Imports

>Importes necessários para rodar a aplicação
```r
library(shiny)
library(tidyverse)
library(OpenStreetMap)
library(readxl)
library(dplyr)
library(ggplot2)
library(tibbletime)
```
## Estrutura interface
>O projeto tem como estrutura 4 paineis superiores, onde em cada painel teremos suas respectivas informações como a tabela lateral e a tabela principal

```r
tabPanel("Mapa",
         plotOutput("mapa")
         ),
tabPanel("Testes de hipóteses",
         sidebarPanel(
           numericInput("variancia", "Informe a variância:", value = 5),
           radioButtons("dados", "Escolher dados",
                        c("Dados 1" = "d1")
           ),
           sliderInput("mu0", "Selecione mu0",
                       min = 4, max = 12, value = 8
           ),
           sliderInput("alfa", "Selecione alfa",
                       min = 0.01, max = 0.10, value = 0.05
           ),
           radioButtons("tipo", "Tipo do teste",
                        c("Bilateral" = "bi",
                          "Unilateral a Esquerda" = "esq",
                          "Unilateral a Direita" = "dir"))
         ),
         tableOutput('table'),
         plotOutput('hist')
         ),
tabPanel("Intervalo de Confiança",
         sidebarPanel(
           sliderInput("ic", "Nível de confiança",
                       min = 0.01, max = 0.99, value = 0.95
           )),
         tableOutput('tableIC')),
tabPanel("Regressão", plotOutput('reg'))
```
## Criação do mapa

> No painel do mapa é plotado um mapa contendo os pontos que indicam a localização do corredor, como a planilha com as informações contem as localizações do corredor durante vários horários, a representação da sua localização no mapa vai ficar parecendo um trajeto que ele fez
```r
# recebendo os dados da planilha
  planilha <- read_excel("dados_de_caminhada_corrida.xlsx")
  planilha <- separate(planilha, Coordenadas, into = c("Latitude", "Longitude"), sep = ", ")
  planilha$Velocidade <- as.numeric(gsub(" km/h", "", planilha$Velocidade))
  planilha$Hora <- as.POSIXct(planilha$Hora, format = "%Y-%m-%d %H:%M:%S")
  planilha <- as_tbl_time(planilha, index = Hora)
  
  #transformando as coordenadas para unidade númerica
  long = as.numeric(planilha$Longitude)
  lat = as.numeric(planilha$Latitude)
  
  #Delimitação do tamanho do mapa
  bb = matrix(c(-34.9509, -34.9494, -8.01861, -8.01420), 2,2, byrow=T)
  rownames(bb) = c('long', 'lat')
  colnames(bb) = c('min', 'max')
  
  #criação de uma tabela com as coordenadas e velocidade
  df = data.frame(veloc = planilha$Velocidade)
  df$long = long
  df$lat = lat
  
  lonr = bb[1,2]; latu = bb[2,2] 
  lonl = bb[1,1]; latd = bb[2,1]
  
  sa_map = openmap(c(latu+0.001, lonl-0.001), 
                   c(latd-0.001, lonr+0.001),
                   type = "osm", mergeTiles = TRUE, minNumTiles = 9L)
  
  sa_map2 = openproj(sa_map)
  
  #Criando o output para a renderização do mapa no dashboard
  output$mapa <- renderPlot({
    OpenStreetMap::autoplot.OpenStreetMap(sa_map2) + 
      geom_point(data = df,
                 aes(x = long, y = lat),
                 colour = "red", size =  2.5) +
      xlab("Longitude") + ylab("Latitude")
  })
```
## Intervalo de Confiança

>No painel de  intervalo de intervalo de confiança, vamos ter uma função para calcular o IC da velocidade do corredor durante o tempo estimado: '2023-03-23 18:45:18'~'2023-03-23 18:49:23'
```r
intervaloInput = reactive({
    as.numeric(input$ic)
  })
  
  # Selecionar dados da caminhada
  caminhada <- planilha %>%  filter_time('2023-03-23 18:45:18'~'2023-03-23 18:49:23')
  
  # Calculando média e desvio padrão
  media <- mean(caminhada$Velocidade)
  desvio_padrao <- sd(caminhada$Velocidade)
  
  # Obtendo tamanho da amostra
  n <- as.numeric(nrow(caminhada))
  
  # Obtendo valor crítico z para o nível de confiança
  z <- reactive({qnorm((1 + intervaloInput()) / 2)})
  #z <- qnorm((1 + 0.95) / 2)
  # Calculando erro padrão da média
  erro_padrao <- desvio_padrao / sqrt(n)
  
  # Calculando intervalo de confiança
  intervalo <- reactive({c(media - z() * erro_padrao, media + z() * erro_padrao)})
  
  output$tableIC <- renderTable({
    data.frame(Resultado = paste0("Intervalo de Confiança: [", intervalo()[1], ", ", intervalo()[2], "]"))
  })
```
## Regressão
> Na tabela de regressão temos a representação da reta que mais se aproxima dos pontos do banco de dados `cars` e os coeficientes de correlação e determinação.
```r
#Fazendo a regressão linear
  reg <- lm(speed ~ dist, data = cars)
  
  #Calculando o coeficiente de correlação e determinação
  correlacao <- cor(cars$speed, cars$dist)
  
  R2 <- correlacao^2
  
  #Obtendo a equação da reta
  eq_reta <- paste0("y = ", round(coef(reg)[2], 3), "x + ", round(coef(reg)[1], 3))
  
  #Criando o gráfico
  output$reg = renderPlot({
    ggplot(cars, aes(x = dist, y = speed)) + 
      geom_point() +
      geom_smooth(formula = y ~ x, method = "lm", se = FALSE) +
      xlab("Distância") + ylab("Velocidade") +
      geom_text(x = 70, y = 8, label = eq_reta, size = 4, color = "red")+ 
      annotate("text", x = 110, y = 10,
      label = paste0("R = ", round(correlacao, 3), "\nR2 = ", round(R2, 3)), hjust = 1, vjust = 1)
  })
```
## Testes de hipóteses

> Aqui temos a representação do teste de hipoteses para os dados filtrados nos seguintes horários: '2023-03-23 18:40:53'~'2023-03-23 18:45:12'
```r
# Selecionar dados da caminhada
  corrida <- planilha %>%  filter_time('2023-03-23 18:40:53'~'2023-03-23 18:45:12')
  
  dados = reactive(input$dados)
  escolha_dados = renderText(dados())
  
  x = reactive({
    c(corrida$Velocidade)
  })
  
  
  n = reactive(length(x()))
  xbarra = reactive(mean(x()))
  sig = reactive(input$variancia)
  sig_xbar = reactive(sig()/sqrt(n()))
  
  mu0 = reactive({
    as.integer(input$mu0)
  })
  
  
  alfa = reactive(as.numeric(input$alfa))
  
  tipo = reactive(input$tipo)
  teste = renderText(tipo())
  
  p = reactive({
    if(teste() == "bi"){
      1 - alfa() + alfa()/2
    }else if(teste() == "esq"){
      alfa()
    }else{
      1-alfa()
    }
  })
  
  
  ztab = reactive(
    as.numeric(qnorm(p()))
  )
  
  
  zcalc = reactive(
    as.numeric((xbarra()-mu0())/sig_xbar())
  )
  
  
  output$table <- renderTable(
    if(teste() == "bi" & zcalc() < ztab() & zcalc() > -ztab() |
       teste() == "esq" & zcalc() < ztab() | 
       teste() == "dir" & zcalc() > ztab()
    ){
      data.frame(Resultado = paste0('Aceitamos H0 ao nível de sig. = ', alfa()))
    }else{
      data.frame(Resultado = paste0('Rejeitamos H0 ao nível de sig. = ', alfa()))
    }
  )
  
  output$hist = renderPlot({
    hist(x(), main='', freq = FALSE)
    abline(v = mu0(), col= 'red')
    abline(v = xbarra(), col= 'blue')
  })
  
```
