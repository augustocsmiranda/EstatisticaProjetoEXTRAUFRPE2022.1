################################################################################
#### UFRPE - ESTATÍSTICA EXPLORATÓRIA I                                     ####
#### PROJETO EXTRA - 2022.1                                                 ####
#### AUGUSTO MIRANDA                                                        ####
################################################################################

if(!require('shiny')) install.packages('shiny')
library(shiny)
if(!require('OpenStreetMap')) install.packages('OpenStreetMap')
library(OpenStreetMap)
if(!require('ggplot2')) install.packages('ggplot2')
library(ggplot2)
if(!require('sp')) install.packages('sp')
library(sp)
if(!require('tidyverse')) install.packages('tidyverse')
library(tidyverse)
if(!require('ggspatial')) install.packages('ggspatial')
library(ggspatial)
if(!require('readxl')) install.packages('readxl')
library(readxl)
if(!require('shinythemes')) install.packages('shinythemes')
library(shinythemes)
if(!require('plotly')) install.packages('plotly')
library(plotly)
if(!require('cowplot')) install.packages('cowplot')
library(cowplot)
if(!require('shinydashboard ')) install.packages('shinydashboard ')
library(shinydashboard )
if(!require('tidyquant ')) install.packages('tidyquant ')
library(tidyquant)
if(!require('dplyr')) install.packages('dplyr')
library(dplyr)
if(!require('e1071')) install.packages('e1071')
library(e1071)




# Define a UI para a Aplicação
ui <- fluidPage(
  
  # Theme IRADO!!!
  theme = shinytheme("cosmo"), # Definir tema como Cyborg :-)
  # Título da Aplicação
  titlePanel("Projeto Extra Estatística Exploratória"),
  tags$hr(),
  
  # Painel Principal com as TABS
  mainPanel(
    
    
    tabsetPanel(type = "tabs",
                #tabPanel("Aba 1 - Cripto Gráfico", plotOutput('cripto')),
                
                tabPanel("Aba 1 - Cripto Gráfico", plotlyOutput("grafico")),
                tabPanel("Aba 2 - Dados da Tabela", 
                         dataTableOutput("tabela2"),
                         tableOutput('table'),     
                ),
                tabPanel("Aba 3 - Estatística",
                         fluidRow(
                           column(width = 6, valueBoxOutput("media_box")),
                           column(width = 6, valueBoxOutput("mediana_box")),
                           column(width = 6, valueBoxOutput("desvioP_box")),
                           column(width = 6, valueBoxOutput("dadosCriptoQuartis_box")),
                           column(width = 6, valueBoxOutput("dadosCriptoModa_Box")),
                           column(width = 6, valueBoxOutput("dadosCriptoAmplitude_Box")),
                           column(width = 6, valueBoxOutput("dadosCriptoVariancia_Box")),
                           column(width = 6, valueBoxOutput("dadosCriptoCV_Box")),
                           column(width = 6, valueBoxOutput("dadosCriptoCoeficienteCorrelacao_Box")),
                           column(width = 6, valueBoxOutput("dadosCriptoAssimetria_Box")),
                           column(width = 6, valueBoxOutput("dadosCriptoCurtose_Box")),
                           column(width = 6, valueBoxOutput("dadosCriptoConfInterval_Box")))
                ),
                tabPanel("Aba 4 - Histograma",
                         fluidRow(
                           plotOutput('hist')
                         )      
                ),
                tabPanel("Aba 5 - Regressão",
                         fluidRow(
                           #column(width = 12, plotOutput("reg_plot"))
                           column(width = 12, plotlyOutput("reg_plot"))
                         )
                )
                
                
    )),)





#######################################################
####################################################### 
#######################################################
####################################################### 
#######################################################
####################################################### 


# Define a lógica que irá funcionar no servidor
server <- function(input, output) {
  
  #DadosCripto - Copia2.xlsx
  dadosCripto = read_excel("C:/Users/kusan/OneDrive/UFRPE/2022.1/Estatistica/2VA/Extra/DadosCripto.xlsx")
  #dadosCripto = read_excel("C:/Users/kusan/OneDrive/UFRPE/2022.1/Estatistica/2VA/Extra/DadosCripto - Copia2.xlsx")
  
  #CONVERTER DADOS DA PLANILHA
  #Data	ultimo	abertura	maxima	minima	vol	var
  
  dadosCripto <- dadosCripto %>%
    mutate(
      Data = as.Date(Data, format = "%d.%m.%Y"),
      ultimo = as.numeric(gsub(",", ".", gsub("\\.", "", ultimo))),
      abertura = as.numeric(gsub(",", ".", gsub("\\.", "", abertura))),
      maxima = as.numeric(gsub(",", ".", gsub("\\.", "", maxima))),
      minima = as.numeric(gsub(",", ".", gsub("\\.", "", minima))),
      vol = as.numeric(gsub(",", ".", gsub("K", "", vol)))*1000,
      var = as.numeric(gsub(",", ".", gsub("%", "", var))) / 100)
  
  
  
  ########### PRINCIPAIS ESTATISTICAS #################
  #######################################################  
  
  ## ESTATISTICAS
  
  #Data	ultimo	abertura	maxima	minima	vol	var
  
  #MEDIA 
  #dadosCriptoMedia <- dadosCripto%>% group_by(Data) %>% summarise(Media = mean(ultimo, abertura, maxima, minima))
  
  #dadosCriptoMedia <- dadosCripto %>% summarise(Media = mean(ultimo))
  
  dadosCriptoMedia <- dadosCripto %>% summarise(
    "Média ultimo" = mean(ultimo),
    "Média Abertura" = mean(abertura),
    "Média Máxima" = mean(maxima),
    "Média Mínima" = mean(minima))
  
  #view(dadosCriptoMedia)
  
  media_box <- valueBox(
    "Média",
    round(dadosCriptoMedia[1, 2], 2),
    icon = icon("signal"),
    color = "purple",
    #style = "border: 2px solid red; background-color: yellow;"
  )
  
  output$media_box <- renderValueBox({
    media_box
  })
  
  #MEDIANA
  #dadosCriptoMediana <- dadosCripto %>% group_by(Data) %>% summarise(mediana_ultimo = median(ultimo),
  #                                                                 mediana_abertura = median(abertura),
  #                                                                mediana_maxima = median(maxima),
  #                                                               mediana_minima = median(minima))
  
  dadosCriptoMediana <- dadosCripto %>% summarise(
    mediana_ultimo = median(ultimo),
    mediana_abertura = median(abertura),
    mediana_maxima = median(maxima),
    mediana_minima = median(minima))
  #view(dadosCriptoMediana)
  
  mediana_box <- valueBox(
    "Mediana",
    round(dadosCriptoMediana[1, 2], 2),
    icon = icon("signal"),
    color = "aqua",
    #style = "border: 2px solid red; background-color: yellow;"
  )
  
  output$mediana_box <- renderValueBox({
    mediana_box
  })
  
  #DESVIO PADRÃO
  dadosCriptoDP <- dadosCripto %>% summarise(
    desvio_padrao_ultimo = sd(ultimo),
    desvio_padrao_abertura = sd(abertura),
    desvio_padrao_maxima = sd(maxima),
    desvio_padrao_minima = sd(minima)
  )
  
  desvioP_box <- valueBox(
    "Desvio Padrão",
    round(dadosCriptoDP[1, 2], 2),
    #dadosCriptoDP[1, 2],
    icon = icon("balance-scale-left"),
    color = "aqua",
    #style = "border: 2px solid red; background-color: yellow;"
  )
  
  output$desvioP_box <- renderValueBox({
    desvioP_box
  })
  
  #view(dadosCriptoDP)
  
  #sd(Desvio_Padrão = dadosCripto$ultimo)
  
  #QUARTIL
  
  
  # dadosCriptoQuartil <- dadosCripto %>% summarise(quantile(ultimo, probs = seq(0,1, 0.25)))
  #view(dadosCriptoQuartil)
  
  #dadosCriptoQuartil <- valueBox(
  # "Quartil",
  #round(dadosCriptoQuartil[2], 2),
  #dadosCriptoDP[1, 2],
  #icon = icon("hourglass-3"),
  #color = "aqua",
  #style = "border: 2px solid red; background-color: yellow;"
  #)
  
  #output$dadosCriptoQuartil <- renderValueBox({
  #  dadosCriptoQuartil
  #})
  dadosCriptoQuartis <- dadosCripto %>% summarise(
    q0 = quantile(ultimo, probs = 0),
    q25 = quantile(ultimo, probs = 0.25),
    q50 = quantile(ultimo, probs = 0.5),
    q75 = quantile(ultimo, probs = 0.75),
    q100 = quantile(ultimo, probs = 1)
  )
  
  dadosCriptoQuartis_box <- valueBox(
    "Quartis",
    paste0(
      "0%: ", round(dadosCriptoQuartis$q0, 2), 
      " | 25%: ", round(dadosCriptoQuartis$q25, 2),
      " | 50%: ", round(dadosCriptoQuartis$q50, 2),
      " | 75%: ", round(dadosCriptoQuartis$q75, 2),
      " | 100%: ", round(dadosCriptoQuartis$q100, 2)
    ),
    icon = icon("chart-bar"),
    color = "aqua"
  )
  
  output$dadosCriptoQuartis_box <- renderValueBox({
    dadosCriptoQuartis_box
  })
  
  
  #MODA
  
  #dadosCriptoModa_Box <- data.frame(dadosCripto$ultimo))
  
  dadosCriptoModa <- names((which.max(table(dadosCripto$ultimo))))
  #view(dadosCriptoModa)
  
  dadosCriptoModa_Box <- valueBox(
    "Moda",
    dadosCriptoModa[1],
    #dadosCriptoDP[1, 2],
    icon = icon("fas fa-chart-pie"),
    color = "aqua",
    #style = "border: 2px solid red; background-color: yellow;"
  )
  
  output$dadosCriptoModa_Box <- renderValueBox({
    dadosCriptoModa_Box
  })
  
  #AMPLITUDE
  
  dadosCriptoAmplitude <- dadosCripto %>% 
    summarise(amplitude = (maxima) - min(minima)) %>% pull(amplitude)
  
  dadosCriptoAmplitude_Box <- valueBox(
    "Amplitude",
    round(dadosCriptoAmplitude[1], 2),
    #dadosCriptoDP[1, 2],
    icon = icon("expand"),
    color = "aqua",
    #style = "border: 2px solid red; background-color: yellow;"
  )
  
  output$dadosCriptoAmplitude_Box <- renderValueBox({
    dadosCriptoAmplitude_Box
  })
  
  #View(dadosCriptoAmplitude)
  
  #VARIANCIA
  dadosCriptoVariancia <- dadosCripto %>% 
    summarise(variancia = var(ultimo))
  
  dadosCriptoVariancia_Box <- valueBox(
    "Variância",
    round(dadosCriptoVariancia$variancia, 2),
    icon = icon("chart-line"),
    color = "green",
    #style = "border: 2px solid red; background-color: yellow;"
  )
  
  output$dadosCriptoVariancia_Box <- renderValueBox({
    dadosCriptoVariancia_Box
  })
  
  #View(dadosCriptoVariancia)
  
  # COEFICIENTE DE VARIAÇÃO
  
  dadosCriptoCV <- dadosCripto %>% 
    summarise(cv = sd(ultimo) / mean(ultimo) * 100)
  
  dadosCriptoCV_Box <- valueBox(
    "Coeficiente de Variação",
    round(dadosCriptoCV$cv, 2),
    icon = icon("fas fa-chart-line"),
    color = "purple",
    #style = "border: 2px solid red; background-color: yellow;"
  )
  
  output$dadosCriptoCV_Box <- renderValueBox({
    dadosCriptoCV_Box
  })
  
  #View(dadosCriptoCV)
  
  #COEFICIENTE DE CORRELAÇÃO
  # converter as colunas para numérico
  dadosCripto$ultimo <- as.numeric(dadosCripto$ultimo)
  dadosCripto$maxima <- as.numeric(dadosCripto$maxima)
  
  # calcular o coeficiente de correlação
  coeficienteCorrelacao <- cor(dadosCripto$ultimo, dadosCripto$maxima)
  
  # criar o valueBox
  dadosCriptoCoeficienteCorrelacao_Box <- valueBox(
    "Coeficiente de Correlação",
    round(coeficienteCorrelacao, 2),
    icon = icon("fas fa-chart-line"),
    color = "purple",
  )
  
  # renderizar o valueBox
  output$dadosCriptoCoeficienteCorrelacao_Box <- renderValueBox({
    dadosCriptoCoeficienteCorrelacao_Box
  })
  
  #SKEWNESS
  
  #dadosCriptoAssimetria <- dadosCripto %>% 
  #  summarise(assimetria = skewness(as.numeric(dadosCripto$ultimo)))
#  
 # dadosCriptoAssimetria_Box <- valueBox(
  #  "Assimetria",
   # round(dadosCriptoAssimetria$assimetria, 2),
    #icon = icon("fas fa-chart-bar"),
    #color = "green",
    #style = "border: 2px solid red; background-color: yellow;"
  #)
  #
  #output$dadosCriptoAssimetria_Box <- renderValueBox({
  #  dadosCriptoAssimetria_Box
  #})
  
  
  dadosCriptoAssimetria <- dadosCripto %>% 
    summarise(assimetria = skewness(as.numeric(dadosCripto$ultimo)))
  
  dadosCriptoAssimetria_Box <- valueBox(
    "Assimetria",
    round(dadosCriptoAssimetria$assimetria, 2),
    icon = icon("fas fa-chart-bar"),
    color = "green",
    #style = "border: 2px solid red; background-color: yellow;"
  )
  #view(dadosCriptoAssimetria)
  
  output$dadosCriptoAssimetria_Box <- renderValueBox({
    dadosCriptoAssimetria_Box
  })
  
  
  #CURTOSE
  curtose <- kurtosis(dadosCripto$ultimo, na.rm = TRUE)
  
  dadosCriptoCurtose_Box <- valueBox(
    "Curtose",
    round(curtose, 2),
    icon = icon("fas fa-chart-bar"),
    color = "red",
    #style = "border: 2px solid red; background-color: yellow;"
  )
  
  output$dadosCriptoCurtose_Box <- renderValueBox({
    dadosCriptoCurtose_Box
  })
  
  #INTERVALO DE CONFIANÇA
  
  # Define o nível de confiança desejado (95%)
  conf.level <- 0.95
  
  # Calcula o intervalo de confiança para a média de "ultima"
  #conf.interval <- t.test(dadosCripto$ultimo, conf.level)$conf.int
  conf.interval <- t.test(dadosCripto$ultimo, dadosCripto$maxima, conf.level, alternative = NULL)$conf.int
  
  
  
  # Cria o valueBox com o intervalo de confiança
  dadosCriptoConfInterval_Box <- valueBox(
    "Intervalo de Confiança",
    paste("(", round(conf.interval[1], 2), ", ", round(conf.interval[2], 2), ")"),
    icon = icon("fas fa-chart-bar"),
    color = "green",
    #style = "border: 2px solid red; background-color: yellow;"
  )
  
  # Renderiza o valueBox
  output$dadosCriptoConfInterval_Box <- renderValueBox({
    dadosCriptoConfInterval_Box
  })
  
  #REGRESSÂO
  
  # Ajusta um modelo de regressão linear simples para "minima" explicando "ultimo"
  modelo <- lm(minima ~ ultimo, data = dadosCripto)
  
  # Cria um gráfico de dispersão para as duas variáveis
  scatter <- ggplot(dadosCripto, aes(x = ultimo, y = minima)) +
    geom_point(color = "purple", size = 3) +
    xlab("Valor da última") +
    ylab("Valor da mínima") +
    ggtitle("Gráfico de Dispersão - Última vs. Mínima")
  
  # Adiciona a linha de regressão ao gráfico de dispersão
  regline <- scatter + geom_smooth(method = "lm", se = FALSE, color = "green") +
    ggtitle("Gráfico de Dispersão com Linha de Regressão")
  
  # Cria o valueBox com o gráfico de dispersão e a linha de regressão
  dadosCriptoRegLine_Box <- valueBox(
    "Linha de Regressão",
    regline,
    #height = "400px",
    icon = icon("fas fa-chart-line"),
    color = "purple"
  )
  
  # Renderiza o valueBox
  #output$dadosCriptoRegLine_Box <- renderValueBox({
  #  dadosCriptoRegLine_Box
  #})
  # Renderiza o valueBox
  output$dadosCriptoRegLine_Box <- renderValueBox({
    valueBox(
      "Linha de Regressão",
      regline,
      icon = icon("fas fa-chart-line"),
      color = "purple",
      #style = "height: 400px"
    )
  })
  
  ########### ITERATIVIDADE
  # Adicionar seletor de criptomoeda
  selectInput("coin", "Selecione uma criptomoeda:", choices = c("BTC", "ETH", "LTC"))
  # Criar um filtro com base na seleção do usuário
  filtered_data <- reactive({
    dadosCripto %>% 
      filter(coin == input$coin) %>%
      filter(date >= input$start_date & date <= input$end_date)
  })
  
  
  output$dadosCriptoPlot <- renderPlotly({
    ggplot(filtered_data(), aes(x = date, y = ultimo)) +
      geom_line(color = "blue") +
      labs(x = "Data", y = "Preço", title = paste("Preço do", input$coin, "no intervalo selecionado"))
  }) 
  
  plotlyOutput("dadosCriptoPlot")
  
  
  
  ############ GRÁFICOS DA ABA 1#########################
  #######################################################
  
  # GRAFICO PARA ABA 1
  output$cripto <- renderPlot({
    ggplot(dadosCripto, aes(x=Data, y=ultimo)) + 
      geom_line(color="blue", size = 1.5) +
      labs(title="Série temporal do último preço da criptomoeda",
           x="Data", y="Preço (em dólares)") +
      scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") +
      scale_y_continuous(limits = c(0, 60000), expand = c(0,0)) +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1),
            panel.background = element_rect(fill = "black"),
            panel.grid.major = element_line(color = "black"))
  })
  ###############
  ##############
  
  ####################################################################
  GraficoGripto <- dadosCripto %>%
    ggplot(aes(x = dadosCripto$Data, y = dadosCripto$ultimo)) +
    geom_line(color = "blue") + 
    scale_y_log10() +
    labs(title = "Série temporal do último preço da criptomoeda", 
         subtitle = "Log Scale", 
         y = "Preço (em dólares)", x = "") +
    #theme(plot.title.position = "plot", plot.title.hjust = 0.5, plot.title.vjust = 1.2)
    theme(plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b = 50))) 
  
  output$grafico <- renderPlotly({
    GraficoGripto <- dadosCripto %>%
      ggplot(aes(x = Data, y = ultimo)) +
      geom_line(color = "blue") + 
      scale_y_log10() +
      labs(title = "Série temporal do último preço da criptomoeda", 
           subtitle = "Log Scale", 
           y = "Preço (em dólares)", x = "")
    ggplotly(GraficoGripto)
  })
  
  #ggplotly(GraficoGripto)
  ####################################################################
  
  ### TABELA PAGINADA
  # Tabela paginada
  output$tabela2 <- renderDataTable({
    DT::datatable(dadosCripto, options = list(pageLength = 10))
  })
  
  output$tabela <- renderDataTable({
    dados_filtrados <- dadosCripto %>%
      filter(Data == input$filtro_dia)
    DT::datatable(dados_filtrados, options = list(pageLength = 10))
  })
  
  #REGRESSÃO OK
  #output$reg_plot <- renderPlot({
  # ggplot(dadosCripto, aes(x = abertura, y = ultimo)) +
  #  geom_point(color = "blue") +
  # geom_smooth(method = "lm", se = FALSE, color = "red") +
  #labs(title = "Regressão linear entre último preço e preço de abertura",
  #    x = "Preço de abertura (em dólares)",
  #    y = "Último preço (em dólares)") +
  #theme(plot.title = element_text(hjust = 0.5))
  #})
  
  #2
  #output$reg_plot <- renderPlotly({
  #  plot_ly(dadosCripto, x = ~abertura, y = ~ultimo, type = 'scatter', mode = 'markers', marker = list(color = 'blue')) %>%
  #add_markers() %>%
  #add_lines(y = ~pred, line = list(color = 'red')) %>%
  # layout(title = "Regressão linear entre último preço e preço de abertura",
  #         xaxis = list(title = "Preço de abertura (em dólares)"),
  #          yaxis = list(title = "Último preço (em dólares)"))
  #})
  
  output$reg_plot <- renderPlotly({
    p <- ggplot(dadosCripto, aes(x = abertura, y = ultimo)) +
      geom_point(color = "blue") +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      labs(title = "Regressão linear entre último preço e preço de abertura",
           x = "Preço de abertura (em dólares)",
           y = "Último preço (em dólares)") +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p)
  })
  
  
  
  
  # Histograma
  output$hist <- renderPlot({
    ggplot(dadosCripto, aes(x = ultimo)) +
      geom_histogram(color = 'black', fill = 'orange', bins = 30) +
      labs(title = 'Histograma do Último Preço da Criptomoeda',
           x = 'Preço (em dólares)', y = 'Frequência') +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # Gráfico da tabela com ggplot2
  # output$table <- renderTable({
  #   dadosCripto
  # })
  output$table <- renderTable({
    dadosCripto$Data <- format(dadosCripto$Data, "%d/%m/%Y")
    dadosCripto
  })
  
  
  
  ### FILTRO DA TABELA
  
  
  # Filtrar tabela baseada no INPUT do usuário
  dados_filtrados <- reactive({
    req(input$tipo_filtro, input$valor_filtro)
    tipo_filtro <- input$tipo_filtro
    valor_filtro <- input$valor_filtro
    if (tipo_filtro == "Dia") {
      dadosCripto %>%
        filter(Data == as.Date(valor_filtro))
    } else {
      dadosCripto %>%
        filter(ultimo >= as.numeric(valor_filtro) & ultimo <= as.numeric(valor_filtro))
    }
  })
  
  # Renderizar tabela com os FILTROS
  output$tabela <- renderTable({
    dados_filtrados()
  })
  
  ################################
  
  #class(dadosCripto$Data)
  #class(dadosCripto$ultimo)
  #class(dadosCripto$abertura)
  #class(dadosCripto$maxima)
  #class(dadosCripto$minima)
  #class(dadosCripto$vol)
  #class(dadosCripto$var)
  
  
  ########## FILTRAR DADOS 2
  
}
# Run the application 
shinyApp(ui = ui, server = server)