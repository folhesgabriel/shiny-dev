####### pacotes
install.packages("leaflet")
install.packages("leaflet.extras")
install.packages("raster")
install.packages("rgdal")
install.packages("shiny")


######### libs
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(readxl)
library(raster)
library(rgdal)
library(shiny)
library(shinydashboard)


####### SHP's
shp_censo_completo <- readOGR("SETORES_CEN_BEL_X_AGREGADOS_CENSO.gpkg", 
                               verbose = F)

shp_pa_mun_auxilio <- readOGR("AUXILIO_X_MUN_PA_DEZ.gpkg", 
                              verbose = F)




########## APP c/ Auxílio emergencial 

ui <- bootstrapPage( 
  titlePanel(p("Destino do Axílio Emergêncial no Pará, para o mês de dezembro/2020.", style = "lightblue")), #adicionar "p" criar um paragrafo
  #usado para outputs
  leafletOutput("mymap"),
  
  tags$style(type = "text/css", "html, 
             body {width:100%;height:100%}"),
  
  #vai mostrar as informacoes do setor censitario definidas como output ao clicar na reigiao dele           
  
  absolutePanel(top = 10, right = 10, draggable = T,
                tags$div(style = "opacity: 0.70; background: #FFFFEE; padding: 8px; ", 
                         helpText("Informacões sobre o Auxílio"),
                         textOutput("text") # to display the lat, long and name in absolute panel when shape is clicked
                )
  ))



# server()
server <- function(input, output, session){ 
  output$mymap <- renderLeaflet({
    
    
    
    leaflet() %>%
      addTiles() %>%  
      addSearchOSM()%>%
      addResetMapButton()%>%
      clearMarkers()%>%
      addMarkers(lng=-48.5024, 
                 lat=-1.45502,
                 popup="Município de Belém") %>% 
      addPolygons(data = shp_pa_mun_auxilio, 
                  weight = 0.75,
                  layerId = ~CD_GEOCODM,
                  label = ~NM_MUNICIP,
                  opacity = 0.5,
                  fillOpacity = 0,
                  dashArray = "1",
                  color = "black") #mostra o cd setor quando o mouse passa por cima dele
    
  })
  
  
  observe({
    
    
    event <- input$mymap_shape_click
    sub = shp_pa_mun_auxilio[shp_pa_mun_auxilio$CD_GEOCODM==input$mymap_shape_click$id, c("CD_GEOCODM",
                                                                                          "NM_MUNICIP",
                                                                                          "mun_PA_lat_long_tot_valor_CADUN_N_BOLSA",
                                                                                          "mun_PA_lat_long_tot_valor_BOLSA_FAM", 
                                                                                          "mun_PA_lat_long_tot_valor_EXTRA_CADUN",
                                                                                          "mun_PA_lat_long_qnt_pes_CADUN_N_BOLSA",
                                                                                          "mun_PA_lat_long_qnt_pes_BOLSA_FAM",
                                                                                          "mun_PA_lat_long_qnt_pes_EXTRA_CADUN",
                                                                                          "mun_PA_lat_long_valor_total_muni")] #precisar criar o subset de variaveis que serao dispostas
    NM_MUNICIP = sub$NM_MUNICIP
    CD_GEOCODM = sub$CD_GEOCODM
    tot_valor_CADUN_N_BOLSA = sub$mun_PA_lat_long_tot_valor_CADUN_N_BOLSA
    tot_valor_BOLSA_FAM = sub$mun_PA_lat_long_tot_valor_BOLSA_FAM
    tot_valor_EXTRA_CADUN = sub$mun_PA_lat_long_tot_valor_EXTRA_CADUN
    qnt_pes_CADUN_N_BOLSA  = sub$mun_PA_lat_long_qnt_pes_CADUN_N_BOLSA
    nt_pes_BOLSA_FAM  = sub$mun_PA_lat_long_qnt_pes_BOLSA_FAM
    qnt_pes_EXTRA_CADUN = sub$mun_PA_lat_long_qnt_pes_EXTRA_CADUN
    valor_total_muni = sub$mun_PA_lat_long_valor_total_muni
    if(is.null(event))
      return()
    else
      leafletProxy("mymap")
  }
  )   
  
  observe({
    event <- input$mymap_shape_click
    sub = shp_pa_mun_auxilio[shp_pa_mun_auxilio$CD_GEOCODM==input$mymap_shape_click$id, c("CD_GEOCODM", "NM_MUNICIP",
                                                                                          "mun_PA_lat_long_tot_valor_CADUN_N_BOLSA",
                                                                                          "mun_PA_lat_long_tot_valor_BOLSA_FAM", 
                                                                                          "mun_PA_lat_long_tot_valor_EXTRA_CADUN",
                                                                                          "mun_PA_lat_long_qnt_pes_CADUN_N_BOLSA",
                                                                                          "mun_PA_lat_long_qnt_pes_BOLSA_FAM",
                                                                                          "mun_PA_lat_long_qnt_pes_EXTRA_CADUN",
                                                                                          "mun_PA_lat_long_valor_total_muni"
    )] #precisar criar o subset de variaveis que serao dispostas
    NM_MUNICIP = sub$NM_MUNICIP
    CD_GEOCODM = sub$CD_GEOCODM
    tot_valor_CADUN_N_BOLSA = sub$mun_PA_lat_long_tot_valor_CADUN_N_BOLSA
    tot_valor_BOLSA_FAM = sub$mun_PA_lat_long_tot_valor_BOLSA_FAM
    tot_valor_EXTRA_CADUN = sub$mun_PA_lat_long_tot_valor_EXTRA_CADUN
    qnt_pes_CADUN_N_BOLSA  = sub$mun_PA_lat_long_qnt_pes_CADUN_N_BOLSA
    qnt_pes_BOLSA_FAM  = sub$mun_PA_lat_long_qnt_pes_BOLSA_FAM
    qnt_pes_EXTRA_CADUN = sub$mun_PA_lat_long_qnt_pes_EXTRA_CADUN
    valor_total_muni = sub$mun_PA_lat_long_valor_total_muni
    if(is.null(event))
      return()
    else
      output$text <- renderText({paste("Nome do Município: ", NM_MUNICIP, "---------------------------------------",
                                       "Valor Total Recebido pelo Município (R$): ",valor_total_muni, "-----------------------------------",
                                       "Valor Total destinado a pessoas inscritas CAD Unico (exclusive bolsa família)R$ : ", tot_valor_CADUN_N_BOLSA, "--------------",
                                       "Valor Total destinado a pessoas inscritas no Bolsa Família (R$): ", tot_valor_BOLSA_FAM, "--------------",
                                       "Valor Total destinado a pessoas não inscritas no CAD Unico (R$): ",tot_valor_EXTRA_CADUN, "--------------",
                                       "Quantidade de pessoas inscritas CAD Unico (exclusive bolsa família): ",qnt_pes_CADUN_N_BOLSA, "--------------",
                                       "Quantidade de pessoas  inscritas no Bolsa Família: ",qnt_pes_BOLSA_FAM, "--------------",
                                       "Quantidade de pessoas não inscritas no CAD Unico: ",qnt_pes_EXTRA_CADUN, "--------------"
                                       
      )}) 
    
    
    
    
  }
  )   
  
  
  
  
}





# shinyApp()
shinyApp(ui = ui, server = server)






########## APP c/ CENSO


# ui object
uic <- bootstrapPage( 
  titlePanel(p("Belém: um olhar a partir dos Setores Censitários do Censo Demográfico de (2010) ", style = "color:#3474A7")), #adicionar "p" criar um paragrafo
  #usado para outputs
  leafletOutput("mymap"),
  
  tags$style(type = "text/css", "html, 
             body {width:100%;height:100%}"),
  
  #vai mostrar as informacoes do setor censitario definidas como output ao clicar na reigiao dele           
  
  absolutePanel(top = 10, right = 10, draggable = T,
                tags$div(style = "opacity: 0.70; background: #FFFFEE; padding: 8px; ", 
                         helpText("Atributos dos Setores Censitarios"),
                         textOutput("text") 
                )
  ))



# server()
serverc <- function(input, output, session){ 
  output$mymap <- renderLeaflet({
    
    
    
    leaflet() %>%
      addTiles() %>%  
      addSearchOSM()%>%
      addResetMapButton()%>%
      clearMarkers()%>%
      addMarkers(lng=-48.5024, 
                 lat=-1.45502,
                 popup="Município de Belém") %>% 
      addPolygons(data = shp_censo_completo, 
                  weight = 0.7,
                  layerId = ~CD_SETOR,
                  label = ~CD_SETOR,
                  opacity = 0.5,
                  fillOpacity = 0,
                  dashArray = "1",
                  color = "black") #mostra o cd setor quando o mouse passa por cima dele
    
  })
  
  
  observe({
    
    
    event <- input$mymap_shape_click
    sub = shp_censo_completo[shp_censo_completo$CD_SETOR==input$mymap_shape_click$id, c("CD_SETOR", 
                                                                                        "pessoas_domicilios_renda_idade_raca_Nome_do_bairro",
                                                                                        "pessoas_domicilios_renda_idade_raca_qnt_Domicilios", 
                                                                                        "pessoas_domicilios_renda_idade_raca_qnt_pessoas",
                                                                                        "pessoas_domicilios_renda_idade_raca_pes_um_meio_sm",
                                                                                        "pessoas_domicilios_renda_idade_raca_pes_um_dois_sm",
                                                                                        "pessoas_domicilios_renda_idade_raca_pes_dois_tres_sm",
                                                                                        "pessoas_domicilios_renda_idade_raca_pes_tres_cinco_sm",
                                                                                        "pessoas_domicilios_renda_idade_raca_pes_cinco_dez_sm",
                                                                                        "pessoas_domicilios_renda_idade_raca_pes_dez_quinze_sm",
                                                                                        "pessoas_domicilios_renda_idade_raca_pes_quinze_vinte_sm",
                                                                                        "pessoas_domicilios_renda_idade_raca_pes_mais_vinte",
                                                                                        "pessoas_domicilios_renda_idade_raca_pes_residentes_indigena",
                                                                                        "pessoas_domicilios_renda_idade_raca_pes_residentes_branca",
                                                                                        "pessoas_domicilios_renda_idade_raca_pes_residentes_preta",
                                                                                        "pessoas_domicilios_renda_idade_raca_pes_residentes_amarela",
                                                                                        "pessoas_domicilios_renda_idade_raca_pes_residentes_parda")] #precisar criar o subset de variaveis que serao dispostas
    Nome_do_bairro = sub$pessoas_domicilios_renda_idade_raca_Nome_do_bairro
    qnt_Domicilios = sub$pessoas_domicilios_renda_idade_raca_qnt_Domicilios 
    qnt_pessoas = sub$pessoas_domicilios_renda_idade_raca_qnt_pessoas
    CD_SETOR = sub$CD_SETOR
    sm_meio_1 = sub$pessoas_domicilios_renda_idade_raca_pes_um_meio_sm
    sm_1_2 = sub$pessoas_domicilios_renda_idade_raca_pes_um_dois_sm
    sm_2_3 = sub$pessoas_domicilios_renda_idade_raca_dois_tres_sm
    sm_3_5 = sub$pessoas_domicilios_renda_idade_raca_tres_cinco_sm
    sm_5_10 = sub$pessoas_domicilios_renda_idade_raca_cinco_dez_sm
    sm_10_15 = sub$pessoas_domicilios_renda_idade_raca_dez_quinze_sm
    sm_15_20 = sub$pessoas_domicilios_renda_idade_raca_pes_quinze_vinte_sm
    sm_mais_vinte = sub$pessoas_domicilios_renda_idade_raca_pes_mais_vinte
    branca = sub$pessoas_domicilios_renda_idade_raca_pes_residentes_branca
    preta = sub$pessoas_domicilios_renda_idade_raca_pes_residentes_preta
    amarela = sub$pessoas_domicilios_renda_idade_raca_pes_residentes_amarela
    indigena = sub$pessoas_domicilios_renda_idade_raca_pes_residentes_indigena
    parda = sub$pessoas_domicilios_renda_idade_raca_pes_residentes_parda
    if(is.null(event))
      return()
    else
      leafletProxy("mymap")
  }
  )   
  
  observe({
    event <- input$mymap_shape_click
    sub = shp_censo_completo[shp_censo_completo$CD_SETOR==input$mymap_shape_click$id, c("CD_SETOR", 
                                                                                        "pessoas_domicilios_renda_idade_raca_Nome_do_bairro",
                                                                                        "pessoas_domicilios_renda_idade_raca_qnt_Domicilios", 
                                                                                        "pessoas_domicilios_renda_idade_raca_qnt_pessoas",
                                                                                        "pessoas_domicilios_renda_idade_raca_pes_um_meio_sm",
                                                                                        "pessoas_domicilios_renda_idade_raca_pes_um_dois_sm",
                                                                                        "pessoas_domicilios_renda_idade_raca_pes_dois_tres_sm",
                                                                                        "pessoas_domicilios_renda_idade_raca_pes_tres_cinco_sm",
                                                                                        "pessoas_domicilios_renda_idade_raca_pes_cinco_dez_sm",
                                                                                        "pessoas_domicilios_renda_idade_raca_pes_dez_quinze_sm",
                                                                                        "pessoas_domicilios_renda_idade_raca_pes_quinze_vinte_sm",
                                                                                        "pessoas_domicilios_renda_idade_raca_pes_mais_vinte",
                                                                                        "pessoas_domicilios_renda_idade_raca_pes_residentes_indigena",
                                                                                        "pessoas_domicilios_renda_idade_raca_pes_residentes_branca",
                                                                                        "pessoas_domicilios_renda_idade_raca_pes_residentes_preta",
                                                                                        "pessoas_domicilios_renda_idade_raca_pes_residentes_amarela",
                                                                                        "pessoas_domicilios_renda_idade_raca_pes_residentes_parda")] #precisar criar o subset de variaveis que serao dispostas
    Nome_do_bairro = sub$pessoas_domicilios_renda_idade_raca_Nome_do_bairro
    qnt_Domicilios = sub$pessoas_domicilios_renda_idade_raca_qnt_Domicilios 
    qnt_pessoas = sub$pessoas_domicilios_renda_idade_raca_qnt_pessoas
    CD_SETOR = sub$CD_SETOR
    sm_meio_1 = sub$pessoas_domicilios_renda_idade_raca_pes_um_meio_sm
    sm_1_2 = sub$pessoas_domicilios_renda_idade_raca_pes_um_dois_sm
    sm_2_3 = sub$pessoas_domicilios_renda_idade_raca_dois_tres_sm
    sm_3_5 = sub$pessoas_domicilios_renda_idade_raca_tres_cinco_sm
    sm_5_10 = sub$pessoas_domicilios_renda_idade_raca_cinco_dez_sm
    sm_10_15 = sub$pessoas_domicilios_renda_idade_raca_dez_quinze_sm
    sm_15_20 = sub$pessoas_domicilios_renda_idade_raca_pes_quinze_vinte_sm
    sm_mais_vinte = sub$pessoas_domicilios_renda_idade_raca_pes_mais_vinte
    branca = sub$pessoas_domicilios_renda_idade_raca_pes_residentes_branca
    preta = sub$pessoas_domicilios_renda_idade_raca_pes_residentes_preta
    amarela = sub$pessoas_domicilios_renda_idade_raca_pes_residentes_amarela
    indigena = sub$pessoas_domicilios_renda_idade_raca_pes_residentes_indigena
    parda = sub$pessoas_domicilios_renda_idade_raca_pes_residentes_parda
    if(is.null(event))
      return()
    else
      output$text <- renderText({paste("Nome do Bairro: ", Nome_do_bairro, "--------------",
                                       "Quantidade de Pessoas no Setor Censitário (SC): ", qnt_pessoas, "--------------",
                                       "Quantidade de Domicílios no SC: ", qnt_Domicilios, "--------------",
                                       "Pessoas com renda mensal entre 1/2 e 1 Salário Mínimo (SM): ",sm_meio_1, "--------------",
                                       "Pessoas com renda mensal entre 1 e 2 SM: ",sm_1_2, "--------------",
                                       "Pessoas com renda mensal entre 2 e 3 SM: ",sm_2_3, "--------------",
                                       "Pessoas com renda mensal entre 3 e 5 SM: ",sm_3_5, "--------------",
                                       "Pessoas com renda mensal entre 5 e 10 SM: ",sm_5_10, "--------------",
                                       "Pessoas com renda mensal entre 10 e 15 SM: ",sm_10_15, "--------------",
                                       "Pessoas com renda mensal entre 15 e 20 SM: ",sm_15_20, "--------------",
                                       "Pessoas com renda mensal de mais de 20 SM: ",sm_mais_vinte, "--------------",
                                       "Pessoas de cor ou raca Preta: ", preta, "--------------",
                                       "Pessoas de cor ou raca Branca: ", branca, "--------------",
                                       "Pessoas de cor ou raca Parda: ", parda, "--------------",
                                       "Pessoas de cor ou raca indígena: ", indigena, "--------------",
                                       "Pessoas de cor ou raca Amarela: ", amarela, "--------------" )}) 
    
    
    
    
  }
  )   
  
  
  
  
}






##### shinyApps

## AE
shinyApp(ui = ui, server = server)


## Censo
shinyApp(ui = uic, server = serverc)
