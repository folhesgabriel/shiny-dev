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

#ler dado

# gpkg
setores_censitarios_belem <- readOGR("SETORES_CEN_BEL_X_AGREGADOS_CENSO.gpkg", 
                              verbose = F)

#app shiny

#- criar ui object
ui <- bootstrapPage( 
  titlePanel(p("[dataviz-teste] setores censitários 2010, Belém-PA", 
               style = "color:#3474A7")), 
  
  leafletOutput("mymap"),
  tags$style(type = "text/css", "html, 
             body {width:100%;height:100%}"),
  
  #vai mostrar as informacoes do setor censitário definidas como output ao clicar na reigiao dele           
  
  absolutePanel(top = 10, right = 10, draggable = T,
                tags$div(style = "opacity: 0.70; background: #FFFFEE; padding: 8px; ", 
                         helpText("Atributos dos Setores Censitarios"),
                         textOutput("text") 
                )
  ))



#- criar server()
server <- function(input, output, session){ 
  
  output$mymap <- renderLeaflet({
    
    #leaflet
    leaflet() %>%
    addTiles() %>%  # OpenStreetMap
      addSearchOSM()%>%
      addResetMapButton()%>%
      clearMarkers()%>%
      addMarkers(
        lng=-48.5024, 
        lat=-1.45502,
        popup="Olá!"
        ) %>% 
      addPolygons(
        data = setores_censitarios_belem, 
        weight = 2,
        layerId = ~CD_SETOR,
        label = ~CD_SETOR)
    #mostra o cd setor quando o mouse passa por cima dele
    
  })
  
  
  observe({
    
    event <- input$mymap_shape_click
    sub = setores_censitarios_belem[setores_censitarios_belem$CD_SETOR==input$mymap_shape_click$id, 
                                    c("CD_SETOR", 
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
                                     "pessoas_domicilios_renda_idade_raca_pes_mais_vinte")] 
    #precisar criar o subset de variaveis que serao dispostas
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
    
    if(is.null(event))
      return()
    else
      leafletProxy("mymap")
  }
  )   
  
  observe({
    event <- input$mymap_shape_click
    sub = setores_censitarios_belem[setores_censitarios_belem$CD_SETOR==input$mymap_shape_click$id, 
                                    c("CD_SETOR", 
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
                                      "pessoas_domicilios_renda_idade_raca_pes_mais_vinte")]
    #precisar criar o subset de variaveis que serao dispostas
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
    
    if(is.null(event))
      return()
    else
      output$text <- renderText(
        {paste("Nome do Bairro: ", Nome_do_bairro, "      ",
               "Quantidade de Pessoas no Setor Censitário: ", qnt_pessoas, "      ",
               "Quantidade de Domicílios no Setor Censitário: ", qnt_Domicilios, "      ",
               "Pessoas com renda mensal entre 1/2 e 1 Salário Mínimo: ",sm_meio_1, "      ",
               "Pessoas com renda mensal entre 1 e 2 Salários Mínimos: ",sm_1_2, "      ",
               "Pessoas com renda mensal entre 2 e 3 Salários Mínimos: ",sm_2_3, "      ",
               "Pessoas com renda mensal entre 3 e 5 Salários Mínimos: ",sm_3_5, "      ",
               "Pessoas com renda mensal entre 5 e 10 Salários Mínimos: ",sm_5_10, "      ",
               "Pessoas com renda mensal entre 10 e 15 Salários Mínimos: ",sm_10_15, "      ",
               "Pessoas com renda mensal entre 15 e 20 Salários Mínimos: ",sm_15_20, "      ",
               "Pessoas com renda mensal de mais de 20 Salários Mínimos: ",sm_mais_vinte, "      "
      )}) 
    
  }
  )   
}





#- criar shinyApp()

shinyApp(ui = ui, server = server)

