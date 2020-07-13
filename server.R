library(shiny)
library(ggplot2)


# Al ser ingeniero electrico estoy familiarizado con la pagina web de Red Electrica de Espanya REE y voy a utilizar datos de alli para el ejercicio fin de modulo.
# obtenemos los datos directamente de la web

mydata <- read.csv("https://www.ree.es/sites/default/files/11_PUBLICACIONES/Documentos/SeriesEstadisticas/mayo_2020/6_Potencia_instalada_05_2020.csv", header=FALSE, sep=';')
Nombres = gsub(' ','_',mydata[,2])
mydata = cbind(mydata[,1],Nombres, mydata[,3:56])
class(mydata)
Nombres
#Adequamos un poco la informaciÃ³n. Solo voy a mostrar la informaciÃ³n general de toda la peninsula de Espanya (tambien sale por comunidad autonoma), y vamos a quitar la informaciÃ³n a partir de 2020
# ya que los valores son nulos. Estaba pensando en poder mostrarlo en mapa y por cada comunidad, si hay proyecto fin de Master.
REE_P_Espanya <- mydata[1:16,3:33]
#Hacemos que de la fila 3 a la 33 (correspondiente a 31 aÃ±os de 1990 a 2020 sean nÃºmericos)
REE_P_Espanya[] <- lapply(REE_P_Espanya, function(x) as.numeric(as.character(x)))

#mydata[1:16,2]
#summary(mydata[1:16,2])
#Nombres = sub(' ','_',mydata[1:16,2])
#Nombres
#class(Nombres)
#summary(Nombres)

#Juntamos la fila 2 del original como character que es la de los nombres de las fuentes de energÃ­a.
REE_P_Espanya = cbind(mydata[1:16,2], REE_P_Espanya)


#La informaciÃ³n esta por filas, la trasposamos para que quede por Columnas.
P_instal_REE_final <- as.data.frame(t(REE_P_Espanya))
REE_P_Espanya <- P_instal_REE_final[2:32,]
#La primera fila se queda como characters que es la de los nombres de las fuentes de energÃ­a.

names(REE_P_Espanya) <- P_instal_REE_final[1,]
#Hacemos que de la fila 2 a la 32 (correspondiente a 31 aÃ±os de 1990 a 2020 sean nÃºmericos)
REE_P_Espanya[] <- lapply(REE_P_Espanya, function(x) as.numeric(as.character(x)))
#P_instal_REE_final[] <- lapply(P_instal_REE_final[2:32,], function(x) as.numeric(as.character(x)))
#P_instal_REE_final
#class(P_instal_REE_final)
#summary(qw)

shinyServer(function(input, output) {
# Si queremos comparar dos fuentes de energÃ­a  
  output$conditionalInput <- renderUI({
    if(input$segunda){
      selectInput('z', 'Elige la fuente de energia a mostrar', names(REE_P_Espanya[2:16]),names(REE_P_Espanya)[16])
    }
  })
  
  output$plot1 <- renderPlot({
    p <- 
      ggplot(REE_P_Espanya, 
                mapping=aes_string(x=REE_P_Espanya$`Potencia_instalada_peninsular_(MW)`, y=input$y)) + geom_point(size=3, colour="#CC0000")  + geom_step(aes(group=1), colour="#000099")  +
                labs(title = "Potencia instalada por fuente elegida", y = "MW instalad", x = "por anyo") 

                  
#    if (input$color != 'None')
#      p <- p + aes_string(color=input$color)
    
#    facets <- paste(input$facet_row, "~ .")
#    if (facets != '. ~ .')
#      p <- p + facet_grid(facets)
    
    if (input$lm)
      p <- p + geom_smooth(method='lm',formula=y~x, na.rm = T)
    if (input$smooth)
      p <- p + geom_smooth(method='loess',formula=y~x, na.rm = T)
      print(p)
      

    
  })
  output$plot2 <- renderPlot({

    q <-
      ggplot(REE_P_Espanya, 
             mapping=aes_string(x=REE_P_Espanya$`Potencia_instalada_peninsular_(MW)`, y=input$z)) + geom_point(size=3, colour="#CC0000")  + geom_step(aes(group=1), colour="#000099")  +
      labs(title = "Potencia instalada por fuente elegida", y = "MW instalad", x = "por anyo") 
    
    #    if (input$color != 'None')
    #      p <- p + aes_string(color=input$color)
    
    #    facets <- paste(input$facet_row, "~ .")
    #    if (facets != '. ~ .')
    #      p <- p + facet_grid(facets)
    
    if (input$lm)
      p <- p + geom_smooth(method='lm',formula=y~x, na.rm = T)
    if (input$smooth)
      p <- p + geom_smooth(method='loess',formula=y~x, na.rm = T)
    print(q)
    
    
    
  })

})

