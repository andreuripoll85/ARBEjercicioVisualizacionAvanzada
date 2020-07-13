#Andreu Ripoll Benaiges
# Ejemplo de app shiny para master UNED Big Data
# Modulo visualizacion avanzada
#

library(shiny)
library(ggplot2)


# Cargamos los datos desde la pagina web de red electrica de espana

mydata <- read.csv("https://www.ree.es/sites/default/files/11_PUBLICACIONES/Documentos/SeriesEstadisticas/mayo_2020/6_Potencia_instalada_05_2020.csv", header=FALSE, sep=';')
Nombres = gsub(' ','_',mydata[,2])
class(mydata)
head(mydata)
Nombres
mydata = cbind(mydata[,1],Nombres, mydata[,3:56])
mydata
#Adequamos un poco la informaciÃ³n. Solo voy a mostrar la informaciÃ³n general de toda la peninsula de Espanya (tambien sale por comunidad autonoma), y vamos a quitar la informaciÃ³n a partir de 2020
# ya que los valores son nulos. Estaba pensando en poder mostrarlo en mapa y por cada comunidad, si hay proyecto fin de Master.
REE_P_Espanya <- mydata[1:16,3:33]
#Hacemos que de la fila 3 a la 33 (correspondiente a 31 aÃ±os de 1990 a 2020 sean nÃºmericos)
REE_P_Espanya[] <- lapply(REE_P_Espanya, function(x) as.numeric(as.character(x)))

#Juntamos la fila 2 del original como character que es la de los nombres de las fuentes de energÃ­a.
REE_P_Espanya = cbind(mydata[1:16,2], REE_P_Espanya)


#La informaciÃ³n esta por filas, la trasposamos para que quede por Columnas.
P_instal_REE_final <- as.data.frame(t(REE_P_Espanya))
REE_P_Espanya <- P_instal_REE_final[2:32,]
#La primera fila se queda como characters que es la de los nombres de las fuentes de energÃ­a.

names(REE_P_Espanya) <- P_instal_REE_final[1,]
#Hacemos que de la fila 2 a la 32 (correspondiente a 31 aÃ±os de 1990 a 2020 sean nÃºmericos)
REE_P_Espanya[] <- lapply(REE_P_Espanya, function(x) as.numeric(as.character(x)))



# distinguimos variables "a nivel de intervalo" ("continuas" para ggplot)
nums <- sapply(REE_P_Espanya, is.numeric)
continuas <- names(REE_P_Espanya)[nums]

# y variables "categoricas" ("discretas" para ggplot)
cats <- sapply(REE_P_Espanya, is.character)
categoricas <- names(REE_P_Espanya)[cats]

shinyUI(
  navbarPage("Potencia instalada peninsular",
             tabPanel("Potencia instalada por fuente de energia",
                      mainPanel(
                        h1("Ejercicio Visualizacion Avanzada", align = "center"),
                        h2("Hecho por Andreu Ripoll", align = "center"),
                        p("Vamos a visualizar la potencia instalada en la peninsula de Espanya por
                            tipo de energia utilizada para la produccion"),
                        p("Es una aplicacion sencilla, donde puedes elegir el tipo de energia utilizada
                          para producir energia electrica."),
                        p("Esta pestanya describe el contenido y el autor."),
                        p("La segunda pestanya plantea un grafico interactivo que explora
                                el tipo de potencia en MW en la peninsula iberica y por anyo"),
                        p("Se ha mostrado con un Geom_step, para representar de la mejor manera los generadores que 
                          hay instalados en Espanya por tipo de energia y por anyo"),
                        p("A partir del dia 1 de Julio de 2020 van a cerrar la mitad de centrales termicas de carbon."),
                        p(""),
                        p(""),
                        h2("Conclusiones", align = "center"),
                        p("En Espanya hay 14 fuentes de energia que se utilizan para generar energia, podemos ver que hay 
                          energias renovables y no renovables- las no renovables son principalmente nuclear y fosil-"),
                        p("Energias renovables van creciendo y tomando protagonismo dentro del mix electrico Espanyol"),
                        p("Energias Nucleares y fosiles con eficiencias bajas van cerrando, si mostramos las centrales de 
                          carbon se puede observar que van cerrando. Si mostramos las centrales nucleares, se puede ver que 
                          van fluctuando, por mantenimiento y mejoras en las actuales, la última que se ábrio fue en 1988 y en 2012 
                          cerro la central nuclear de Garoña, 500MW"),
                        p("Se puede observar que la potencia instalada de ciclo combinado ha ido augmentando mucho en los ultimos años"),
                        p("Hay valores, outage en la producción de energia eolica y energia solar, y las graficas mostradas no pueden 
                          verse correctamente"),
                        p("Se puede observar que la potencia total instalada va creciendo en Espanya, y el resultado final es positivo, 
                          cuando se programa la parada de un gran productor, Se tiene ya planificado la puesta en marcha de otros generadores"),
                        p(""),
                        h3("Notas", align = "center"),
                        p("He tenido que grabar el archivo con un sistema de encoding UTF 8"),
                        p("Por esta razon no hay acentos"),
                        p("La -ny- representa la ene"),
                        p("Me gustaria preparar para proyecto fin de Master,  una aplicacion shiny, y utilizar los valores del data set de 
                          REE para visualizaion en mapa"),
                        p("Ademas comparar las potencias instaladas por comunidad autonoma, con el consumo de industria de cada autonomia - otro
                          data set disponible en la web de REE"),
                        p("y hacer una predicción de machine learning para saber cuanta potencia se va a instalar en cada autonomia para hacer 
                          frente las posibles nuevas demandas dependiendo si la potencia instalada aumenta o disminuye"),
                      )),
             tabPanel("Produccion de energia electrica en la peninsula iberica",
                      sidebarPanel(
                        
                        selectInput('y', 'Elige la fuente de energia a mostrar', names(REE_P_Espanya[2:16]),names(REE_P_Espanya)[4]),
                        wellPanel(
                        checkboxInput('segunda','quieres una segunda grafica?', TRUE),
                        uiOutput("conditionalInput")
                        ),
#                       selectInput('x', 'Elige variable para eje Y', names(qw)[1]),
#                       selectInput('color', 'Color', c('Verde', 'Rojo')),
#                       Queria hacer un checkboxgroupinput para seleccionar varias fuentes de energia y mostrarlas pero no me salio, ya que 'y' tiene 2 o n valores y no pude solucionar el error
#                        checkboxGroupInput('y', 'Elige la fuente de energia a mostrar', names(REE_P_Espanya[2:16]),names(REE_P_Espanya)[4]),
#                        checkboxGroupInput('y', 'Elige la fuente de energia a mostrar', choices=list(names(REE_P_Espanya[2:16])),names(REE_P_Espanya)[4]),
                        checkboxInput('lm', 'La linea de regresion'),
                        checkboxInput('smooth', 'Suavizado LOESS'),
#                        checkboxInput('facet_row', 'Elige variable para facetas por filas', c(None='.', names(REE_P_Espanya[2:16])))
#        +

                                  ),
                      
                      mainPanel(
                        fluidRow(
                          splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot1"), plotOutput("plot2"))
                        ),
                        p("Tened en cuenta que la potencia instalada -eje de la y- representa escalas distintas en las dos graficas"),
                        p("La unidad de MW viene directamente de la web de REE, creo que es un error y deberia ser GW"),
                      )
             )#,
#             tabPanel("Trabajo adicional",
#                      h1("ÃÃÂ¿QuÃÃÂ© otros grÃÃÂ¡ficos podÃÃÂ©is plantear con estos datos?", align = "center"),
#                      p("Podemos plantearnos en esta pestaÃÃÂ±a boxplot, mosaico, grÃÃÂ¡ficos de balÃÃÂ³n...")
#             )
             
  ))

