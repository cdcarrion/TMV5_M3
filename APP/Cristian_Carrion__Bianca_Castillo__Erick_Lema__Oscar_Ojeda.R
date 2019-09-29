library(sqldf)
library(knitr) 
library(kfigr)
library(pander)
library(stargazer)
library(xtable)
library(plyr)
library(knitr)
library(lme4)
library(corrplot)
library(optimx)
library(nloptr)
library(ggplot2)
library(gridExtra)
library(ordinal)
library(texreg)
library(cem)
library(arm)
library(broom)
library(kableExtra)
library(extrafont)
library(fitdistrplus)
library(dplyr)
library(tidyr)
library(panelView)
library(rddtools)
library(boot)
library(shiny)
library(markdown)
library(shiny)
library(shinythemes)
library(png)
library(readxl)
library(readr)

crime_t = read_csv("crime_t.csv")  
proj = read_csv("Data_final.csv")


ui <- fluidPage(

  navbarPage(title = "Marihuana",
             header = tags$h3(" - ",tags$head(tags$link(rel='shortcut icon', 
                                                        href='descarga.ico', 
                                                        type='image/x-icon'))),
             position = "fixed-top",theme=shinytheme('yeti'),#theme = 'estilo.css',
             footer = fluidRow(tags$hr(),column(12,img(src='LOGO_EPN.png',width='90px',align='center'),
                                                tags$b('Proyecto: '),
                                                ' "Análisis de la Marihuana y Criminalidad".' ,
                                                '-',tags$a('Escuela Politécnica Nacional',
                                                           href='https://www.epn.edu.ec'),
                                                tags$b('  ||  '),tags$b('Desarrollado por: '),
                                                tags$a('Carrión Cristian, Castillo Bianca, Lema Erick, Oscar Ojeda',
                                                       href='https://github.com/cristian1512')
             )
             ),
             
             #INTRODUCCION E INFORMACION DEL PROYECTO ---------------------------
             tabPanel('Introducción',icon=icon('home'),
                      
                      fluidRow(
                        
                        sidebarPanel(img(src='Lepn.png',width='30%',align='center' ),
                                     fluidRow(' '),
                                     hr(),
                                     fluidRow(
                                       column(3,tags$b('Proyecto:')),column(1),
                                       column(8,'Efecto de la Marihuana sobre el Crimen - E.E.U.U.')
                                     ),hr(),
                                     fluidRow(
                                       column(3,tags$b('Linea de Investigación:')),column(1),
                                       column(8,'Política Econímica')
                                     ),hr(),
                                     fluidRow(
                                       column(3,tags$b('Unidad:')),column(1),
                                       column(8,'Escuela Politécnica Nacional')
                                     ),hr(),
                                     fluidRow(
                                       column(3,tags$b('Director:')),column(1),
                                       column(8,'PhD. Juan Pablo Díaz')
                                     ),hr(),
                                     fluidRow(
                                       column(3,tags$b('Autores:')),column(1),
                                       column(8,'Carrion Cristian, Castillo Bianca, Lema Erick, Oscar Ojeda')
                                     )
                                     
                        ),
                        
                        mainPanel(
                          h3('El Efecto de la Legalización de la Marihuana sobre la Criminalidad'),
                          h4('Estados Unidos, Periodo 1980-2014'),hr(),
                          fluidRow(' '),
                          p('
El debate ha rodeado la legalización de la marihuana con fines médicos o recreativos durante décadas, 
algunos han argumentado que la legalización de la marihuana medicinal representa una amenaza para la 
salud pública y la seguridad. En los últimos años, algunos estados de EE.UU. han legalizado la 
marihuana con fines recreativos, reactivando el interés político y público en el impacto de la 
legalización de la marihuana en una serie de resultados.
'),
                          p('
La legalización de la marihuana para uso recreativo en los Estados Unidos sigue siendo un tema 
muy debatido a medida que más estados legalizan la marihuana para uso recreativo y con fines 
médicos. El tema abordado en este trabajo es si la *Legalización de la Marihuana (LM)*  tiene 
el efecto de aumentar el crimen, si bien hay muchos mecanismos por los cuales el LM podría 
afectar los índices de delincuencia, como se mencionará más adelante, los comentarios que apoyan 
la legalización de la marihuana se centran en una posible disminución de la delincuencia debido a 
la reducción en el mercado y la actividad delictiva asociada con ella[@Maier2017]. Si la marihuana 
se legaliza o incluso se despenaliza, se argumenta que los agentes de la ley dedicarán menos 
tiempo y recursos a hacer cumplir las leyes[@Caulkins2016]. Por otra parte, los defensores de 
la LM comentan sobre los beneficios de salud para las personas con ciertas enfermedades y 
afecciones médicas[@Eddy2005]. Uruguay se convirtió en el primer país del mundo en legalizar 
completamente la marihuana se enfocará este análisis en los Estados Unidos porque ha habido muchos 
cambios legales con respecto a la despenalización o legalización de la marihuana en los últimos 
años, la tendencia de la criminalidad neta se puede apreciar en la Figura
                          '),
                          tags$br(),
                          img(src="01.png",width='80%',align='center' )
                        )    
                      ),#,hr()
                      p('
El propósito de este análisis de datos no es explorar si existe una relación entre la legalización y 
despenalización de la marihuana y su uso, sino más bien observar la relación, si existe, entre las 
leyes de la marihuana y las tasas de delincuencia.
'),
                      h4("Método"),
                      h5("Datos y Medidas"),                      
                      h5("Variables Dependientes"),
                      p('
Los datos sobre los cuatro delitos (Asesinato y homicidio no negligente, Violación, Robo y Asalto 
agravado) entre 1980 y 2014 se obtuvieron del *FBI’s Uniform Crime Reporting* (https://bjs.gov/). 
Todos los datos se recopilaron para cada uno de los 50 estados de EE.UU. A lo largo del período 
de tiempo de 24 años para un total de N = 1785.
'),
                      h5("Variable Independiente"),
                      p('
Para determinar si y cuándo ocurrió la LM dentro de un estado, buscamos en el sitio web legislativo 
oficial para cada estado de los EE. UU. Entre 1980 y 2014, los siguientes 23 estados legalizaron la 
marihuana para uso médico, con el año en que se aprobó como se observa en la Cuadro A.1. El año de 
inicio de la LM se obtuvo del sitio web oficial [NORML](https://norml.org/states) para cada estado[@NORML]. 
La variable Dummy representa el número de años que la ley ha estado vigente con un valor de cero para todos 
los años anteriores a la aprobación de la ley, un valor de 1 para los años en que se aprobó la ley para 
capturar cualquier cambio en el Tendencia lineal del delito que se puede observar a lo largo del tiempo 
para corroborar los opositores de la LM si están en lo cierto[@Morris2014].
'),
                      h5("Variables de Control"),
                      p('
Las variables sociodemográficas se incluyeron en el análisis para ayudar a controlar una amplia gama de otras influencias que varían en el tiempo. Específicamente, incluyen:

  - El porcentaje de la fuerza laboral civil desempleada de cada estado, se obtuvo del sitio web de la Oficina de Estadísticas Laborales ([BLS](https://www.bls.gov/lau/))
  - La tasa de empleo total, se obtuvo del sitio web de la Oficina de Estadísticas Laborales ([BLS](https://www.bls.gov/sae/))
  - El porcentaje de la población que vive por debajo del umbral de pobreza, se obtuvo de la [Oficina del Censo](https://www.census.gov/topics/income-poverty/poverty.html)
  - La tasa de consumo de cerveza per cápita[@Scribner1999], los datos sobre el consumo de cerveza se tomaron del sitio web del National Institute on Alcohol Abuse and Alcoholism ([(NIAAA)](https://pubs.niaaa.nih.gov/publications/surveillance110/CONS16.htm))
  
Las estadísticas de resumen para estas variables explicativas se presentan en el Cuadro
'),
                      br(),
                      img(src="02.png"),
                      br(),
                      
                      
                      
                      
                      
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "x",
                                      label = "Variables:",
                                      choices = names(select(proj, Murder.and.nonnegligent.Manslaughter, Legacy.rape..1, Robbery, Aggravated.assault, povert_rate, Employ, Gallons.of.ethanol, Dummy)), 
                                      selected = "Robbery")
                        ),
                        
                        # Outputs
                        mainPanel(
                          verbatimTextOutput("table")
                        )
                      ),
                      
                      
                      
                      
                      h5("Plan de Análisis"),
                      p('
Para identificar el efecto de la LM en el crimen, se usará un diseño de panel de efectos aleatorios, que explota la variación dentro del estado introducida por la LM en 50 estados durante el período de observación de 22 años con la depuración de los datos perdidos como se observa en el Cuadro \ref{tab:misst}. Esto permite evaluar si los estados que adoptaron la LM experimentaron cambios en la tendencia de la delincuencia al analizar las tasas de delincuencia a lo largo del tiempo y comparar esos cambios con las tendencias de la tasa de criminalidad entre los estados que no aprobaron la LM. Además, también incluimos "efectos aleatorios por año", que capturan cualquier influencia nacional sobre el crimen que no se refleja en ninguna de las variables explicativas que varían con el tiempo. Los errores estándar robustos se agrupan a nivel estatal para evitar errores estándar sesgados debido a la no independencia de los puntos de datos a lo largo del tiempo[@Morris2014].
'),
                      br(),
                      img(src="03.png", width='80%', align = "center"),
                      br(),
                      
                      h5("Plan de Análisis"),
                      p('
Para identificar el efecto de la LM en el crimen, se usará un diseño de panel de efectos aleatorios, que explota la variación dentro del estado introducida por la LM en 50 estados durante el período de observación de 22 años con la depuración de los datos perdidos como se observa en el Cuadro \ref{tab:misst}. Esto permite evaluar si los estados que adoptaron la LM experimentaron cambios en la tendencia de la delincuencia al analizar las tasas de delincuencia a lo largo del tiempo y comparar esos cambios con las tendencias de la tasa de criminalidad entre los estados que no aprobaron la LM. Además, también incluimos "efectos aleatorios por año", que capturan cualquier influencia nacional sobre el crimen que no se refleja en ninguna de las variables explicativas que varían con el tiempo. Los errores estándar robustos se agrupan a nivel estatal para evitar errores estándar sesgados debido a la no independencia de los puntos de datos a lo largo del tiempo[@Morris2014].
'),
               
                      h4("Resultados"),
                      p("AntesAntes de consultar los resultados de los modelos de regresión, se generaron una serie de regresiones discontinuas para el crimen total de los 2 estados más grandes de EE.UU. que son California (Legalizado-1996), Texas(Ileagal) y  New Jersey el segundo estado más peligroso[@CNN] como se observa en la Figura \ref{plot2}. Tomar en cuenta que existe 2 tendencias para el índice de crimen. Una tendencia *(lado izquierdo)* muestra la criminalidad por año, cuando todavía no habían aprobado la LM que se tomará como base al estado de California. Por lo tanto, el impacto de la LM contribuyen la recta *(lado derecho)* hasta el año vigente de aprobación. Como se esperaba de la tendencia general de delitos durante este período, la recta del lado derecho revela estos estados experimentaron una reducción de crimen gradualmente para los estados de California y New Jersey para el periodo 1980 a 2014. Los estados que aprueban la LM experimentaron reducciones en el crimen. Estos *RESULTADOS PRELIMINARES* sugieren que la LM puede tener un efecto de reducción del crimen, pero hay que tomar en cuenta que otros factores pueden estar relacionados con las tendencias de las series de tiempo que no se han tomado en cuenta para estas regresiones."),
                      br(),
                      img(src="04.png", width='50%', align = "center"),
                      br(),
                      p("LosLos resultados de los análisis de efectos aleatorios se presentan en el Cuadro \ref{tab:mdl} Es importante tener en cuenta que se realizó una prueba de Hausman para determinar si el modelo de efectos aleatorios era preferible al modelo de efectos fijos. Los resultados clave obtenidos de los análisis de efectos aleatorios se presentan en la fila 1 de la Tabla \ref{tab:mdl} que se puede visualizar de mejor manera en la Figura \ref{fig:plot3}, que revela el impacto de la variable de tendencia MML en las tasas de criminalidad, mientras controla las otras variables explicativas que varían en el tiempo. De los diferentes análisis de regresión de efectos aleatorios surgieron hallazgos dignos de mención."),
                      br(),
                      img(src="05.png",width='80%', align = "center"),
                      br(),
                      br(),
                      img(src="06.png",width='60%', align = "center"),
                      br(),
                      p("El impacto de la LM en el crimen fue negativo en uno de los modelos que es de *Homicidio*, lo que sugiere que la aprobación de la LM puede tener un efecto no atenuante en el resto de delitos. Específicamente, los resultados indican aproximadamente una reducción del 0.9 por ciento en Asesinato, respectivamente, por cada año adicional en que la ley esté vigente."),
                      h4("Conclusiones"),
                      p("Los efectos de la marihuana medicinal legalizada han sido muy debatidos en los últimos años. Sin embargo, la investigación empírica sobre la relación directa entre las leyes sobre la Marihuana y el crimen es escasa y las consecuencias del consumo de marihuana en el crimen siguen siendo desconocidas, por lo tanto, al final no se encontró que la LM tenga un efecto de mejora del crimen para ninguno de los tipos de delitos analizados."),
                      p("Si bien es importante mantenerse cauteloso al interpretar estos hallazgos como evidencia de que la LM reduce el crimen, estos resultados se ajustan a la evidencia reciente y se ajustan a la idea de que la legalización de la marihuana puede llevar a una reducción en el consumo de alcohol debido a individuos que sustituyen a la marihuana por alcohol. Además, los hallazgos actuales también deben tomarse en contexto con la naturaleza de los datos disponibles. Se basan en los registros oficiales de arresto (UCR), que no tienen en cuenta los delitos que no se denunciaron a la policía. Por ende, esta evaluación de impacto de la LM sobre las tasas de crimen no parecen tener ningun efecto negativo en la criminalidad oficialmente reportada durante los años en que las leyes están vigentes. También es importante tener en cuenta que los datos de la UCR utilizados aquí no tuvieron en cuenta la delincuencia juvenil, que puede o no estar vinculada empíricamente a la LM de una forma u otra.")
                      
                      
                      
                      
                      
             ),
             
             
             
             tabPanel('Análisis',
                      
                      fluidRow(
                        # Panel Lateral -------------------------------
                        column(width = 2, p(" ")       )
                      ),
                      
                      mainPanel(
                        h2('Presentación'),
                        fluidRow(' '),
                        p('
La replicacion de los archivos están disponibles en la página Github del autor 
(https://github.com/cristian1512))"
'           )
                      ),
                      tabsetPanel(
                        tabPanel("Informe", 
                                 tags$iframe(style="height:400px; width:100%; scrolling=yes", 
                                             src="mar_doc.pdf")),
                        tabPanel("Láminas",
                                 tags$iframe(style="height:400px; width:100%; scrolling=yes", 
                                             src="pres.pdf"))
                      )
             )
  )
)
  


server <- function(input, output, session) {
  output$table <- renderPrint({
    summary(proj[(input$x)])
  })
  
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
  observeEvent(input$generate, {
    output$pdfview <- renderUI({
      tags$iframe(style="height:600px; width:100%", src="foo.pdf")
    })
  })
}



shinyApp(ui = ui, server = server)

