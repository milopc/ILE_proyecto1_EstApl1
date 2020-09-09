# PROYECTO 1 - ANÁLISIS SOBRE DATOS DE INTERRUPCIÓN LEGAL
# DEL EMBARAZO (ILE) EN CDMX

library (readr)
library (tidyverse)
library (lubridate)
library (moments)
library (ggExtra)
library (knitr)
library (kableExtra)
library (ggplot2)
library (cowplot)
library (meme)
library (dplyr)
library (tidyr)

# Leer base de datos
datos <- read_csv("interrupcion-legal-del-embarazo.csv")
dicc <- read_csv("Diccionario de datos ILE.csv")

# Visualizar el diccionario de la base de datos
kable_styling(kable(select(dicc, `Variable`, `Descripción`), booktabs = T), latex_options = "striped")

# Crear base para trabajar sobre embarazos previos a aborto
aborto <- datos

#Se realizan diagramas de caja y brazos para cada uno de los años. Se eliminaron las observaciones donde no se especifica la edad de la persona.
(ggplot(aborto
        %>% filter(año != "NA")
        %>% mutate(año = factor(año)),
        aes(x = año,
            y = edad,
            fill = año)
)
  + geom_boxplot()
  + scale_fill_manual(values = c("#102437", "#274B6C", "#336A98", "#5AA8E6", "#5AA8E6"))
  + labs(title = "Distribución de edad al momento de la ILE por año",
         subtitle = "Datos de 2020 hasta el 30 de junio",
         x = "Año",
         y = "Edad",
         caption = "FUENTE: Datos de la Ssa, CDMX")
  + theme(legend.position = "none")
)


# Se realiza un histograma que muestra la frecuencia absoluta de las personas que tienen n cantidad de hijos (para n=0,1,2,...,10).
  # Calcula la media y la mediana del número de hijos
datos_con_hijos <- datos %>% filter(!is.na(nhijos))
media_hijos <- mean(datos_con_hijos$nhijos)
mediana_hijos <- median(datos_con_hijos$nhijos)

(ggplot(aborto,
        aes(x = nhijos
        )
)
  + geom_histogram(binwidth = .5,
                   fill = "#102437")
  + labs(title = "Número de hijos",
         subtitle = "Datos de 2020 hasta el 30 de junio",
         x = "Número de hijos",
         y = "Número de observaciones",
         caption = "FUENTE: Datos de la Ssa, CDMX")
  + scale_x_continuous(breaks = seq(from = 0,
                                    to = 10,
                                    by = 1
  )
  )
  + scale_y_continuous(limits = c(0, 27500),
                       breaks = seq(from = 0,
                                    to = 27500,
                                    by = 5500
                       )
  )
)


#Estadísticos de la edad de inicio de vida sexual
datos_vida_sexual <- datos %>% filter(!is.na(fsexual))
tabla_vida_sexual <- datos_vida_sexual %>% summarise(Media = mean(fsexual),
                                                     Mediana = median(fsexual),
                                                     MAD = mad(fsexual),
                                                     IQR = IQR(fsexual))

#Tabla de estadísticos de la edad de inicio de la vida sexual
kable(tabla_vida_sexual, booktabs = T) %>%
  kable_styling(latex_options = "striped")

#Conteo de los métodos anticonceptivos especificados
datos_anticonceptivos <- datos %>% filter(!is.na(anticonceptivo))
anticonceptivos <- datos_anticonceptivos %>% group_by(anticonceptivo) %>% tally()

#Nos quedamos con los métodos de un solo anticonceptivo
unico_antic <- anticonceptivos %>% filter(str_detect(anticonceptivo," \\+",
                                                     negate = TRUE))

#Porcentaje de quienes no usan anticonceptivo
porc_no_antic <- (unico_antic[7,2]/nrow(datos_anticonceptivos))*100
porc_no_antic <- porc_no_antic$n

#Gráfica de quienes utilizan un solo anticonceptivo
ggplot(unico_antic, aes(x = anticonceptivo, y = n)) +
  geom_point(size = 3, color = "#102437") +
  geom_segment(aes(x=anticonceptivo,
                   xend = anticonceptivo,
                   y=0,
                   yend = n), color = "#102437") +
  labs( x = "Tipo de anticonceptivo",
        y = "Cantidad de pacientes",
        title = "Pacientes que utilizan solamente un anticonceptivo",
        subtitle = "ILE en CDMX desde 2016 hasta junio 2020",
        caption = "FUENTE: Datos de la Ssa, CDMX")+
  theme(axis.text.x = element_text(angle = 90, size = 6))


# Frecuencia de los anticonceptivos
frecuencia_antic <- 100*table(datos$anticonceptivo)/nrow(datos)
kable(frecuencia_antic, booktabs = T,
      col.names = c("Anticonceptivo","Frecuencia (%)")) %>%
  kable_styling(latex_options = "hold_positions")


# Crear base de si se presentó dolor o no
dolor <- as.data.frame(table(datos$c_dolor))
colnames(dolor) <- c("r","Cantidad")

# Gráfica de si tuvieron dolor
ggplot(dolor) + 
  geom_col(aes(x = r, y = Cantidad), color = c("#102437", "#274B6C"), fill = c("#102437", "#274B6C"), show.legend = FALSE) +
  labs(
    x = "¿Presentó dolor?",
    y = "Cantidad de pacientes",
    title = "Presencia de dolor en ILE",
    subtitle = "ILE en CDMX desde 2016 hasta junio 2020",
    caption = "FUENTE: Datos de la Ssa, CDMX"
  )

# Agregar columnas con mes, día y año en que se realizó el procedimiento del ILE
datos <- mutate(datos, mes_proced = month(fingreso), 
                día_año_proced = yday(fingreso), 
                año_proced = year(fingreso))

# Eliminar entradas de año_proced con NA y 2015 (sólo 1)
datos <- datos %>% mutate(fingreso = ymd(fingreso))
datos <- filter(datos, !(year(datos$fingreso) == 2015))

# Crear base de no. de procedimientos por año TOTALES
conteo_proced_año <- tally(group_by(datos, año_proced))

# Graficar no. de procedimientos por año TOTALES
ggplot()+
  geom_col(aes(x = conteo_proced_año$año_proced, y= conteo_proced_año$n, 
               color = conteo_proced_año$año_proced, fill = conteo_proced_año$año_proced), show.legend = FALSE)+
  labs(
    title = "Procedimientos de ILE totales por año",
    subtitle = "Datos de 2020 hasta el 30 de junio",
    x = "Año de procedimiento",
    y = "Número de procedimientos totales",
    caption = "FUENTE: Datos de la Ssa, CDMX"
  )

# Crear base de no. de procedimientos por día de cada año, TOTALES
conteo_proced_día <- tally(group_by(datos, fingreso))

# Agregar columnas de día y año a base de no. de procedimientos por día de cada año, TOTALES
conteo_proced_día <- mutate(conteo_proced_día, día = yday(fingreso),
                        año = year(fingreso))

# Graficar no. de procedimientos por día de cada año TOTALES
ggplot(conteo_proced_día, aes(día, n)) + 
  geom_line(aes(color = año), show.legend=FALSE) + 
  facet_grid(año ~ .)+
  geom_vline(xintercept = 83, color = "pink")+ # Inicio de JNSD
  labs(
    title = "Procedimientos de ILE por día, por año",
    subtitle = "Datos de 2020 hasta el 30 de junio",
    x = "Día (núm.)",
    y = "Número de procedimientos por día",
    caption = "FUENTE: Datos de la Ssa, CDMX"
  )

# Crear base que incluya días del año hasta el día 30 de junio (último proced.
# en 2020 es en esa fecha) para cada año y poder comparar por año
datos_comp.2020 <- filter(datos, día_año_proced <= 182)

# Crear base de no. de procedimientos por año hasta 30 de junio por año
conteo_proced_compaño <-tally(group_by(datos_comp.2020, año_proced))

# Graficar no. de procedimientos por año hasta 30 de junio por año
ggplot()+
  geom_col(aes(x = conteo_proced_compaño$año_proced, y= conteo_proced_compaño$n, 
               color = conteo_proced_compaño$año_proced, 
               fill = conteo_proced_compaño$año_proced), show.legend = FALSE)+
  labs(
    title = "Procedimientos de ILE por día, por año",
    subtitle = "Datos comparativos hasta el 30 de junio, por año",
    x = "Día (núm.)",
    y = "Número de procedimientos hasta el 30 de junio",
    caption = "FUENTE: Datos de la Ssa, CDMX"
  )
# Colores por año: "#102437", "#274B6C", "#336A98", "#5AA8E6"

# Crear base de no. de procedimientos por día hasta 30 de junio por año
conteo_proced_día_comp2020 <- tally(group_by(datos_comp.2020, fingreso))

# Modificar base de no. de procedimientos por día hasta 30 de junio por año 
# para agregar columnas de día y año
conteo_proced_día_comp2020 <- mutate(conteo_proced_día_comp2020, día = yday(fingreso),
       año = year(fingreso))

#Calcular promedio de procedimientos por día hasta el 30 de junio de todos los años
prom_dia_comp2020 <- mean(conteo_proced_día_comp2020$n)

# Graficar no. de procedimientos por día hasta 30 de junio de cada año 
ggplot(conteo_proced_día_comp2020, aes(día, n)) + 
  geom_line(aes(color = año), show.legend = FALSE) + 
  facet_grid(año ~ .)+
  geom_hline(yintercept = prom_dia_comp2020, color = "pink")+ #promedio diario
  geom_vline(xintercept = 83, color = "pink")+ # Inicio de JNSD
  labs(
    title = "Comparativo de procedimientos de ILE por año",
    subtitle = "Datos comparativos hasta el 30 de junio, por año",
    x = "Año de procedimiento",
    y = "Número de procedimientos hasta el 30 de junio",
    caption = "FUENTE: Datos de la Ssa, CDMX"
  )

# Creamos base que nos permita realizar el estudio por semana de geestación
datos_comp_sgd <- filter(datos, !(is.na(p_semgest)))
conteo_semgest <- tally(group_by(datos_comp_sgd, p_semgest))


# Graficar no. de procedimientos por p_semgest
ggplot(conteo_semgest, aes(x = p_semgest))+
  geom_col(aes(y = n, color = p_semgest, fill = p_semgest), show.legend = FALSE)+
  geom_vline(xintercept = 12.5, color = "pink")+ # Límite de 12 semanas, legal
  geom_vline(xintercept = mean(conteo_semgest$p_semgest), 
             linetype = "dashed", color = "black")+ # media
  geom_label(aes(x = mean(conteo_semgest$p_semgest), y = 1000),
             label = "Media")+
  geom_label(aes(x = 12.5, y = 2500),
             label = "Lim. legal")+
  labs(
    title = "Conteo de procedimientos por semana de gestación",
    subtitle = "Datos de 2016 hasta 30 de junio de 2020",
    x = "Semana de gestación",
    y = "Número de procedimientos",
    caption = "FUENTE: Datos de la Ssa, CDMX"
  )


# Y ahora, un MEME 
if (.Platform$OS.type == "windows") {
  windowsFonts(
    Impact = windowsFont("Impact"),
    Courier = windowsFont("Courier")
  )
}
meme("barco.jpg", "No, tampoco aqui encontramos la", "causalidad de tu correlación",size = "auto", r = 0.1)






















