# PROYECTO 1 - ANÁLISIS SOBRE DATOS DE INTERRUPCIÓN LEGAL
# DEL EMBARAZO (ILE) EN CDMX
#       ANÁLISIS DE PROCEDIMIENTOS

library (readr)
library (tidyverse)
library (lubridate)
library (moments)
library(ggExtra)

# Leer base de datos
datos <- read_csv("interrupcion-legal-del-embarazo.csv")
dicc <- read_csv("Diccionario de datos ILE.csv")

# Eliminar un dato de 2015
datos <- filter(datos, !(year(datos$fingreso) == 2015))

# Agregar columnas con mes, día y año en que se realizó el procedimiento del ILE
datos <- mutate(datos, mes_proced = month(fingreso), 
                día_año_proced = yday(fingreso), 
                año_proced = year(fingreso))

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
  
datos_comp_sgd <- filter(datos, !(is.na(p_semgest)) & !(is.na(c_dolor)))

# Crear base de no. de procedimientos por p_semgest y conteo de c_dolor en SI o NO
conteo_semgest <- tally(group_by(datos_comp_sgd, p_semgest))
`SI` = tally(group_by(datos_comp_sgd, p_semgest),c_dolor =="SI")[2]
`NO` = tally(group_by(datos_comp_sgd, p_semgest),c_dolor =="NO")[2]

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

# Crear base con procedimientos que presentaron y no presentaron
# dolor por semana de gestación
dolor <- data.frame(Sí = SI, No = NO)
names(dolor)[1] = "SI"
names(dolor)[2] = "NO"

ggplot(dolor)+
  geom_point(aes(x = c(1:14), y = (SI)), color = "#102437")+
  geom_point(aes(x = c(1:14), y = (NO)), color = "#5AA8E6")+
  geom_smooth(aes(x = c(1:14), y = (SI)), color = "#102437",
              method = "lm", alpha = 0.5, se = F)+
  geom_smooth(aes(x = c(1:14), y = (NO)), color = "#5AA8E6",
              method = "lm", alpha = 0.5, se = F)+
  geom_line(aes(x = c(1:14), y = (SI)), color = "#102437")+
  geom_line(aes(x = c(1:14), y = (NO)), color = "#5AA8E6")+
  xlim(0, 14) + ylim(0, 3630)+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5)) +
  scale_x_continuous(labels = as.character(c(1:14)), breaks = c(1:14))+
  labs(
    title = "Conteo de procedimientos por semana de gestación",
    subtitle = "Pacientes con dolor y sin dolor post procedimiento",
    x = "Semana de gestación",
    y = "Número de pacientes",
    caption = "FUENTE: Datos de la Ssa, CDMX"
  )+
  scale_fill_discrete(name = "Dose", labels = c("A", "B"))
  






















