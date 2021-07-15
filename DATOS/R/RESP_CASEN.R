library(haven)
library(ggplot2)
library(RColorBrewer)

#Se lee la Casen y se filtra
Casen_2017 <- read_dta("DTA/Casen_red.dta")

#Se generan los grupos de edad
Casen_2017$g_edad = NA
Casen_2017$g_edad[Casen_2017$edad <= 10] = "menos10"
Casen_2017$g_edad[Casen_2017$edad > 10 & Casen_2017$edad <= 20 ] = "10a20"
Casen_2017$g_edad[Casen_2017$edad > 20 & Casen_2017$edad <= 30 ] = "20a30"
Casen_2017$g_edad[Casen_2017$edad > 30 & Casen_2017$edad <= 40 ] = "30a40"
Casen_2017$g_edad[Casen_2017$edad > 40 & Casen_2017$edad <= 50 ] = "40a50"
Casen_2017$g_edad[Casen_2017$edad > 50 & Casen_2017$edad <= 60 ] = "50a60"
Casen_2017$g_edad[Casen_2017$edad > 60 & Casen_2017$edad <= 70 ] = "60a70"
Casen_2017$g_edad[Casen_2017$edad > 70 & Casen_2017$edad <= 80 ] = "70a80"
Casen_2017$g_edad[Casen_2017$edad > 80 & Casen_2017$edad <= 90 ] = "80a90"
Casen_2017$g_edad[Casen_2017$edad > 90 & Casen_2017$edad <= 100 ] = "90a100"
Casen_2017$g_edad[Casen_2017$edad > 100] = "mas100"



#Se pasan a etiquetas algunas variables
Casen_2017$educ_l = as_factor(Casen_2017$educ)
Casen_2017$rama1_l = as_factor(Casen_2017$rama1)
Casen_2017$s27c_l = as_factor(Casen_2017$s27c)
Casen_2017$s28_l = as_factor(Casen_2017$s28) 
Casen_2017$s31a1_l = as_factor(Casen_2017$s31a1)
Casen_2017$s31a2_l = as_factor(Casen_2017$s31a2)
Casen_2017$s31a3_l = as_factor(Casen_2017$s31a3)

####S27a####
#Filtro excluyendo "No" y "No sabe/recuerda"
s27 = Casen_2017[Casen_2017$s27a < 9,]

#Histograma pregunta 27a
ggplot(s27, aes(x = s27a)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "#3d85c6") +
  labs(title = "Histograma de quienes respondieron Sí" ,y = "Frecuencia")

#Agrupar en S?, No y Blanco
Casen_2017$s27a_r = NA
Casen_2017$s27a_r[Casen_2017$s27a <= 8] = "SI"
Casen_2017$s27a_r[Casen_2017$s27a == 9] = "NO"
Casen_2017$s27a_r[Casen_2017$s27a == 99] = "NO SABE"

#Gr?fico para ver que tantos "No", "S?" y "No Sabe" hay
ggplot(Casen_2017, aes(x = s27a_r, fill = s27a_r)) + 
  geom_bar(stat = "count") +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black") +
  labs(x = "Respuesta s27_a", y = "Frecuencia") +
  scale_fill_brewer(palette = "Set1")


##Por Grupos de Edad
ggplot(s27, aes(x = s27a)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "#3d85c6")+
  labs(x = "Respuesta s27_a", y = "Frecuencia")+
  facet_wrap(~ g_edad, ncol = 5)

##Por Educaci?n
ggplot(s27, aes(x = s27a)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "#3d85c6")+
  labs(x = "Respuesta s27_a", y = "Frecuencia") +
  facet_wrap(~ educ_l, ncol = 7)

##Gr?fico sacando NA y Sin dato
s27_act = s27[s27$rama1 < 20 & is.na(s27$rama1)== FALSE, ]

##Por actividad econ?mica
ggplot(s27_act, aes(x = s27a)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "#3d85c6")+
  labs(x = "Respuesta s27_a", y = "Frecuencia") +
  facet_wrap(~ rama1_l, ncol = 5)


####S27b####
#s27b. ?Cu?ntos d?as estuvo hospitalizado por ese problema o condici?n de salud?

#DF que excluye a blancos y no sabe/no recuerda
DF_s27b = Casen_2017[Casen_2017$s27b < 60 & is.na(Casen_2017$s27b) == FALSE ,]

##Histograma
ggplot(DF_s27b, aes(x = s27b)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "#3d85c6") +
  labs(title = "Histograma de quienes respondieron" ,y = "Frecuencia")

##Por grupo de edad
ggplot(DF_s27b, aes(x = s27b)) + 
  geom_histogram(binwidth = 1.5, color = "black", fill = "#3d85c6") +
  labs(title = "Histograma de quienes respondieron" ,y = "Frecuencia") +
  facet_wrap(~ g_edad, ncol = 5)

##Por educaci?n
ggplot(DF_s27b, aes(x = s27b)) + 
  geom_histogram(binwidth = 1.5, color = "black", fill = "#3d85c6") +
  labs(title = "Histograma de quienes respondieron" ,y = "Frecuencia") +
  facet_wrap(~ educ_l, ncol = 7)


s27b_act = DF_s27b[DF_s27b$rama1 < 20 & is.na(DF_s27b$rama1)== FALSE, ]

##Por actividad econ?mica
ggplot(s27b_act, aes(x = s27b)) + 
  geom_histogram(binwidth = 1.5, color = "black", fill = "#3d85c6") +
  labs(title = "Histograma de quienes respondieron" ,y = "Frecuencia") +
  facet_wrap(~ rama1_l, ncol = 5)


####S27c####
DF_s27c = Casen_2017[is.na(Casen_2017$s27c) == FALSE ,]

##Edad
ggplot(DF_s27c, aes(x = s27c, fill = s27c_l)) + 
  geom_bar(stat = "count") +
  #geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black") +
  labs(x = "Respuesta s27_c", y = "Frecuencia") +
  scale_fill_brewer(palette = "Set1") +
  theme(text = element_text(size=9)) +
  facet_wrap(~ g_edad, ncol = 5)

##Educaci?n
ggplot(DF_s27c, aes(x = s27c, fill = s27c_l)) + 
  geom_bar(stat = "count") +
  #geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black") +
  labs(x = "Respuesta s27_c", y = "Frecuencia") +
  scale_fill_brewer(palette = "Set1") +
  theme(text = element_text(size=9)) +
  facet_wrap(~ educ_l, ncol = 7)

s27c_act = DF_s27c[DF_s27c$rama1 < 20 & is.na(DF_s27c$rama1)== FALSE, ]

##Actividad
ggplot(s27c_act, aes(x = s27c, fill = s27c_l)) + 
  geom_bar(stat = "count") +
  #geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black") +
  labs(x = "Respuesta s27_c", y = "Frecuencia") +
  scale_fill_brewer(palette = "Set1") +
  theme(text = element_text(size=9)) +
  facet_wrap(~ rama1_l, ncol = 5)


####S28####

#Agrupar en S?, No y Blanco
Casen_2017$s28_r = NA
Casen_2017$s28_r[Casen_2017$s28 <= 21] = "SI"
Casen_2017$s28_r[Casen_2017$s28 == 22] = "NO"
Casen_2017$s28_r[Casen_2017$s28 == 99] = "NO SABE"

#Gr?fico para ver que tantos "No", "S?" y "No Sabe" hay
ggplot(Casen_2017, aes(x = s28_r, fill = s28_r)) + 
  geom_bar(stat = "count") +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black") +
  labs(x = "Ha estado en tratamiento", y = "Frecuencia") +
  scale_fill_brewer(palette = "Set1")

#DF de solamente personas que contestaron
DF_s28 = Casen_2017[Casen_2017$s28 <= 21,]

#Gr?fico de barras de quienes si respondieron
nb.cols <- 21
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)

ggplot(DF_s28, aes(x = s28, fill = s28_l)) + 
  geom_bar(stat = "count") +
  #geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black") +
  labs(x = "Respuesta s27_c", y = "Frecuencia") +
  scale_fill_manual(values = mycolors) +
  theme(text = element_text(size=9))

#Histogramas por grupos de edad
ggplot(DF_s28, aes(x = s28, fill = s28_l)) + 
  geom_bar(stat = "count") +
  #geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black") +
  labs(x = "Respuesta s27_c", y = "Frecuencia") +
  scale_fill_manual(values = mycolors) +
  theme(text = element_text(size=9)) +
  facet_wrap(~ g_edad, ncol = 5)

#Histogramas por Educaci?n
ggplot(DF_s28, aes(x = s28, fill = s28_l)) + 
  geom_bar(stat = "count") +
  #geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black") +
  labs(x = "Respuesta s27_c", y = "Frecuencia") +
  scale_fill_manual(values = mycolors) +
  theme(text = element_text(size=9)) +
  facet_wrap(~ educ_l, ncol = 7)

#Se eliminan los "No" y Blancos
s28_act = DF_s28[DF_s28$rama1 < 20 & is.na(DF_s28$rama1)== FALSE, ]

##Actividad
ggplot(s28_act, aes(x = s28, fill = s28_l)) + 
  geom_bar(stat = "count") +
  #geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black") +
  labs(x = "Respuesta s27_c", y = "Frecuencia") +
  scale_fill_manual(values = mycolors) +
  theme(text = element_text(size=9)) +
  facet_wrap(~ rama1_l, ncol = 5)

####S31a1####
#DF de solamente personas que contestaron
DF_s31a1 = Casen_2017[Casen_2017$s31a1 <= 6,]

##Edad
ggplot(DF_s31a1, aes(x = s31a1, fill = s31a1_l)) + 
  geom_bar(stat = "count") +
  labs(x = "Respuesta s27_c", y = "Frecuencia") +
  scale_fill_brewer(palette = "Set1") +
  theme(text = element_text(size=9)) +
  facet_wrap(~ g_edad, ncol = 5)

##Educaci?n
ggplot(DF_s31a1, aes(x = s31a1, fill = s31a1_l)) + 
  geom_bar(stat = "count") +
  labs(x = "Respuesta s27_c", y = "Frecuencia") +
  scale_fill_brewer(palette = "Set1") +
  theme(text = element_text(size=9)) +
  facet_wrap(~ educ_l, ncol = 7)

#Se eliminan los "No" y Blancos
s31a1_act = DF_s31a1[DF_s31a1$rama1 < 20 & is.na(DF_s31a1$rama1)== FALSE, ]

##Actividad
ggplot(s31a1_act, aes(x = s31a1, fill = s31a1_l)) + 
  geom_bar(stat = "count") +
  labs(x = "Respuesta s27_c", y = "Frecuencia") +
  scale_fill_brewer(palette = "Set1") +
  theme(text = element_text(size=9)) +
  facet_wrap(~ rama1_l, ncol = 5)

####S31a2####
#DF de solamente personas que contestaron
DF_s31a2 = Casen_2017[Casen_2017$s31a2 <= 6,]

##Edad
ggplot(DF_s31a2, aes(x = s31a2, fill = s31a2_l)) + 
  geom_bar(stat = "count") +
  labs(x = "Respuesta s27_c", y = "Frecuencia") +
  scale_fill_brewer(palette = "Set1") +
  theme(text = element_text(size=9)) +
  facet_wrap(~ g_edad, ncol = 5)

##Educaci?n
ggplot(DF_s31a2, aes(x = s31a2, fill = s31a2_l)) + 
  geom_bar(stat = "count") +
  labs(x = "Respuesta s27_c", y = "Frecuencia") +
  scale_fill_brewer(palette = "Set1") +
  theme(text = element_text(size=9)) +
  facet_wrap(~ educ_l, ncol = 7)

#Se eliminan los "No" y Blancos
s31a1_act = DF_s31a1[DF_s31a1$rama1 < 20 & is.na(DF_s31a1$rama1)== FALSE, ]

##Actividad
ggplot(s31a1_act, aes(x = s31a2, fill = s31a2_l)) + 
  geom_bar(stat = "count") +
  labs(x = "Respuesta s27_c", y = "Frecuencia") +
  scale_fill_brewer(palette = "Set1") +
  theme(text = element_text(size=9)) +
  facet_wrap(~ rama1_l, ncol = 5)

####S31a2####
#DF de solamente personas que contestaron
DF_s31a3 = Casen_2017[Casen_2017$s31a3 <= 6,]

##Edad
ggplot(DF_s31a3, aes(x = s31a3, fill = s31a3_l)) + 
  geom_bar(stat = "count") +
  labs(y = "Frecuencia") +
  scale_fill_brewer(palette = "Set1") +
  theme(text = element_text(size=9)) +
  facet_wrap(~ g_edad, ncol = 5)

##Educaci?n
ggplot(DF_s31a3, aes(x = s31a3, fill = s31a3_l)) + 
  geom_bar(stat = "count") +
  labs(y = "Frecuencia") +
  scale_fill_brewer(palette = "Set1") +
  theme(text = element_text(size=9)) +
  facet_wrap(~ educ_l, ncol = 7)

#Se eliminan los "No" y Blancos
s31a3_act = DF_s31a3[DF_s31a3$rama1 < 20 & is.na(DF_s31a3$rama1)== FALSE, ]

##Actividad
ggplot(s31a3_act, aes(x = s31a3, fill = s31a3_l)) + 
  geom_bar(stat = "count") +
  labs(x = "Respuesta s27_c", y = "Frecuencia") +
  scale_fill_brewer(palette = "Set1") +
  theme(text = element_text(size=9)) +
  facet_wrap(~ rama1_l, ncol = 5)

