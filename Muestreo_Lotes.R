library(tidyverse)
library(patchwork)
library(RColorBrewer)
theme_set(theme_minimal())

#https://bookdown.org/lawson/an_introduction_to_acceptance_sampling_and_spc_with_r26/attribute-sampling-plans.html#rectification-sampling
# Capitulo 2.8
# Curva OC ----------------------------------------------------------------

n <- 100 # Tamaño de la muestra
d <- 5 # Criterio de aceptacion
 

oc_data <-tibble("p" = seq(0 , 0.1, by = 0.0001)) # P: serie de proporcion de defectuoso

oc_data <- oc_data %>% 
  mutate(Pa =  pbinom(q = d, size = n, prob = p)) # Pa: Probabilidad de aceptación.
  
ggplot(oc_data, aes(y=Pa , x=p)) + geom_line( size = 2,color = "#386cb0") +  
     labs(title = paste("Curva OC para el plan: ",n,"-",d,"/",(d+1),sep =""),
     x = "Fracción defectuosas en el lote",
     y = "Probabilidad de aceptación") + 
  theme_minimal() + theme(text=element_text(size=12,face="bold")) + 
  scale_x_continuous(limits=c(0,0.1))


# Animacion n increase ----------------------------------------------------
# A medida que n aumeta, la curva se hace mas pronuncaida
# Debemos mantener c proporcional

n2 <- c(50,100,200) # Tamaño de la muestra
d2 <- c(1,2,4) # Criterio de aceptacion

p = seq(0 , 0.1, by = 0.001) # P: serie de proporcion de defectuoso

pa_funct <-  tibble ( n2 = rep(n2,each=length(p)),
  d2 = rep (d2, each = length(p)),
  p2 = rep (p,3) , 
  Pa =  pbinom(q = d2, size = n2, prob = p2)
  
)

label1 = paste ( "n = ", n2, " / d = ", d2 )

ggplot(pa_funct, aes(y=Pa , x=p2 , group = d2)) +
  geom_line( aes(color = factor(d2)), size = 1.5) +  
  labs(#title = paste("Curva OC para el plan: ",n,"-",d,"/",(d+1),sep =""),
       x = "Fracción defectuosas en el lote",
       y = "Probabilidad de aceptación",
       color = NULL) + 
  scale_x_continuous(limits=c(0,0.1)) + 
  scale_color_brewer(palette = "Set1" , labels = all_of(label1)) + 
  theme_minimal() +
  theme(text=element_text(size=12,face="bold"),
        legend.position = "top")  
  

# Curva OC con errores alfa y betas ------------------------------------------------
aql = 0.035
rql = 0.075

alfa = pbinom(q = d, size = n, prob = aql)
beta = pbinom(q = d, size = n, prob = rql)

type_errors <- oc_data %>% 
  filter (Pa == alfa | Pa ==  beta)


ggplot(oc_data, aes(y=Pa , x=p)) + geom_line( size = 2,color = "#386cb0") +  
  labs(title = paste("Curva OC para el plan: ",n," - ",d,"/",(d+1),sep =""),
       x = "Fracción defectuosas en el lote",
       y = "Probabilidad de aceptación") + 
  theme_minimal() + theme(text=element_text(size=12,face="bold")) + 
  scale_x_continuous(limits=c(0,0.1)) + 
  geom_segment(data = type_errors , aes(x = p  , y = 0, xend = p, yend = Pa),
               color="#666666",linetype="dashed",size = 1) +
  geom_segment(data = type_errors , aes(x = 0  , y = Pa, xend = p , yend = Pa),
               color="#666666",linetype="dashed",size = 1) 


# Probabilidad de defectos de 1.4 %
defectos <- tibble (pfail=0.014)

defectos <- defectos %>% 
  mutate(pa2 = pbinom(q = d, size = n, prob = pfail))


ggplot(oc_data, aes(y=Pa , x=p)) + geom_line( size = 2) +  
  labs(title = paste("Curva OC para el plan: ",n,"-",d,"/",(d+1),sep =""),
       x = "Fracción defectuosas en el lote",
       y = "Probabilidad de aceptación") +
   theme_classic() + theme(text=element_text(size=12,face="bold")) + 
   scale_x_continuous(limits=c(0,0.04)) + 
  geom_segment(data = defectos , aes(x = pfail, y = 0, xend = pfail, yend = pa2),color="red",linetype="dashed") +
  geom_segment(data = defectos, aes(x = 0, y = pa2, xend = pfail, yend = pa2),color="red",linetype="dashed") + 
  annotate("text", x = 0.001, y = (defectos$pa2+0.05), label = paste(round((defectos$pa2*100),2),"%"))


# AceptaceSampling package ------------------------------------------------

library(AcceptanceSampling)
# PRP: Producer Risk Point (AQL, 1-alfa)
# CRP: Consumer Risk Point (RQL, Beta)

plan_ideal <- find.plan(PRP=c(0.05,0.95),CRP=c(0.15,0.20),type="hypergeom",N=500)

  




# AOQL ---------------------------------------------------------------------
N <- 6000
n <- 200
d <- 1
p <- seq(0 , 0.1, by = 0.001)

aoq_data<- tibble(p) %>% 
  mutate (Pa = pbinom(q = d, size = n, prob = p),
          Aoq =  (Pa * p * (N-n)) / N)

Aoql <- max(aoq_data$Aoq)


ggplot(aoq_data, aes(y=Aoq , x=p)) + geom_line( size = 2) +  
  labs(title = "Curva AOQ para el plan: 200-1/2",
       x = "Fracción defectuosas en el lote",
       y = "Fracción defectuosa promedio de salida (AOQ)") + 
  theme_classic() + theme(text=element_text(size=12,face="bold")) + 
  scale_x_continuous(limits=c(0,0.04)) 
