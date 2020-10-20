library(tidyverse)
library(patchwork)
library(RColorBrewer)
theme_set(theme_minimal())

#https://bookdown.org/lawson/an_introduction_to_acceptance_sampling_and_spc_with_r26/attribute-sampling-plans.html#rectification-sampling
# Capitulo 2.8
# Curva OC ----------------------------------------------------------------

n <- 200
d <- 1
p <- seq(0 , 0.1, by = 0.001)

oc_data<-tibble(p=p)

oc_data <- oc_data %>% 
  mutate(Pa =  pbinom(q = d, size = n, prob = p))
  
ggplot(oc_data, aes(y=Pa , x=p)) + geom_line( size = 2) +  
     labs(title = "Curva OC para el plan: 200-1/2",
     x = "Fracción defectuosas en el lote",
     y = "Probabilidad de aceptación") + 
  theme_classic() + theme(text=element_text(size=12,face="bold")) + 
  scale_x_continuous(limits=c(0,0.04))

ggplot(oc_data, aes(y=Pa , x=p)) + geom_line( size = 2) +  
  labs(x = "Fracción defectuosas en el lote",
       y = "Probabilidad de aceptación") + 
  theme_classic() + theme(text=element_text(size=12,face="bold")) + 
  scale_x_continuous(limits=c(0,0.04))


# Curva OC con prop fallas ------------------------------------------------

# Probabilidad de defectos de 1.4 %
defectos <- tibble (pfail=0.014)

defectos <- defectos %>% 
  mutate(pa2 = pbinom(q = d, size = n, prob = pfail))


ggplot(oc_data, aes(y=Pa , x=p)) + geom_line( size = 2) +  
  labs(title = "Curva OC para el plan: 200-1/2",
       x = "Fracción defectuosas en el lote",
       y = "Probabilidad de aceptación") +
   theme_classic() + theme(text=element_text(size=12,face="bold")) + 
   scale_x_continuous(limits=c(0,0.04)) + 
  geom_segment(data = defectos , aes(x = pfail, y = 0, xend = pfail, yend = pa2),color="red",linetype="dashed") +
  geom_segment(data = defectos, aes(x = 0, y = pa2, xend = pfail, yend = pa2),color="red",linetype="dashed") + 
  annotate("text", x = 0.001, y = (defectos$pa2+0.05), label = paste(round((defectos$pa2*100),2),"%"))


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
