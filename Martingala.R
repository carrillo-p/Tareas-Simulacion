rm(list = ls())
library(ggplot2)

#Tarea 1
martingala <- function(bolsa, apuesta, limite, prob){
    if(apuesta > limite){stop("El limite no puede ser inferior a la apuesta inicial")}
    if(bolsa <= 0){stop("La bolsa no puede empezar en 0")}
    end <- FALSE
    a <- apuesta
    b <- bolsa
    while(!end){
        color <- runif(1, 0, 1)
        if (color < prob){
            b <- b + a
            a <- apuesta
        }
        else{
            b <- b - a
            a <- a * 2
        }
        if(b <= 0 | limite < a | a > b)
            end <- TRUE
    }
    return(b)
}

martingala(100, 10, 50, 18/37)

martingala_media <- function(bolsa, apuesta, limite, prob, repeticiones){
    if(apuesta > limite){stop("El limite no puede ser inferior a la apuesta inicial")}
    if(bolsa <= 0){stop("La bolsa no puede empezar en 0")}
    recogedor <- numeric(repeticiones)
    for (i in 1:repeticiones){
        recogedor[i] <- martingala(bolsa, apuesta, limite, prob)
    }
    media <- mean(recogedor)
    dt <- sd(recogedor)
    LIIC <- media - 1.95*(dt/sqrt(repeticiones))
    LSIC <- media + 1.95*(dt/sqrt(repeticiones))
    resultados <- list("Media" = media, "LIIC" = LIIC, "LSIC" = LSIC) 
    return(resultados)
}

martingala_media(100, 10, 50, 18/37, 1000)

#Tarea 2
prob <- seq(0.2, 0.6, 0.01)
recogedor2 <- data.frame(Media = 0, LIIC = 0, LSIC = 0, prob = 0)
for (i in 1:length(prob)){
    resultados <- martingala_media(100, 10, 50, prob[i], 1000)
    recogedor2[i, 1] <- resultados[[1]]
    recogedor2[i, 2] <- resultados[[2]]
    recogedor2[i, 3] <- resultados[[3]]
    recogedor2[i, 4] <- prob[i]
}
recogedor2

ggplot(recogedor2, aes(prob, Media))+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymax = LSIC, ymin = LIIC))+
    geom_hline(yintercept = 100, linetype = "dashed", color = "turquoise3")+
    geom_text(aes(x = 0.25, y = 98), label = "Bolsa inicial", colour = "turquoise3")+
    theme_bw()+
    xlab("Probabilidad de ganar")+
    ylab("Media bolsa final")


#Tarea 3
bolsa <- seq(10, 200, 5)
recogedor2 <- data.frame(Media = 0, LIIC = 0, LSIC = 0, bolsa_incial = 0)
for (i in 1:length(bolsa)){
    resultados <- martingala_media(bolsa[i], 10, 50, 18/37, 1000)
    recogedor2[i, 1] <- resultados[[1]]
    recogedor2[i, 2] <- resultados[[2]]
    recogedor2[i, 3] <- resultados[[3]]
    recogedor2[i, 4] <- bolsa[i]
}
recogedor2

ggplot(recogedor2, aes(bolsa, Media))+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymax = LSIC, ymin = LIIC))+
    geom_hline(yintercept = 100, linetype = "dashed", color = "turquoise3")+
    geom_text(aes(x = 13, y = 98), label = "Bolsa inicial", colour = "turquoise3")+
    theme_bw()+
    xlab("Tamaño inicial de la bolsa")+
    ylab("Media bolsa final")


#Tarea 4
limite <- seq(50, 500, 10)
recogedor2 <- data.frame(Media = 0, LIIC = 0, LSIC = 0, limite_mesa = 0)
for (i in 1:length(limite)){
    resultados <- martingala_media(100, 10, limite[i], 18/37, 1000)
    recogedor2[i, 1] <- resultados[[1]]
    recogedor2[i, 2] <- resultados[[2]]
    recogedor2[i, 3] <- resultados[[3]]
    recogedor2[i, 4] <- limite[i]
}
recogedor2

ggplot(recogedor2, aes(limite, Media))+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymax = LSIC, ymin = LIIC))+
    geom_hline(yintercept = 100, linetype = "dashed", color = "turquoise3")+
    geom_text(aes(x = 50, y = 102), label = "Bolsa inicial", colour = "turquoise3")+
    theme_bw()+
    xlab("Limite de apuesta en mesa")+
    ylab("Media bolsa final")

#Tarea 5
#Se comprueba si hay ganancia al realizar la martingala cuando se añade una condición de control para que pare cuando llegue a un determinado porcentaje de ganancia.
martingala <- function(bolsa, apuesta, limite, prob, por_ganancia){
    if(apuesta > limite){stop("El limite no puede ser inferior a la apuesta inicial")}
    if(bolsa <= 0){stop("La bolsa no puede empezar en 0")}
    end <- FALSE
    a <- apuesta
    b <- bolsa
    while(!end){
        color <- runif(1, 0, 1)
        if (color < prob){
            b <- b + a
            a <- apuesta
        }
        else{
            b <- b - a
            a <- a * 2
        }
        if(b <= 0 | limite < a | a > b | b == bolsa*(por_ganancia))
            end <- TRUE
    }
    return(b)
}

martingala_media <- function(bolsa, apuesta, limite, prob, repeticiones, por_ganancia){
    if(apuesta > limite){stop("El limite no puede ser inferior a la apuesta inicial")}
    if(bolsa <= 0){stop("La bolsa no puede empezar en 0")}
    recogedor <- numeric(repeticiones)
    for (i in 1:repeticiones){
        recogedor[i] <- martingala(bolsa, apuesta, limite, prob, por_ganancia)
    }
    media <- mean(recogedor)
    dt <- sd(recogedor)
    LIIC <- media - 1.95*(dt/sqrt(repeticiones))
    LSIC <- media + 1.95*(dt/sqrt(repeticiones))
    resultados <- list("Media" = media, "LIIC" = LIIC, "LSIC" = LSIC) 
    return(resultados)
}

porcentaje <- seq(1, 100, 5)
recogedor2 <- data.frame(Media = 0, LIIC = 0, LSIC = 0, limite_mesa = 0)
for (i in 1:length(porcentaje)){
    resultados <- martingala_media(100, 10, 50, 18/37, 1000, porcentaje[i])
    recogedor2[i, 1] <- resultados[[1]]
    recogedor2[i, 2] <- resultados[[2]]
    recogedor2[i, 3] <- resultados[[3]]
    recogedor2[i, 4] <- porcentaje[i]
}

ggplot(recogedor2, aes(porcentaje, Media))+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymax = LSIC, ymin = LIIC))+
    geom_hline(yintercept = 100, linetype = "dashed", color = "turquoise3")+
    geom_text(aes(x = 60, y = 102), label = "B.I.", colour = "turquoise3")+
    theme_bw()+
    xlab("Porcentaje de ganancia sobre la bolsa inicial")+
    ylab("Media bolsa final")

#Tarea 6
inicio <- Sys.time()
vector <- numeric(length = 1000)
for (i in 1:1000){
    vector[i] <- i
}
fin <- Sys.time()
tarea1 <- fin-inicio

inicio <- Sys.time()
vector <- numeric()
for (i in 1:1000){
    vector[i] <- i
}
fin <- Sys.time()
tarea2 <- fin - inicio

tarea1; tarea2

#Tarea 7
a <- 1:10
inicio <- Sys.time()
a[a<4] <- 1
a[a>=4] <- 5
fin <- Sys.time()
tarea1 <- fin-inicio

a <- 1:10
inicio <- Sys.time()
for(i in 1:length(a)){
    if (a[i] < 4){
        a[i] <- 1
    }
    else{
        a[i] <- 5
    }
}
fin <- Sys.time()
tarea2 <- fin-inicio

#Tarea 8

inicio <- Sys.time()
x <- 1
for(i in 1:1000000){
    x = x+1
}
fin <- Sys.time()
tarea1 <- fin - inicio

inicio <- Sys.time()
x <- 1
repeat{
    x = x+1
    if (x == 1000001){
        break
    }
}
fin <- Sys.time()
tarea2 <- fin - inicio

inicio <- Sys.time()
x <- 1
while(x < 1000001){
    x = x+1
}
fin <- Sys.time()
tarea3 <- fin - inicio

#Tarea 9
mt <- matrix(1:10000, 100, 100)

inicio <- Sys.time()
suma <- numeric(length(ncol(mt)))
for(i in 1:nrow(mt)){
    suma <- suma + mt[i, ]
    media <- suma/nrow(mt)
}
fin <- Sys.time()
tarea1 <- fin - inicio


inicio <- Sys.time()
apply(mt, 1, mean)
fin <- Sys.time()
tarea2 <- fin - inicio

#Tarea 10
vector_p <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
inicio <- Sys.time()
mean(vector_p)
fin <- Sys.time()
tarea1 <- fin - inicio

vector_p <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
inicio <- Sys.time()
psych::describe(vector_p)
fin <- Sys.time()
tarea2 <- fin - inicio