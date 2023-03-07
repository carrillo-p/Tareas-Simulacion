#setwd("Tarea Dinamicos")
rm(list = ls())
library(ggplot2)

#Tarea 1

fact <- function(x){
    if (x == 0 | x == 1){
        return(1)
    }
    else{
        return(x*fact(x-1))
    }
}

#Tarea 2

fib <- function(x,y, conta = 0, stop=10){
    if (conta > stop) return("terminado")
    z = x+y
    print(z)
    fib(y,z, conta+1, stop)
}

fib(0,1, stop = 14)

#Tarea 3
SIR <- function(alfa, Beta, N, t){
    matriz <- matrix(nrow = t, ncol = 3)
    S = N
    I = 1
    R = 0
    for(i in 1:t){
        S <- rbinom(1, S, (1-alfa)^(I))
        R <- R + rbinom(1, I, Beta)
        I <- N + 1 - R - S
        matriz[i, 1] <- S
        matriz[i, 3] <- R
        matriz[i, 2] <- I
    }
    return(matriz)
}

set.seed(1)
m <- SIR(0.0005, 0.1, 10000, 50)
plot(m[,1], type="l", col="black")
lines(m[,2], type="l", col="red")
lines(m[,3], type="l", col="darkblue")
head(m)

#Tarea 4
SIDR <- function(alfa, bd, br, N, t){
    matriz <- matrix(nrow = t, ncol = 4)
    S = N
    I = 1
    D = 0
    R = 0
    N = N+1
    for(i in 1:t){
        S <- rbinom(1, S, (1-alfa)^(I))
        Dr <- rbinom(1, I, bd)
        D <- D + Dr
        I <- I - Dr
        R <- R + rbinom(1, I, br)
        I <- N - R - D - S
        matriz[i, 1] <- S
        matriz[i, 2] <- I
        matriz[i, 3] <- D
        matriz[i, 4] <- R
    }
    return(matriz)
}

set.seed(1)
m <- SIDR(alfa = 0.0001, bd = 0.04, br=0.06, N=10000, t=50)
plot(m[,1], type="l", col="black")
lines(m[,2], type="l", col="red")
lines(m[,3], type="l", col="darkblue")
lines(m[,4], type="l", col="lightblue")
head(m)

#Tarea 5

alfas <- seq(0.5, 0.9, 0.1)
betad <- seq(0.01, 0.05, 0.01)
betar <- seq(0.01, 0.15, 0.01)


alfa_t <- c()
betar_t <- c()
betad_t <- c()
muertes <- c()

for (i in 1:length(alfas)){
    for(j in 1:length(betad)){
        for(k in 1:length(betar)){
        alfa_t <- append(alfa_t, alfas[i])
        betad_t <- append(betad_t, betad[j])
        betar_t <- append(betar_t, betar[k])
        res <- SIDR(alfas[i], betad[j], betar[k], 10000, 50)
        muertes <- append(muertes, max(res[, 3]))
        }
    }
}


df <- as.data.frame(cbind(alfa_t, betad_t, betar_t, muertes))
colnames(df) <- c("alfa", "betaD", "betaR", "muertes")

mejor <- df[df$muertes == min(df$muertes), ] #mejor escenario
mejor_plot <- SIDR(mejor[, 1], mejor[, 2], mejor[, 3], 10000, 50)

plot(mejor_plot[,1], type="l", col="black", main = "Mejor resultado", ylim = c(0, 10000))
lines(mejor_plot[,2], type="l", col="red")
lines(mejor_plot[,3], type="l", col="darkblue")
lines(mejor_plot[,4], type="l", col="lightblue")

peor <- df[df$muertes == max(df$muertes), ] #peor escenario
peor_plot <- SIDR(peor[, 1], peor[, 2], peor[, 3], 10000, 50)

plot(peor_plot[,1], type="l", col="black", main = "Peor resultado", ylim = c(0, 10000))
lines(peor_plot[,2], type="l", col="red")
lines(peor_plot[,3], type="l", col="darkblue")
lines(peor_plot[,4], type="l", col="lightblue")

#Version paralelizada
require(doParallel)
cl <- makeCluster(6)
registerDoParallel(cl)

set.seed(1) #se añade una semilla para consistencia en los resultados
res_loop <- foreach(i = alfas, .combine = 'c') %:% foreach(j = betad, .combine = 'c') %:% foreach(k = betar, .combine = 'c') %dopar%{
    x <- SIDR(i, j, k, 10000, 50)
    y <- x[50, 3]
    y
}

m <- matrix(0, nrow = length(alfas)*length(betad)*length(betar), ncol = 4)
m[, 1] <- rep(alfas, each = length(betad)*length(betar))
m[, 2] <- rep(betad, each = length(betar)*length(alfas))
m[, 3] <- rep(betar, times = length(betad)*length(alfas))
m[, 4] <- res_loop

m <- as.data.frame(m)
colnames(m) <- c("alfa", "betaD", "betaR", "muertes")

mejor <- m[m$muertes == min(m$muertes), ] #mejor escenario
mejor_plot <- SIDR(mejor[, 1], mejor[, 2], mejor[, 3], 10000, 50)
plot(mejor_plot[,1], type="l", col="black", main = "Mejor resultado", ylim = c(0, 10000))
lines(mejor_plot[,2], type="l", col="red")
lines(mejor_plot[,3], type="l", col="darkblue")


peor <- m[m$muertes == max(m$muertes), ] #peor escenario
peor_plot <-SIDR(peor[, 1], peor[, 2], peor[, 3], 10000, 50)
plot(peor_plot[,1], type="l", col="black", main = "Peor resultado", ylim = c(0, 10000))
lines(peor_plot[,2], type="l", col="red")
lines(peor_plot[,3], type="l", col="darkblue")
lines(peor_plot[,4], type="l", col="lightblue")


stopCluster(cl)

#Tarea 6
require(caTools)

sumVecinos <- function(M,i,j, valor){
    f_min <- max(1, (i - 1))
    f_max <- min(nrow(M), (i + 1))
    c_min <- max(1, (j - 1))
    c_max <- min(ncol(M), (j + 1))
    sum(M[f_min:f_max, c_min:c_max]==valor)
}

lado = 50
alfa = 0.2
beta = 0.2
Bosque <- matrix(1, nrow=lado, ncol=lado) # el bosque
# prendemos fuego al arbol de la posicion 25, 25
ini <- matrix(c( 25, 25), ncol=2, byrow=T)

incendio <- function(M, a, b, ini, steps, filename = "incendio", gif = F){
    lado = nrow(M)
    for(i in 1:nrow(ini)){
        M[ini[i, 1], ini[i, 2]] <- 2
    }
    t_0 <- data.frame(S = length(which(M == 1)), I = length(which(M == 2)), R = length(which(M == 3)))
    M2 <- matrix(0, nrow = lado, ncol = lado)
    storage <- array(0, c(lado, lado, steps))
    recogedor <- matrix(0, nrow = steps, ncol = 3)
    for(k in 1:steps){
        for(i in 1:nrow(M)){
            for(j in 1:nrow(M)){
                if (M[i, j] == 0){
                    M2[i, j] <- 0
                }
                if (M[i, j] == 1){
                    M2[i, j] <- if(runif(1) > (1-a)**sumVecinos(M, i, j, 2)) 2 else 1
                }
                if (M[i, j] == 2){
                    M2[i, j] <- if(runif(1) < b) 3 else 2
                }
                if (M[i, j] == 3){
                    M2[i, j] <- 3
                }
            }
        }
        M = M2
        storage[, , k] <- M
        recogedor[k, 1] <- length(which(M == 1))
        recogedor[k, 2] <- length(which(M == 2))
        recogedor[k, 3] <- length(which(M == 3))
    }
    storage <- storage/max(storage)
    recogedor <- as.data.frame(recogedor)
    colnames(recogedor) <- c("S", "I", "R")
    recogedor <- rbind(t_0, recogedor)
    recogedor <- cbind(recogedor, data.frame(huecos = length(which(M == 0))))
    if (gif) write.gif(storage, filename = filename, col = "jet", delay = 50)
    return(recogedor)
}

t6 <- incendio(Bosque, a=alfa, b=beta, ini, 100, "incendio.gif", T) 

plot(t6[,1], type="l", col="green")
lines(t6[,2], type="l", col="red")
lines(t6[,3], type="l", col="black")



#Tarea 7
densidad <- c(0.2, 0.4, 0.6, 0.8)

p <- list()
for(i in 1:length(densidad)){
    Bosque <- matrix(rbinom(lado**2, 1, densidad[i]), nrow = lado, ncol = lado)
    Bosque[25, 25] <- 1
    ini <- matrix(c(25, 25), ncol = 2, byrow = T)
    df_densidad <- incendio(Bosque, a=alfa, b=beta, ini, 100, "incendio.gif", F)
    p[[i]] <- ggplot(df_densidad) +
        geom_line(aes(x = 1:101, y = S), color = "green")+
        geom_line(aes(x = 1:101, y = I), color = "red")+
        geom_line(aes(x = 1:101, y = R), color = "black")+
        xlab("t")+
        ylab("N")+
        ggtitle(paste("Densidad:", densidad[i]))+
        theme_bw()
}

do.call(gridExtra::grid.arrange, p)

#Tarea 8
Bosque <- matrix(rbinom(lado**2, 1, 0.6), nrow = lado, ncol = lado)
ini <- matrix(c( 25, 25), ncol=2, byrow=T)
ini2 <- matrix(c(2, 2, 45, 45), ncol = 2, byrow = T)

dos_focos <- incendio(Bosque, a=alfa, b=beta, ini2, 100, "incendio_unf.gif", F)
un_foco <- incendio(Bosque, a=alfa, b=beta, ini, 100, "incendio_dosf.gif", F)

max(dos_focos$R); max(un_foco$R)

#Tarea 9

#Mitad damero, mitad aleatoria
Bosque <- matrix(0, nrow = lado, ncol = lado)

for(i in 1:(nrow(Bosque)/2)){
    for(j in 1:ncol(Bosque)){
        if (i %% 2 == 0 & j %% 2 == 0){
            Bosque[i, j] <- 1
        }
        else if(i %% 2 != 0 & j %% 2 != 0){
            Bosque[i, j] <- 1
        } 
    }
}

for(i in ((nrow(Bosque)/2)+1):nrow(Bosque)){
    for(j in 1:ncol(Bosque)){
        Bosque[i, j] <- rbinom(1, 1, 0.6)
    }
}
ini <- matrix(c(25, 25), ncol = 2, byrow = T)
mitad_damero <- incendio(Bosque, a = alfa, b = beta, ini, 100, "incendio_dam.gif", T)

#Cortafuegos mitad
Bosque <-  matrix(rbinom(lado**2, 1, 0.8), nrow = lado, ncol = lado)
Bosque[23:26, ] <- 0

ini <- matrix(c(35, 35), ncol = 2, byrow = T)
cortafuegos <- incendio(Bosque, a = alfa, b = beta, ini, 100, "incendio_corta.gif", T)
