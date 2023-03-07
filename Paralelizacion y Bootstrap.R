rm(list = ls())
require(parallel)
library(foreach)
library(doParallel)

cl <- makeCluster(6) #Depende del número de nucleos disponibles
registerDoParallel(cl)

N = 1000
n = 100

pob <- rnorm(N, 5, 2)
m.pob = mean(pob)
sd.pob = sd(pob)
muestra <- sample(pob, n)

nBoot = 10000
nsub = 25

#Tarea 1
### Funcion
boot_media <- function(nBoot, muestra, nsub){
    res <- vector(length = nBoot)
    res <- foreach(i = 1:nBoot, .combine = 'c') %dopar%{
        x <- sample(muestra, nsub, replace = T)
        mean(x)
    }
    return(res)
}

res <- boot_media(nBoot, muestra, nsub)


sprintf("media poblacion = %.4f, media bootstrap = %.4f", m.pob, mean(res))

sprintf("sd/sqrt(n) = %.4f, sd bootstrap = %.4f", sd.pob/sqrt(nsub), sd(res))

### Forma bucle


res2 <- foreach(i = 1:nBoot, .combine = 'c') %dopar%{
    x <- sample(muestra, nsub, replace = T)
    mean(x)
}

sprintf("media poblacion = %.4f, media bootstrap = %.4f", m.pob, mean(res2))

sprintf("sd/sqrt(n) = %.4f, sd bootstrap = %.4f", sd.pob/sqrt(nsub), sd(res2))

# Tarea 2
###Funcion
boot_var <- function(nBoot, muestra, nsub){
    res <- vector(length = nBoot)
    res <- foreach(i = 1:nBoot, .combine = 'c') %dopar%{
        x <- sample(muestra, nsub, replace = T)
        var(x)
    }
    return(res)
}
### Bucle
res <- foreach(i = 1:nBoot, .combine = 'c') %dopar%{
    x <- sample(muestra, nsub, replace = T)
    var(x)
}

res <- boot_var(nBoot, muestra, nsub)

a <- qchisq(p= 0.975, df=24, lower.tail=FALSE)
b <- qchisq(p=.025, df=24, lower.tail=FALSE)

LI <- ((nsub-1)*mean(res))/b
LS <- ((nsub-1)*mean(res))/a


sprintf("Limite inferior = %.4f, Limite superior = %.4f", LI, LS)

sprintf("Limite inferior = %.4f, Estimate = %.4f, Limite superior = %.4f", quantile(res, 0.025), quantile(res, 0.5), quantile(res, 0.975))

#Tarea 3
N = 1000
nsub <- 1:25
n <- 1:100
nBoot <- 1000

pob <- rnorm(N, 5, 2)
m.pob = mean(pob)
sd.pob = sd(pob)

muestra <- foreach(i = n) %dopar%{
    muestra <- sample(pob, i)
    muestra
}

#Funcion

boot_media <- function(nBoot, muestra, nsub){
    res <- vector(length = length(nsub)*length(n))
    res <- foreach(j = n) %:% foreach(k = nsub) %:% foreach(i = 1:nBoot, .combine = 'c') %dopar%{
        x <- sample(muestra[[j]], k, replace = T)
        mean(x)
    }
    res2 <- vector(length = length(n)*length(nsub))
    res2 <- foreach(j = n, .combine = 'rbind') %:% foreach(k = nsub, .combine = 'c') %dopar%{
        mean(res[[j]][[k]])
    }
    return(res2)
}

#Bucle

res_loop <- foreach(j = n) %:% foreach(k = nsub) %:% foreach(i = 1:nBoot, .combine = 'c') %dopar%{
    x <- sample(muestra[[j]], k, replace = T)
    mean(x)
}

res2_loop <- foreach(j = n, .combine = 'rbind') %:% foreach(k = nsub, .combine = 'c') %dopar%{
    mean(res_loop[[j]][[k]])
}


resultados <- boot_media(nBoot, muestra, nsub)

resultados <- as.data.frame(resultados)
colnames(resultados) <- paste0("nsub", seq(1:length(nsub)))
rownames(resultados) <- paste0("n", seq(1:length(n)))
resultados$tamano_m <- seq(1:100)

library("tidyverse")
df <- resultados %>%
    select(everything()) %>%
    gather(key = "variable", value = "value", -tamano_m)
head(df)


ggplot(df, aes(x = tamano_m, y = value))+
    geom_line(aes(colour = variable, linetype = variable), size = 0.8)+
    geom_hline(yintercept = m.pob, linetype = "dashed", colour = "red")+
    labs(color = "Tamaño submuestras")+
    xlab("Tamaño muestral")+
    ylab("Media Estimada")+
    theme_bw()


#Tarea 4.1

#Funcion

datos <- read.csv("datos.csv")
ponderacion_valores <- c(0.5, 0.3, 0.2)

n <- as.matrix(datos[, c(grep("n", names(datos), value = TRUE))]) %*% ponderacion_valores 
v <- as.matrix(datos[, c(grep("v", names(datos), value = TRUE))]) %*% ponderacion_valores 
p <- as.matrix(datos[, c(grep("p", names(datos), value = TRUE))]) %*% ponderacion_valores 

ponderacion_cog <- c(0.5, 1.2)

cog <- as.matrix(cbind(v, n))
cog <- cog %*% ponderacion_cog

datos$cog <- cog
datos$p <- p
datos2 <- datos[, c("cog", "p", "y")]

nBoot = 10000
nsub = 25

betas <- function(nBoot, nsub, datos){
    matriz <- array(0, dim = c(3, 2))
    res <- foreach(i = 1:nBoot, .combine = 'rbind') %dopar%{
        df <- datos[sample(1:nrow(datos), nsub, replace = T), ]
        fit <- lm(y~cog+p, df)
        coff <- fit$coefficients
    }
    ic <- foreach(i = 1:3, .combine = 'rbind') %dopar%{
        matriz[i, ] <- c((mean(res[,i])-1.96*(sd(res[,i])/nBoot)), (mean(res[,i])+1.96*(sd(res[,i])/nBoot))) 
    }
    return(ic)
}

res_ic <- betas(nBoot, nsub, datos2)

#Bucle

res <- foreach(i = 1:nBoot, .combine = 'rbind') %dopar%{
    df <- datos[sample(1:nrow(datos), nsub, replace = T), ]
    fit <- lm(y~cog+p, df)
    coff <- fit$coefficients
}

matriz <- array(0, dim = c(3, 2))

ic <- foreach(i = 1:3, .combine = 'rbind') %dopar%{
    matriz[i, ] <- c((mean(res[,i])-1.96*(sd(res[,i])/nBoot)),
                     (mean(res[,i])+1.96*(sd(res[,i])/nBoot)))
}

#Tarea 4.2

#Parallel
inicio <- Sys.time()
res <- foreach(i = 1:nBoot, .combine = 'rbind') %dopar%{
    df <- datos[sample(1:nrow(datos), nsub, replace = T), ]
    fit <- lm(y~cog+p, df)
    coff <- fit$coefficients
}

matriz <- array(0, dim = c(3, 2))

ic <- foreach(i = 1:3, .combine = 'rbind') %dopar%{
    matriz[i, ] <- c((mean(res[,i])-1.96*(sd(res[,i])/nBoot)),
                     (mean(res[,i])+1.96*(sd(res[,i])/nBoot)))
}
fin <- Sys.time()
tarea <- fin-inicio
tarea1 <- fin - inicio

#For
B=array(0,dim=c(nBoot, 3))
matriz <- matrix(0, nrow = 3, ncol = 2)

inicio <- Sys.time()
for(i in 1:nBoot){
    df <- datos2[sample(1:nrow(datos2), nsub, replace = T), ]
    fit <- lm(y~cog+p, df)
    B[i, ] <- fit$coefficients
}

for(j in 1:3){
    matriz[j, ] <- c((mean(B[, j]) - 1.96*(sd(B[, j])/sqrt(nBoot))),
                     ((mean(B[, j]) + 1.96*(sd(B[, j])/sqrt(nBoot)))))
}
fin <- Sys.time()
tarea2 <- fin - inicio
stopCluster(cl)

tarea1; tarea2