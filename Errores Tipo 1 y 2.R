rm(list = ls())
library(ggplot2)
# Tarea 1
asimetria <- seq(-3, 3, 0.2)
curtosis <- seq(0, 6, 0.2)
tmpList <- NULL
mt <- matrix(0, 0, 2)

for (i in asimetria){
    for(j in curtosis){
        tmpVar <- tryCatch(
            {
                SuppDists::JohnsonFit(c(0, 1, i, j), moment = "use")
                invisible(1)
            },
            error = function(cond){
                invisible(-1)
            }
        )
        tmpList <- rbind(tmpList, tmpVar)
        mt <- rbind(mt, (c(i, j)))
    }
}
tmpList <- as.vector(tmpList)
mt2 <- cbind(mt, tmpList)

mt3 <- matrix(0, 0, 2)
for(i in 1:nrow(mt2)){
    if(mt2[i, 3] == 1)
        mt3 <- rbind(mt3 , mt2[i,1:2])
}

mt3 <- as.data.frame(mt3)
colnames(mt3) <- c("Asimetria", "Curtosis")

ggplot(mt3, aes(x = Asimetria, y = Curtosis))+
    geom_point()+
    scale_x_continuous(breaks = seq(-3, 3, 0.2))+
    scale_y_continuous(breaks = seq(0, 6, 0.2))+
    theme_bw()

parms <- SuppDists::JohnsonFit(c(0, 1, 0, 3), moment = "use")
normal <- SuppDists::rJohnson(1000, parms)


plot(density(normal), main = "Distribucion normal")



parms <- SuppDists::JohnsonFit(c(0, 1, 0, 2), moment = "use")
plati <- SuppDists::rJohnson(1000, parms)

plot(density(plati), main = "Distribucion platicurtica")

parms <- SuppDists::JohnsonFit(c(0, 1, 0, 3), moment = "use")
meso <- SuppDists::rJohnson(1000, parms)

plot(density(meso), main = "Distribucion mesocurtica")

parms <- SuppDists::JohnsonFit(c(0, 1, 0, 6), moment = "use")
lepto <- SuppDists::rJohnson(1000, parms)

plot(density(lepto), main = "Distribucion leptocurtica")

parms <- SuppDists::JohnsonFit(c(0, 1, 1, 5), moment = "use")
a_pos <- SuppDists::rJohnson(1000, parms)

plot(density(a_pos), main = "Distribucion asimetrica positiva")

parms <- SuppDists::JohnsonFit(c(0, 1, -1, 5), moment = "use")
a_neg <- SuppDists::rJohnson(1000, parms)

plot(density(a_neg), main = "Distribucion asimetrica negativa")

#Tarea 2
## Uniforme
set.seed(1)
N = 1000 
n = 25 
k = 25000


poblacion <- runif(N, 2, 8)

mu.pob <- mean(poblacion)
sd.pob <- sd(poblacion)


means <- vector(length=k)

for (i in 1:k){
    muestra <- sample(poblacion, n)
    means[i] <- mean(muestra)
}

m.teo <- mu.pob
sd.teo <- sd.pob/sqrt(n)
m.emp <- mean(means)
sd.emp <- sd(means)

hist(means, main  = "Histograma de medias", xlab = "Medias")

sprintf("Media teorica: %.2f -- Media empirica : %.2f", m.teo, m.emp)
sprintf("Sd teorica: %.2f -- Sd empirica : %.2f", sd.teo, sd.emp)

## Chi cuadrado
set.seed(1)
N = 1000 
n = 25 
k = 25000

poblacion <- rchisq(N, 60)

mu.pob <- mean(poblacion)
sd.pob <- sd(poblacion)

means <- vector(length=k)

for (i in 1:k){
    muestra <- sample(poblacion, n)
    means[i] <- mean(muestra)
}

m.teo <- mu.pob
sd.teo <- sd.pob/sqrt(n)
m.emp <- mean(means)
sd.emp <- sd(means)
sprintf("Media teorica: %.2f -- Media empirica : %.2f", m.teo, m.emp)
sprintf("Sd teorica: %.2f -- Sd empirica : %.2f", sd.teo, sd.emp)

hist(means, main  = "Histograma de medias", xlab = "Medias")

# Tarea 3
set.seed(1)
N  = 1000
n1 = 30
n2 = 3
k= 25000

poblacion <- rbinom(N, 1, 0.5)

mu.pob <- mean(poblacion)
sd.pob <- sd(poblacion)

## Muestras grande

means <- vector(length=k)

for (i in 1:k){
    muestra <- sample(poblacion, n1)
    means[i] <- mean(muestra)
}


hist(means, main = "Histograma de Medias", xlab = "Medias")

means <- vector(length=k)

## Muestras pequeÃ±a

for (i in 1:k){
    muestra <- sample(poblacion, n2)
    means[i] <- mean(muestra)
}

hist(means, main = "Histograma de Medias", xlab = "Medias")

m.teo <- mu.pob
sd.teo <- sd.pob/sqrt(n2)
m.emp <- mean(means)
sd.emp <- sd(means)

sprintf("Media teorica: %.2f -- Media empirica : %.2f", m.teo, m.emp)
sprintf("Sd teorica: %.2f -- Sd empirica : %.2f", sd.teo, sd.emp)

#Tarea 4

set.seed(1)
N=1000  
n = 25 
k = 25000
poblacion <- rnorm(N, 5, 2)


mu.pob <- mean(poblacion)
sd.pob <- sd(poblacion)
var.pob <- sum((poblacion-mu.pob)^2)/N

## 4.1

varianza <- vector(length=k)
for (i in 1:k){
    muestra <- sample(poblacion, n)
    varianza[i] <- sum((muestra-mean(muestra))^2)/n
}
mean(varianza)
esperanza_var <- ((N-1)*sd.pob^2)/N

sprintf("La esperanza de la varianza sesgada: %.2f -- n-1*sd/n : %.2f", mean(varianza), esperanza_var)

## 4.2

varianza1 <- vector(length = k)
for (i in 1:k){
    muestra1 <- sample(poblacion, n)
    varianza1[i] <- sum((muestra1-mean(muestra1))^2)/(n-1) 
}
mean(varianza1)
var2 <- sd.pob^2

sprintf("La varianza insesgada: %.2f -- La varianza poblacional : %.2f", mean(varianza1), var2)

## 4.3 
varianza2 <- vector(length=k)

for (i in 1:k){
    muestra2 <- sample(poblacion, n)
    varianza_sesg <- sum((muestra2-mean(muestra2))^2)/n
    varianza2[i] <- n*varianza_sesg/sd.pob^2
}
var2 <- sd(varianza2)
var2 <- var2^2
var2_1 <- 2*(n-1)

sprintf("La varianza de nS^2/sd^2: %.2f -- 2*(n-1) : %.2f", var2, var2_1)

## 4.4 

plot(density(varianza2), type = 'l', main = "Diagrama de densidad")
lines(density(rchisq(k, 24)), lwd = '2', col = 'red')

#Tarea 5

## alfa 0.05
N <- 1000
n <- 25
k <- 25000

poblacion <- rchisq(N, 5, 2)
mu.pob <- mean(poblacion)
sd.pob <- sd(poblacion)

means <- vector(length = k)

for (i in 1:k){
    muestra <- sample(poblacion, n)
    means[i] <- mean(muestra)
}

LIIC <- mu.pob - (1.96*(sd.pob/sqrt(n)))
LSIC <- mu.pob + (1.96*(sd.pob/sqrt(n)))

total <- vector(length = k)
for(i in 1:k){
    if ((LIIC < means[i]) & (means[i] < LSIC)){
        total[i] <- 1
    }
    else{
        total[i] <- 0
    }
}

Porcentaje <- (sum(total)/k)*100

## alfa 0.01

N <- 1000
n <- 25
k <- 25000

poblacion <- rchisq(N, 5, 2)
mu.pob <- mean(poblacion)
sd.pob <- sd(poblacion)

means <- vector(length = k)

for (i in 1:k){
    muestra <- sample(poblacion, n)
    means[i] <- mean(muestra)
}

LIIC <- mu.pob - (2.58*(sd.pob/sqrt(n)))
LSIC <- mu.pob + (2.58*(sd.pob/sqrt(n)))

total <- vector(length = k)
for(i in 1:k){
    if ((LIIC < means[i]) & (means[i] < LSIC)){
        total[i] <- 1
    }
    else{
        total[i] <- 0
    }
}

Porcentaje <- (sum(total)/k)*100

# Tarea 6

rm(list = ls())
N = 100000
n = 25
k = 20000
repeticiones = 200
categorias <- c(2:15)

prueba <- function(categorias, N, n, k){
    poblacion <- sample(1:categorias, N, replace = T)
    mu.pob <- mean(poblacion)
    sd.pob <- sd(poblacion)
    p <- vector(length = k)
    for (i in 1:k){
        muestra <- poblacion[sample(1:N, n)]
        p[i] <- t.test(muestra, mu = mu.pob)$p.value
    }
    return(p)
}

a_emp <- numeric(length(repeticiones))
a_emp_tot <- numeric(length(categorias))
a_teo <- 0.05
p_tot <- matrix(0, nrow = k, ncol = repeticiones)
for(i in 1:length(categorias)){
    for(j in 1:repeticiones){
        p_tot[, j] <- prueba(categorias[i], N, n, k)
        a_emp[j] <- length(p_tot[p_tot[, j]<a_teo, j])/k
    }
    a_emp_tot[i] <- mean(a_emp)
}

df <- data.frame("categorias" = categorias, "alfa_empirico" = a_emp_tot, "alfa_teorico" = rep(a_teo, times = 14))

ggplot(df, aes(categorias, alfa_empirico))+
    geom_line()+
    geom_point()+
    geom_hline(yintercept = a_teo, linetype = 'dashed', colour = 'red')+
    scale_y_continuous(limits = c(0.04, 0.06))+
    theme_bw()

#Tarea 7

rm(list = ls())
N = 100000
n = c(2:50)
k = 20000

prueba <- function(N, n, k){
    poblacion <- rnorm(N, 10, 10)
    mu.pob <- mean(poblacion)
    sd.pob <- sd(poblacion)
    p <- vector(length = k)
    for (i in 1:k){
        muestra <- poblacion[sample(1:N, n)]
        p[i] <- t.test(muestra, mu = mu.pob)$p.value
    }
    return(p)
}

a_emp <- numeric(length(n))
a_teo <- 0.05
for(i in 1:length(n)){
    p <- prueba(N, n[i], k)
    a_emp[i] <- length(p[p<a_teo])/k
}

df <- data.frame("n" = n, "alfa_empirico" = a_emp, "alfa_teorico" = rep(a_teo, times = length(n)))

ggplot(df, aes(n, alfa_empirico))+
    geom_line()+
    geom_point()+
    geom_hline(yintercept = a_teo, linetype = 'dashed', colour = 'red')+
    theme_bw()

# Tarea 8

rm(list = ls())
N = 100000
n = 25
k = 20000
asimetria <- seq(-1, 1, 0.2)

prueba <- function(asimetria, N, n, k){
    parms <- SuppDists::JohnsonFit(c(0, 1, asimetria, 5), moment = "use")
    poblacion <- SuppDists::rJohnson(N, parms)
    mu.pob <- mean(poblacion)
    sd.pob <- sd(poblacion)
    p <- vector(length = k)
    for (i in 1:k){
        muestra <- poblacion[sample(1:N, n)]
        p[i] <- t.test(muestra, mu = mu.pob)$p.value
    }
    return(p)
}

a_emp <- numeric(length(asimetria))
a_teo <- 0.05
for(i in 1:length(asimetria)){
    p <- prueba(asimetria[i], N, n, k)
    a_emp[i] <- length(p[p<a_teo])/k
}

df <- data.frame("Asimetria" = asimetria, "alfa_empirico" = a_emp)

ggplot(df, aes(asimetria, alfa_empirico))+
    geom_line()+
    geom_point()+
    geom_hline(yintercept = a_teo, linetype = 'dashed', colour = 'red')+
    #scale_y_continuous(limits = c(0.04, 0.06))+
    scale_x_continuous(limits = c(-1, 1), breaks = c(-1, -0.8, -0.6,-0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1))+
    theme_bw()

# Tarea 9

rm(list = ls())
library(pwr)
library(ggplot2)

set.seed(1)
N = 100000
n = 25 
k = 20000 
a_teo = 0.05 
repeticiones <- 10 
categorias <- 2:15

prueba <- function(categorias, N, n, k){
    poblacion <- sample(1:categorias, N, replace = T) # Crea la poblacion en base a las categorias que se indiquen
    mu.pob <- mean(poblacion)
    sd.pob <- sd(poblacion)
    p <- vector(length = k)
    media <- vector(length = k) # Media de k medias
    for (i in 1:k){
        muestra <- poblacion[sample(1:N, n)]
        media[i] <- mean(muestra)
        p[i] <- t.test(muestra, mu = mu.pob)$p.value
    }
    media_muestra <- mean(media) # Media con la que se comprueba el supuesto
    parametros <- list("p" = p, "mu_pob" = mu.pob, 
                       "mu_muestra" = media_muestra, "sd_pob" = sd.pob) 
    return(parametros)
}

#Recogedores que dependen de las repeticiones de la simulacion
beta_emp <- numeric(length(repeticiones))
p_tot <- matrix(0, nrow = k, ncol = repeticiones) 
sd.pob <- vector(length = length(repeticiones))
media <- vector(length = length(repeticiones))
mu.pob <- vector(length = length(repeticiones))

#Recogedore que dependen del numero de categorias
beta_emp_tot <- numeric(length(categorias))
d <- vector(length = length(categorias))
sd_tot <- vector(length =  length(categorias))
media_tot <- vector(length = length(categorias))
media_mues_tot <- vector(length = length(categorias))


for(i in 1:length(categorias)){
    for(j in 1:repeticiones){
        parms <- prueba(categorias[i], N, n, k)
        p_tot[, j] <- parms$p 
        media[j] <- parms$mu_muestra #Media muestral de comparacion
        mu.pob[j] <- parms$mu_pob 
        sd.pob[j] <- parms$sd_pob
        beta_emp[j] <- length(p_tot[p_tot[, j]>a_teo, j])/k  
    }
    media_tot[i] <- mean(mu.pob)
    media_mues_tot[i] <- mean(media)
    sd_tot[i] <- mean(sd.pob)
    beta_emp_tot[i] <- mean(beta_emp)
    d[i] <- (media_tot[i]-media_mues_tot[i])/sd_tot[i]
}

beta_teo <- vector(length = length(categorias))

for (i in 1:length(categorias)){
    beta_teo[i] <- 1-pwr.t.test(n,
                                d=d[i],
                                sig.level = a_teo,
                                type = "one.sample")$power
}

f_res <- data.frame("BT" = beta_teo, "BE" = beta_emp_tot)

df1 <- data.frame("Tipo" = "Beta Teorica", "Valor" = beta_teo)
df2 <- data.frame("Tipo" = "Beta Empirica", "Valor" = beta_emp_tot)
df <- rbind(df1, df2)
df <- cbind(df, data.frame("Numero_categorias" = categorias))
df$Tipo <- as.factor(df$Tipo)

ggplot(df, aes(x = Numero_categorias, y = Valor))+
    geom_line(aes(color = Tipo), size = 0.8)+
    ylim(0.85, 1)+
    theme_bw()

# Tarea 10

rm(list = ls())
library(pwr)
library(ggplot2)

N = 100000 
n = 2:50 
k = 20000
a_teo = 0.05 
repeticiones <- 10 

prueba <- function(N, n, k){
    poblacion <- rnorm(N, 10, 10) 
    mu.pob <- mean(poblacion)
    sd.pob <- sd(poblacion)
    p <- vector(length = k)
    media <- vector(length = k) 
    for (i in 1:k){
        muestra <- poblacion[sample(1:N, n)]
        media[i] <- mean(muestra)
        p[i] <- t.test(muestra, mu = mu.pob)$p.value
    }
    media_muestra <- mean(media) 
    parametros <- list("p" = p, "mu_pob" = mu.pob, 
                       "mu_muestra" = media_muestra, "sd_pob" = sd.pob) 
    return(parametros)
}

#Recogedores que dependen de las repeticiones de la simulacion
beta_emp <- numeric(length(repeticiones))
p_tot <- matrix(0, nrow = k, ncol = repeticiones) 
sd.pob <- vector(length = length(repeticiones))
media <- vector(length = length(repeticiones))
mu.pob <- vector(length = length(repeticiones))

#Recogedore que dependen del numero de categorias
beta_emp_tot <- numeric(length(n))
d <- vector(length = length(n))
sd_tot <- vector(length =  length(n))
media_tot <- vector(length = length(n))
media_mues_tot <- vector(length = length(n))


for(i in 1:length(n)){
    for(j in 1:repeticiones){
        parms <- prueba(N, n[i], k)
        p_tot[, j] <- parms$p 
        media[j] <- parms$mu_muestra 
        mu.pob[j] <- parms$mu_pob 
        sd.pob[j] <- parms$sd_pob 
        beta_emp[j] <- length(p_tot[p_tot[, j]>a_teo, j])/k  
    }
    media_tot[i] <- mean(mu.pob)
    media_mues_tot[i] <- mean(media)
    sd_tot[i] <- mean(sd.pob)
    beta_emp_tot[i] <- mean(beta_emp)
    d[i] <- (media_tot[i]-media_mues_tot[i])/sd_tot[i]
}

beta_teo <- vector(length = length(n))
for (i in 1:length(n)){
    beta_teo[i] <- 1-pwr.t.test(n[i],
                                d=d[i],
                                sig.level = a_teo,
                                type = "one.sample")$power
}

df_res <- data.frame("BT" = beta_teo, "BE" = beta_emp_tot)

df1 <- data.frame("Tipo" = "Beta Teorica", "Valor" = beta_teo)
df2 <- data.frame("Tipo" = "Beta Empirica", "Valor" = beta_emp_tot)
df <- rbind(df1, df2)
df <- cbind(df, data.frame("Tamaño_n" = n))
df$Tipo <- as.factor(df$Tipo)

ggplot(df, aes(x = Tamaño_n, y = Valor))+
    geom_line(aes(color = Tipo), size = 0.8)+
    ylim(0.85, 1)+
    theme_bw()

# Tarea 11
rm(list = ls())
library(pwr)
library(ggplot2)

N = 100000
n = 25 
k = 20000
a_teo = 0.05 
asimetria <- seq(-1, 1, 0.2)
repeticiones <- 10 # Opcional si hay repeticiones

prueba <- function(asimetria, N, n, k){
    parms <- SuppDists::JohnsonFit(c(0, 1, asimetria, 5), moment = "use")
    poblacion <- SuppDists::rJohnson(N, parms)
    mu.pob <- mean(poblacion)
    sd.pob <- sd(poblacion)
    p <- vector(length = k)
    media <- vector(length = k) # Media de las k medias
    for (i in 1:k){
        muestra <- poblacion[sample(1:N, n)]
        media[i] <- mean(muestra)
        p[i] <- t.test(muestra, mu = mu.pob)$p.value
    }
    media_muestra <- mean(media) 
    parametros <- list("p" = p, "mu_pob" = mu.pob, 
                       "mu_muestra" = media_muestra, "sd_pob" = sd.pob) 
    return(parametros)
}

#Recogedores que dependen de las repeticiones de la simulacion
beta_emp <- numeric(length(repeticiones))
p_tot <- matrix(0, nrow = k, ncol = repeticiones) 
sd.pob <- vector(length = length(repeticiones))
media <- vector(length = length(repeticiones))
mu.pob <- vector(length = length(repeticiones))

#Recogedore que dependen del numero de categorias
beta_emp_tot <- numeric(length(asimetria))
d <- vector(length = length(asimetria))
sd_tot <- vector(length =  length(asimetria))
media_tot <- vector(length = length(asimetria))
media_mues_tot <- vector(length = length(asimetria))


for(i in 1:length(asimetria)){
    for(j in 1:repeticiones){
        parms <- prueba(asimetria[i], N, n, k)
        p_tot[, j] <- parms$p 
        media[j] <- parms$mu_muestra 
        mu.pob[j] <- parms$mu_pob
        sd.pob[j] <- parms$sd_pob 
        beta_emp[j] <- length(p_tot[p_tot[, j]>a_teo, j])/k  
    }
    media_tot[i] <- mean(mu.pob)
    media_mues_tot[i] <- mean(media)
    sd_tot[i] <- mean(sd.pob)
    beta_emp_tot[i] <- mean(beta_emp)
    d[i] <- (media_tot[i]-media_mues_tot[i])/sd_tot[i]
}

beta_teo <- vector(length = length(asimetria))
for (i in 1:length(asimetria)){
    beta_teo[i] <- 1-pwr.t.test(n,
                                d=d[i],
                                sig.level = a_teo,
                                type = "one.sample")$power
}

df_res <- data.frame("BT" = beta_teo, "BE" = beta_emp_tot)

df1 <- data.frame("Tipo" = "Beta Teorica", "Valor" = beta_teo)
df2 <- data.frame("Tipo" = "Beta Empirica", "Valor" = beta_emp_tot)
df <- rbind(df1, df2)
df <- cbind(df, data.frame("Asimetria" = asimetria))
df$Tipo <- as.factor(df$Tipo)

ggplot(df, aes(x = Asimetria, y = Valor))+
    geom_line(aes(color = Tipo), size = 0.8)+
    ylim(0.9, 0.98)+
    scale_x_continuous(limits = c(-1, 1), breaks = c(-1, -0.8, -0.6,-0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1))+
    theme_bw()
