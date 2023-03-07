rm(list =ls())
setwd("Tarea Sistemas")
#install.packages("Metrics")
datos <- read.csv("datos.csv")
ponderacion_valores <- c(0.5, 0.3, 0.2)

summary(datos)

n <- as.matrix(datos[, c(grep("n", names(datos), value = TRUE))]) %*% ponderacion_valores 
v <- as.matrix(datos[, c(grep("v", names(datos), value = TRUE))]) %*% ponderacion_valores 
p <- as.matrix(datos[, c(grep("p", names(datos), value = TRUE))]) %*% ponderacion_valores 

ponderacion_cog <- c(0.5, 1.2)

cog <- as.matrix(cbind(v, n))
cog <- cog %*% ponderacion_cog


# Tarea 1
int <- cog + p

y_pred <- 1/(1+exp(-int))

datos$y_pred <- y_pred


# Tarea 2
Metrics::rmse(datos$y, datos$y_pred)
Metrics::mse(datos$y, datos$y_pred)


# Tarea 3

prediccion <- function(BetaCog, BetaP){
    int <- (cog*BetaCog)+(p*BetaP)
    y <- 1/(1+exp(-int))
    return(y)
}

secuencia <- seq(0.1, 2, 0.01)
rmse_t <- c()
mse_t <- c()
Betacog <- c()
Betap <- c()

for (i in 1:length(secuencia)){
    for(j in 1:length(secuencia)){
        Betacog <- append(Betacog, secuencia[i])
        Betap <- append(Betap, secuencia[j])
        y_pred <- prediccion(secuencia[i], secuencia[j])
        rmse <- Metrics::rmse(datos$y, y_pred)
        mse <- Metrics::mse(datos$y, y_pred)
        rmse_t <- append(rmse_t, rmse)
        mse_t <- append(mse_t, mse)
    }
}

df <- as.data.frame(cbind(Betacog, Betap, rmse_t, mse_t))
colnames(df) <- c("Beta_Cog", "Beta_P", "RMSE", "MSE")

df[df[, 3] == min(df[, 3]), ] 
df[df[, 4] == min(df[, 4]), ]
