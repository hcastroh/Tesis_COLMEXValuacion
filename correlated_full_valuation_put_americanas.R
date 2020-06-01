################################################################################
######################## Algoritmo de Full Valuation  #########################
################################################################################

list.of.packages <- c("tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

parametros <- read_csv("parametros_valuacion.csv")
n_correlated <- read_csv("aleatorios_correlacionados.csv")

Full_valuation <- matrix(rep(NA, 5), ncol = 5)
colnames(Full_valuation) <- c("accion", "MC", "R_i", "S_0", "put")

set.seed(12345)
for (accion in 1:6) {
  parametros[accion,]
  for (mc in 1:10000) {
    volatidad <- parametros[accion,]$VDia1
    R_i <- volatidad*sqrt(10)*n_correlated[mc,accion]
    S_0 <- parametros[accion,]$S_0*exp(R_i)
    Strike <- parametros[accion,]$Strike
    fecha_T <- (61-10)/260
    tiempo_para_T <- (61-10)/260
    repeticiones <- 1
    maximos_pasos <- 100
    volatidad <- parametros[accion,]$VAnual61
    k <- repeticiones
    j <- maximos_pasos
    n_Arbol <- j
    B_inv <- exp(0.07*(as.numeric(tiempo_para_T)/n_Arbol))
    B_cinv <- exp(0.05*as.numeric(tiempo_para_T)/n_Arbol)
    B <- B_inv^-1
    B_c <- B_cinv^-1
    u <- exp(volatidad*sqrt(as.numeric(tiempo_para_T)/n_Arbol))
    d <- exp(-volatidad*sqrt(as.numeric(tiempo_para_T)/n_Arbol))
    q <- (B_inv-d)/(u-d)
    q_matrix <- c(q, 1-q)
    n_nodos_finales <- n_Arbol+1
    M_nodos_finales <- as.tibble(matrix(data = NA, ncol = 2, nrow = n_nodos_finales))
    for (i in 0:n_Arbol) {
      M_nodos_finales[i+1,1] <- round(S_0*u^(n_Arbol-i)*d^i, 2)
      M_nodos_finales[i+1,2] <- round(max(Strike-S_0*u^(n_Arbol-i)*d^i,0), 2) #payoff de put
    }
    colnames(M_nodos_finales) <- c("Precio", "Payoff")
    M1 <- matrix(cbind(M_nodos_finales$Payoff[1:n_nodos_finales-1],M_nodos_finales$Payoff[2:n_nodos_finales]), ncol = 2)
    M1 <- cbind(M1, B_c*(M1 %*% q_matrix))
    M1 <- cbind(M1, M_nodos_finales[1:n_Arbol,1]*u^-1-Strike)
    M1$Precio <- ifelse(M1$Precio<0,0,M1$Precio)
    M1 <- mutate(M1, Max = pmax(`3`, Precio))
    M1 <- mutate(M1, Ejercer = ifelse(Precio == 0, 0, ifelse(Precio == Max, 1, 0)))
    colnames(M1) <- c("Arriba", "Abajo", "No_ejercer", "Ejercer", "Max", "Ejerce")
    for (i in 2:n_Arbol) {
      nombre_nueva_matrix <- paste0("M",i)
      nombre_anter_matrix <- paste0("M",i-1)
      assign(nombre_nueva_matrix, matrix(cbind(get(nombre_anter_matrix)[1:(nrow(get(nombre_anter_matrix))-1),5], get(nombre_anter_matrix)[2:nrow(get(nombre_anter_matrix)),5]), ncol = 2))
      assign(nombre_nueva_matrix, cbind(get(nombre_nueva_matrix), B_c*(get(nombre_nueva_matrix) %*% q_matrix)))
      assign(nombre_nueva_matrix, cbind(get(nombre_nueva_matrix), M_nodos_finales[1:(n_Arbol-(i-1)),1]*u^-i-Strike))
      assign(nombre_nueva_matrix, mutate(get(nombre_nueva_matrix), Precio = ifelse(Precio < 0, 0, Precio)))
      assign(nombre_nueva_matrix, mutate(get(nombre_nueva_matrix), Max = pmax(`3`, Precio)))
      assign(nombre_nueva_matrix, mutate(get(nombre_nueva_matrix), Ejercer = ifelse(Precio == 0, 0, ifelse(Precio == Max, 1, 0))))
      aux0 <- get(nombre_nueva_matrix)
      colnames(aux0) <- c("Arriba", "Abajo", "No_ejercer", "Ejercer", "Max", "Ejerce")
      assign(nombre_nueva_matrix, aux0)
    }
    print(paste0(parametros[accion,1], "  ", mc, "  ", round(R_i, 5), "  ", round(S_0, 5), "  ", round(M100$Max, 5)))
    Full_valuation <- rbind(Full_valuation, matrix(c(as.character(parametros[accion,1]), mc, as.numeric(R_i), as.numeric(S_0), M100$Max), ncol = 5))
  }
}
Full_valuation <- Full_valuation[-1,]
Full_valuation <- as.tibble(Full_valuation)