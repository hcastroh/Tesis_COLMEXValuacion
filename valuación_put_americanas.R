################################################################################
####### Algoritmo de valiación de una put Americana 100% colateralizada  #######
################################################################################

list.of.packages <- c("tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#############################################################
########## Datos de la put Americana (anualizados) ##########
#############################################################

volatilidad <- 0.232515
S_0 <- 55.12
Strike <- 55.12
tiempo_para_T <-   61/260
tasa_libre_riesgo <- 0.07
tasa_libre_colateral <- 0.05

#############################################################
################### Settings del programa  ##################
#############################################################

repeticiones <- 1            #cuántas veces hará la valuación
maximos_pasos <- 1000        #número de paso del árbol

#############################################################
####################### El programa  ########################
#############################################################

Resultados <- matrix(c(rep(NA,(maximos_pasos-2)*repeticiones)), ncol = repeticiones)
Tiempos <- matrix(c(rep(NA,(maximos_pasos-2)*repeticiones)), ncol = repeticiones)

for (k in 1:repeticiones) {
  print(paste0("------------------------------------------------------------",k,"------------------------------"))
                    for (j in maximos_pasos:maximos_pasos) {
                      Inicio <- Sys.time()
                      n_Arbol <- j
                      B_inv <- exp(tasa_libre_riesgo*(as.numeric(tiempo_para_T)/n_Arbol))
                      B_cinv <- exp(tasa_libre_colateral*as.numeric(tiempo_para_T)/n_Arbol)
                      B <- B_inv^-1
                      B_c <- B_cinv^-1
                      u <- exp(volatilidad*sqrt(as.numeric(tiempo_para_T)/n_Arbol))
                      d <- exp(-volatilidad*sqrt(as.numeric(tiempo_para_T)/n_Arbol))
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
                      M1 <- cbind(M1, B_c*(M1 %*% q_matrix)) # B o B_c
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
                      Fin <- Sys.time()
                      print(paste0("Número de pasos ", j,"El resultado es ", as.numeric(select(get(nombre_nueva_matrix), Max))))
                      Resultados[j-2,k] <- as.numeric(select(get(nombre_nueva_matrix), Max))
                      Tiempos[j-2,k] <- seconds(Fin-Inicio)
                    }
}