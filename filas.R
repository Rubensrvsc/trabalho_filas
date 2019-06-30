## Limpando memÃ³ria ----
rm(list = ls())
gc()



## IntroduÃ§Ã£o Ã  simulaÃ§Ã£o ----

#http://www.uel.br/pessoal/mariogoto/pages/arquivos/SIMULAGE/2%20Monte%20Carlo.pdf

curve(expr = - x^4 + 10*x^3 + x^2 + x ,from= 0,to = 10)
# Qual % da Ã¡rea abaixo do grÃ¡fico?


minha_funcao <- function(x){
  return(- x^4 + 10*x^3 + x^2 + x)
}


# Valor mÃ¡ximo
valor_max <- max(minha_funcao(seq(0,10,by = .0001)))


# Ãrea total
area_total <- valor_max * (10-0) 


N_sim <- 1000
valores_aleatorios_x <- runif(N_sim,0,10) 
valores_aleatorios_y <- runif(N_sim,0,valor_max) 


points(valores_aleatorios_x,valores_aleatorios_y,col = 2, cex = .1)


# Verificando aqueles abaixo do grÃ¡fico
valores_aleatorios_y_funcao <- minha_funcao(valores_aleatorios_x)


selecionados <- valores_aleatorios_y < valores_aleatorios_y_funcao 


curve(expr = - x^4 + 10*x^3 + x^2 + x ,from= 0,to = 10)
points(valores_aleatorios_x[selecionados],
       valores_aleatorios_y[selecionados],col = 2, cex = .1)


## Aproximadamente

prop.table(table(selecionados))

### for/while/if ----

# criando matriz para armazenar resultados
N_for <- 10000
resultados <- matrix(0,ncol = 5 , nrow =  N_for)
set.seed(28052019)
i <- 1

valor_drift <- 0.05

valor_acumulado <- 0
for(i in 1:N_for){
  valor_aleatorio1 <- runif(1)
  resultados[i,1] <-  valor_aleatorio1
  valor_aleatorio2 <- runif(1)+ valor_drift
  resultados[i,2] <-  valor_aleatorio2
  
  
  ## IF ----
  ## teste se v1 < v2
  valor_soma_simples <- ifelse(valor_aleatorio1<valor_aleatorio2,
                               valor_aleatorio2-valor_aleatorio1,
                               0)
  
  ## teste se v1 < v2 (ideal para muitas operaÃ§Ãµes lÃ³gicas)
  if(valor_aleatorio1<valor_aleatorio2){
    valor_soma <- valor_aleatorio2-valor_aleatorio1
  }
  
  
  resultados[i,3] <-  valor_soma_simples 
  resultados[i,4] <-  valor_soma_simples   + valor_acumulado
  valor_acumulado <- resultados[i,4]
  
}


mean(resultados[,3])
quantile(resultados[,3])




### Leitura ----

library(readxl)
chegadas <- read.csv("C:\\Users\\ruben\\Downloads\\Estatística\\trabalho_filas\\base_filas-chegada.csv")
tempo_atendimento <- read.csv("C:\\Users\\ruben\\Downloads\\Estatística\\trabalho_filas\\base_filas-tempo_atendimento.csv")



## Ritmo de chegada ----

lambda <- mean(chegadas$nc)

## Tempo mÃ©dio entre chegadas
mu_chegadas <- 1/lambda


## Ritmo mÃ©dio de atendimento
tea <- mean(tempo_atendimento$x)

mu_atendimentos <- 1/tea



### SimulaÃ§Ã£o 1 (bÃ¡sica) ----
N <-  1000

# instante no tempo em 16h
t_max <- 60*(16-11 )

# semente aleatÃ³ria
set.seed(28052019)

# simulando tempo entre chegadas
tempos_chegadas <- rexp( N ,rate = mu_chegadas)

# verificando instantes no tempo
tempos_chegadas_acum <- cumsum(tempos_chegadas)

# Filtrando clientes que chegaram depois das 16h
tempos_chegadas_acum <- tempos_chegadas_acum[tempos_chegadas_acum<=t_max]

# nÃºmero de clientes entre 11h e 16h
n_clientes_entrada <- length(tempos_chegadas_acum)

# tempo de atendimento de cada cliente
tempos_atendimentos <- rexp(n = n_clientes_entrada,rate = mu_atendimentos)


# tempo de atendimento de cada cliente (considerando 1 atendente)
tempos_atendimentos_acum <- cumsum(tempos_atendimentos)


### Taxa de utilizaÃ§Ã£o dos atendentes ----

rho <- lambda/( 1 * mu_atendimentos )


### Visualizando o problema ----

plot(tempos_atendimentos_acum/60 + 11,tempos_chegadas_acum/60 + 11,
     type ="l",
     xlab = "Tempo de atendimento (1 atendente)",
     ylab = "Tempo de chegada ",
     main = "Instantes no tempo (10h Ã s 16h)")



# Calculando tempo de espera
tempo_espera <- tempos_atendimentos_acum - tempos_chegadas_acum


plot(1:n_clientes_entrada,
     tempo_espera,
     type ="l",
     xlab = "Cliente",
     ylab = "Tempo de espera ",
     main = "Tempo de espera por cliente (10h Ã s 16h)")



