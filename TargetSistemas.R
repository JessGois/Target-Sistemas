# Teste Estágio - Target Sistemas

#### Questão 1 ####
# Nomeando variáveis
INDICE <- 13
SOMA <- 0
K <- 0

# Loop 
while (K < INDICE) {
  K <- K + 1
  SOMA <- SOMA + K
}

# Valor de SOMA
SOMA


#### Questão 2 ####
# Criando a função Fibonacci
fibonacci <- function(n) {
  sequencia <- c(0, 1)
  while (TRUE) {
    proximo_valor <- sum(tail(sequencia, 2)) # Soma os últimos dois valores da sequência
    if (proximo_valor > n) {
      break
    }
    sequencia <- c(sequencia, proximo_valor)
  }
  if (n %in% sequencia) {
    return(paste(n, "pertence à sequência de Fibonacci."))
  } else {
    return(paste(n, "não pertence à sequência de Fibonacci."))
  }
}

# Função na prática
numero <- 72 #Número a ser informado
mensagem <- fibonacci(numero)

# Mensagem
print(mensagem)

#### Questão 3 ####
rm(list=ls())
pkgs = installed.packages()

if (!("jsonlite" %in% pkgs)) install.packages("jsonlite");
library(jsonlite)
if (!("dplyr" %in% pkgs)) install.packages("dplyr");
library(dplyr)

# Faturamento diário
faturamento_diario <- fromJSON("dados.json") %>%
  filter(valor > 0)

# Menor, maior e média do valor de faturamento
menor <- min(faturamento_diario$valor)
maior <- max(faturamento_diario$valor)
media <- mean(faturamento_diario$valor)

# Número de dias em que o faturamento diário foi superior à média mensal
acima_media <- sum(faturamento_diario$valor > media)

# Resultados
acima_media


#### Questão 4 ####
# Dados faturamento mensal distribuidora
Estado <- c("SP", "RJ", "MG", "ES", "Outros")
Faturamento <- c(67836.43, 36678.66, 29229.88, 27165.48, 19849.53)
distribuidora <- data.frame(Estado, Faturamento)

# Percentual de representação de cada estado
total <- sum(Faturamento)
percentual <- (Faturamento / total) * 100
percentual
print(paste(Estado, round(percentual,2),"%"))


#### Questão 5 ####
# Função para inverter string
inversa <- function(s) {
  chars <- strsplit(s, NULL)[[1]]
  chars_invertidos <- rev(chars)
  string_invertida <- paste(chars_invertidos, collapse = "")
  
  return(string_invertida)
}

# Usando a função
string_original <- "Target Sistemas" #String a ser informado
string_invertida <- inversa(string_original)
print(string_invertida)
