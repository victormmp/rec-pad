rm(list = ls())
require(RnavGraphImageData)
# Carregando a Base de dados
data( faces )
faces <- t( faces )
rotate <- function(x) t( apply(x, 2, rev) )

source("functionsImagem.R")

#Gerando os rotulos
y <- NULL
for(i in 1:nrow(faces) )
{
  y <- c( y, ((i-1) %/% 10) + 1 )
}

# Nomeando os atributos
nomeColunas <- NULL
for(i in 1:ncol(faces) )
{
  nomeColunas <- c(nomeColunas, paste("a", as.character(i), sep=".") )
}
colnames(faces) <- nomeColunas
rownames(faces) <- NULL

rm(nomeColunas)
# =========================

# Get random for tests

dim_classe <- 10

N <- sample(0.7 * dim_classe)
n <- sample(0.3 * dim_classe)
xtreino <- c()
ytreino <- c()
xteste <- c()
yteste <- c()

for()


for(i in N) {
  xtreino <- rbind(xtreino, t(faces[i,]))
  ytreino <- c(ytreino,(y[i]))
}

for(i in n) {
  xteste <- rbind(xteste, t(faces[i,]))
  yteste <- c(yteste,(y[i]))
}

#==============================







