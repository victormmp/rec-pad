rm(list = ls())
require(RnavGraphImageData)
# Carregando a Base de dados
data( faces )
faces <- t( faces )
rotate <- function(x) t( apply(x, 2, rev) )

MostraImagem <- function( x )
{
  img <- matrix( x, nrow=64 )
  cor <- rev( gray(50:1/50) )
  image( rotate( img ), col=cor )
}
MostraImagem( faces[1,] )

#Gerando os r´otulos
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
