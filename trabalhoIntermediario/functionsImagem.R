MostraImagem <- function( x )
{
  img <- matrix( x, nrow=64 )
  cor <- rev( gray(50:1/50) )
  image( rotate( img ), col=cor )
}
# MostraImagem( faces[1,] )
