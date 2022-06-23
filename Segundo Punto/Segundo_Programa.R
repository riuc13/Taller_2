####Creacion del menu

Programa <- function(){
  
  print("1 Ver el archivo Experimento_a.csv ")
  print("2 ver el archivo Experimento_b.csv")
  print("3 Desea saber si la media de los datos es especificamente significativa? ")
  print("4 Ver la correlacion de Pearson y Spearman")
  print("5 Graficar el diagrama de difraccion y la linea recta que aproxime los datos calculada por una regecion lineal por minimos cuadrados")
  val <- as.integer(readline("Ingresar opción: "))
  
  if (val==1){
    Mi_archivoA <- read.csv("Experimento_a.csv", header = FALSE)
    print(paste("los elementos del archivo Experimento_a.csv son : ",Mi_archivoA))
    
  }
  
  if (val==2){
    Mi_archivoB <- read.csv("Experimento_b.csv", header = FALSE)
    print(paste("los elementos del archivo Mi_archivoB son : ",Mi_archivoB))
    
  }
  if(val==3){
    Mi_archivoA <- read.csv("Experimento_a.csv", header = FALSE)
    Mi_archivoB <- read.csv("Experimento_b.csv", header = FALSE)
    print(paste("La media de los elementos de mi archivo Experimento_a.csv es: ",sum(Mi_archivoA)/length(Mi_archivoA)))
    print(paste("La media de los elemntos de mi archivo Experimento_b.csv es: ",sum(Mi_archivoB)/length(Mi_archivoB)))
    diferencia <- (sum(Mi_archivoA)/length(Mi_archivoA)) - (sum(Mi_archivoB)/length(Mi_archivoB))
    print(paste("La diferencia de las media es: ",diferencia*-1))
  }
  
  if(val==4){
    Mi_archivoA <- read.csv("Experimento_a.csv", header = FALSE)
    Mi_archivoB <- read.csv("Experimento_b.csv", header = FALSE)
    
    a<- cor(x=Mi_archivoA,y=Mi_archivoB)
    print(paste("La correlacion de Pearson es: ",a))

    b<- cor(x=Mi_archivoA,y=Mi_archivoB,  method=c("spearman"))
    print(paste("La correlacion de Spearman es: ",b))
    
  } 
  
  if(val==5){
    Mi_archivoA <- read.csv("Experimento_a.csv", header = FALSE)
    Mi_archivoB <- read.csv("Experimento_b.csv", header = FALSE)
    a<- cor(x=Mi_archivoA,y=Mi_archivoB)
    b<- cor(x=Mi_archivoA,y=Mi_archivoB,  method=c("spearman"))
    
    Dat <- data.frame(Vec1 = Mi_archivoA,
                      Vec2 = Mi_archivoB)
    plot(Dat[,1], Dat[,2], main="Gráfica 01.1a.: Diagrama de Dispersión")
    abline(lm(Dat[,2]~Dat[,1]), col="red")
  }
  
  else{
    print("Muchas gracias")
  }
}

Menu2<-Programa()
5