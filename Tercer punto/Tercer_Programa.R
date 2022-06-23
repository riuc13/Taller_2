# 3. Implementar un programa con las siguientes opciones:
# Graficar la función de densidad de una distribución uniforme.
# Graficar la función de densidad de una distribución Bernoulli.
# Graficar la función de densidad de una distribución Poisson.
# Graficar la función de densidad de una distribución Exponencial.
library(base)
library(ggplot2)
Densidad<-function(){
 
  print("1. Graficar la función de densidad de una distribución uniforme.")
  print("2. Graficar la función de densidad de una distribución Bernoulli. ")
  print("3. Graficar la función de densidad de una distribución Poisson. ")
  print("4. Graficar la función de densidad de una distribución Exponencial. ")
  val <- as.integer(readline("Ingresar opción: "))
  if(val==1){
          #valores al azar de la distribución normal
          mu <- as.double(readline("Ingresar media: "))
          dev <- as.double(readline("Ingresar desviación estándar: "))
          npts <- as.integer(readline("Ingresar número de puntos: "))
          randNorm <- rnorm(npts,mu,dev)
          #calculo de su densidad
          DensidadNormal <- dnorm(randNorm,mu,dev)
          
          #gráfica
              ggplot(data.frame(x = randNorm, y =DensidadNormal)) + 
                      aes(x = x, y = y) +
                           geom_point() + 
                           labs(x = "Random Normal", y = "Densidad")

  }else if(val==2){
         eje <- as.integer(readline("Ingrese el  valores Maximo del eje X: "))
         n <- as.integer(readline("Ingresar el número de ensayos(n>0): "))
          while (n<0) {
           print("Ingrese un valor valido para n: ")
           n <- as.integer(readline("Ingresar el número de ensayos(n>0): "))
          }
         pro <- as.double(readline("Ingresar la probabilidad de éxito de cada ensayo (entre 0 y 1): "))
         while (pro<0 & pro>1) {
           print("Ingrese una probailidad valida: ")
           pro <- as.double(readline("Ingresar la probabilidad de éxito de cada ensayo (entre 0 y 1): "))
         }
       bin<-rbinom(eje,n, pro)
    
       #Visualización de la distribución binomial
       ggplot(as.data.frame(bin), mapping = aes(bin))+geom_histogram(binwidth =pro/10, 
                    color="firebrick3", fill="firebrick3")+
        labs(title = "Distribusión binomial o distribucion Bernoulli ")
    
  }else if(val==3){
    lambda <- as.integer(readline(" Ingrese el número promedio de eventos esperados por unidad de tiemp: "))
    a<- as.integer(readline("Ingresar el número Maximo de de eventos: "))
    x_pois <- 0:a
    y_dpois <- dpois(x_pois, lambda) #Algoritmo que calcula la distribución de densidad
    datos_dpois <- data.frame(x_pois, y_dpois)
    #Dibujamos la gráfica
    ggplot(datos_dpois, aes(x=x_pois, y=y_dpois)) + 
      geom_line(colour="blue")+
      ggtitle("Función de densidad de una distribución Poisson ")+
      theme_minimal()
  }else if(val==4){
    eje1 <- as.integer(readline("Ingrese los valores del eje: ")) 
    l <- as.integer(readline("ingrese el valor de lambda (l>0): "))
    while (l<0) {
      print("Ingrese un valor valido para n: ")
      l <- as.integer(readline("ingrese el valor de lambda (l>0): "))
    }
    x_exp <- seq(0,eje1, by= 0.5) 
    y_dexp <- dexp(x_exp, rate=l) #Algoritmo que calcula la distribución de densidad
    datos_dexp <- data.frame(x_exp, y_dexp)
    
    #Dibujamos la gráfica
    
    ggplot(datos_dexp, aes(x=x_exp, y=y_dexp )) + 
      geom_line(colour="blue")  + 
      ggtitle ("Función exponencial de densidad") + 
      theme_minimal()
  } else{
    print("ingrese una opcion correcta")
   }
  }
 

