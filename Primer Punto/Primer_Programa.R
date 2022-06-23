#######TALLER 2########
      #PUNTO 1

#Exportar el conjunto de datos gapminder en formato "xlsx". El 10 %
#de los valores de las columnas lifeEx, pop, y gdpPercap se debe
#reemplazar de forma aleatoria por valores no asignados NA.


#install.packages("openxlsx")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("gapminder")
library(openxlsx)
library(gapminder)
library(ggplot2)
library(dplyr)
library(base)


main<-function(){
  print("1. Guardar gapminder ")
  print("2. Leer gapminder.xlsx ")
  print("3. Graficar el diagrama de dispersión lifeEx vs pop. ")
  print("4. Graficar el diagrama de dispersión gdpPercap vs pop.")
  print("5. Graficar los diagramas de cajas de la variable gdpPercap ") 
  print("   discriminados por continentes desde 1990 a 2007.")
  val <- as.integer(readline("Ingresar opción: "))
  
  if(val==1){
      #Encontrar las dimensiones
      d=dim(gapminder)
      d
      X=(0.1*d[1])
      X=as.integer(X)
      X
      indi1=sample(1:d[1],X)
      indi2=sample(1:d[1],X)
      indi3=sample(1:d[1],X)
      
      x2=list(indi1,indi2,indi3)
      
      gapminder$lifeExp[indi1]=NA
      gapminder$pop[indi2]=NA
      gapminder$gdpPercap[indi3]=NA
        
  }else if(val==2){
    
      #Importar el archivo gapminder en formato "xlsx".
      write.xlsx(gapminder,"gapminder1_open.xlsx")
      df<- read.xlsx("gapminder1_open.xlsx")
      df
      # x2=list(indi,indi2,indi3)
      cc =  1
      for (i in df[][4:6]){
        m1 <- mean(i, na.rm = TRUE)
        print(m1)
        indi = x2[cc]
        cc=cc+1
        i[indi[[1]]]=m1
      }
      
      m1 <- mean(df$lifeExp, na.rm = TRUE)
      df$lifeExp[indi]=m1
      m1 <- mean(df$gdpPercap, na.rm = TRUE)
      df$gdpPercap[indi]=m1
      m1 <- mean(df$lifeExp, na.rm = TRUE)
      df$lifeExp[indi]=m1
  }else if(val==3){
      #Graficar el diagrama de dispersión lifeEx vs pop.
      ggplot(df, aes(lifeExp, log(gdpPercap),col=continent)) + geom_point()
      ggplot(df, aes(lifeExp, log(gdpPercap),col=continent)) + geom_point()
  }else if(val==4){
      #El diagrama de dispersión gdpPercap vs pop.
      ggplot(df, aes(log(gdpPercap), log(pop),col=continent)) + geom_point()
      ggplot(df, aes(log(gdpPercap), log(pop),col=continent)) + geom_point()
  }else if(val==5){
      #Graficar los diagramas de cajas de la variable gdpPercap discrimina-
      #dos por continentes desde 1990 a 2007
      boxplot5=gapminder%>%select(gdpPercap,continent, year)%>%
        filter(year>=1990 )
      boxplot(gdpPercap~continent,data=boxplot5)
  }else{
    print("ingrese una opción valida")
  }
}
