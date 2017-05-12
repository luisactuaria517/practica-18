# practica-18
##,,,continuacion acciones netflix

install.packages("fpp")
require(fpp)

net<-read.csv(file.choose())

tnet<- ts(net, frequency =252, start=2016)
tnet


mode1<-ses(tnet, alpha=.2, initial="simple", h=4)
mode2<-ses(tnet, alpha=.4, initial="simple", h=4)
mode3<-ses(tnet, alpha=.6, initial="simple", h=4)
mode4<-ses(tnet, alpha=.8, initial="simple", h=4)

####Analisis de residuales###

#lo que se va a analizar de los residuales es que cumplan
#con las propiedades de residuales que consisten en:
#1) los residuales no esten correlacionados
#2) tienen media cero
#3) tienen varianza constante
#4) Los residuales estan normalmente distribuidos

##Para esto en r se tienen que obtener los residuales
##de cada uno de los 4 modelis con la finalidad de analizarlos
#y conocer su comportamiento

#Para lo anterior primero vamos a graficar nuestros datos 
#con los valores ajustados para despues extraer  los residuales de cada
#modelo y comenzar los analisis correspondientes

x11()

plot(mode1, ylab = "Año", main = "Acciones netflix", type="l")
lines(mode1$fitted, col="brown", type="l")
lines(mode2$fitted, col="red", type="l")
lines(mode3$fitted, col="green", type="l")
lines(mode4$fitted, col="blue", type="l")

res1<-residuals(mode1)
res2<-residuals(mode2)
res3<-residuals(mode3)
res4<-residuals(mode4)

plot(res1, ylab = "residuales", xlab = "netflix")
plot(res2, ylab = "residuales", xlab = "netflix")
plot(res3, ylab = "residuales", xlab = "netflix")
plot(res4, ylab = "residuales", xlab = "netflix")


hist(res1, nclass = "FD", main = "Histograma residuales 1")
hist(res2, nclass = "FD", main = "Histograma residuales 2")
hist(res3, nclass = "FD", main = "Histograma residuales 3")
hist(res4, nclass = "FD", main = "Histograma residuales 4")
#para conocer si es normal

#prueba de autocorrelacion a los residuales
#para analizar las autocorrelaciones hemos visto que utilizzamos
#la funcion acf en la cual nos muestra un correlograma que nos permite
#mostrar la relacion que existe entre los datos
#ademas de esta prueba que es una prueba grafica
#se utiliza la prueba de ljung box para esta prueba se utiliza la funcion
#box test que permite conocer la crrelacion que existe entre
#los residuales a traves de la prueba de hipotesis

##para obtener el correlograma de los residuales analizados
##se realizara de la siguiente manera

Acf(res1, main="residuales 1 netflix")
Acf(res2, main="residuales 2 netflix")
Acf(res3, main="residuales 3 netflix")
Acf(res4, main="residuales 4 netflix")

##se puede decir que para el modelo 4 no existe correlacion ya que cada linea esta dentro de los limites requeridos

##para coprobar la correlacion de los residuales utilizamos 
## una prueba dque se llama ljung box que nos permite saber con mayor 
#certeza si existe correlacion o no de los residuales

##box.test(residuales, desface, ljung box)
Box.test(res1, lag=10, type="Lj")
Box.test(res2, lag=10, type="Lj")
Box.test(res3, lag=10, type="Lj")
Box.test(res4, lag=10, type="Lj")

#el estadistico Q de Ljung box prueba la hipotesis nula de que las 
#autocorrelaciones de hasta un desfase k son iguales a cero (es decir
#los valores de los datos son aleatorios e independientes hasta un cierto
#numero de desfases). si  el LQB es mayor que un valor critico especificado,
#las autocorrelaciones para uno o mas desfases podrian ser estadisticamente
#significativas diferentes de cero, lo que indicaria que los valores no son 
# aleatorio ni independientes

##las etapas que se utilizan en un analisis de residuales son:
#1)elegir el modelo a traves de las medidas MAE y RMSE
#2) realizar las pruebas a los residuales de normalidad, independencia,
#y varianza constante una vez que las pruebas son viables ya se puede
#elegir el modelo como una opcion a considerar para realizar 
#los pronosticos necesarios.
