print("Punto 1 del taller ")
numero =3
a = [[1,2,3], [4,5,6],[7,8,9]]
print("Ingrese 0 si quiere sumar la diagonal inferior de la matriz")
opcion = input("Ingrese 1 si quiere sumar la diagonal superior de la matriz")
iteraciones =0
sum =0
opcion = int(opcion)
numero = int(numero)

if(opcion==0):
  for i in range(numero):
    for j in range(i):
      iteraciones=iteraciones +1
      if(i>0):
       sum =sum +a[i][j]
elif(opcion ==1):
  for i in range(numero):
    for j in range(numero):
      if(i<numero and j>i):
        iteraciones=iteraciones +1
        sum =sum +a[i][j]

print("El resuldato de la suma es: ",sum)
print("Para una matriz del tamaño: ", numero,"La canditad de iteraciones son: ",iteraciones)

print("Punto 2 del taller ")
n = input("Ingrese el numero para sumar los primeros n^2 numeros ")

n=int(n)
sumaCuadrados =0
iteracionesCuadrado =0
i=0
for i in range(n+1):
  iteracionesCuadrado=iteracionesCuadrado+1
  sumaCuadrados = sumaCuadrados + i*i

print("Para el numero: ", n,"La canditad de iteraciones para elevar al cuadrado son: ",iteracionesCuadrado,"Y el resultado es: ", sumaCuadrados)

import math as mt
print("Punto 3.1")
def f(x):
    return mt.log(x+2) - mt.sin(x)

def secante(E,x_1,x_2):
    error=mt.inf
    errorx = []
    cont=0
    contIt=0
    cont2 = 1 
    while(error>E):
        print(x_1, x_2)
        cont=cont+1
        nx=(x_1-((f(x_1)*(x_1-x_2))/(f(x_1)-f(x_2))))
        error=mt.fabs(f(nx)-0)
        errorx.append(error)
        cont2 = cont2+1
        print(error)
        x_2=x_1
        contIt=contIt+1
        x_1=nx
    
    print("cantidad de iteraciones es: ", contIt)
    return format(nx, ".10g"),cont,nx,f(nx),errorx

print(secante(10**-8,-1.9,-1.1)) 

form,conta,nnx,ffx,errorT = secante(10**-8,-1.9,-1.1)
x = range(len(errorT)) #Eje x de la gráfica


## Punto 3.2
print("Punto 3.2")

def secante(E,x,x_1):
    error=mt.inf
    cont=0
    errorx = []
    cont2 = 0 
    while(error>E):
        print(x,x_1)
        cont=cont+1
        nx_1 = (x - f(x)*(x-x_1)/(f(x)-f(x_1)))
        error=mt.fabs(f(nx_1)-0)
        errorx.append(error)
        print(error)
        x_1=x
        x=nx_1
        cont2= cont2+1
    print("cantidad de iteraciones es: ", cont2)
    return format(nx_1, ".10g"),cont, errorx

print(secante(10**-8,-1.9,-1.1))
form1, cont1, errorx1 = secante(10**-8,-1.9,-1.1)






