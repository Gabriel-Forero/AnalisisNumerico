#include <iostream>
using namespace std;
#include <stdlib.h>
void Adams();
void euler();
int menu()
{
  int op;
  do
  {
    cout<<"SELECCIONE UNA OPCION"<<endl;
    cout<<"1. Metodo de euler"<<endl;
    cout<<"2. Adams-Bashforth"<<endl;
    cout<<"3. Salir"<<endl;
    cout<<"seleccion la opcion: "<<endl;
    cin>> op;
  }while(op<1 || op>3);
  return op;
}


int main() 
{
 int opcion;
 do
 {  
   opcion=menu();
 if(opcion ==1)
    euler();
 if(opcion ==2)
    Adams();
  if(opcion==3)
    break;

 }while(opcion==3);
 
}

void euler()
{
  double OtInicial=20;
  double DtInicial=10;
  double Tiempo;
  double Ot;
  double Dt;
  double h;
  cout<<"METODO EULER"<<endl;
  cout<<"------------"<<endl;
  cout<<"introduzca el valor inicial de la Oferta"<<endl;
  cin>>OtInicial;
  cout<<"introduzca el valor inicial de la Demanda"<<endl;
  cin>>DtInicial;
  cout<<"Introduzca el valor del paso"<<endl;
  cin>>h;
  cout<<"Introduzca el tiempo a simular"<<endl;
  cin>>Tiempo;

  for(int i=0;i<Tiempo;i++)
  {
    Ot=OtInicial - h*(OtInicial - (1.2)* DtInicial);
    Dt= 3*h +DtInicial;
    OtInicial =Ot;
    DtInicial =Dt;
    cout<<"I: "<<i<<" Oferta: " <<Ot<<" Demanda: "<<Dt<<"\n";
  }
}

void Adams()
{
  double Tiempo;
  double OtInicial=20;
  double DtInicial=10;
  double Ot;
  double Dt;
  double h;
  double Oact;
  double Oant;
  double Osig;
  double Dact;
  double Dant;
  double Dsig;
 cout<<"METODO DE ADAMS BASHFORH"<<endl; 
 cout<<"------------------------"<<endl;
 cout<<"introduzca el valor inicial de la Oferta"<<endl;
 cin>>OtInicial;
 cout<<"introduzca el valor inicial de la Demanda"<<endl;
 cin>>DtInicial;
 cout<<"Introduzca el valor del paso"<<endl;
 cin>>h;
 cout<<"Introduzca el tiempo a simular"<<endl;
 cin>>Tiempo;
 
 Oant=OtInicial;
 Dant = DtInicial;
 Dact=Dant+3*h;
 Oact = Oant +h*(Oant - (1.2)* Dant);
 Dsig=DtInicial;

 for(int i=0;i<Tiempo;i++)
 {

   Dsig=Dsig+3*h;
   Osig=Oact-h/2*(3*(Oact - (1.2)* Dact) - (Oant- Dant));
   cout<<"I: "<<i<<" Oferta: " <<Osig<<" Demanda: "<<Dsig<<"\n";
   Oant=Oact;
   Dant=Dact;
   Oact=Osig;
   Dact=Dsig;

 }
}