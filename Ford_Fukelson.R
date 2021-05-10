
#Tableau des sommets

X=1:7

#Matrice d'adjacence du reseau avec ses capacités

A=rbind(c(0,5,8,0,0,0,0),c(0,0,0,4,2,0,0),c(0,0,0,0,5,2,0),
          c(0,0,0,0,0,0,7),c(0,0,0,0,0,0,3),c(0,0,0,0,0,0,3),c(0,0,0,0,0,0,0))

#Matrice des flots realisables

P=matrix(0,nrow=length(X),ncol=length(X))

#Matrice de la marque m

m= matrix(NA,nrow =length(X),ncol =3)

#definition Flotmax

Ford_Fukelson = function(X,A,P,m)
{
  #definition du Flotmax
  Flotmax = 0
  #definition de alphaj
  alphaj = 0
  
  #definition de l'infinit
  inf = 50000
  
  #L'ensemble S
  S = vector()
  
  s = 1
  p = 7
  
  m[s,] = c(NA,inf,1)
  S = append(S,s)
  
  #permet d'obtenir la position des arcs ou le flot est diff de la capacite de l'arc
  R1=A-P>0
  
  #permet d'obtenir la position des arcs ou le flot de j a i est sup a zero
  R2=t(P)>0
  
  #l'union de R1 et R2
  C=R1|R2
  
  #Permet d'avoir S barre
  Sb=setdiff(X,S)
  
  #renvoi la position des arcs respectant les conditions de la boucle
  Cnd=which(matrix(C[S,Sb]==TRUE,nrow=length(S),ncol=length(Sb)),arr.ind=TRUE)
  
  while(length(Cnd)>0)
  {
   x = S[Cnd[1,1]]
   y = Sb[Cnd[1,2]]
   
   if(R1[x,y]==TRUE)
   {
     V = A[x,y]-P[x,y]
     alphaj = min(c(m[x,2],V))
     m[y,] = c(x,alphaj,1)
   }
   else if(R2[x,y]==TRUE){
     V = P[y,x]
     alphaj = min(c(m[x,2],V))
     m[y,] = c(x,alphaj,-1)
   }
   S = append(S,y)
   if(y == p){
     Flotmax = Flotmax + alphaj
     break
   }
   Sb=setdiff(X,S)
   Cnd=which(matrix(C[S,Sb]==TRUE,nrow=length(S),ncol=length(Sb)),arr.ind=TRUE)
  }
  if(is.element(p,S))
  {
    y = p
    x = m[y,1]
    while(y != s)
    {
      if(m[y,3]==1)
      {
        P[x,y] = P[x,y] + m[p,2]
      }
      else if(m[y,3]==-1)
      {
        P[x,y] = P[x,y] - m[p,2]
      }
      y = m[y,1]
      x = m[y,1]
    }
   Flotmax = Flotmax + Ford_Fukelson(X,A,P,m)
    
  }
  else
  {
    print(P)
    print(m)
    return(Flotmax)
  }
}
valflotMax = Ford_Fukelson(X,A,P,m)
print(paste("Le flot de valeur max obtenu par l'algo de Ford Fukelson est : ",valflotMax))
