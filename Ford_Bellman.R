#Tableau des sommets

X=1:7

#Les Matrice d'adjacence du Graphe

A1=rbind(c(0,5,8,0,0,0,0),c(5,0,0,4,-2,0,0),c(8,0,0,0,5,2,0),
        c(0,4,0,0,0,0,7),c(0,-2,5,0,0,0,3),c(0,0,2,0,0,0,-3),c(0,0,0,7,3,-3,0))


A2=rbind(c(0,5,8,0,0,0,0),c(5,0,0,4,0,0,0),c(8,0,0,0,5,2,0),
         c(0,4,0,0,0,0,0),c(0,0,5,0,0,0,3),c(0,0,2,0,0,0,-3),c(0,0,0,0,3,-3,0))

A3=rbind(c(0,7,8,0,0,0),c(0,0,0,4,1,2),c(0,2,0,0,0,2),
         c(0,0,0,0,0,0),c(0,0,-2,2,0,0),c(0,0,0,0,3,0))


Ford_Bellman = function(X,A,s)
{
  #on definit le table pi
  pi = vector(mode="integer",length = length(X))
  
  #on definit la valeur de l'infinit
  inf = 50000
  
  for(i in 1:length(X)){
    if(i != s)
    {
      pi[i] = inf
    }
  }
  A = t(A)
  var1 = vector()
  var2 = vector()
  pi_av = vector(mode="integer",length = length(X))
  n = 0
  while(all(pi_av == pi)== FALSE)
  {
    pi_av = pi 
    
    for(i in 1:length(X)){
      if(i != s)
      {
        tabj = which(A[i,] !=0,arr.ind=TRUE)
        
            for(j in 1:length(tabj))
             {
                  var1 = append(var1,pi[tabj[j]]+A[i,tabj[j]])
            }
        min1 = min(var1)
        var2 = c(min1,pi[i])
        minfinal = min(var2)
        
        pi[i] = minfinal
      
        var1 = vector()
        var2 = vector()
      }
    }
    n = n+1
  }
  return(pi)
  
}

#Fb = Ford_Bellman(X,A1,4)
print("Le plus court chemin en partant du sommet 4 (GRAPHE 1)par Ford_Bellman n'existe pas boucle infinie : ")
#print(Fb)

Fb = Ford_Bellman(X,A2,7)
print("Le plus court chemin en partant du sommet 7 (GRAPHE 2)par Ford_Bellman est : ")
print(Fb)

Fb = Ford_Bellman(1:6,A3,1)
print("Le plus court chemin en partant du sommet 1 (GRAPHE COURS)par Ford_Bellman est : ")
print(Fb)