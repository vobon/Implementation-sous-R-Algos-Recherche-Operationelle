#Tableau des sommets

X=1:7

#Matrice d'adjacence du reseau avec ses capacités

A=rbind(c(0,5,8,0,0,0,0),c(5,0,0,4,2,0,0),c(8,0,0,0,5,2,0),
        c(0,4,0,0,0,0,7),c(0,2,5,0,0,0,3),c(0,0,2,0,0,0,3),c(0,0,0,7,3,3,0))

Prim = function(X,A)
{
  #On prend un sommet au hasard
  Xp = vector()
  Xp = append(Xp,sample(X,1))
  
  #on definit X_prime_barre
  Xpb = setdiff(X,Xp)
  
  #on cree la matrice de possibilites de choix d'aretes allant de Xp a Xpb
  Mc=matrix(A[Xp,Xpb],nrow=length(Xp),ncol=length(Xpb))
  
  #on definit U_prime
  Up = matrix(0,nrow=length(X),ncol=length(X))
  
  while(length(Xp)<length(X))
  {
    #on remplace les zeros dans matrice des possibilites par des NA pour ne pas les selectionnes
    Mc = replace(Mc, Mc==0, NA)
    min = min(Mc,na.rm=TRUE)
    print(Mc)
    print(min)
    #on cherche les indices du nombre min(du premier si il y'a des exaequos)
    arete=which(Mc == min,arr.ind=TRUE)
    
    #une fois les indices obtenus on change le referentiel pour obtenir les sommets
    #connectes qui ont ce poids min 
    x = Xp[arete[1,1]]
    y = Xpb[arete[1,2]]
    
    #on met a jour les sommets de l'ensemble X_prime et X_prime_barre
    Xp = append(Xp,y)
    Xpb = setdiff(X,Xp)
    
    #On rajoute le poids de l'arete dans U_prime
    Up[x,y]= min
    Up[y,x]= min
    
    #on recalcule la nouvelle matrice des possibilités de selection
    Mc=matrix(A[Xp,Xpb],nrow=length(Xp),ncol=length(Xpb))
    
  }
  return(Up)
}

arbre_rpm = Prim(X,A)
print(paste("L'arbre recouvrant de poids min est : "))
print(arbre_rpm)