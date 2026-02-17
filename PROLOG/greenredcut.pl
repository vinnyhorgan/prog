%green cut
max(X, Y, X) :- X >= Y. 
max(_, Y, Y).

%red cut
tipo_animale(X, mammifero) :- mammifero(X). 
tipo_animale(X, uccello) :- uccello(X). 
tipo_animale(X, sconosciuto).  
 
mammifero(cane). 
mammifero(gatto). 
uccello(aquila). 
uccello(cane).