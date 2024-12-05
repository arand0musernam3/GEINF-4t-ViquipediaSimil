OK 1. Map de contingut a referències. Reduce de referències a count unique. Master calcula la mitjana un cop tots actors acabin.
2. Dos mapreduces:
   1. Fer `ngrama` per filtrar els documents que contenen la query. Map: de contingut a resultat ngrama. Reduce: si conté la query, que retorni el nombre d'ocurrències (0 == false).
   2. Fer algoritme PR. Map: aplicar fòrmula a cada pàgina (agafar la pàgina i treure el número). Reduce: Fer la recomanació (una espècie de "sort").
3. Map: trobar parelles de documents que no es referencien. (pot ser que ja sigui un map-reduce). Reduce: calcular el tf_idf i filtrar que la puntuació > 0.5
OK 4. Declarar la funció amb el paràmetre a **lazy**.
5. Abstreure la classe genèrica 1,4,10,20 nombre d'actors i passar-li com a paràmetre el que ha d'executar d'alguna forma.
OK 6. MR demana input, funcio mapping i funcio reduce. S'espavila de gestionar la resta.

8. a mirar després examen, tenim una referència al git que ens va donar el profe no se quan.


**Pistes** 
- A la modificacio del MapReduce per tenir un cert nombre de mappers i reducers, us podeu
inspirar en com es repartia el comptatge de primers entre els mappers i els reducers a
l’exemple vist a classe. 
- L’input dels MapReduce que han de treballar amb els fitxers hauria de ser una llista dels
noms dels fitxers, de manera que les funcions de mapping ja fessin la feina de llegir i
processar-los. 
- És important que, per fer la recomanació de les pàgines mes importants donada una query,
treballeu amb els documents on, dins del corpus, aparegui aquesta query. Per aconseguir-
ho, podeu fer us del metode ngrames que us vam fer implementar a la primera part de la
practica. 
- La recomanacio de les 100 pagines mes rellevants i la similitud de 0.5 son uns mınims.
Com mes queries complexes i mes fitxers pugueu tractar, millor. Podrıeu arribar a fer
queries que apareguin en totes les pagines?