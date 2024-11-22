1. Map de contingut a referències. Reduce de referències a count unique. Master calcula la mitjana un cop tots actors acabin.
2. Dos mapreduces:
   1. Fer `ngrama` per filtrar els documents que contenen la query. Map: de contingut a resultat ngrama. Reduce: si conté la query, que retorni el nombre d'ocurrències (0 == false).
   2. Fer algoritme PR. Map: aplicar fòrmula a cada pàgina (agafar la pàgina i treure el número). Reduce: Fer la recomanació (una espècie de "sort").
3. Map: trobar parelles de documents que no es referencien. (pot ser que ja sigui un map-reduce). Reduce: calcular el tf_idf i filtrar que la puntuació > 0.5
4. Declarar la funció amb el paràmetre a **lazy**.
5. Abstreure la classe genèrica 1,4,10,20 nombre d'actors i passar-li com a paràmetre el que ha d'executar d'alguna forma.
6. MR demana input, funcio mapping i funcio reduce. S'espavila de gestionar la resta.
7. puta mare
8. a mirar després examen, tenim una referència al git que ens va donar el profe no se quan.
