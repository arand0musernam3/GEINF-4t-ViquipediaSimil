package segonaEntrega.mapreduce

import akka.actor.{Actor, ActorRef, PoisonPill, Props}

case class MapReduceCompute()
case class toMapper[K1,V1](fitxer: K1, text: List[V1])
case class fromMapper[K2,V2](intermig: List[(K2,V2)])
case class toReducer[K2,V2](word:K2, fitxers:List[V2])
case class fromReducer[K2,V3](finals: (K2,V3))


class Mapper[K1,V1,K2,V2](mapping:(K1,List[V1]) => List[(K2,V2)]) extends Actor {
    def receive: Receive = {

        case toMapper(clau:K1,valor:List[V1])=>
            sender ! fromMapper(mapping(clau,valor))
    }
}

class Reducer[K2,V2,V3](reducing:(K2,List[V2])=> (K2,V3)) extends Actor {
    def receive: Receive = {

        case toReducer(clau:K2,valor:List[V2])=>
            sender ! fromReducer(reducing(clau, valor))
    }
}



// L'Actor MapReduce és polimòrfic amb els tipus de les claus valor de l'entrada [K1,V1], la clau i valor intermedis [k2,v2]
// i la clau i valor finals [K2,V3].
// - input és el paràmetre d'entrada (compte perquè depenent de la mida pot ser un problema)
// - mapping és la funció dels mappers
// - reducing és la funció dels reducers
class MapReduce[K1,V1,K2,V2,V3](
                                   input:List[(K1,List[V1])],
                                   mapping:(K1,List[V1]) => List[(K2,V2)],
                                   reducing:(K2,List[V2])=> (K2,V3),
                                   mapperNumber : Int,
                                   reducerNumber: Int) extends Actor {


    var nmappers: Int = mapperNumber
    var missatgesMappersPendents = 0
    var nreducers: Int = reducerNumber
    var missatgesReducersPendents = 0

    // dict serà el diccionari amb el resultat intermedi
    var dict: Map[K2, List[V2]] = Map[K2, List[V2]]() withDefaultValue List()
    // resultatFinal recollirà les respostes finals dels reducers
    var resultatFinal: Map[K2, V3] = Map()

    // Ens apuntem qui ens ha demanat la feina
    var client:ActorRef = null

    var mappers : Seq[ActorRef] = Nil
    var reducers : Seq[ActorRef] = Nil


    def receive: Receive = {

        // En rebre el missatge MapReduceCompute engeguem el procés.
        case MapReduceCompute() =>
            //println("Hem rebut lencarrec")
            client = sender() // Ens apuntem qui ens ha fet l'encàrrec per enviar-li el missatge més tard.

            mappers = for (i <- 0 until nmappers) yield {
                context.actorOf(Props(new Mapper(mapping)), "mapper" + i)
            }

            for(((p1,p2),i)<-input.zipWithIndex) mappers(i % nmappers) ! toMapper(p1: K1, p2 :List[V1])

            mappers.foreach(_ ! PoisonPill) // once the mappers have finished working, we kill them to save resources

            // Necessitem controlar quan s'han acabat tots els mappers per poder llençar els reducers després...
            missatgesMappersPendents = input.length



        case fromMapper(list_clau_valor:List[(K2,V2)]) =>
            for ((clau, valor) <- list_clau_valor)
                dict += (clau -> (valor :: dict(clau)))

            missatgesMappersPendents -= 1

            // Quan ja hem rebut tots els missatges dels mappers:
            if (missatgesMappersPendents==0)
            {

                missatgesReducersPendents = dict.size // actualitzem els reducers pendents
                reducers = for (i <- 0 until nreducers) yield
                    context.actorOf(Props(new Reducer(reducing)), "reducer"+i)

                for (((key:K2, lvalue:List[V2]), i) <- dict.zipWithIndex)
                    reducers(i % nreducers) ! toReducer(key, lvalue)

                reducers.foreach(_ ! PoisonPill) // once the reducers have finished working, we kill them to save resources

            }

        case fromReducer(entradaDiccionari:(K2,V3)) =>
            resultatFinal += entradaDiccionari
            missatgesReducersPendents -= 1


            if (missatgesReducersPendents == 0) {
                client ! resultatFinal
                context.stop(self)
            }
    }

}

