package segonaEntrega.tools

import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.util.Timeout
import segonaEntrega.mapreduce.{MapReduce, MapReduceCompute}
import akka.pattern.ask

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps


object MRWrapper {

    private val timeoutValue: FiniteDuration = 10000 seconds // TODO adjust this timeout


    def MR[K1, V1, K2, V2, V3](input: List[(K1, List[V1])],
                               mappingFunction: (K1,List[V1]) => List[(K2,V2)],
                               reducingFunction: (K2,List[V2])=> (K2,V3)): (K2,V3) = {
        val system: ActorSystem = ActorSystem("system")
        val orchestrator = system.actorOf(Props(new MapReduce(input,mappingFunction,reducingFunction)), name = "orchestrator")

        implicit val timeout = Timeout(timeoutValue)
        val futureResult = orchestrator ? MapReduceCompute()

        println("Awaiting")
        // En acabar el MapReduce ens envia un missatge amb el resultat
        val finalResult:(K2,V3) = Await.result(futureResult,timeoutValue).asInstanceOf[(K2,V3)]


        println("Results Quant Ha Gastat")
        finalResult
    }
}
