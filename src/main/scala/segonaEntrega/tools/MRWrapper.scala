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

    var mapperNumber = 1
    var reducerNumber = 1

    def MR[K1, V1, K2, V2, V3](input: List[(K1, List[V1])],
                                mappingFunction: (K1,List[V1]) => List[(K2,V2)],
                                reducingFunction: (K2,List[V2])=> (K2,V3)) : Map[K2, V3] = {

        val system : ActorSystem = ActorSystem("MapReduceSystem")

        val orchestrator = system.actorOf(Props(new MapReduce(input, mappingFunction, reducingFunction, mapperNumber, reducerNumber)), "orchestrator")

        implicit val timeout: Timeout = Timeout(timeoutValue)

        val futureResult = (orchestrator ? MapReduceCompute()).mapTo[Map[K2, V3]]

        val res = Await.result(futureResult, timeoutValue)

        system.terminate()

        res
    }
}
