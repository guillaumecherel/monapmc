import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.concurrent.Executors

//import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.math._
import scala.util.Random

import blocks.algorithm.approximateBayesianComputation.{lenormand2012, steadyState}

import ToyModel._

object SimulationsSampleParamSteadyState extends App {

    implicit val rng = new Random()




    // Sample the algorithm parameters.


    for { alpha <- (0.1 to 0.9 by 0.1).par
          pAccMin <- Vector(0.01, 0.05, 0.1, 0.2).par
          parallel <- Vector(1).par
          replication <- (1 to 50).par
    } yield {

      println(s"Running simulation steadyState alpha=$alpha pAccMin=$pAccMin parallel=$parallel replication=$replication")

      val executor = Executors.newSingleThreadExecutor()
      val ec = ExecutionContext.fromExecutorService(executor)

      val (posterior, step) = steadyState(x => Future.successful(f(x)), priorSample, prior, distanceToData,
                                          n = 5000, alpha = alpha,
                                          pAccMin = pAccMin, parallel = parallel
      )(rng, ec).zipWithIndex.drop(5000).grouped(5000).map{_.head}.toVector.last

      executor.shutdown

      println(s"Done Running simulation steadyState alpha=$alpha pAccMin=$pAccMin parallel=$parallel replication=$replication ($step steps)")

      Files.write(
        Paths.get("../output/blocks/param_sampling/steadyState_%d_%.1f_%.2f_%d_%d_%d.csv".formatLocal(java.util.Locale.US, 5000, alpha, pAccMin, parallel, step, replication)),
        posterior.map{_.head}.mkString("\n").getBytes(StandardCharsets.UTF_8)
      )
    }
}
