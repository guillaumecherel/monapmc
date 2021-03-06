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

object SimulationsSampleParamLenormand2012 extends App {

    implicit val rng = new Random()

    // Sample the algorithm parameters.

    for { alpha <- (0.1 to 0.9 by 0.1).par
          pAccMin <- Vector(0.01, 0.05, 0.1, 0.2).par
          replication <- (1 to 50).par
    } yield {
      println(s"Running simulation lenormand2012 alpha=$alpha pAccMin=$pAccMin replication=$replication")
      val (posterior, step) = lenormand2012(f, priorSample, prior, distanceToData,
                                    n = 5000, alpha = alpha, pAccMin = pAccMin).zipWithIndex.toStream.last
      println(s"Done Running simulation lenormand2012 alpha=$alpha pAccMin=$pAccMin replication=$replication ($step steps)")

      Files.write(
        Paths.get("../output/blocks/param_sampling/lenormand2012_%d_%.1f_%.2f_%d_%d.csv"
                    .formatLocal(java.util.Locale.US, 5000, alpha, pAccMin, step, replication)),
        posterior.map{_.head}.mkString("\n").getBytes(StandardCharsets.UTF_8)
      )
    }
}
