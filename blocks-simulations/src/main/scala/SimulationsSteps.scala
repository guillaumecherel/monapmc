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

object SimulationsSteps extends App {

    implicit val rng = new Random()
    val ec = ExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor())

    // Run simulations and write samples to files

    // Write each step of the algorithms with fixed parameter values to a file.

    println(s"Running simulation lenormand2012 alpha=0.1 pAccMin=0.01")
    val resLenormand2012 = lenormand2012(f, priorSample, prior, distanceToData,
      n = 5000, alpha = 0.1, pAccMin = 0.01).zipWithIndex.toVector

    for { (posterior,i) <- resLenormand2012 } yield {
      Files.write(
        Paths.get("../output/blocks/5steps/lenormand2012_%d_%.1f_%.2f_%d_%d.csv"
                    .formatLocal(java.util.Locale.US, 5000, 0.1, 0.01, i, 1)),
        posterior.map{_.head}.mkString("\n").getBytes(StandardCharsets.UTF_8)
      )
    }

    println(s"Running simulation steadyState alpha=0.1 pAccMin=0.01 parallel=1")
    val resSteadyState = steadyState(x => Future.successful[Vector[Double]](f(x)), priorSample, prior, distanceToData,
      n = 5000, alpha = 0.1, pAccMin = 0.01, parallel = 1)(rng, ec).zipWithIndex.drop(5000).grouped(5000).map{_.head}.toVector

    for { (posterior,i) <- resSteadyState } yield {
      println(i)
      Console.flush()
      Files.write(
        Paths.get("../output/blocks/5steps/steadyState_%d_%.1f_%.2f_%d_%d_%d.csv".formatLocal(java.util.Locale.US, 5000, 0.1, 0.01, 1, i, 1)),
        posterior.map{_.head}.mkString("\n").getBytes(StandardCharsets.UTF_8)
      )
    }
}

