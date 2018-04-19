import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.concurrent.Executors

//import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.math._
import scala.util.Random

import blocks.algorithm.approximateBayesianComputation.{lenormand2012, steadyState}


object ToyModel {

  // Toy model to compare with the study by Lenormand et al 2012 

  def gaussianPdf(x: Double, mean: Double, variance: Double) : Double = 
    exp(- pow(x - mean, 2) / (2 * variance)) / sqrt(2 * Pi * variance)

  def gaussianSample(mean:Double, variance:Double)(implicit rng: Random)
  : Double = rng.nextGaussian * sqrt(variance) + mean

  val var1 = 1.0 / 100.0
  val var2 = 1.0

  def prior(theta: Vector[Double]): Double = 
    if (theta.head >= -10 && theta.head <= 10) 1 else 0

  def priorSample(implicit rng: Random)
  : Vector[Double] = 
    Vector(rng.nextDouble * 20 - 10)

  def likelihood(x: Double, theta: Double): Double = 
    0.5 * gaussianPdf(x, theta, var1) + 0.5 * gaussianPdf(x, theta, var2)
    
  def f(theta: Vector[Double])(implicit rng: Random)
  : Vector[Double] = 
    if (rng.nextDouble < 0.5) {
      Vector(gaussianSample(theta.head, var1))
    } else {
      Vector(gaussianSample(theta.head, var2))
    }

  // Observed data point is 0
  val observed = 0
  def distanceToData(x: Vector[Double]): Double = abs(x.head - observed)

}
