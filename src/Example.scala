// Worked example of section 5.4: energy use of a passenger car on a motorway.
// Run: scala-cli run .     Test: scala-cli test .
package reconcile

import Unit.*
import ImplType.*

object Example:
  // Dom_M follows the level rule of 5.1: every expression here is a polynomial, so Dom_M = ℝ; non-negativity
  // of speed, power and density is physical, i.e. contextual.
  // Road-load parameters of M2. ρ is a numeric context parameter in Θ_2; the rest are text assumptions.
  val rho = 1.225   // kg/m³, sea level, 15 °C
  val cd = 0.30
  val area = 2.2    // m²
  val mass = 1500.0 // kg
  val crr = 0.012
  val g = 9.81

  /** f_2: speed in m/s → road-load power in kW. Non-decreasing for v ≥ 0. */
  def roadLoadKw(v: Double, rho: Double = rho): Double =
    (crr * mass * g + 0.5 * rho * cd * area * v * v) * v / 1000

  /** Relative overestimate of aerodynamic drag when ρ = 1.225 is used where the air density is rhoActual. */
  def dragOverestimate(rhoActual: Double, v: Double = 25): Double =
    def aero(r: Double) = roadLoadKw(v, r) - roadLoadKw(v, 0.0)
    aero(rho) / aero(rhoActual) - 1

  /** Relative overestimate of the whole road-load power (rolling resistance + drag) at speed v, m/s. */
  def roadLoadOverestimate(rhoActual: Double, v: Double = 25): Double =
    roadLoadKw(v) / roadLoadKw(v, rhoActual) - 1

  /** Dom_C^ρ: densities for which the aerodynamic drag of M2 is off by at most `tol` relative to the true value. */
  def rhoDomain(tol: Double = 0.05): Interval = Interval(rho / (1 + tol), rho / (1 - tol))

  val target: Interval = Interval(40, 130) // T, km/h

  def speed(name: String, u: Unit, ctx: Interval, impl: ImplType, eps: Double): Param =
    Param(name, "vehicle speed relative to the road", u, Domain(Interval.Reals, ctx, impl, eps))

  val rhoParam: Param = Param("rho", "air density", KilogramPerCubicMetre,
    Domain(Interval.Reals, rhoDomain(), Float64, 1e-3))

  val m1 = Model("M1", speed("V", KilometrePerHour, Interval(40, 130), IntType(8, signed = true), 1), None,
    Context.text("dry road", "motorway"), "motorway driving", "author of M1")
  val m1Fixed = m1.copy(in = speed("V", KilometrePerHour, Interval(40, 130), IntType(8, signed = false), 1))
  val m2 = Model("M2", speed("v", MetrePerSecond, Interval(0, 100), Float64, 0.01),
    Some(Param("P", "road-load power at the wheels", Watt, Domain(Interval.Reals, Interval.NonNegative, Float64, 1))),
    Context(Seq(rhoParam), Set("no wind", "crr = 0.012", "m = 1500 kg", "Cd = 0.30", "A = 2.2 m2")),
    "road load", "author of M2")
  val m3 = Model("M3", speed("v", MetrePerSecond, Interval(10, 25), Float64, 0.01), None,
    Context.text("vehicle class", "level road"), "fuel-use regression", "author of M3")
  val m4 = Model("M4", Param("P", "power demand at the wheels", Kilowatt,
    Domain(Interval.Reals, Interval(5, 90), Float64, 0.01)), None,
    Context.text("warm engine", "steady state"), "engine fuel map", "author of M4")

  /** δ of M2: no error estimate is given, so (17) uses δ = 0, a passing link would be only Conditional,
    * and the assumption is recorded in IC of the integrated model. */
  val deltaM2: Option[Double] = None

  final case class Result(
      step4Signed: Verdict, step4Unsigned: Verdict,
      parallel: Interval, overlap: Overlap, kappaParallel: Double,
      link: LinkCheck, reconciled: Interval, kappa: Double)

  /** Steps 2–5. `rhoActual` is the air density of the road section; 1.058 kg/m³ is 1500 m in the standard atmosphere. */
  def run(rhoActual: Double = rho): Result =
    // Step 2: every Dom_C of a speed input in km/h.
    val ctxKmh = Seq(m1Fixed, m2, m3).map(_.in.ctxIn(KilometrePerHour))
    // Step 5, parallel part: (14)–(16).
    val parallel = Procedure.common(ctxKmh)
    // Step 5, sequential link M2 → M4: (17) on the reconciled input domain D, per level of the M4 input.
    val f2Kmh: Double => Double = vKmh => roadLoadKw(vKmh / 3.6, rhoActual)
    val link = Procedure.sequential(f2Kmh, parallel, deltaM2, m4.in.dom)
    val reconciled = if link.status != LinkStatus.Violated then parallel else Procedure.sequentialPreimage(f2Kmh, parallel, deltaM2, m4.in.dom)
    Result(
      m1.in.dom.implRange, m1Fixed.in.dom.implRange,
      parallel, Procedure.classify(ctxKmh), Procedure.kappa(parallel, target),
      link, reconciled, Procedure.kappa(reconciled, target))

  @main def report(): scala.Unit =
    val r = run()
    println(s"step 4, (4) for M1 with int8:  ${r.step4Signed}")
    println(s"step 4, (4) for M1 with uint8: ${r.step4Unsigned}")
    println(s"step 5, (14) parallel domain:  ${r.parallel} km/h, ${r.overlap}")
    println(f"step 5, (16) kappa parallel:   ${r.kappaParallel}%.3f")
    println(s"step 5, (17) Y = [f2](D):      ${r.link.image} kW, Dom_M: ${r.link.math}, Dom_C: ${r.link.ctx}, Dom_I: ${r.link.impl}, link: ${r.link.status}")
    println(s"step 5, preimage D':           ${r.reconciled} km/h")
    println(f"step 5, (16) kappa with (17):  ${r.kappa}%.3f")
    println(s"IC_2, (11): Dom_C^rho = ${rhoDomain()} kg/m3; at 1500 m (rho = 1.058): ${m2.ic.check(Map("rho" -> Interval(1.058, 1.058)))}")
    println(f"IC_2 at 1500 m: aerodynamic drag overestimated by ${100 * dragOverestimate(1.058)}%.1f %%, road load at 90 km/h by ${100 * roadLoadOverestimate(1.058)}%.1f %%")
    val h = run(1.058)
    println(s"step 5 at 1500 m: Y = ${h.link.image} kW, preimage D' = ${h.reconciled} km/h")
    println(f"step 5 at 1500 m: (16) kappa with (17): ${h.kappa}%.3f")
