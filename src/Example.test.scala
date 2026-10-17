package reconcile

import ImplType.*

/** Every number of section 5.4 and Table 3, recomputed. */
class ExampleSuite extends munit.FunSuite:
  val r = Example.run()
  val tol = 0.005

  test("step 4: (4) fails for signed 8-bit speed, holds for unsigned 8-bit"):
    assertEquals(r.step4Signed, Verdict.Violated)
    assertEquals(r.step4Unsigned, Verdict.Holds)

  test("step 5, parallel: Dom_1..3 = [40, 90] km/h, partial overlap, kappa ≈ 0.56"):
    assertEqualsDouble(r.parallel.lo, 40, 1e-9)
    assertEqualsDouble(r.parallel.hi, 90, 1e-9)
    assertEquals(r.overlap, Overlap.Partial)
    assertEqualsDouble(r.kappaParallel, 50.0 / 90, 1e-9)

  test("step 5, sequential M2 → M4: Y = [2.52, 10.73] kW; (17) holds for Dom_M and Dom_I, fails for Dom_C"):
    assertEqualsDouble(r.link.image.lo, 2.52, tol)
    assertEqualsDouble(r.link.image.hi, 10.73, tol)
    assertEquals(r.link.math, Verdict.Holds)
    assertEquals(r.link.ctx, Verdict.Violated)
    assertEquals(r.link.impl, Verdict.Holds)
    assertEquals(r.link.status, LinkStatus.Violated)

  test("step 5, sequential: preimage D' ≈ [61.3, 90] km/h, kappa ≈ 0.32"):
    assertEqualsDouble(r.reconciled.lo, 61.3, 0.05)
    assertEqualsDouble(r.reconciled.hi, 90, 1e-9)
    assertEqualsDouble(r.kappa, 0.32, tol)

  test("IC_2: drag at 1500 m is overestimated by about 16 %"):
    assertEqualsDouble(Example.dragOverestimate(1.058), 0.16, 0.005)

  test("road load: total overestimate at 1500 m ≈ 9 % at 90 km/h, ≈ 3 % at 40 km/h; the 90 kW bound of M4 never binds"):
    assertEqualsDouble(Example.roadLoadOverestimate(1.058), 0.087, 0.005)
    assertEqualsDouble(Example.roadLoadOverestimate(1.058, 40 / 3.6), 0.031, 0.005)
    assertEqualsDouble(Example.roadLoadKw(130 / 3.6), 25.4, 0.05)

  test("ρ = 1.058 in step 5: Y = [2.44, 9.87] kW, preimage D' ≈ [63.3, 90] km/h, kappa ≈ 0.297"):
    val h = Example.run(1.058)
    assertEqualsDouble(h.link.image.lo, 2.44, tol)
    assertEqualsDouble(h.link.image.hi, 9.87, tol)
    assertEquals(h.link.ctx, Verdict.Violated)
    assertEqualsDouble(h.reconciled.lo, 63.3, 0.05)
    assertEqualsDouble(h.kappa, 0.297, 0.001)

  test("(6) on an accumulating computation: Patriot clock, 0.1 s chopped to 23 fraction bits"):
    // Illustrative ε_out = 0.01 s. The error of one tick is what the register loses on 0.1;
    // (6) is applied after the largest number of ticks N that the context allows.
    val tick = 0.1 - FixedPoint(24, 23).nearest(0.1)
    assertEqualsDouble(tick, 9.54e-8, 1e-10)
    def drift(ticks: Long) = tick * ticks
    def ticksIn(hours: Double) = (hours * 3600 / 0.1).toLong
    assertEqualsDouble(drift(ticksIn(100)), 0.343, 0.001)
    assert(!Procedure.resultPrecision(drift, ticksIn(100), 0.01)) // violated for 100 h of continuous operation
    assert(Procedure.resultPrecision(drift, ticksIn(2.5), 0.01))   // holds for a session of a few hours
    // (5) for the input 0.1 itself holds with any sensible ε_in: one tick loses < 1e-7 s.
    assert(FixedPoint(24, 23).maxRepError(Interval(0.1, 0.1)) < 1e-7)

/** Conditions (3), exact (5), (11), step 6, per-level (17) with δ, (15) for n models, κ under unit change. */
class ChecksSuite extends munit.FunSuite:

  test("(3): 1/x with Dom_M = (0, ∞) – (2) holds, but chopping to 23 fraction bits sends 1e-9 to 0"):
    val d = Domain(Interval.Positive, Interval(1e-9, 1), FixedPoint(24, 23), 1e-6)
    assertEquals(d.mathAdmissible, Verdict.Holds)
    assertEquals(d.mathAdmissibleImpl, Verdict.Violated)
    assertEquals(d.copy(impl = Float64).mathAdmissibleImpl, Verdict.Holds)

  test("(3) follows from (2) for closed bounds represented exactly"):
    val d = Domain(Interval.NonNegative, Interval(0, 1), FixedPoint(24, 23), 1e-6)
    assertEquals(d.mathAdmissible, Verdict.Holds)
    assertEquals(d.mathAdmissibleImpl, Verdict.Holds)

  test("(5) is computed exactly, not on a grid"):
    assertEqualsDouble(IntType(8, signed = false).maxRepError(Interval(40, 130)), 0.5, 0)
    assertEqualsDouble(IntType(8, signed = false).maxRepError(Interval(40.1, 40.3)), 0.3, 1e-12)
    assertEqualsDouble(FixedPoint(24, 23).maxRepError(Interval(0, 1)), math.pow(2, -23), 0)
    assert(Float64.maxRepError(Interval(0, 100)) > 0) // binary64 is not treated as exact
    assertEquals(Domain(Interval.Reals, Interval(40, 130), IntType(8, signed = false), 1).implPrecision, Verdict.Holds)
    assertEquals(Domain(Interval.Reals, Interval(40, 130), IntType(8, signed = false), 0.4).implPrecision, Verdict.Violated)

  test("(11): Dom_C^rho = [1.167, 1.289]; 1.058 at 1500 m violates it, 1.225 does not"):
    val rho = Example.rhoDomain()
    assertEqualsDouble(rho.lo, 1.167, 0.0005)
    assertEqualsDouble(rho.hi, 1.289, 0.0005)
    val ic = Example.m2.ic
    assertEquals(ic.check(Map("rho" -> Interval(1.058, 1.058))).violated, Seq("rho"))
    assert(ic.check(Map("rho" -> Interval(1.225, 1.225))).ok)
    assertEquals(ic.check(Map.empty).unknown, Seq("rho")) // not given: left for manual review, not passed

  test("step 6: shared θ with disjoint domains is a formally detected conflict; text is pooled"):
    val other = Context(Seq(Example.rhoParam.copy(dom = Example.rhoParam.dom.copy(domC = Some(Interval(1.0, 1.1))))), Set("warm engine"))
    val c = Example.m2.ic.combine(other)
    assertEquals(c.conflicts, Seq("rho"))
    assert(c.ic.text.contains("no wind") && c.ic.text.contains("warm engine"))
    val compatible = Example.m2.ic.combine(Context(Seq(Example.rhoParam.copy(dom = Example.rhoParam.dom.copy(domC = Some(Interval(1.2, 1.3))))), Set()))
    assert(compatible.conflicts.isEmpty)
    assertEqualsDouble(compatible.ic.theta.head.dom.domC.get.lo, 1.2, 0)

  test("(17) with δ > 0: the image widens and the preimage shrinks"):
    val f2Kmh: Double => Double = v => Example.roadLoadKw(v / 3.6)
    val d = Interval(40, 90)
    val l = Procedure.sequential(f2Kmh, d, Some(0.5), Example.m4.in.dom)
    assertEqualsDouble(l.image.lo, 2.52 - 0.5, 0.005)
    val p0 = Procedure.sequentialPreimage(f2Kmh, d, Some(0.0), Example.m4.in.dom)
    val p1 = Procedure.sequentialPreimage(f2Kmh, d, Some(0.5), Example.m4.in.dom)
    assert(p1.lo > p0.lo)

  test("(17), third inclusion – the Ariane 501 link: BH (float64) into the 16-bit integer conversion overflows"):
    // M1 = SRI alignment function, output BH (float64); M2 = conversion of BH to int16. The trajectory only
    // supplies M1's input, it is not a link. The inquiry report gives no BH values, so the image [0, 40000]
    // is illustrative; only the fact Y ⊄ [−32768, 32767] is from the report. Dom_C of the conversion
    // (BH on the Ariane 4 trajectory) was never recorded, so the second inclusion cannot be checked.
    val sri = Domain(Interval.Reals, None, IntType(16, signed = true), 0.5)
    val l = Procedure.sequential(identity, Interval(0, 40000), Some(0.0), sri)
    assertEquals(l.math, Verdict.Holds)
    assertEquals(l.ctx, Verdict.NotCheckable)
    assertEquals(l.impl, Verdict.Violated)
    assertEquals(l.status, LinkStatus.Violated)

  test("(15) for n models: inclusion when one domain lies in all others"):
    assertEquals(Procedure.classify(Seq(Interval(0, 10), Interval(2, 8), Interval(1, 9))), Overlap.Inclusion)
    assertEquals(Procedure.classify(Seq(Interval(0, 10), Interval(5, 15), Interval(1, 9))), Overlap.Partial)
    assertEquals(Procedure.classify(Seq(Interval(0, 1), Interval(2, 3))), Overlap.Incompatible)

  test("κ does not change under an affine change of units (12)"):
    val r = Example.run()
    val ms = Interval(r.reconciled.lo / 3.6, r.reconciled.hi / 3.6)
    val tMs = Interval(Example.target.lo / 3.6, Example.target.hi / 3.6)
    assertEqualsDouble(Procedure.kappa(ms, tMs), r.kappa, 1e-12)

  test("open bounds: (0, 1] is a subset of [0, 1], not the other way round"):
    val half = Interval(0, 1, loOpen = true)
    assert(half.subsetOf(Interval(0, 1)))
    assert(!Interval(0, 1).subsetOf(half))
    assert(!half.contains(0) && half.contains(1e-300))

/** The verdict rules: a decreasing f, an unrecorded level, an unknown δ, a fixed-width register. */
class VerdictSuite extends munit.FunSuite:
  val m4 = Example.m4.in.dom

  test("(17) for a decreasing f: the image is not empty and the violation is reported"):
    val l = Procedure.sequential(v => 1000 - v, Interval(0, 2000), Some(0.0), m4)
    assertEquals(l.image, Interval(-1000, 1000))
    assertEquals(l.math, Verdict.Holds) // Dom_M of M4 is ℝ (a polynomial); the old empty image passed every level
    assertEquals(l.ctx, Verdict.Violated)
    assertEquals(l.status, LinkStatus.Violated)

  test("preimage for a decreasing f"):
    val p = Procedure.sequentialPreimage(v => 100 - v, Interval(0, 100), Some(0.0), m4)
    assertEqualsDouble(p.lo, 10, 1e-6)
    assertEqualsDouble(p.hi, 95, 1e-6)

  test("unrecorded Dom_C: the second inclusion is not checkable, and the link is not marked compatible"):
    val code = Domain(Interval.Reals, None, IntType(16, signed = true), 0.5)
    val l = Procedure.sequential(identity, Interval(0, 100), Some(0.0), code)
    assertEquals(l.ctx, Verdict.NotCheckable)
    assertEquals(l.impl, Verdict.Holds)
    assertEquals(l.status, LinkStatus.NotCheckable)
    assertEquals(code.mathAdmissible, Verdict.NotCheckable)

  test("unknown δ: a link that passes (17) is only conditionally compatible"):
    val f: Double => Double = v => Example.roadLoadKw(v / 3.6)
    val d = Interval(70, 90)
    assertEquals(Procedure.sequential(f, d, None, m4).status, LinkStatus.Conditional)
    assertEquals(Procedure.sequential(f, d, Some(0.5), m4).status, LinkStatus.Compatible)

  test("(4) for a fixed-width fixed-point register: 24 bits with 23 fraction bits hold values below 2"):
    val reg = FixedPoint(24, 23)
    assertEqualsDouble(reg.range.hi, (math.pow(2, 24) - 1) / math.pow(2, 23), 0)
    assertEquals(Domain(Interval.Reals, Interval(0, 1.5), reg, 1e-6).implRange, Verdict.Holds)
    assertEquals(Domain(Interval.Reals, Interval(0, 3), reg, 1e-6).implRange, Verdict.Violated)
