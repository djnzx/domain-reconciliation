// Multi-level domain of definition and the reconciliation checks of the article.
// Formula numbers in comments refer to section 5 of the manuscript.
package reconcile

/** Interval from lo to hi; each bound may be open. The empty interval has lo > hi. */
final case class Interval(lo: Double, hi: Double, loOpen: Boolean = false, hiOpen: Boolean = false):
  def isEmpty: Boolean = lo > hi || (lo == hi && (loOpen || hiOpen))
  def length: Double = if isEmpty then 0.0 else hi - lo
  infix def intersect(o: Interval): Interval =
    val (l, lOpen) = if lo > o.lo then (lo, loOpen) else if o.lo > lo then (o.lo, o.loOpen) else (lo, loOpen || o.loOpen)
    val (h, hOpen) = if hi < o.hi then (hi, hiOpen) else if o.hi < hi then (o.hi, o.hiOpen) else (hi, hiOpen || o.hiOpen)
    Interval(l, h, lOpen, hOpen)
  def subsetOf(o: Interval): Boolean =
    isEmpty || {
      val loOk = o.lo < lo || (o.lo == lo && (!o.loOpen || loOpen))
      val hiOk = hi < o.hi || (hi == o.hi && (!o.hiOpen || hiOpen))
      loOk && hiOk
    }
  def contains(x: Double): Boolean =
    (if loOpen then lo < x else lo <= x) && (if hiOpen then x < hi else x <= hi)
  /** [lo − δ, hi + δ]: the image widened by the model error δ ≥ 0, see (17). */
  def widen(delta: Double): Interval = copy(lo = lo - delta, hi = hi + delta)
  override def toString: String =
    if isEmpty then "∅" else f"${if loOpen then "(" else "["}$lo%.4g, $hi%.4g${if hiOpen then ")" else "]"}"

object Interval:
  val Reals: Interval = Interval(Double.NegativeInfinity, Double.PositiveInfinity)
  val NonNegative: Interval = Interval(0.0, Double.PositiveInfinity)
  val Positive: Interval = Interval(0.0, Double.PositiveInfinity, loOpen = true)

/** Dimension as exponents of the seven SI base quantities L, M, T, I, Θ, N, J (7). */
final case class Dim(l: Int = 0, m: Int = 0, t: Int = 0, i: Int = 0, th: Int = 0, n: Int = 0, j: Int = 0)

object Dim:
  val Speed: Dim = Dim(l = 1, t = -1)
  val Power: Dim = Dim(l = 2, m = 1, t = -3)
  val Time: Dim = Dim(t = 1)
  val Density: Dim = Dim(l = -3, m = 1)
  val None: Dim = Dim()

/** Unit U = ⟨u, dim⟩ (7) with an affine map to the coherent SI unit: x_SI = k·x + b. */
final case class Unit(symbol: String, dim: Dim, k: Double = 1.0, b: Double = 0.0):
  def toSI(x: Double): Double = k * x + b
  def fromSI(x: Double): Double = (x - b) / k

object Unit:
  val MetrePerSecond: Unit = Unit("m/s", Dim.Speed)
  val KilometrePerHour: Unit = Unit("km/h", Dim.Speed, k = 1 / 3.6)
  val Watt: Unit = Unit("W", Dim.Power)
  val Kilowatt: Unit = Unit("kW", Dim.Power, k = 1000)
  val Second: Unit = Unit("s", Dim.Time)
  val KilogramPerCubicMetre: Unit = Unit("kg/m3", Dim.Density)
  val One: Unit = Unit("1", Dim.None)

enum Scale:
  case Linear, Log10

/** Outcome of one condition: a level that is not recorded cannot be checked, which is not the same as holding. */
enum Verdict:
  case Holds, Violated, NotCheckable

object Verdict:
  def of(holds: Boolean): Verdict = if holds then Holds else Violated

/** Implementation type: what Dom_I looks like for a concrete representation. */
enum ImplType:
  case IntType(bits: Int, signed: Boolean)
  case FixedPoint(bits: Int, fractionBits: Int, signed: Boolean = false)
  case Float64

  /** Bounds of Dom_I, used by condition (4) and the third inclusion of (17). */
  def range: Interval = this match
    case IntType(bits, true)  => Interval(-math.pow(2, bits - 1), math.pow(2, bits - 1) - 1)
    case IntType(bits, false) => Interval(0, math.pow(2, bits) - 1)
    case FixedPoint(bits, fb, signed) =>
      val q = math.pow(2, -fb)
      if signed then Interval(-math.pow(2, bits - 1) * q, (math.pow(2, bits - 1) - 1) * q)
      else Interval(0, (math.pow(2, bits) - 1) * q)
    case Float64              => Interval(-Double.MaxValue, Double.MaxValue)

  /** fl(x): the representable value for x, used by (3) and (5). Non-decreasing in x. */
  def nearest(x: Double): Double = this match
    case IntType(_, _)  => math.rint(x)
    case FixedPoint(_, fb, _) => math.floor(x * math.pow(2, fb)) / math.pow(2, fb) // chopping, as in the Patriot register
    case Float64        => x // a Double already is a binary64 value; its error is bounded in maxRepError

  /** sup |x − fl(x)| over d, computed exactly instead of sampling a grid; used by (5). */
  def maxRepError(d: Interval): Double = this match
    case IntType(_, _) =>
      // |x − rint(x)| peaks at half-integers; between two of them it is largest at the ends.
      val firstHalf = math.ceil(d.lo - 0.5) + 0.5
      if firstHalf <= d.hi then 0.5 else math.max(math.abs(d.lo - math.rint(d.lo)), math.abs(d.hi - math.rint(d.hi)))
    case FixedPoint(_, fb, _) =>
      // Chopping error lies in [0, q) and grows within a step; it approaches q below every grid point.
      val q = math.pow(2, -fb)
      val gridAbove = math.floor(d.lo / q) * q + q
      if gridAbove <= d.hi then q else d.hi - nearest(d.hi)
    case Float64 =>
      // Round to nearest: at most half an ulp of the largest magnitude in d.
      math.ulp(math.max(math.abs(d.lo), math.abs(d.hi))) / 2

/** Dom = ⟨Dom_M, Dom_C, Dom_I, ε⟩ (8); Dom_I is given by the implementation type.
  * For an input parameter ε is ε_in of (5); for an output parameter it is ε_out of (6).
  * Dom_C is None when it was never recorded, as with reused code where only the expression and the type are known;
  * every condition on Dom_C is then NotCheckable. */
final case class Domain(domM: Interval, domC: Option[Interval], impl: ImplType, eps: Double):
  private def onCtx(holds: Interval => Boolean): Verdict = domC.fold(Verdict.NotCheckable)(c => Verdict.of(holds(c)))
  /** Dom_eff = Dom_M ∩ Dom_C ∩ Dom_I (1), with Dom_I replaced by its bounds; an unrecorded Dom_C does not narrow. */
  def effective: Interval = domC.fold(domM)(domM.intersect).intersect(impl.range)
  /** (2) Dom_C ⊆ Dom_M. */
  def mathAdmissible: Verdict = onCtx(_.subsetOf(domM))
  /** (3) ∀x ∈ Dom_C: fl(x) ∈ Dom_M. fl is non-decreasing and Dom_M is an interval, so the two ends decide. */
  def mathAdmissibleImpl: Verdict =
    onCtx(c => c.isEmpty || (domM.contains(impl.nearest(c.lo)) && domM.contains(impl.nearest(c.hi))))
  /** (4) Dom_C ⊆ [min Dom_I, max Dom_I]. */
  def implRange: Verdict = onCtx(_.subsetOf(impl.range))
  /** (5) ∀x ∈ Dom_C: |x − fl(x)| ≤ ε_in, exact. */
  def implPrecision: Verdict = onCtx(c => impl.maxRepError(c) <= eps)

object Domain:
  def apply(domM: Interval, domC: Interval, impl: ImplType, eps: Double): Domain = Domain(domM, Some(domC), impl, eps)

/** Parameter P = ⟨N, D, U, Dom, S⟩ (9). */
final case class Param(n: String, d: String, u: Unit, dom: Domain, s: Scale = Scale.Linear):
  /** Dom_C expressed in another unit of the same dimension, step 2 and formula (12). */
  def ctxIn(target: Unit): Interval =
    require(u.dim == target.dim, s"$n: dimension ${u.dim} is not ${target.dim}")
    val c = dom.domC.getOrElse(throw IllegalArgumentException(s"$n: Dom_C is not recorded"))
    val a = target.fromSI(u.toSI(c.lo))
    val b = target.fromSI(u.toSI(c.hi))
    Interval(math.min(a, b), math.max(a, b))

/** Implicit context IC = ⟨Θ, A_txt⟩: numeric context parameters with their own Dom_C, and text assumptions. */
final case class Context(theta: Seq[Param], text: Set[String]):
  /** (11) T_θ ⊆ Dom_C^θ for every θ whose values in the new setting and whose Dom_C^θ are known;
    * the rest are listed for review. */
  def check(actual: Map[String, Interval]): Context.Check =
    val (known, unknown) = theta.partition(p => actual.contains(p.n) && p.dom.domC.isDefined)
    Context.Check(known.filterNot(p => actual(p.n).subsetOf(p.dom.domC.get)).map(_.n), unknown.map(_.n))

  /** IC of the integrated model, step 6: Dom_C of a shared θ is the intersection; text assumptions are pooled. */
  def combine(o: Context): Context.Combined =
    val names = (theta.map(_.n) ++ o.theta.map(_.n)).distinct
    val merged = names.map { n =>
      (theta.find(_.n == n), o.theta.find(_.n == n)) match
        case (Some(a), Some(b)) =>
          require(a.u == b.u, s"$n: bring θ to one unit (step 2) before combining")
          val c = (a.dom.domC, b.dom.domC) match
            case (Some(x), Some(y)) => Some(x.intersect(y))
            case (x, y)             => x.orElse(y)
          a.copy(dom = a.dom.copy(domC = c))
        case (a, b) => a.orElse(b).get
    }
    Context.Combined(Context(merged, text ++ o.text), merged.filter(_.dom.domC.exists(_.isEmpty)).map(_.n))

object Context:
  final case class Check(violated: Seq[String], unknown: Seq[String]):
    def ok: Boolean = violated.isEmpty
  /** conflicts: shared θ whose domains do not intersect – a formally detected contradiction of contexts. */
  final case class Combined(ic: Context, conflicts: Seq[String])
  def text(assumptions: String*): Context = Context(Nil, assumptions.toSet)

/** Model M = ⟨f, {P_i}, {R_j}, IC, D, A⟩ (10), one input and one output are enough here. */
final case class Model(name: String, in: Param, out: Option[Param], ic: Context, d: String, a: String)

enum Overlap:
  case Inclusion, Partial, Incompatible

/** Status of a sequential link. Conditional: (17) holds, but δ was not estimated and 0 was assumed. */
enum LinkStatus:
  case Compatible, Conditional, NotCheckable, Violated

/** Verdict of (17) per level of the downstream input: which inclusion holds. */
final case class LinkCheck(image: Interval, math: Verdict, ctx: Verdict, impl: Verdict, deltaKnown: Boolean):
  def levels: Seq[Verdict] = Seq(math, ctx, impl)
  /** A violated level decides; a link with an unchecked level is never marked compatible. */
  def status: LinkStatus =
    if levels.contains(Verdict.Violated) then LinkStatus.Violated
    else if levels.contains(Verdict.NotCheckable) then LinkStatus.NotCheckable
    else if !deltaKnown then LinkStatus.Conditional
    else LinkStatus.Compatible

object Procedure:
  /** (14) Dom_1..n for any number of models, all converted to a common unit. */
  def common(ctxs: Seq[Interval]): Interval = ctxs.reduce(_ intersect _)

  /** (15) inclusion: the domain of some model equals the common domain; otherwise partial overlap or incompatibility. */
  def classify(ctxs: Seq[Interval]): Overlap =
    val all = common(ctxs)
    if all.isEmpty then Overlap.Incompatible
    else if ctxs.exists(c => c.subsetOf(all)) then Overlap.Inclusion
    else Overlap.Partial

  /** (16) κ = μ(Dom ∩ T) / μ(T) for intervals. */
  def kappa(dom: Interval, target: Interval): Double = dom.intersect(target).length / target.length

  /** Interval extension [f](D) for a function monotone on D, in either direction: exact, endpoints only.
    * A general [f] would need interval arithmetic or subdivision; the article's example is monotone. */
  def monotoneImage(f: Double => Double, d: Interval): Interval =
    val (a, b) = (f(d.lo), f(d.hi))
    Interval(math.min(a, b), math.max(a, b))

  /** (17) with Y = [f_1](D) + [−δ, δ], checked against each level of the downstream input domain.
    * delta = None: no estimate of the upstream model error; δ = 0 is used and the link is at best Conditional. */
  def sequential(f: Double => Double, d: Interval, delta: Option[Double], next: Domain): LinkCheck =
    val y = monotoneImage(f, d).widen(delta.getOrElse(0.0))
    LinkCheck(y, Verdict.of(y.subsetOf(next.domM)), next.domC.fold(Verdict.NotCheckable)(c => Verdict.of(y.subsetOf(c))),
      Verdict.of(y.subsetOf(next.impl.range)), delta.isDefined)

  /** D' = {x ∈ D : [f(x) − δ, f(x) + δ] ⊆ E}, E = Dom_M ∩ Dom_C ∩ [min Dom_I, max Dom_I] of the downstream input. */
  def sequentialPreimage(f: Double => Double, d: Interval, delta: Option[Double], next: Domain): Interval =
    val e = next.effective
    val dl = delta.getOrElse(0.0)
    monotonePreimage(f, d, Interval(e.lo + dl, e.hi - dl))

  /** Preimage {x ∈ D : f(x) ∈ target} for f monotone on D, by bisection on both ends; a decreasing f is mirrored. */
  def monotonePreimage(f: Double => Double, d: Interval, target: Interval, tol: Double = 1e-9): Interval =
    if f(d.lo) > f(d.hi) then increasingPreimage(x => -f(x), d, Interval(-target.hi, -target.lo), tol)
    else increasingPreimage(f, d, target, tol)

  private def increasingPreimage(f: Double => Double, d: Interval, target: Interval, tol: Double): Interval =
    def firstAtLeast(y: Double): Double =
      if f(d.lo) >= y then d.lo
      else if f(d.hi) < y then Double.PositiveInfinity
      else
        var (lo, hi) = (d.lo, d.hi)
        while hi - lo > tol do
          val mid = (lo + hi) / 2
          if f(mid) >= y then hi = mid else lo = mid
        hi
    def lastAtMost(y: Double): Double =
      if f(d.hi) <= y then d.hi
      else if f(d.lo) > y then Double.NegativeInfinity
      else
        var (lo, hi) = (d.lo, d.hi)
        while hi - lo > tol do
          val mid = (lo + hi) / 2
          if f(mid) <= y then lo = mid else hi = mid
        lo
    Interval(firstAtLeast(target.lo), lastAtMost(target.hi))

  /** (6) |f(x) − f̂_N(x)| ≤ ε_out after the largest number of operations N allowed by IC.
    * errorAfter(n) is the worst-case error of the result after n operations, non-decreasing in n,
    * so checking n = N covers every shorter run. */
  def resultPrecision(errorAfter: Long => Double, nMax: Long, epsOut: Double): Boolean =
    errorAfter(nMax) <= epsOut
