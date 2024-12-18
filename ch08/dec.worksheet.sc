enum Dec[A]:
  case Yes[A](proof: A)            extends Dec[A]
  case No[A](contra: A => Nothing) extends Dec[A]
object Dec:
  import scala.compiletime.summonFrom

  inline def apply[A]: Dec[A] = summonFrom:
    case a: A => Yes(a)
    case _    => No(contra)

  lazy val contra: Any => Nothing = new:
    def apply(a: Any): Nothing      = throw IllegalStateException("No Contra")
    override def toString(): String = "contra"
end Dec
import Dec.*

Dec[1 =:= 1]
Dec[1 =:= 2]

enum `=`[A, B]:
  case Refl[A]() extends (A `=` A)
object `=`:
  given [A]: (A `=` A) = Refl()

import `=`.*

summon[1 `=` 1]
Dec[1 `=` 1]

import scala.compiletime.ops.int.S
enum Nat[N <: Int]:
  case Zero                      extends Nat[0]
  case Succ[N <: Int](n: Nat[N]) extends Nat[S[N]]
end Nat
object Nat:
  extension [M <: Int](m: Nat[M]) inline def =?[N <: Int](n: Nat[N]): Dec[M `=` N] = check(m, n).result

  import scala.util.control.TailCalls.*
  private def check[M <: Int, N <: Int]: (Nat[M], Nat[N]) => TailRec[Dec[M `=` N]] =
    case (Zero, Zero) => done(Yes(Refl()))
    case (Zero, _)    => done(No(contra))
    case (_, Zero)    => done(No(contra))
    case (Succ(m), Succ(n)) =>
      tailcall(check(m, n)).map:
        case No(_)       => No(contra)
        case Yes(Refl()) => Yes(Refl())

end Nat
import Nat.*

Zero =? Zero
Zero =? Succ(Zero)
