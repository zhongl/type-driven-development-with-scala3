import scala.annotation.tailrec
import scala.compiletime.ops.int.S
import scala.util.NotGiven

enum Vect[N <: Int, +A]:
  case `[]`                                                  extends Vect[0, Nothing]
  case ::[N <: Int, +A] private[Vect] (x: A, xs: Vect[N, A]) extends Vect[S[N], A]

  def ::[B >: A](b: B) = new ::(b, this)
end Vect

object Vect:
  extension [N <: Int, A](xs: Vect[N, A])
    @tailrec
    def seq(acc: Seq[A] = Seq()): Seq[A] = xs match
      case `[]`     => acc
      case x :: xxs => xxs.seq(acc :+ x)

    def show(using f: Show[Vect[N, A]]) = f(xs, Pad("  "))

    def zip[B, C](ys: Vect[N, B], f: (A, B) => C): Vect[N, C] = (xs, ys) match
      case (`[]`, `[]`)         => `[]`
      case (x :: xxs, y :: yys) => f(x, y) :: xxs.zip(yys, f)
  end extension

  opaque type Show[-A] = (A, Pad) => String
  object Show:
    given leaf[A]: Show[Vect[?, A]] = (xs, p) => xs.seq().mkString(s"$p[", ", ", "]")
    given branche[A <: Vect[?, ?]](using f: Show[A]): Show[Vect[?, A]] =
      (xs, p) => xs.seq().map(f(_, p.inc)).mkString(s"$p[\n", s"\n$p", s"\n$p]")
  end Show
  
  class Pad private (base: String, count: Int):
    def this(base: String) = this(base, 0)
    def inc                         = Pad(base, count + 1)
    override def toString(): String = base * count

  type Mat[R <: Int, C <: Int, A] = Vect[R, Vect[C, A]]
  extension [R <: Int, C <: Int, A](mat: Mat[R, C, A])
    def trans: Nat[C] ?=> Mat[C, R, A] =
      mat match
        case `[]`    => fill[C](`[]`)
        case x :: xs => x.zip(xs.trans, _ :: _)
    end trans
  end extension

  def fill[N <: Int](using n: Nat[N]): [A] => A => Vect[N, A] = n match
    case Nat.Zero    => [A] => _ => `[]`
    case Nat.Succ(n) => [A] => a => a :: fill(using n)(a)

  enum Nat[N <: Int]:
    case Zero                      extends Nat[0]
    case Succ[N <: Int](n: Nat[N]) extends Nat[S[N]]
  end Nat
  object Nat:
    given Nat[0]                                 = Zero
    given [N <: Int](using n: Nat[N]): Nat[S[N]] = Succ(n)
end Vect

import Vect.*

`[]`.show

(1 :: 2 :: `[]`).show

val mat: Mat[3, 2, Int] = (1 :: 2 :: `[]`)
  :: (3 :: 4 :: `[]`)
  :: (5 :: 6 :: `[]`) :: `[]`

mat.show

mat.trans.show

val v = 1 :: `[]`

((v :: `[]`) :: `[]`).show
