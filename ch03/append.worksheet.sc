import scala.compiletime.ops.int.*

enum Vect[N <: Int, +A]:
  case `[]`                                                  extends Vect[0, Nothing]
  case ::[N <: Int, +A] private[Vect] (x: A, xs: Vect[N, A]) extends Vect[S[N], A]

  def ::[B >: A](b: B) = new ::(b, this)
end Vect

object Vect:
  trait ++[XS, YS]:
    type Out
    def apply(xs: XS, ys: YS): Out

  extension [A, N <: Int](xs: Vect[N, A])
    def ++[B >: A, M <: Int](ys: Vect[M, B])(using f: ++[Vect[N, B], Vect[M, B]]): f.Out = f(xs, ys)
end Vect

import Vect.*

opaque type Append[M <: Int, N <: Int, O <: Int, A] = (Vect[M, A], Vect[N, A]) => Vect[O, A]
object Append:
  given nil[N <: Int, A]: Append[0, N, N, A] = (_, ys) => ys
  given cons[M <: Int, N <: Int, O <: Int, A](using
    f: Append[M, N, O, A]
  ): Append[S[M], N, S[O], A] =
    case (x :: xs, ys) => x :: f(xs, ys)

  given instance[M <: Int, N <: Int, O <: Int, A](using
    f: Append[M, N, O, A]
  ): ++[Vect[M, A], Vect[N, A]] with
    type Out = Vect[O, A]
    def apply(xs: Vect[M, A], ys: Vect[N, A]): Out = f(xs, ys)
end Append

import Append.given

val xs = 1 :: `[]`

xs ++ `[]`

(`[]` ++ xs)

xs ++ xs

// Copy from https://github.com/milessabin/strangeloop-2013/blob/master/src/main/scala/strangeloop/vect.scala#L55
sealed trait Concat[A, XS, YS]:
  type Sum <: Int
  type Out = Vect[Sum, A]
  def apply(xs: XS, ys: YS): Out
end Concat
object Concat:
  given nil[A, N <: Int]: Concat[A, Vect[0, A], Vect[N, A]] with
    type Sum = N
    def apply(xs: Vect[0, A], ys: Vect[N, A]): Out = ys
  end nil

  given cons[A, M <: Int, N <: Int](using
    cc: Concat[A, Vect[M, A], Vect[N, A]]
  ): Concat[A, Vect[S[M], A], Vect[N, A]] with
    type Sum = S[cc.Sum]
    def apply(xs: Vect[S[M], A], ys: Vect[N, A]): Out = (xs: @unchecked) match
      case h :: t => h :: (cc(t, ys))
  end cons

  given instance[A, M <: Int, N <: Int](using cc: Concat[A, Vect[M, A], Vect[N, A]]): ++[Vect[M, A], Vect[N, A]] with
    type Out = cc.Out
    def apply(xs: Vect[M, A], ys: Vect[N, A]): Out = cc(xs, ys)
end Concat

(xs ++ xs)(using Concat.instance)
