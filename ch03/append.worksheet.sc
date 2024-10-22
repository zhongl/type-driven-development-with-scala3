//> using scala 3.5.2-RC1

import scala.compiletime.ops.int.*

enum Vect[N <: Int, +A]:
  case `[]`                                                       extends Vect[0, Nothing]
  case ::[N <: Int, +A] private[Vect] (head: A, tail: Vect[N, A]) extends Vect[S[N], A]

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

sealed trait Prop[A, M <: Int, N <: Int]:
  extension (xs: Vect[M, A]) def concat(ys: Vect[N, A]): Vect[M + N, A]
object Prop:
  import scala.language.implicitConversions

  given [A, B](using A =:= B): Conversion[A, B] = _.asInstanceOf

  type Eq1[A, N <: Int] = Vect[N, A] =:= Vect[0 + N, A]

  given nil[A, N <: Int](using Eq1[A, N]): Prop[A, 0, N] with
    extension (xs: Vect[0, A]) def concat(ys: Vect[N, A]): Vect[0 + N, A] = ys
  end nil

  type Eq2[A, M <: Int, N <: Int] = Vect[S[M + N], A] =:= Vect[S[M] + N, A]

  given cons[A, M <: Int, N <: Int](using Prop[A, M, N], Eq2[A, M, N]): Prop[A, S[M], N] with
    extension (xs: Vect[S[M], A])
      def concat(ys: Vect[N, A]): Vect[S[M] + N, A] = (xs: @unchecked) match
        case h :: t => h :: t.concat(ys)
  end cons

  given instance[A, M <: Int, N <: Int](using Prop[A, M, N]): ++[Vect[M, A], Vect[N, A]] with
    type Out = Vect[M + N, A]
    def apply(xs: Vect[M, A], ys: Vect[N, A]): Out = xs.concat(ys)
end Prop

import Prop.given

val xs = 1 :: `[]`

xs ++ `[]`

(`[]` ++ xs) //(using summon[++[Int, 0, S[0]]]) not working blow v3.5.2-RC1

xs ++ xs

// Copy from https://github.com/milessabin/strangeloop-2013/blob/master/src/main/scala/strangeloop/vect.scala#L55
trait Concat[A, XS, YS]:
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