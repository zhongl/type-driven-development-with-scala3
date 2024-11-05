
object Printf:
  import scala.compiletime.ops.string.*

  def apply(s: String)(using r: Result[Args[s.type]]): r.Out = r(s)

  trait Result[T <: Tuple]:
    type Out
    def apply(pattern: String): Out

  given string: Result[EmptyTuple] with
    type Out = Unit
    def apply(pattern: String): Out = print(pattern)

  given func[A]: Result[A *: EmptyTuple] with
    type Out = A => Unit
    def apply(pattern: String): Out = a => print(pattern.formatted(a))

  given tuple[T <: Tuple]: Result[T] with
    type Out = T => Unit
    def apply(pattern: String): Out = t => print(pattern.formatted(t.toArray*))

  type Args[S <: String] <: Tuple = Length[S] match
    case 0 | 1 => EmptyTuple
    case _ =>
      Read[S] match
        case ('%', t) =>
          Read[t] match
            case ('d', tt) => Int *: Args[tt]
            case ('s', tt) => String *: Args[tt]
            case (_, tt)   => Args[tt]
        case (_, t) => Args[t]

  type Read[S <: String] <: (Char, String) = S match
    case _ => (CharAt[S, 0], Substring[S, 1, Length[S]])

end Printf

Printf("abc")
Printf("num: %d")(1)
Printf("num: %d, text: %s")(1, "foo")
