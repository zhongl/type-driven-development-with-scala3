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

enum `=`[-A, +B]:
  case Refl extends `=`[Any, Nothing]
object `=`:
  given [A, B](using A =:= B): `=`[A, B] = Refl

summon[1 `=` 1]

Dec[1 `=` 1]
Dec[1 `=` 2]
