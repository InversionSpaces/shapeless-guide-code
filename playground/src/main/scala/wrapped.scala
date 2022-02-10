import shapeless._
import shapeless.ops.hlist._

object WrappedMain extends Demo {
    def getWrappedValue1[A, H](a: A)(
        implicit
        // Compiler can not find Repr
        // and check it against constraint
        // at the same time
        gen: Generic.Aux[A, H :: HNil]
    ): H = gen.to(a).head

    final case class Wrapped(a: Int)

    // Does not compile
    //println(getWrappedValue1(Wrapped(1)))

    def getWrappedValue2[A, H, T <: HList, Repr <: HList](a: A)(
        implicit
        gen: Generic.Aux[A, Repr],
        ev: IsHCons.Aux[Repr, H, T]
    ): H = ev.head(gen.to(a))

    println(getWrappedValue2(Wrapped(2)))
}

