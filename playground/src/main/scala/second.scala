import shapeless._

trait Second[L <: HList] {
    type Out

    def apply(list: L): Out
}

object Second {
    type Aux[L <: HList, O] = Second[L] {type Out = O}

    def apply[L <: HList](
        implicit second: Second[L]
    ): Aux[L, second.Out] = second
    // ^ returning Aux so that
    // Out type is not erased as with
    // returning Second[L]

    implicit def second[A, B, R <: HList]: Aux[A :: B :: R, B] =
        new Second[A :: B :: R] {
            type Out = B

            def apply(list: A :: B :: R): B = 
                list.tail.head
        }
}

object MainSecond extends Demo {
    val list: Int :: Boolean :: Float :: HNil =
        1 :: true :: 1.2f :: HNil

    println(Second[Int :: Boolean :: Float :: HNil].apply(list))
}