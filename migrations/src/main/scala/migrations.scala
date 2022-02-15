import cats.Monoid
import cats.instances.all._
import shapeless._
import shapeless.labelled.{field, FieldType}
import shapeless.ops.hlist
import shapeless.ops.coproduct

trait Migration[A, B] {
  def apply(original: A): B
}

object Migration {
  def pure[A, B](func: A => B): Migration[A, B] =
    new Migration[A, B] {
      def apply(original: A): B =
        func(original)
    }

  implicit def genericMigration[
    A,
    B,
    AR <: HList,
    BR <: HList,
    I <: HList,
    N <: HList,
    C <: HList,
  ](
    implicit agen: LabelledGeneric.Aux[A, AR],
    bgen: LabelledGeneric.Aux[B, BR],
    inter: hlist.Intersection.Aux[AR, BR, I],
    diff: hlist.Diff.Aux[BR, I, N],
    monoid: Monoid[N],
    concat: hlist.Prepend.Aux[I, N, C],
    align: hlist.Align[C, BR],
  ): Migration[A, B] =
    pure(value =>
      bgen.from(
        align.apply(
          concat.apply(
            inter.apply(
              agen.to(value)
            ),
            monoid.empty
          )
        )
      )
    )

}

case class SameA(a: String, b: Int, c: Boolean)
case class SameB(a: String, b: Int, c: Boolean)

case class DropFieldA(a: String, b: Int, c: Boolean)
case class DropFieldB(a: String, c: Boolean)

case class AddFieldA(a: String)
case class AddFieldB(a: String, z: Int)

case class ReorderA(a: String, z: Int)
case class ReorderB(z: Int, a: String)

case class KitchenSinkA(a: String, b: Int, c: Boolean)
case class KitchenSinkB(c: Boolean, z: Option[String], a: String)

object Main extends Demo {
  implicit class MigrationOps[A](original: A) {
    def migrateTo[B](implicit migration: Migration[A, B]): B =
      migration(original)
  }

  implicit val hnilMonoid: Monoid[HNil] = Monoid.instance(
    HNil,
    (_, _) => HNil
  )

  implicit def hconsMonoid[K <: Symbol, H, T <: HList](
    implicit hm: Lazy[Monoid[H]],
    tm: Monoid[T],
  ): Monoid[FieldType[K, H] :: T] = Monoid.instance(
    field[K](hm.value.empty) :: tm.empty,
    (left, right) =>
      field[K](hm.value.combine(
        left.head,
        right.head
      )) :: tm.combine(left.tail, right.tail)
  )

  print(SameA("abc", 123, true).migrateTo[SameB])
  print(DropFieldA("abc", 123, true).migrateTo[DropFieldB])
  print(AddFieldA("abc").migrateTo[AddFieldB])
  print(ReorderA("abc", 123).migrateTo[ReorderB])
  print(KitchenSinkA("abc", 123, true).migrateTo[KitchenSinkB])
}
