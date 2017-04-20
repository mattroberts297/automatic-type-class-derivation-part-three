trait DefaultedParser[A] {
  def parse(args: List[String]): A
}

object DefaultedParser {
  import shapeless.LabelledGeneric
  import shapeless.{HList, HNil, ::}
  import shapeless.Lazy
  import shapeless.Witness
  import shapeless.labelled.FieldType
  import shapeless.labelled.field
  import shapeless.Default

  def apply[A](
    implicit
    st: Lazy[DefaultedParser[A]]
  ): DefaultedParser[A] = st.value

  def create[A](thunk: List[String] => A): DefaultedParser[A] = {
    new DefaultedParser[A] {
      def parse(args: List[String]): A = thunk(args)
    }
  }

  implicit def genericParser[A, R <: HList, D <: HList](
    implicit
    defaults: Default.AsOptions.Aux[A, D],
    generic: LabelledGeneric.Aux[A, R],
    parser: Lazy[UnderlyingParser[R, D]]
  ): DefaultedParser[A] = {
    create { args => generic.from(parser.value.parse(args, defaults())) }
  }
}
