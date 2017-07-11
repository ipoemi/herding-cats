sealed trait Person {}

case object John extends Person {}

case object Mary extends Person {}

case object Sam extends Person {}

val a: Set[Person] = Set[Person](John, Mary, Sam)


sealed trait Breakfast {}

case object Eggs extends Breakfast {}

case object Oatmeal extends Breakfast {}

case object Toast extends Breakfast {}

case object Coffee extends Breakfast {}

val favoriteBreakfast: Person => Breakfast = {
  case John => Eggs
  case Mary => Coffee
  case Sam => Coffee
}

val favoritePerson: Person => Person = {
  case John => Mary
  case Mary => John
  case Sam => Mary
}

identity(John)

val favoritePersonsBreakfast = favoriteBreakfast compose favoritePerson


val johnPoint: Unit => Person = { case () => John }

val johnFav = favoriteBreakfast compose johnPoint

johnFav(())

