import fpinscala.state.RNG
import fpinscala.state.RNG._

RNG.double3(Simple(42))
RNG.ints(15)(Simple(42))

sequence(List(unit(1), unit(2), unit(3)))(Simple(42))._1

_ints(5)(Simple(42))
