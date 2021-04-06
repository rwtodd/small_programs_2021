package org.rwtodd.testdot

// Compute the Vedic decan rulers and print them out...
// along with some other astrology facts.
// 
// Just playing with Scala 3.0.0-RC2 ... 
//   - the enums are great
//   - the indentation syntax is better than I thought it would be
//   - the package-level functions are also very nice

enum Element { case Fire, Air, Water, Earth }

enum Quadruplicity { case Cardinal, Fixed, Mutable }

enum Planet:
   case Saturn, Jupiter, Mars, Sun, Venus, Mercury, Moon

   def exaltation = this match
      case Saturn => Sign.Libra
      case Jupiter => Sign.Cancer
      case Mars => Sign.Capricorn
      case Sun => Sign.Aries
      case Venus => Sign.Pisces
      case Mercury => Sign.Virgo
      case Moon => Sign.Taurus

   def fall = this.exaltation.opposite

   def rules = for { s <- Sign.values; if s.ruler == this } yield s


enum Sign(val element: Element,
          val quadruplicity: Quadruplicity,
          val ruler: Planet):
    case Aries extends Sign(Element.Fire, Quadruplicity.Cardinal, Planet.Mars)
    case Taurus extends Sign(Element.Earth, Quadruplicity.Fixed, Planet.Venus)
    case Gemini extends Sign(Element.Air, Quadruplicity.Mutable, Planet.Mercury)
    case Cancer extends Sign(Element.Water, Quadruplicity.Cardinal, Planet.Moon)
    case Leo extends Sign(Element.Fire, Quadruplicity.Fixed, Planet.Sun)
    case Virgo extends Sign(Element.Earth, Quadruplicity.Mutable, Planet.Mercury)
    case Libra extends Sign(Element.Air, Quadruplicity.Cardinal, Planet.Venus)
    case Scorpio extends Sign(Element.Water, Quadruplicity.Fixed, Planet.Mars)
    case Sagittarius extends Sign(Element.Fire, Quadruplicity.Mutable, Planet.Jupiter)
    case Capricorn extends Sign(Element.Earth, Quadruplicity.Cardinal, Planet.Saturn)
    case Aquarius extends Sign(Element.Air, Quadruplicity.Fixed, Planet.Saturn)
    case Pisces extends Sign(Element.Water, Quadruplicity.Mutable, Planet.Jupiter)

    def opposite = Sign.values((this.ordinal + 6) % 12)
    def decanRuler(n: Int) = Sign.values((this.ordinal + (n-1)*4) % 12).ruler
    def similar = this match
      case Leo    => Cancer
      case Cancer => Leo
      case _      => this.ruler.rules.find(s => s != this).get
    def compliment = this.similar.opposite


@main def hello: Unit =
    println("**** DECAN RULERS ****")
    for {
       s <- Sign.values
       d <- Seq(1,2,3)
    } do println(s"Decan $s-$d: ${s.decanRuler(d)}")
    println("**** RULERS ****")
    for {
       p <- Planet.values
    } do println(s"Planet $p rules ${p.rules.mkString(", ")}")
    println("**** SIMILARS (other sign ruled by same ruler) ****")
    for { 
       s <- Sign.values
    } do println(s"Similar to $s is ${s.similar}")
    println("**** COMPLIMENTS (opposite of similar sign) ****")
    for { 
       s <- Sign.values
    } do println(s"Compliment to $s is ${s.compliment}")
    println("**** EXALTATIONS and FALLS (of sign ruler) ****")
    for {
       s <- Sign.values
    } do println(s"Exaltation/Fall of $s is ${s.ruler.exaltation}/${s.ruler.fall}")
