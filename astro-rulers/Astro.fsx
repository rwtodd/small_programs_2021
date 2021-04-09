module Astrology

(*

  Just playing around with FSharp, comparing it to the Scala3
  code also in this directory.
  
*)

type Element = Fire | Air | Water | Earth
type Quadruplicity = Cardinal | Fixed | Mutable
type Planet = Saturn | Jupiter | Mars | Sun | Venus | Mercury | Moon
type Sign = Aries | Taurus | Gemini | Cancer
          | Leo | Virgo | Libra | Scorpio
          | Sagittarius | Capricorn | Aquarius | Pisces

let private planets =
   [| Saturn ; Jupiter ; Mars ; Sun ; Venus ; Mercury ; Moon |]

let private signs =
   [|  Aries ; Taurus ; Gemini ; Cancer ;
       Leo ; Virgo ; Libra ; Scorpio ;
       Sagittarius ; Capricorn ; Aquarius ; Pisces |]
                
let exaltation = function
      | Saturn  -> Libra
      | Jupiter -> Cancer
      | Mars    -> Capricorn
      | Sun     -> Aries
      | Venus   -> Pisces
      | Mercury -> Virgo
      | Moon    -> Taurus

let sign_triplicity = function
      | Aries | Leo | Sagittarius  -> Fire
      | Taurus | Virgo | Capricorn -> Earth
      | Gemini | Libra | Aquarius  -> Air
      | Cancer | Scorpio | Pisces  -> Water

let sign_quadrupliticy = function
      | Aries | Cancer | Libra | Capricorn    -> Cardinal
      | Leo | Scorpio | Aquarius | Taurus     -> Fixed
      | Sagittarius | Pisces | Gemini | Virgo -> Mutable

let sign_ruler = function
      | Capricorn | Aquarius -> Saturn
      | Sagittarius | Pisces -> Jupiter
      | Aries | Scorpio      -> Mars
      | Leo                  -> Sun
      | Libra | Taurus       -> Venus
      | Gemini | Virgo       -> Mercury
      | Cancer               -> Moon

let planet_rules p = seq { for s in signs do if (sign_ruler s) = p then s } 

let private sign_shift sign amount =
  signs.[((match sign with
           | Aries       -> 0
           | Taurus      -> 1
           | Gemini      -> 2
           | Cancer      -> 3
           | Leo         -> 4
           | Virgo       -> 5
           | Libra       -> 6
           | Scorpio     -> 7
           | Sagittarius -> 8
           | Capricorn   -> 9
           | Aquarius    -> 10
           | Pisces      -> 11) + amount) % 12]

let sign_opposite s = sign_shift s 6

let fall = exaltation >> sign_opposite

let decan_ruler sign nth = sign_shift sign ((nth - 1)*4) |> sign_ruler
     
let sign_similar = function
  | Leo    -> Cancer
  | Cancer -> Leo
  | s      -> s |> sign_ruler |> planet_rules |> Seq.find (fun s2 -> s2 <> s)
   
let sign_compliment = sign_similar >> sign_opposite

let hello () =
    printfn "**** DECAN RULERS ****"
    for s in signs do
       for d in 1 .. 3 do
          printfn "Decan %A-%d: %A" s d (decan_ruler s d)
    printfn "**** RULERS ****"
    for p in planets do
       printfn "Planet %A rules %A" p (planet_rules p)
    printfn "**** SIMILARS (other sign ruled by same ruler) ****"
    for s in signs do
       printfn "Similar to %A is %A" s (sign_similar s)
    printfn "**** COMPLIMENTS (opposite of similar sign) ****"
    for s in signs do
       printfn "Compiment to %A is %A" s (sign_compliment s)
    printfn "**** EXALTATIONS and FALLS (of sign ruler) ****"
    for s in signs do
       let ruler = sign_ruler s
       printfn "Exaltation/Fall of %A is %A/%A" s (exaltation ruler) (fall ruler)

hello ()
