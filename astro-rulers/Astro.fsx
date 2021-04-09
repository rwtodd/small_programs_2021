module Astrology

(*

  Just playing around with FSharp, comparing it to the Scala3
  code also in this directory.
  
*)

type Element = | Fire | Air | Water | Earth
type Quadruplicity = | Cardinal | Fixed | Mutable
type Planet = | Saturn | Jupiter | Mars | Sun | Venus | Mercury | Moon
type Sign =
     | Aries | Taurus | Gemini | Cancer
     | Leo | Virgo | Libra | Scorpio
     | Sagittarius | Capricorn | Aquarius | Pisces

let private planets =
   [| Saturn ; Jupiter ; Mars ; Sun ; Venus ; Mercury ; Moon |]

let private signs =
   [|  Aries ; Taurus ; Gemini ; Cancer ;
       Leo ; Virgo ; Libra ; Scorpio ;
       Sagittarius ; Capricorn ; Aquarius ; Pisces |]
                
let exaltation p =
   match p with
      | Saturn  -> Libra
      | Jupiter -> Cancer
      | Mars    -> Capricorn
      | Sun     -> Aries
      | Venus   -> Pisces
      | Mercury -> Virgo
      | Moon    -> Taurus

let sign_triplicity s =
   match s with
     | Aries | Leo | Sagittarius -> Fire
     | Taurus | Virgo | Capricorn -> Earth
     | Gemini | Libra | Aquarius -> Air
     | Cancer | Scorpio | Pisces -> Water

let sign_quadrupliticy s =
   match s with
     | Aries | Cancer | Libra | Capricorn -> Cardinal
     | Leo | Scorpio | Aquarius | Taurus -> Fixed
     | Sagittarius | Pisces | Gemini | Virgo -> Mutable

let sign_ruler s =
   match s with
     | Capricorn | Aquarius -> Saturn
     | Sagittarius | Pisces -> Jupiter
     | Aries | Scorpio -> Mars
     | Leo -> Sun
     | Libra | Taurus -> Venus
     | Gemini | Virgo -> Mercury
     | Cancer -> Moon

let planet_rules p = seq {
   for s in signs do
      if (sign_ruler s) = p then
         s
} 

let sign_opposite s =
   match s with
     | Aries       -> Libra
     | Taurus      -> Scorpio
     | Gemini      -> Sagittarius
     | Cancer      -> Capricorn
     | Leo         -> Aquarius
     | Virgo       -> Pisces
     | Libra       -> Aries
     | Scorpio     -> Taurus
     | Sagittarius -> Gemini
     | Capricorn   -> Cancer
     | Aquarius    -> Leo
     | Pisces      -> Virgo

let fall = exaltation >> sign_opposite

let decan_ruler sign nth =
  let idx = (Array.findIndex (fun s -> s = sign) signs) + (nth - 1)*4
  sign_ruler signs.[idx % 12]
     
let sign_similar s =
  match s with
     | Leo    -> Cancer
     | Cancer -> Leo
     | _ -> s |> sign_ruler |> planet_rules |> Seq.find (fun s2 -> s2 <> s)
   
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
       printfn "Compiment to to %A is %A" s (sign_compliment s)
    printfn "**** EXALTATIONS and FALLS (of sign ruler) ****"
    for s in signs do
       let ruler = sign_ruler s
       printfn "Exaltation/Fall of %A is %A/%A" s (exaltation ruler) (fall ruler)

hello ()
