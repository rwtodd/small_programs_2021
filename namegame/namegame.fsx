module NameGame

(*
   still playing with fsharp... this is a perl challenge I saw at
   https://raku-musings.com/named-roots.html

   > NameGame.play "Annie";;
   Annie, Annie, bo-bannie
   Bonana-fanna fo-fannie
   Fee fi mo-mannie
   Annie!
   val it : unit = ()
   
   > NameGame.play "Sam";;
   Sam, Sam, bo-bam
   Bonana-fanna fo-fam
   Fee fi mo-mam
   Sam!
   val it : unit = ()
   
*)

let y_is_vowel = false
let private vowels = Set.ofList [ 'a'; 'e'; 'i'; 'o'; 'u' ]
                     |> Set.union (if y_is_vowel then Set.ofList [ 'y' ] else Set.empty)

let play (name:string) =
   let lower = name.ToLower()
   let y = if (Set.contains lower.[0] vowels) then lower
           else lower.Substring(1)
   let rename letter = if lower.[0] = letter then y else string letter + y
   printfn "%s, %s, bo-%s" name name (rename 'b')
   printfn "Bonana-fanna fo-%s" (rename 'f')
   printfn "Fee fi mo-%s" (rename 'm')
   printfn "%s!" name

