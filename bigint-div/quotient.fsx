(*
    BigInteger quotients...

    How can I take two bigints and get a floating-point quotient out of it? (at least, as a string?)
*)
open System.Numerics

(* Try #1 ... just replicate grade-school long-division *)
let quotient prec a b =
    let sbuff = new System.Text.StringBuilder()
    let rec worker (remainder, prec) =
        let (qt, rm) = BigInteger.DivRem(remainder, b)
        sbuff.Append(qt) |> ignore
        if rm.IsZero || (prec = 0) 
        then sbuff.ToString()
        else worker (rm * 10I, prec - 1)
    let (qt, rm) = BigInteger.DivRem(a,b)
    sbuff.Append(qt).Append('.') |> ignore
    worker (rm * 10I, prec)

let fdiv = quotient 12
printfn "22/7         = %s" <| fdiv 22I 7I
printfn "10493/393149 = %s" <| fdiv 10493I 393149I
printfn "1294/12247   = %s" <| fdiv 1294I 12247I
printfn "4314/741     = %s" <| fdiv 4314I 741I

