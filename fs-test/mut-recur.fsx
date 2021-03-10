(* does mutual tail-recursion blow the stack in FSHARP??? *)

let rec t1 limit x =
   if limit = 0 then x else t2 (limit - 1) (x - 21I)
and t2 limit x =
   if limit = 0 then x else t1 (limit - 1) (x + 33I)

Seq.initInfinite (fun limit -> (limit, t1 (limit*10_000) 0I)) |> Seq.iter (printfn "it's %O")

(* no, the stack is not blown.  It seems mutual tail recursion works. *)
