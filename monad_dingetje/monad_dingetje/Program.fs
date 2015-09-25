open System

type option<'a> =
 | Just of 'a
 | None

let ret a = match a with | Just a -> a | None -> 0

type OptionBuilder() = 
  member this.Bind(o,f) =
    match o with 
    | Just x -> f x
    | None -> None
  member this.Return x = Just x
  member this.Zero = None

let opt = OptionBuilder()
let sum (a:option<'a>) (b:option<'a>) = 
  opt{
    let! a_v = a  
    let! b_v = b  
    return a_v + b_v
  }

let doFunc f a:option<'a> =
  match a with
  | Just x -> Just(f (ret a))
  | None -> None

let add a b =
   b + a
  
let i = Just 1
let j = doFunc (add 1) i

let x = sum i j

printf "%A" (ret x)
Console.ReadKey()

//Type OptionBuilder (x:option<'a>) (y:option<'b>) = {
//    this.bind(x,y)
//    let! x_v = fun Just x -> x
//    let! y_v = fun Just y -> y
//    ret (x_v + y_v)
//  }

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
