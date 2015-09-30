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

type list<'a> =
  | Full of 'a * list<'a>
  | Empty

type ListBuilder() =
  member this.Bind (o:list<'a>, f:'a->list<'b>) :list<'b> = 
    match o:list<'a> with
    | Full (x:'a :: xs:list<'a>) :list<'b> -> (f x) @ (this.Bind xs f)
    | Empty -> Empty
  member this.Return x = Full x
  member this.Zero = Empty
  
let lst = ListBuilder()
  
let addToList (a:int) (b:list<'a>) =
  lst{
    return a :: b
    
  }


type State<'a,'s> = 's -> ('a * 's)

type StateBuilder() =
  member this.bind (o:State<'a,'s>, f:'s -> State<'b,'s>):State<'b,'s> =
    fun s ->
    let (a,s') = o s
    let (b,s'') = f a s'
    (b,s'')
  member this.Return x y = ((fun (a:'a)  (s:'s) -> (a * s)) x y)
  member this.Zero = ()
  
  
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
