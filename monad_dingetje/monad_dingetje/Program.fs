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

// printf "%A" (ret x)
// Console.ReadKey()

// type list<'a> =
//   | Full of 'a * list<'a>
//   | Empty

type ListBuilder() =
  member this.Bind (o:list<'a>, f:'a->list<'b>) :list<'b> = 
    match o:list<'a> with
    | x::xs -> (f x) @ this.Bind (xs,f)
    | [] -> []
  member this.Return x = x
  member this.Zero = []

// let lst = ListBuilder()

// let addToList (a:int) (b:list<'a>) =
//   lst{
//     return a :: b

//   }


type State<'a,'s> = 's -> ('a * 's)

type StateBuilder() =
  member this.Bind (o:State<'a,'s>, f:'a -> State<'b,'s>):State<'b,'s> =
    fun s ->
      let (a,s') = o s
      f a s'
  member this.Return x  = fun (s:'s) -> x,s  
  // member this.Zero = (),o

let sb = StateBuilder()

let getState = fun(s:'s) -> s,s
let setState newState = fun(s:'s) -> (),newState

let createState i =
  sb{
    let! x = getState
    do! setState (x + i)
    return x
  }

//----------------------------------------------
type Result<'a> = Done of 'a | Crash of string


type MaybeBuilder() =
  member this.Bind (a:Result<'a>, b: 'a -> Result<'b>) : Result<'b>=
    match a with
      | Done c -> b c
      | Crash e -> Crash e
    // member this.Return a:'a = Done a
    member this.ReturnFrom a:'a = a
    // member this.zero = Crash "haha grapjas"
    member this.zero = ()

let mb = MaybeBuilder()


//----------------------------------------------
type MaybeState<'a,'s> = 's -> Result<'a * 's>
type MaybeStateBuilder() =
  member this.Bind (o:MaybeState<'a,'s>, f:'a -> MaybeState<'b,'s>) : MaybeState<'b,'s> =
    fun (s:'s) ->
      mb{
        let! a,s' = o s
        return! f a s'
      }
  member this.ReturnFrom x  = x  
  member this.Return x  = fun s -> Done (x s) 
  member this.Zero = Crash


let ms = MaybeStateBuilder()

let getMaybeState = fun(s:'s) -> Done(s,s)
let setMaybeState newState = fun(s:'s) -> s,newState

let createMaybeState(i) =
  ms{
    let! x = getMaybeState
    let y = setMaybeState (x + i)
    return y
  }


//----------------------------------------------
// type MaybeStateList<'a,'s> = 's -> (List<'a>,'s)


//----------------------------------------------
// type ListMaybeState<list<'a,'s>> = list<> -> 


[<EntryPoint>]
let main argv = 
  // printfn "%A" argv
  // let hal = createState f1 
  // let hol = maybeStateTest2 f1 hal 7
  let hel = createState 3 
  let hal = createMaybeState 3 
  // printfn "%A" (hal)
  printfn "%A" (hel)
  printfn "%A" (hel 8)
  printfn "%A" (hal)
  printfn "%A" (hal 8)
  // printfn "%A" (maybeStateTest2 hal 7)
  // printfn "%A" (maybeStateTest2 hol 8)
  // printfn "%A" (maybeStateTest 9)
  0 // return an integer exit code
