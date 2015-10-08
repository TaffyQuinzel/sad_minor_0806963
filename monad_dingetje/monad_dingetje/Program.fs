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


type MaybeState<'a,'s> = 's -> Result<'a * 's>
type MaybeStateBuilder() =
  member this.Bind (o:MaybeState<'a,'s>, f:'a -> MaybeState<'b,'s>) : MaybeState<'b,'s> =
    fun (s:'s) ->
      mb{
        let! a,s' = o s
        return! f a s'
      }
      // let x,y = o s
      // match x with
      //   | Done a ->
      //     let v,w = f a y
      //     match v with
      //       | Done b -> v,w
      //       | Crash b -> Crash b,w
      //   | Crash a -> Crash a,y
  member this.ReturnFrom x  = fun s -> Done x,s 
  member this.Zero = Crash


let ms = MaybeStateBuilder()

let f1 a = (fun o -> o) a
let f2 a:State<'a,'s> = (fun o -> o) a

let maybeStateTest (i) =
  ms{
    return! i
  }
let maybeStateTest2 (i:State<Result<'a>,'s>) =
  ms{
    let a = i
    return! a
  }
let createState i = Done,i    

// let msTest (a:int)(w:int)=
//   ms{
//     let! a = (f1,1)
//     let! w = f2 9
//     return! w
//   }

  

//Type OptionBuilder (x:option<'a>) (y:option<'b>) = {
//    this.bind(x,y)
//    let! x_v = fun Just x -> x
//    let! y_v = fun Just y -> y
//    ret (x_v + y_v)
//  }


// type MaybeStateList<'a,'s> = 's -> (List<'a>,'s)


// type ListMaybeState<list<'a,'s>> = list<> -> 


[<EntryPoint>]
let main argv = 
  // printfn "%A" argv
  let hal = maybeStateTest 4
  let hel = createState 3
  printfn "%A" (hal)
  printfn "%A" (hel)
  printfn "%A" (maybeStateTest2 hal hel)
  printfn "%A" (maybeStateTest 9)
  0 // return an integer exit code
