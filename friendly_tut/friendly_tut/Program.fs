// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open Chapter1

[<EntryPoint>]
let main argv = 
    do Chapter1.BallSimulation.simulation()
    do Chapter1.BallSimulation.simulation_extended()
    0 // return an integer exit code
