namespace Chapter1
    module BallSimulation =
        open System
        let dt = 0.1
        let g = -9.81
        
        let simulation_step (y,v) =
            let y',v' = (y+v*dt, v+g*dt)
            if y' < 0.0 then
                (0.0, -v'*0.7)
            else
                (y',v')
        
        let print_scene (y1,v1,y2,v2) =
            do Console.Clear()
            let y1,v1,y2,v2 = int y1, int v1, int y2, int v2
            for j = 10 downto 0 do
                for i = 0 to 30 do
                    if ((y1+1) = j && i = 10) || ((y2+1) = j && i = 20) then
                        Console.Write("b")
                    elif j = 0 || i = 0 || j = 10 || i = 30 then
                        Console.Write("*")
                    else
                        Console.Write(" ")
                Console.Write("\n")
            ignore(Console.ReadKey())

        let simulation () =
            let rec simulation (y1,v1,y2,v2) =
                do print_scene (y1,v1,y2,v2)
                let y1',v1' = simulation_step (y1,v1)
                let y2',v2' = simulation_step (y2,v2)
                if abs v1' > 0.1 || abs v2' > 0.1 then
                    do simulation (y1',v1',y2',v2')
            do simulation (5.0,-2.0,7.0,-3.0)