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
        
        let simulation_step_extended (x,y,v) =
            let x',y',v' = (x+(abs v)*dt, y+v*dt, v+g*dt)
            if y' < 0.0 then
                (x', 0.0, -v'*0.7)
            else
                (x',y',v')
        
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

        let print_scene_extended (x,y,v) =
            do Console.Clear()
            let x,y,v = int x, int y, int v
            for j = 10 downto 0 do
                for i = 0 to 50 do
                    if (y+1) = j && (x+1) = i then
                        Console.Write("b")
                    elif j = 0 || i = 0 || j = 10 || i = 50 then
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
            
        let simulation_extended () =
            let rec simulation_extended (x,y,v) =
                do print_scene_extended (x,y,v)
                let x',y',v' = simulation_step_extended (x,y,v)
                if abs v' > 0.1 then
                    do simulation_extended (x',y',v')
            do simulation_extended (0.0,5.0,-2.0)