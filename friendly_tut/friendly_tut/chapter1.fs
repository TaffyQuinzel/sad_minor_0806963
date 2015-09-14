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
            
    module BallSimulation_extended =
        open System
        let dt = 0.1
        let g = -9.81

        let simulation_step(x,y,dx) =
            let x',y',dx' = (x+dx*dt, y+dx*dt, dx+g*dt)
            if y' < 0.0 then
                (x', 0.0, -dx'*0.7)
            elif x' < 0.0 then
                (0.0, y', -dx'*0.7)
            else
                (x',y',dx')
        
        let print_scene(x1,y1,x2,y2,x3,y3,x4,y4) =
            do Console.Clear()
            let x1,y1,x2,y2,x3,y3,x4,y4 = int x1, int y1, int x2, int y2, int x3, int y3, int x4, int y4
            for j = 30 downto 0 do
                for i = 0 to 30 do
                    if ((y1) = j && (x1) = i) || ((y2) = j && (x2) = i) || ((y3) = j && (x3) = i) || ((y4) = j && (x4) = i) then
                        Console.Write("b")
                    elif j = 0 || i = 0 || j = 30 || i = 30 then
                        Console.Write("*")
                    else
                        Console.Write(" ")
                Console.Write("\n")
            ignore(Console.ReadKey())

        let simulation() =
            let rec simulation(x1,y1,v1, x2,y2,v2, x3,y3,v3, x4,y4,v4) =
                do print_scene(x1,y1,x2,30.0-y2,30.0-x3,y3,30.0-x4,30.0-y4)
                let x1',y1',v1' = simulation_step(x1,y1,v1)
                let x2',y2',v2' = simulation_step(x2,y2,v2)
                let x3',y3',v3' = simulation_step(x3,y3,v3)
                let x4',y4',v4' = simulation_step(x4,y4,v4)
                if abs v1' > 0.1 || abs v2' > 0.1 || abs v3' > 0.1 || abs v4' > 0.1 then
                    do simulation(x1',y1',v1', x2',y2',v2', x3',y3',v3', x4',y4',v4') 
            do simulation(15.0,15.0,-0.5, 15.0,15.0,-1.0, 15.0,15.0,-1.5, 15.0,15.0,-2.0)