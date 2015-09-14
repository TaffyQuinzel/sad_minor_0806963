namespace Chapter2
    module Chapter2 =

    module Math =
        [<Measure>]
        type m

        [<Measure>]
        type kg

        [<Measure>]
        type s

        [<Measure>]
        type N = kg*m/s^2

        type Vector2<[<Measure>] 'a> =
            {
                X : float<'a>
                Y : float<'a>
            }
