  ReachInstance
    { drawUsing = Circo
    , goal = [ ( Place 1, 1 ), ( Place 2, 1 ), ( Place 3, 1 ), ( Place 4, 3 ) ]
    , minLength = 8
    , noLongerThan = Just 8
    , petriNet = Net
        { places = fromList [ Place 1, Place 2, Place 3, Place 4 ]
        , transitions = fromList [ Transition 1, Transition 2, Transition 3, Transition 4 ]
        , connections =
          [ ( [ Place 1, Place 2 ], Transition 1, [ Place 1, Place 4 ] )
          , ( [ Place 1, Place 4 ], Transition 2, [ Place 4, Place 3 ] )
          , ( [ Place 4, Place 3 ], Transition 3, [ Place 2, Place 1, Place 3 ] )
          , ( [ Place 1, Place 4 ], Transition 4, [ Place 2, Place 4 ] )
          ]
        , capacity = Unbounded
        , start = [ ( Place 1, 1 ), ( Place 2, 1 ), ( Place 3, 1 ), ( Place 4, 1 ) ]
        }
    , showGoalNet = True
    , showSolution = True
    , withLengthHint = Just 8
    , withMinLengthHint = Just 8
    }
