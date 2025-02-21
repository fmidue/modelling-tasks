DeadlockInstance 
  { drawUsing = Circo
  , minLength = 8
  , noLongerThan = Just 8
  , petriNet = Net 
      { places = fromList [ Place 1, Place 2, Place 3, Place 4 ]
      , transitions = fromList [ Transition 1, Transition 2, Transition 3, Transition 4 ]
      , connections = 
        [ ( [ Place 3 ], Transition 1, [ Place 4, Place 1 ] )
        , ( [ Place 2 ], Transition 2, [ Place 3, Place 1 ] )
        , ( [ Place 4, Place 1 ], Transition 3, [ Place 2, Place 3 ] )
        , ( [ Place 2, Place 1 ], Transition 4, [ Place 4 ] )
        ]
      , capacity = Unbounded
      , start = [ ( Place 1, 0 ), ( Place 2, 1 ), ( Place 3, 0 ), ( Place 4, 0 ) ]
      }
  , showSolution = True
  , withLengthHint = Just 8
  , withMinLengthHint = Just 8 
  }
