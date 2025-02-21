FindInstance 
  { drawFindWith = DrawSettings 
      { withPlaceNames = True
      , withSvgHighlighting = True
      , withTransitionNames = True
      , with1Weights = False
      , withGraphvizCommand = Neato 
      }
  , toFind = Conflict 
      { conflictTrans = ( Transition 2, Transition 4 ), conflictPlaces = [ Place 5, Place 6 ] }
  , net = PetriLike 
      { allNodes = fromList
          [ ( "s1"
            , SimplePlace 
                { initial = 1
                , flowOut = fromList [ ( "t1", 1 ), ( "t3", 1 ) ]
                } 
            )
          , ( "s2"
            , SimplePlace 
                { initial = 2
                , flowOut = fromList [ ( "t1", 2 ), ( "t3", 1 ), ( "t5", 1 ) ]
                } 
            )
          , ( "s3"
            , SimplePlace { initial = 0, flowOut = fromList [ ( "t3", 1 ) ] }
            )
          , ( "s4"
            , SimplePlace { initial = 0, flowOut = fromList [ ( "t1", 1 ) ] }
            )
          , ( "s5"
            , SimplePlace 
                { initial = 1
                , flowOut = fromList [ ( "t1", 1 ), ( "t2", 1 ), ( "t4", 1 ) ]
                } 
            )
          , ( "s6"
            , SimplePlace 
                { initial = 1
                , flowOut = fromList
                    [ ( "t1", 1 ), ( "t2", 1 ), ( "t3", 1 ), ( "t4", 1 ) ] 
                } 
            )
          , ( "t1", SimpleTransition { flowOut = fromList [ ] } )
          , ( "t2", SimpleTransition { flowOut = fromList [ ( "s5", 1 ) ] } )
          , ( "t3", SimpleTransition { flowOut = fromList [ ] } )
          , ( "t4", SimpleTransition { flowOut = fromList [ ] } )
          , ( "t5", SimpleTransition { flowOut = fromList [ ] } )
          ] 
      }
  , numberOfPlaces = 6
  , numberOfTransitions = 5
  , showSolution = True 
  }
