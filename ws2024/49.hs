SigInstance
      { symbols = 
        [ d : T x U -> S
        , e : U x T x S -> R
        , f : U
        , g : T x T -> U
        , h : T
        , i : S 
        ]
      , terms = 
        [ e(f,h,d(h,g(h,h(i))))
        , e(g(h,g(h,h)),h,i)
        , e(g(h,h),h,i)
        , e(g(h,h),h,d(h,g(h)))
        , e(g(h,h),h,d(h,f))
        , e(f,h,d(h,g(h,h)))
        , e(g(h,h),h,d(h,g(h,h)))
        , e(f,d(h,f),h)
        , e(f,h,d(h,f)) 
        ]
      , correct = [ 3, 5, 6, 7, 9 ]
      , moreFeedback = True
      , showSolution = True
      , addText = Nothing
      }
