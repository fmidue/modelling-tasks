(ClassDiagram {classNames = ["A","B","C","D"], relationships = [Inheritance {subClass = "D", superClass = "A"},Inheritance {subClass = "C", superClass = "B"},Composition {compositionName = "y", compositionPart = LimitedLinking {linking = "B", limits = (0,Nothing)}, compositionWhole = LimitedLinking {linking = "A", limits = (0,Just 1)}},Aggregation {aggregationName = "z", aggregationPart = LimitedLinking {linking = "C", limits = (1,Just 1)}, aggregationWhole = LimitedLinking {linking = "D", limits = (2,Just 2)}}]},[(Change {add = Nothing, remove = Just ("A","B",Assoc Composition' "y" (0,Just 1) (0,Nothing) False)},ClassDiagram {classNames = ["A","B","C","D"], relationships = [Inheritance {subClass = "D", superClass = "A"},Inheritance {subClass = "C", superClass = "B"},Aggregation {aggregationName = "z", aggregationPart = LimitedLinking {linking = "C", limits = (1,Just 1)}, aggregationWhole = LimitedLinking {linking = "D", limits = (2,Just 2)}}]}),(Change {add = Nothing, remove = Just ("D","C",Assoc Aggregation' "z" (2,Just 2) (1,Just 1) False)},ClassDiagram {classNames = ["A","B","C","D"], relationships = [Inheritance {subClass = "D", superClass = "A"},Inheritance {subClass = "C", superClass = "B"},Composition {compositionName = "y", compositionPart = LimitedLinking {linking = "B", limits = (0,Nothing)}, compositionWhole = LimitedLinking {linking = "A", limits = (0,Just 1)}}]}),(Change {add = Nothing, remove = Just ("D","A",Inheritance')},ClassDiagram {classNames = ["A","B","C","D"], relationships = [Inheritance {subClass = "C", superClass = "B"},Composition {compositionName = "y", compositionPart = LimitedLinking {linking = "B", limits = (0,Nothing)}, compositionWhole = LimitedLinking {linking = "A", limits = (0,Just 1)}},Aggregation {aggregationName = "z", aggregationPart = LimitedLinking {linking = "C", limits = (1,Just 1)}, aggregationWhole = LimitedLinking {linking = "D", limits = (2,Just 2)}}]})],4)