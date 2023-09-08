ClassDiagramInstance {instanceClassDiagram = ClassDiagram {classNames = ["A","B","C","D"], relationships = [Inheritance {subClass = "D", superClass = "C"},Inheritance {subClass = "C", superClass = "B"},Association {associationName = "z", associationFrom = LimitedLinking {linking = "D", limits = (1,Just 2)}, associationTo = LimitedLinking {linking = "B", limits = (0,Just 1)}},Association {associationName = "y", associationFrom = LimitedLinking {linking = "A", limits = (0,Nothing)}, associationTo = LimitedLinking {linking = "C", limits = (1,Just 1)}}]}, instanceRelationshipNames = ["z","y"], instanceChangesAndCds = [ChangeAndCd {relationshipChange = Change {add = Nothing, remove = Just (Inheritance {subClass = "C", superClass = "B"})}, changeClassDiagram = ClassDiagram {classNames = ["A","B","C","D"], relationships = [Inheritance {subClass = "D", superClass = "C"},Association {associationName = "z", associationFrom = LimitedLinking {linking = "D", limits = (1,Just 2)}, associationTo = LimitedLinking {linking = "B", limits = (0,Just 1)}},Association {associationName = "y", associationFrom = LimitedLinking {linking = "A", limits = (0,Nothing)}, associationTo = LimitedLinking {linking = "C", limits = (1,Just 1)}}]}},ChangeAndCd {relationshipChange = Change {add = Nothing, remove = Just (Association {associationName = "z", associationFrom = LimitedLinking {linking = "D", limits = (1,Just 2)}, associationTo = LimitedLinking {linking = "B", limits = (0,Just 1)}})}, changeClassDiagram = ClassDiagram {classNames = ["A","B","C","D"], relationships = [Inheritance {subClass = "D", superClass = "C"},Inheritance {subClass = "C", superClass = "B"},Association {associationName = "y", associationFrom = LimitedLinking {linking = "A", limits = (0,Nothing)}, associationTo = LimitedLinking {linking = "C", limits = (1,Just 1)}}]}},ChangeAndCd {relationshipChange = Change {add = Just (Inheritance {subClass = "B", superClass = "C"}), remove = Just (Inheritance {subClass = "C", superClass = "B"})}, changeClassDiagram = ClassDiagram {classNames = ["A","B","C","D"], relationships = [Inheritance {subClass = "B", superClass = "C"},Inheritance {subClass = "D", superClass = "C"},Association {associationName = "z", associationFrom = LimitedLinking {linking = "D", limits = (1,Just 2)}, associationTo = LimitedLinking {linking = "B", limits = (0,Just 1)}},Association {associationName = "y", associationFrom = LimitedLinking {linking = "A", limits = (0,Nothing)}, associationTo = LimitedLinking {linking = "C", limits = (1,Just 1)}}]}}]}