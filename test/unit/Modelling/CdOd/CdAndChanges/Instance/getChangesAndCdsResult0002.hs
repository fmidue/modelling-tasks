ClassDiagramInstance {instanceClassDiagram = AnyClassDiagram {anyClassNames = ["A","B","C","D"], anyRelationships = [Right (Inheritance {subClass = "D", superClass = "A"}),Right (Inheritance {subClass = "C", superClass = "B"}),Right (Composition {compositionName = "y", compositionPart = LimitedLinking {linking = "B", limits = (0,Nothing)}, compositionWhole = LimitedLinking {linking = "A", limits = (0,Just 1)}}),Right (Aggregation {aggregationName = "z", aggregationPart = LimitedLinking {linking = "C", limits = (1,Just 1)}, aggregationWhole = LimitedLinking {linking = "D", limits = (2,Just 2)}})]}, instanceRelationshipNames = ["z","y"], instanceChangesAndCds = [ChangeAndCd {relationshipChange = Change {add = Nothing, remove = Just (Right (Composition {compositionName = "y", compositionPart = LimitedLinking {linking = "B", limits = (0,Nothing)}, compositionWhole = LimitedLinking {linking = "A", limits = (0,Just 1)}}))}, changeClassDiagram = AnyClassDiagram {anyClassNames = ["A","B","C","D"], anyRelationships = [Right (Inheritance {subClass = "D", superClass = "A"}),Right (Inheritance {subClass = "C", superClass = "B"}),Right (Aggregation {aggregationName = "z", aggregationPart = LimitedLinking {linking = "C", limits = (1,Just 1)}, aggregationWhole = LimitedLinking {linking = "D", limits = (2,Just 2)}})]}},ChangeAndCd {relationshipChange = Change {add = Nothing, remove = Just (Right (Aggregation {aggregationName = "z", aggregationPart = LimitedLinking {linking = "C", limits = (1,Just 1)}, aggregationWhole = LimitedLinking {linking = "D", limits = (2,Just 2)}}))}, changeClassDiagram = AnyClassDiagram {anyClassNames = ["A","B","C","D"], anyRelationships = [Right (Inheritance {subClass = "D", superClass = "A"}),Right (Inheritance {subClass = "C", superClass = "B"}),Right (Composition {compositionName = "y", compositionPart = LimitedLinking {linking = "B", limits = (0,Nothing)}, compositionWhole = LimitedLinking {linking = "A", limits = (0,Just 1)}})]}},ChangeAndCd {relationshipChange = Change {add = Nothing, remove = Just (Right (Inheritance {subClass = "D", superClass = "A"}))}, changeClassDiagram = AnyClassDiagram {anyClassNames = ["A","B","C","D"], anyRelationships = [Right (Inheritance {subClass = "C", superClass = "B"}),Right (Composition {compositionName = "y", compositionPart = LimitedLinking {linking = "B", limits = (0,Nothing)}, compositionWhole = LimitedLinking {linking = "A", limits = (0,Just 1)}}),Right (Aggregation {aggregationName = "z", aggregationPart = LimitedLinking {linking = "C", limits = (1,Just 1)}, aggregationWhole = LimitedLinking {linking = "D", limits = (2,Just 2)}})]}}]}