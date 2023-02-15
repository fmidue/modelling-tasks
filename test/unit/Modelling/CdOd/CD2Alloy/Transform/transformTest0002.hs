ClassDiagram {
  classNames = ["A", "B", "C", "D"],
  relationships = [
    Association {
      associationName = "x",
      associationFrom = LimitedConnector {
        connectTo = "A",
        limits = (0, Nothing)
        },
      associationTo = LimitedConnector {
        connectTo = "B",
        limits = (1, Just 2)
        }
      },
    Aggregation {
      aggregationName = "y",
      aggregationPart = LimitedConnector {
        connectTo = "D",
        limits = (0, Nothing)
        },
      aggregationWhole = LimitedConnector {
        connectTo = "C",
        limits = (1, Just 1)
        }
      }
    ]
  }
