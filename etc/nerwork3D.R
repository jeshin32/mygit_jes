install.packages("networkD3")
library(networkD3)

data(MisLinks)
data(MisNodes)

forceNetwork(Links= MisLinks, Nodes = MisNodes,
             Source = 'source', Target = "target",
             Value = "value", NodeID = "name",
             Group = "group", opacity = 0.8)
head(MisLinks)
head(MisNodes)

forceNetwork(Links= MisLinks, Nodes = MisNodes,
             Source = 'source', Target = "target",
             Value = "value", NodeID = "name",
             Nodesize = "size",
             Group = "group", opacity = 0.8)  # opacity = 투명도

?forceNetwork