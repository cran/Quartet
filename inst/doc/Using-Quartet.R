## ----set-up-------------------------------------------------------------------
tree1 <- ape::read.tree(text = '(A, ((B, (C, (D, E))), ((F, G), (H, I))));')
tree2 <- ape::read.tree(text = '(A, ((B, (C, (D, (H, I)))), ((F, G), E)));')

## ----load-package, message=FALSE----------------------------------------------
library('Quartet')

## ----quartet-status-----------------------------------------------------------
statuses <- QuartetStatus(tree1, tree2)

## ----measure-distance---------------------------------------------------------
QuartetDivergence(statuses, similarity = FALSE)

## ----all-metrics--------------------------------------------------------------
SimilarityMetrics(statuses, similarity = TRUE)

## ----visualize-quartets-------------------------------------------------------
VisualizeQuartets(tree1, tree2)

## ----partitions---------------------------------------------------------------
SimilarityMetrics(SplitStatus(tree1, tree2))

## ----multi-trees--------------------------------------------------------------
library('TreeTools', quietly = TRUE, warn.conflicts = FALSE)
oneTree <- CollapseNode(as.phylo(0, 11), 14)
twoTrees <- structure(list(bal = BalancedTree(11), pec = PectinateTree(11)),
                      class = 'multiPhylo')

status <- SharedQuartetStatus(twoTrees, cf = oneTree)
QuartetDivergence(status)

## ----one-to-many--------------------------------------------------------------
forest <- as.phylo(0:5, 11)
names(forest) <- letters[1:6]
status <- SharedQuartetStatus(forest)
QuartetDivergence(status)

## ----many-to-many-------------------------------------------------------------
status <- ManyToManyQuartetAgreement(forest)
QuartetDivergence(status, similarity = FALSE)

## ----pairwise-----------------------------------------------------------------
status <- TwoListQuartetAgreement(forest[1:4], forest[5:6])
QuartetDivergence(status, similarity = FALSE)

## ----different-tips, fig.height = 1.5, fig.width = 4, fig.align = "center"----
treeAG <- PectinateTree(letters[1:7])
treeBI <- PectinateTree(letters[2:9])
treeEJ <- PectinateTree(letters[5:10])
par(mfrow = c(1, 3), mar = rep(0.3, 4), cex = 1)
plot(treeAG); plot(treeBI); plot(treeEJ)

QuartetState(letters[1:4], treeAG) # 3: C is closest to D
QuartetState(letters[1:4], treeBI) # 0: unresolved in this tree

# Calculate status for all leaves observed in trees: here, A..I
QuartetStatus(treeAG, treeBI, nTip = TRUE)

# Calculate status for specified number of leaves
# Here, we have ten taxa A..J, but J does not occur in either of these trees
QuartetStatus(treeAG, treeBI, nTip = 10)

# Compare a list of trees with different numbers of leaves to a reference
QuartetStatus(c(treeAG, treeBI, treeEJ), cf = treeAG, nTip = TRUE)

# Compare all pairs of trees in a list.
# "u" shows how many possible quartets are unresolved in both trees
ManyToManyQuartetAgreement(c(treeAG, treeBI, treeEJ), nTip = TRUE)[, , "u"]

## ----in-one-only--------------------------------------------------------------
interestingTree <- as.phylo(42, 7)
referenceTrees <- list(BalancedTree(7), PectinateTree(7))
status <- CompareQuartetsMulti(interestingTree, referenceTrees)

