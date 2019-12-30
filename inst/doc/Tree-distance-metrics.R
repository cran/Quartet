## ----PhangornDistLinker, echo=FALSE--------------------------------------
PhangornDist <- function (linkText) {
  paste0('[`', linkText, '`](https://www.rdocumentation.org/packages/phangorn/topics/treedist])')
}

## ----Three-four-taxon-trees, echo=FALSE, cache=TRUE, fig.width=6, fig.asp=1/3, out.width='66%', fig.align='center'----
par(mfrow=c(1, 3), mar=c(0.5, 1, 0.5, 1), cex=1)
plot(ape::read.tree(text='((A, B), (C, D));'),
     tip.color=Ternary::cbPalette8[c(1, 4, 7, 5)], font=2)
plot(ape::read.tree(text='((A, C), (B, D));'),
     tip.color=Ternary::cbPalette8[c(1, 7, 4, 5)], font=2)
plot(ape::read.tree(text='((A, D), (C, B));'),
     tip.color=Ternary::cbPalette8[c(1, 5, 7, 4)], font=2)

## ----Plot-a-quartet, echo=FALSE, cache=TRUE, fig.asp=1.3/3, fig.width=6, out.width='80%', fig.align='center'----
par(mfrow=c(1, 3))
suppressWarnings(RNGversion("3.5.0")) # Stopgap until R 3.6.0 is widely available 
set.seed(7)
trees7 <- lapply(logical(3), function (X) {
    tr <- ape::rtree(7, br=NULL)
    tr$edge.length <- rep(1, 12)
    tr$tip.label <- LETTERS[1:7]
    tr
  })
Quartet::PlotQuartet(trees7, LETTERS[1:4], cex=1.4, font=2)

