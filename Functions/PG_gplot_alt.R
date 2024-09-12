# This modifies the gplot() function from sna package slightly
# See the original function at https://rdrr.io/cran/sna/src/R/visualization.R

PG_gplot_alt <- function (dat, g = 1, gmode = "digraph", diag = FALSE, label = NULL, 
          coord = NULL, jitter = TRUE, thresh = 0, thresh.absval = TRUE, 
          usearrows = TRUE, mode = "fruchtermanreingold", displayisolates = TRUE, 
          interactive = FALSE, interact.bycomp = FALSE, xlab = NULL, 
          ylab = NULL, xlim = NULL, ylim = NULL, pad = 0.2, label.pad = 0.5, 
          displaylabels = !is.null(label), boxed.labels = FALSE, label.pos = 0, 
          label.bg = "white", vertex.enclose = FALSE, vertex.sides = NULL, 
          vertex.rot = 0, arrowhead.cex = 1, label.cex = 1, loop.cex = 1, 
          vertex.cex = 1, edge.col = 1, label.col = 1, vertex.col = NULL, 
          label.border = 1, vertex.border = 1, edge.lty = NULL, edge.lty.neg = 2, 
          label.lty = NULL, vertex.lty = 1, edge.lwd = 0, label.lwd = par("lwd"), 
          edge.len = 0.5, edge.curve = 0.1, edge.steps = 50, loop.steps = 20, 
          object.scale = 0.01, uselen = FALSE, usecurve = FALSE, suppress.axes = TRUE, 
          vertices.last = TRUE, new = TRUE, layout.par = NULL, ...) 
{
  bellstate <- options()$locatorBell
  expstate <- options()$expression
  on.exit(options(locatorBell = bellstate, expression = expstate))
  options(locatorBell = FALSE, expression = Inf)
  "%iin%" <- function(x, int) (x >= int[1]) & (x <= int[2])
  d <- as.edgelist.sna(dat, force.bipartite = (gmode == "twomode"))
  if (is.list(d)) 
    d <- d[[g]]
  n <- attr(d, "n")
  if (is.null(label)) {
    if (displaylabels != TRUE) 
      displaylabels <- FALSE
    if (!is.null(attr(d, "vnames"))) 
      label <- attr(d, "vnames")
    else if ((gmode == "twomode") && (!is.null(attr(d, "bipartite")))) 
      label <- c(paste("R", 1:attr(d, "bipartite"), sep = ""), 
                 paste("C", (attr(d, "bipartite") + 1):n, sep = ""))
    else {
      label <- 1:n
    }
  }
  if (gmode == "graph") {
    usearrows <- FALSE
  }
  else if ((gmode == "twomode") && (!is.null(attr(d, "bipartite")))) {
    Rn <- attr(d, "bipartite")
    if (is.null(vertex.col)) 
      vertex.col <- c(rep(2, Rn), rep(4, n - Rn))
    if (is.null(vertex.sides)) 
      vertex.sides <- c(rep(50, Rn), rep(4, n - Rn))
  }
  if (is.null(vertex.col)) 
    vertex.col <- 2
  if (is.null(vertex.sides)) 
    vertex.sides <- 50
  d <- d[!is.na(d[, 3]), , drop = FALSE]
  if (is.null(edge.lty)) {
    edge.lty <- rep(1, NROW(d))
    if (!is.null(edge.lty.neg)) 
      edge.lty[d[, 3] < 0] <- edge.lty.neg
  }
  else {
    if (length(edge.lty) != NROW(d)) {
      edge.lty <- rep(edge.lty, NROW(d))
      if (!is.null(edge.lty.neg)) 
        edge.lty[d[, 3] < 0] <- edge.lty.neg
    }
    else {
      if (!is.null(edge.lty.neg)) 
        edge.lty[d[, 3] < 0] <- edge.lty.neg
    }
  }
  d.raw <- d
  if (thresh.absval) 
    d <- d[abs(d[, 3]) > thresh, , drop = FALSE]
  else d <- d[d[, 3] > thresh, , drop = FALSE]
  attr(d, "n") <- n
  if (!is.null(coord)) {
    x <- coord[, 1]
    y <- coord[, 2]
  }
  else {
    layout.fun <- try(match.fun(paste("gplot.layout.", mode, 
                                      sep = "")), silent = TRUE)
    if (inherits(layout.fun, "try-error")) 
      stop("Error in gplot: no layout function for mode ", 
           mode)
    temp <- layout.fun(d, layout.par)
    x <- temp[, 1]
    y <- temp[, 2]
  }
  if (jitter) {
    x <- jitter(x)
    y <- jitter(y)
  }
  use <- displayisolates | (!is.isolate(d, ego = 1:n))
  if (is.null(xlab)) 
    xlab = ""
  if (is.null(ylab)) 
    ylab = ""
  if (is.null(xlim)) 
    xlim <- c(min(x[use]) - pad, max(x[use]) + pad)
  if (is.null(ylim)) 
    ylim <- c(min(y[use]) - pad, max(y[use]) + pad)
  xrng <- diff(xlim)
  yrng <- diff(ylim)
  xctr <- (xlim[2] + xlim[1])/2
  yctr <- (ylim[2] + ylim[1])/2
  if (xrng < yrng) 
    xlim <- c(xctr - yrng/2, xctr + yrng/2)
  else ylim <- c(yctr - xrng/2, yctr + xrng/2)
  baserad <- min(diff(xlim), diff(ylim)) * object.scale * 
    16/(4 + n^(1/2))
  if (new) {
    plot(0, 0, xlim = xlim, ylim = ylim, type = "n", xlab = xlab, 
         ylab = ylab, asp = 1, axes = !suppress.axes, ...)
  }
  vertex.cex <- rep(vertex.cex, length = n)
  vertex.radius <- rep(baserad * vertex.cex, length = n)
  vertex.sides <- rep(vertex.sides, length = n)
  vertex.border <- rep(vertex.border, length = n)
  vertex.col <- rep(vertex.col, length = n)
  vertex.lty <- rep(vertex.lty, length = n)
  vertex.rot <- rep(vertex.rot, length = n)
  loop.cex <- rep(loop.cex, length = n)
  label.bg <- rep(label.bg, length = n)
  label.border <- rep(label.border, length = n)
  if (!is.null(label.lty)) {
    label.lty <- rep(label.lty, length = n)
  }
  label.lwd <- rep(label.lwd, length = n)
  label.col <- rep(label.col, length = n)
  label.cex <- rep(label.cex, length = n)
  if (!vertices.last) {
    if (vertex.enclose) 
      gplot.vertex(x[use], y[use], radius = vertex.radius[use], 
                   sides = 50, col = "#FFFFFFFF", border = vertex.border[use], 
                   lty = vertex.lty[use])
    gplot.vertex(x[use], y[use], radius = vertex.radius[use], 
                 sides = vertex.sides[use], col = vertex.col[use], 
                 border = vertex.border[use], lty = vertex.lty[use], 
                 rot = vertex.rot[use])
  }
  px0 <- vector()
  py0 <- vector()
  px1 <- vector()
  py1 <- vector()
  e.lwd <- vector()
  e.curv <- vector()
  e.type <- vector()
  e.col <- vector()
  e.hoff <- vector()
  e.toff <- vector()
  e.diag <- vector()
  e.rad <- vector()
  if (NROW(d) > 0) {
    if (length(dim(edge.col)) == 2) 
      edge.col <- edge.col[d[, 1:2]]
    else edge.col <- rep(edge.col, length = NROW(d))
    if (length(dim(edge.lty)) == 2) 
      edge.lty <- edge.lty[d[, 1:2]]
    else edge.lty <- rep(edge.lty, length = NROW(d))
    if (length(dim(edge.lwd)) == 2) {
      edge.lwd <- edge.lwd[d[, 1:2]]
      e.lwd.as.mult <- FALSE
    }
    else {
      if (length(edge.lwd) == 1) 
        e.lwd.as.mult <- TRUE
      else e.lwd.as.mult <- FALSE
      edge.lwd <- rep(edge.lwd, length = NROW(d))
    }
    if (!is.null(edge.curve)) {
      if (length(dim(edge.curve)) == 2) {
        edge.curve <- edge.curve[d[, 1:2]]
        e.curv.as.mult <- FALSE
      }
      else {
        if (length(edge.curve) == 1) 
          e.curv.as.mult <- TRUE
        else e.curv.as.mult <- FALSE
        edge.curve <- rep(edge.curve, length = NROW(d))
      }
    }
    else edge.curve <- rep(0, length = NROW(d))
    dist <- ((x[d[, 1]] - x[d[, 2]])^2 + (y[d[, 1]] - y[d[, 
                                                          2]])^2)^0.5
    tl <- d * dist
    tl.max <- max(tl)
    for (i in 1:NROW(d)) if (use[d[i, 1]] && use[d[i, 2]]) {
      px0 <- c(px0, as.double(x[d[i, 1]]))
      py0 <- c(py0, as.double(y[d[i, 1]]))
      px1 <- c(px1, as.double(x[d[i, 2]]))
      py1 <- c(py1, as.double(y[d[i, 2]]))
      e.toff <- c(e.toff, vertex.radius[d[i, 1]])
      e.hoff <- c(e.hoff, vertex.radius[d[i, 2]])
      e.col <- c(e.col, edge.col[i])
      e.type <- c(e.type, edge.lty[i])
      if (edge.lwd[i] > 0) {
        if (e.lwd.as.mult) 
          e.lwd <- c(e.lwd, edge.lwd[i] * d.raw[i, 3])
        else e.lwd <- c(e.lwd, edge.lwd[i])
      }
      else e.lwd <- c(e.lwd, 1)
      e.diag <- c(e.diag, d[i, 1] == d[i, 2])
      e.rad <- c(e.rad, vertex.radius[d[i, 1]] * loop.cex[d[i, 
                                                            1]])
      if (uselen) {
        if (tl[i] > 0) {
          e.len <- dist[i] * tl.max/tl[i]
          e.curv <- c(e.curv, edge.len * sqrt((e.len/2)^2 - 
                                                (dist[i]/2)^2))
        }
        else {
          e.curv <- c(e.curv, 0)
        }
      }
      else {
        if (e.curv.as.mult) 
          e.curv <- c(e.curv, edge.curve[i] * dist[i])
        else e.curv <- c(e.curv, edge.curve[i])
      }
    }
  }
  if (diag && (length(px0) > 0) && sum(e.diag > 0)) {
    gplot.loop(as.vector(px0)[e.diag], as.vector(py0)[e.diag], 
               length = 1.5 * baserad * arrowhead.cex, angle = 25, 
               width = e.lwd[e.diag] * baserad/10, col = e.col[e.diag], 
               border = e.col[e.diag], lty = e.type[e.diag], offset = e.hoff[e.diag], 
               edge.steps = loop.steps, radius = e.rad[e.diag], 
               arrowhead = usearrows, xctr = mean(x[use]), yctr = mean(y[use]))
  }
  if (length(px0) > 0) {
    px0 <- px0[!e.diag]
    py0 <- py0[!e.diag]
    px1 <- px1[!e.diag]
    py1 <- py1[!e.diag]
    e.curv <- e.curv[!e.diag]
    e.lwd <- e.lwd[!e.diag]
    e.type <- e.type[!e.diag]
    e.col <- e.col[!e.diag]
    e.hoff <- e.hoff[!e.diag]
    e.toff <- e.toff[!e.diag]
    e.rad <- e.rad[!e.diag]
  }
  if (!usecurve & !uselen) {
    if (length(px0) > 0) 
      gplot.arrow(as.vector(px0), as.vector(py0), as.vector(px1), 
                  as.vector(py1), length = 2 * baserad * arrowhead.cex, 
                  angle = 20, col = e.col, border = e.col, lty = e.type, 
                  width = e.lwd * baserad/10, offset.head = e.hoff, 
                  offset.tail = e.toff, arrowhead = usearrows, 
                  edge.steps = edge.steps)
  }
  else {
    if (length(px0) > 0) {
      gplot.arrow(as.vector(px0), as.vector(py0), as.vector(px1), 
                  as.vector(py1), length = 2 * baserad * arrowhead.cex, 
                  angle = 20, col = e.col, border = e.col, lty = e.type, 
                  width = e.lwd * baserad/10, offset.head = e.hoff, 
                  offset.tail = e.toff, arrowhead = usearrows, 
                  curve = e.curv, edge.steps = edge.steps)
    }
  }
  if (vertices.last) {
    if (vertex.enclose) 
      gplot.vertex(x[use], y[use], radius = vertex.radius[use], 
                   sides = 50, col = "#FFFFFFFF", border = vertex.border[use], 
                   lty = vertex.lty[use])
    gplot.vertex(x[use], y[use], radius = vertex.radius[use], 
                 sides = vertex.sides[use], col = vertex.col[use], 
                 border = vertex.border[use], lty = vertex.lty[use], 
                 rot = vertex.rot[use])
  }
  if (displaylabels & (!all(label == "")) & (!all(use == FALSE))) {
    if (label.pos == 0) {
      xhat <- yhat <- rhat <- rep(0, n)
      xoff <- x[use] - mean(x[use])
      yoff <- y[use] - mean(y[use])
      roff <- sqrt(xoff^2 + yoff^2)
      for (i in (1:n)[use]) {
        ij <- unique(c(d[d[, 2] == i & d[, 1] != i, 
                         1], d[d[, 1] == i & d[, 2] != i, 2]))
        ij.n <- length(ij)
        if (ij.n > 0) {
          for (j in ij) {
            dx <- x[i] - x[j]
            dy <- y[i] - y[j]
            dr <- sqrt(dx^2 + dy^2)
            xhat[i] <- xhat[i] + dx/dr
            yhat[i] <- yhat[i] + dy/dr
          }
          xhat[i] <- xhat[i]/ij.n
          yhat[i] <- yhat[i]/ij.n
          rhat[i] <- sqrt(xhat[i]^2 + yhat[i]^2)
          if (rhat[i] != 0) {
            xhat[i] <- xhat[i]/rhat[i]
            yhat[i] <- yhat[i]/rhat[i]
          }
          else {
            xhat[i] <- xoff[i]/roff[i]
            yhat[i] <- yoff[i]/roff[i]
          }
        }
        else {
          xhat[i] <- xoff[i]/roff[i]
          yhat[i] <- yoff[i]/roff[i]
        }
        if (xhat[i] == 0) 
          xhat[i] <- 0.01
        if (yhat[i] == 0) 
          yhat[i] <- 0.01
      }
      xhat <- xhat[use]
      yhat <- yhat[use]
    }
    else if (label.pos < 5) {
      xhat <- yhat <- rep(0, n)
      xhat[1:30] <- c(-1, 0, 1, 1, 1, -1, -1, -1, -1, 0, 0, -1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, -1, -1, -1, 0, 0, 1)
      yhat[1:30] <- c(0, -1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, -1, 0, 0, 0, 1, 1, 0)
    }
    else if (label.pos == 6) {
      xoff <- x[use] - mean(x[use])
      yoff <- y[use] - mean(y[use])
      roff <- sqrt(xoff^2 + yoff^2)
      xhat <- xoff/roff
      yhat <- yoff/roff
    }
    else {
      xhat <- 0
      yhat <- 0
    }
    lw <- strwidth(label[use], cex = label.cex[use])/2
    lh <- strheight(label[use], cex = label.cex[use])/2
    if (boxed.labels) {
      rect(x[use] + xhat * vertex.radius[use] - (lh * 
                                                   label.pad + lw) * ((xhat < 0) * 2 + (xhat == 
                                                                                          0) * 1), y[use] + yhat * vertex.radius[use] - 
             (lh * label.pad + lh) * ((yhat < 0) * 2 + (yhat == 
                                                          0) * 1), x[use] + xhat * vertex.radius[use] + 
             (lh * label.pad + lw) * ((xhat > 0) * 2 + (xhat == 
                                                          0) * 1), y[use] + yhat * vertex.radius[use] + 
             (lh * label.pad + lh) * ((yhat > 0) * 2 + (yhat == 
                                                          0) * 1), col = label.bg[use], border = label.border[use], 
           lty = label.lty[use], lwd = label.lwd[use])
    }
    text(x[use] + xhat * vertex.radius[use] + (lh * label.pad + 
                                                 lw) * ((xhat > 0) - (xhat < 0)), y[use] + yhat * 
           vertex.radius[use] + (lh * label.pad + lh) * ((yhat > 
                                                            0) - (yhat < 0)), label[use], cex = label.cex[use], 
         col = label.col[use], offset = 0)
  }
  if ((interactive | interact.bycomp) && ((length(x) > 0) && 
                                          (!all(use == FALSE)))) {
    os <- c(0.2, 0.4) * par()$cxy
    textloc <- c(min(x[use]) - pad, max(y[use]) + pad)
    tm <- "Select a vertex to move, or click \"Finished\" to end."
    tmh <- strheight(tm)
    tmw <- strwidth(tm)
    text(textloc[1], textloc[2], tm, adj = c(0, 0.5))
    fm <- "Finished"
    finx <- c(textloc[1], textloc[1] + strwidth(fm))
    finy <- c(textloc[2] - 3 * tmh - strheight(fm)/2, textloc[2] - 
                3 * tmh + strheight(fm)/2)
    finbx <- finx + c(-os[1], os[1])
    finby <- finy + c(-os[2], os[2])
    rect(finbx[1], finby[1], finbx[2], finby[2], col = "white")
    text(finx[1], mean(finy), fm, adj = c(0, 0.5))
    clickpos <- unlist(locator(1))
    if ((clickpos[1] %iin% finbx) && (clickpos[2] %iin% 
                                      finby)) {
      cl <- match.call()
      cl$interactive <- FALSE
      cl$coord <- cbind(x, y)
      cl$dat <- dat
      return(eval(cl))
    }
    else {
      clickdis <- sqrt((clickpos[1] - x[use])^2 + (clickpos[2] - 
                                                     y[use])^2)
      selvert <- match(min(clickdis), clickdis)
      if (all(label == "")) 
        label <- 1:n
      rect(textloc[1], textloc[2] - tmh/2, textloc[1] + 
             tmw, textloc[2] + tmh/2, border = "white", col = "white")
      if (interact.bycomp) 
        tm <- "Where should I move this component?"
      else tm <- "Where should I move this vertex?"
      tmh <- strheight(tm)
      tmw <- strwidth(tm)
      text(textloc[1], textloc[2], tm, adj = c(0, 0.5))
      fm <- paste("Vertex", label[use][selvert], "selected")
      finx <- c(textloc[1], textloc[1] + strwidth(fm))
      finy <- c(textloc[2] - 3 * tmh - strheight(fm)/2, 
                textloc[2] - 3 * tmh + strheight(fm)/2)
      finbx <- finx + c(-os[1], os[1])
      finby <- finy + c(-os[2], os[2])
      rect(finbx[1], finby[1], finbx[2], finby[2], col = "white")
      text(finx[1], mean(finy), fm, adj = c(0, 0.5))
      clickpos <- unlist(locator(1))
      if (interact.bycomp) {
        dx <- clickpos[1] - x[use][selvert]
        dy <- clickpos[2] - y[use][selvert]
        comp.mem <- component.dist(d, connected = "weak")$membership
        same.comp <- comp.mem[use] == comp.mem[use][selvert]
        x[use][same.comp] <- x[use][same.comp] + dx
        y[use][same.comp] <- y[use][same.comp] + dy
      }
      else {
        x[use][selvert] <- clickpos[1]
        y[use][selvert] <- clickpos[2]
      }
      cl <- match.call()
      cl$coord <- cbind(x, y)
      cl$dat <- dat
      return(eval(cl))
    }
  }
  invisible(cbind(x, y))
}
