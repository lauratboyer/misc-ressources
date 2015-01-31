## Modified legend for colour bars
## uses a version of legend() modified to work w Hershey Fonts
## -------------------------------------------------------
## Author: Laura Tremblay-Boyer (l.boyer@fisheries.ubc.ca)
## Written on: January 30, 2014
## Time-stamp: <2015-01-31 14:20:19 Laura>
legend.ltb <- function(legd,colv,pos="topright",
                       max.pt=1,revleg=FALSE,
                       leg.title=NA,cex=1,max.val=NA,
                       horiz=FALSE,left.start=FALSE,roundleg=FALSE,
                       title.cex=1.25,...) {

    ll=length(legd) # longueur
    # format max.val
    if(!missing(max.val)) {
        num.sig <- max(sapply(gregexpr("[1-9]",legd),length))
        max.val <- paste("max~",signif(max.val,num.sig),sep="") }

    # make legend
    if(!horiz){
    lt <- legend(pos,legend=rep(NA,ll),bty="n",
                 col=c(colv,"white"),pch=15,pt.cex=2.5*max.pt,
                 xpd=NA,...)

    # décaler les étiquettes
    pa <- ifelse(revleg,2,4) # reverse text position
    step <- lt$text$y[1]-lt$text$y[2]

    xpos <- ifelse(revleg,lt$rect$lef,rep(lt$text$x[1],ll))

    text(xpos,lt$text$y+0.5*step,legd,
         pos=ifelse(revleg,2,4),xpd=NA,cex=cex)

    if(!missing(max.val)) {
    text(xpos,lt$text$y[ll]-0.5*step,
         max.val,pos=ifelse(revleg,2,4),xpd=NA,cex=cex)}

    if(!missing(leg.title)) {
        text(ifelse(revleg,lt$rect$w,lt$rect$left),
             lt$rect$top + step, leg.title,
             pos=pa, xpd=NA, cex=title.cex) }

    }else{

    lt <- legend.ltb.2(pos,legend=rep(NA,ll),bty="n",
                 col=c(colv,"white"),pch=15,pt.cex=2.5*max.pt,
                 xpd=NA,horiz=TRUE,...)
    ypos <- lt$rect$top - lt$rect$h
    xdif <- diff(lt$text$x[1:2])
    xv <- lt$text$x[-length(legd)]
    if(left.start) xv <- xv - xdif
    if(roundleg) legd <- round(legd)
    text(xv,ypos,
         legd[-length(legd)],xpd=NA,pos=1)

    if(!missing(leg.title)) {
        x0 <- lt$text$x[1]-1.4*xdif
        text(x0,1.2*lt$rect$top, leg.title,
             pos=4, xpd=NA, cex=1.25) }

    }


}

## legend-ltb-2.r
## Defines a (very slightly) modified version of legend()
## to allow for Hershey fonts to be used
legend.ltb.2 <- function (x, y = NULL, legend, fill = NULL, col = par("col"),
    border = "black", lty, lwd, pch, angle = 45, density = NULL,
    bty = "o", bg = par("bg"), box.lwd = par("lwd"), box.lty = par("lty"),
    box.col = par("fg"), pt.bg = NA, cex = 1, pt.cex = cex, pt.lwd = lwd,
    xjust = 0, yjust = 1, x.intersp = 1, y.intersp = 1, adj = c(0,
        0.5), text.width = NULL, text.col = par("col"), text.font = NULL,
    merge = do.lines && has.pch, trace = FALSE, plot = TRUE,
    ncol = 1, horiz = FALSE, title = NULL, inset = 0, xpd, title.col = text.col,
    title.adj = 0.5, seg.len = 2)
{

    # ltb-modif define textfont
    if(missing(text.font) && grepl("Hershey",par()$family)) {
        text.font <- tolower(gsub("Hershey","",par()$family))
        if(text.font[1]=="sans") text.font[1] <- "sans serif"
        text.font[2] <- c("plain","bold","italic")[par()$font]
        if(par()$font>3) stop("font type not defined!")
    }

    if (missing(legend) && !missing(y) && (is.character(y) ||
        is.expression(y))) {
        legend <- y
        y <- NULL
    }
    mfill <- !missing(fill) || !missing(density)
    if (!missing(xpd)) {
        op <- par("xpd")
        on.exit(par(xpd = op))
        par(xpd = xpd)
    }
    title <- as.graphicsAnnot(title)
    if (length(title) > 1)
        stop("invalid title")
    legend <- as.graphicsAnnot(legend)
    n.leg <- if (is.call(legend))
        1
    else length(legend)
    if (n.leg == 0)
        stop("'legend' is of length 0")
    auto <- if (is.character(x))
        match.arg(x, c("bottomright", "bottom", "bottomleft",
            "left", "topleft", "top", "topright", "right", "center"))
    else NA
    if (is.na(auto)) {
        xy <- xy.coords(x, y)
        x <- xy$x
        y <- xy$y
        nx <- length(x)
        if (nx < 1 || nx > 2)
            stop("invalid coordinate lengths")
    }
    else nx <- 0
    xlog <- par("xlog")
    ylog <- par("ylog")
    rect2 <- function(left, top, dx, dy, density = NULL, angle,
        ...) {
        r <- left + dx
        if (xlog) {
            left <- 10^left
            r <- 10^r
        }
        b <- top - dy
        if (ylog) {
            top <- 10^top
            b <- 10^b
        }
        rect(left, top, r, b, angle = angle, density = density,
            ...)
    }
    segments2 <- function(x1, y1, dx, dy, ...) {
        x2 <- x1 + dx
        if (xlog) {
            x1 <- 10^x1
            x2 <- 10^x2
        }
        y2 <- y1 + dy
        if (ylog) {
            y1 <- 10^y1
            y2 <- 10^y2
        }
        segments(x1, y1, x2, y2, ...)
    }
    points2 <- function(x, y, ...) {
        if (xlog)
            x <- 10^x
        if (ylog)
            y <- 10^y
        points(x, y, ...)
    }
    text2 <- function(x, y, ...) {
        if (xlog)
            x <- 10^x
        if (ylog)
            y <- 10^y
        text(x, y, ...)
    }
    if (trace)
        catn <- function(...) do.call("cat", c(lapply(list(...),
            formatC), list("\n")))
    cin <- par("cin")
    Cex <- cex * par("cex")
    if (is.null(text.width))
        text.width <- max(abs(strwidth(legend, units = "user",
            cex = cex, vfont = text.font)))
    else if (!is.numeric(text.width) || text.width < 0)
        stop("'text.width' must be numeric, >= 0")
    xc <- Cex * xinch(cin[1L], warn.log = FALSE)
    yc <- Cex * yinch(cin[2L], warn.log = FALSE)
    if (xc < 0)
        text.width <- -text.width
    xchar <- xc
    xextra <- 0
    yextra <- yc * (y.intersp - 1)
    ymax <- yc * max(1, strheight(legend, units = "user", cex = cex, vfont=text.font)/yc)
    ychar <- yextra + ymax
    if (trace)
        catn("  xchar=", xchar, "; (yextra,ychar)=", c(yextra,
            ychar))
    if (mfill) {
        xbox <- xc * 0.8
        ybox <- yc * 0.5
        dx.fill <- xbox
    }
    do.lines <- (!missing(lty) && (is.character(lty) || any(lty >
        0))) || !missing(lwd)
    n.legpercol <- if (horiz) {
        if (ncol != 1)
            warning("horizontal specification overrides: Number of columns := ",
                n.leg)
        ncol <- n.leg
        1
    }
    else ceiling(n.leg/ncol)
    has.pch <- !missing(pch) && length(pch) > 0
    if (do.lines) {
        x.off <- if (merge)
            -0.7
        else 0
    }
    else if (merge)
        warning("'merge = TRUE' has no effect when no line segments are drawn")
    if (has.pch) {
        if (is.character(pch) && !is.na(pch[1L]) && nchar(pch[1L],
            type = "c") > 1) {
            if (length(pch) > 1)
                warning("not using pch[2..] since pch[1L] has multiple chars")
            np <- nchar(pch[1L], type = "c")
            pch <- substr(rep.int(pch[1L], np), 1L:np, 1L:np)
        }
    }
    if (is.na(auto)) {
        if (xlog)
            x <- log10(x)
        if (ylog)
            y <- log10(y)
    }
    if (nx == 2) {
        x <- sort(x)
        y <- sort(y)
        left <- x[1L]
        top <- y[2L]
        w <- diff(x)
        h <- diff(y)
        w0 <- w/ncol
        x <- mean(x)
        y <- mean(y)
        if (missing(xjust))
            xjust <- 0.5
        if (missing(yjust))
            yjust <- 0.5
    }
    else {
        h <- (n.legpercol + (!is.null(title))) * ychar + yc
        w0 <- text.width + (x.intersp + 1) * xchar
        if (mfill)
            w0 <- w0 + dx.fill
        if (do.lines)
            w0 <- w0 + (seg.len + x.off) * xchar
        w <- ncol * w0 + 0.5 * xchar
        if (!is.null(title) && (abs(tw <- strwidth(title, units = "user",
            cex = cex, vfont=text.font) + 0.5 * xchar)) > abs(w)) {
            xextra <- (tw - w)/2
            w <- tw
        }
        if (is.na(auto)) {
            left <- x - xjust * w
            top <- y + (1 - yjust) * h
        }
        else {
            usr <- par("usr")
            inset <- rep(inset, length.out = 2)
            insetx <- inset[1L] * (usr[2L] - usr[1L])
            left <- switch(auto, bottomright = , topright = ,
                right = usr[2L] - w - insetx, bottomleft = ,
                left = , topleft = usr[1L] + insetx, bottom = ,
                top = , center = (usr[1L] + usr[2L] - w)/2)
            insety <- inset[2L] * (usr[4L] - usr[3L])
            top <- switch(auto, bottomright = , bottom = , bottomleft = usr[3L] +
                h + insety, topleft = , top = , topright = usr[4L] -
                insety, left = , right = , center = (usr[3L] +
                usr[4L] + h)/2)
        }
    }
    if (plot && bty != "n") {
        if (trace)
            catn("  rect2(", left, ",", top, ", w=", w, ", h=",
                h, ", ...)", sep = "")
        rect2(left, top, dx = w, dy = h, col = bg, density = NULL,
            lwd = box.lwd, lty = box.lty, border = box.col)
    }
    xt <- left + xchar + xextra + (w0 * rep.int(0:(ncol - 1),
        rep.int(n.legpercol, ncol)))[1L:n.leg]
    yt <- top - 0.5 * yextra - ymax - (rep.int(1L:n.legpercol,
        ncol)[1L:n.leg] - 1 + (!is.null(title))) * ychar
    if (mfill) {
        if (plot) {
            if (!is.null(fill))
                fill <- rep(fill, length.out = n.leg)
            rect2(left = xt, top = yt + ybox/2, dx = xbox, dy = ybox,
                col = fill, density = density, angle = angle,
                border = border)
        }
        xt <- xt + dx.fill
    }
    if (plot && (has.pch || do.lines))
        col <- rep(col, length.out = n.leg)
    if (missing(lwd))
        lwd <- par("lwd")
    if (do.lines) {
        if (missing(lty))
            lty <- 1
        lty <- rep(lty, length.out = n.leg)
        lwd <- rep(lwd, length.out = n.leg)
        ok.l <- !is.na(lty) & (is.character(lty) | lty > 0) &
            !is.na(lwd)
        if (trace)
            catn("  segments2(", xt[ok.l] + x.off * xchar, ",",
                yt[ok.l], ", dx=", seg.len * xchar, ", dy=0, ...)")
        if (plot)
            segments2(xt[ok.l] + x.off * xchar, yt[ok.l], dx = seg.len *
                xchar, dy = 0, lty = lty[ok.l], lwd = lwd[ok.l],
                col = col[ok.l])
        xt <- xt + (seg.len + x.off) * xchar
    }
    if (has.pch) {
        pch <- rep(pch, length.out = n.leg)
        pt.bg <- rep(pt.bg, length.out = n.leg)
        pt.cex <- rep(pt.cex, length.out = n.leg)
        pt.lwd <- rep(pt.lwd, length.out = n.leg)
        ok <- !is.na(pch) & (is.character(pch) | pch >= 0)
        x1 <- (if (merge && do.lines)
            xt - (seg.len/2) * xchar
        else xt)[ok]
        y1 <- yt[ok]
        if (trace)
            catn("  points2(", x1, ",", y1, ", pch=", pch[ok],
                ", ...)")
        if (plot)
            points2(x1, y1, pch = pch[ok], col = col[ok], cex = pt.cex[ok],
                bg = pt.bg[ok], lwd = pt.lwd[ok])
    }
    xt <- xt + x.intersp * xchar
    if (plot) {
        if (!is.null(title))
            text2(left + w * title.adj, top - ymax, labels = title,
                adj = c(title.adj, 0), cex = cex, col = title.col)
        text2(xt, yt, labels = legend, adj = adj, cex = cex,
            col = text.col, vfont = text.font)
    }
    invisible(list(rect = list(w = w, h = h, left = left, top = top),
        text = list(x = xt, y = yt)))
}
