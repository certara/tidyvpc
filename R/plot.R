#' Plot a \code{tidyvpcobj}.
#' 
#' Use ggplot2 graphics to plot and customize the appearance of VPC
#' 
#' @title plot
#' @param x A tidyvpcobj object.
#' @param facet Should the resulting plot automatically facet by category? Only applicable for categorical vpc.
#' @param show.points Should the observed data points be plotted?
#' @param show.boundaries Should the bin boundary be displayed?
#' @param show.stats Should the VPC stats be displayed?
#' @param show.binning Should the binning be displayed by coloring the observed data points by bin?
#' @param xlab A character label for the x-axis.
#' @param ylab A character label for the y-axis.
#' @param color A character vector of colors for the percentiles, from low to high.
#' @param linetype A character vector of linetyps for the percentiles, from low to high.
#' @param legend.position A character string specifying the position of the legend.
#' @param facet.scales A character string specifying the `scales` argument to use for facetting.
#' @param custom.theme A Character string specifying theme from ggplot2 package
#' @param ... Further arguments can be specified but are ignored.
#' @return A `ggplot` object.
#' @seealso
#' \code{ggplot}
#' @rdname plot
#' @export
plot.tidyvpcobj <- function(x, ..., facet = FALSE, show.points=TRUE, show.boundaries=TRUE, show.stats=!is.null(x$stats), show.binning=isFALSE(show.stats), xlab=NULL, ylab=NULL, color=c("red", "blue", "red"), linetype=c("dotted", "solid", "dashed"), legend.position="top", facet.scales="free", custom.theme = "ggplot2::theme_bw") {
  
  xbin <- lo <- hi <- qname <- md <- y <- xleft <- xright <- ypc <- l.ypc <- bin <- NULL
  . <- list

  vpc <- x
  
  vpc.type <- vpc$vpc.type
  
  if(is.null(vpc.type)) vpc.type <- "continuous"
  
  qlvls <- levels(vpc$stats$qname)
  qlbls <- paste0(100*as.numeric(sub("^q", "", qlvls)), "%")
  
  if (isTRUE(vpc$predcor)) {
    ylab <- paste0(ylab, "\nPrediction Corrected")
  }
  
  has_ggplot2 <- requireNamespace("ggplot2", quietly=TRUE)
  if (!has_ggplot2) {
    stop("Package 'ggplot2' is required for plotting. Please install it to use this method.")
  }
  
  if(vpc.type == "continuous"){
    if (show.stats) {
      if (!is.null(vpc$rqss.obs.fits)) {
      g <- ggplot2::ggplot(vpc$stats, ggplot2::aes(x = x)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin=lo, ymax=hi, fill=qname, col=qname, group=qname), alpha=0.1, col=NA) +
        ggplot2::geom_line(ggplot2::aes(y=md, col=qname, group=qname)) +
        ggplot2::geom_line(ggplot2::aes(y=y, linetype=qname), size=1) +
        ggplot2::scale_colour_manual(
          name=sprintf("Simulated Percentiles\nMedian (lines) %s%% CI (areas)", 100*vpc$conf.level),
          values=color,
          breaks=qlvls,
          labels=qlbls) +
        ggplot2::scale_fill_manual(
          name=sprintf("Simulated Percentiles\nMedian (lines) %s%% CI (areas)", 100*vpc$conf.level),
          values=color,
          breaks=qlvls,
          labels=qlbls) +
        ggplot2::scale_linetype_manual(
          name="Observed Percentiles\n(black lines)",
          values=linetype,
          breaks=qlvls,
          labels=qlbls) +
        ggplot2::guides(
          fill=ggplot2::guide_legend(order=2),
          colour=ggplot2::guide_legend(order=2),
          linetype=ggplot2::guide_legend(order=1))
    } else {
      g <- ggplot2::ggplot(vpc$stats, ggplot2::aes(x = xbin)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin=lo, ymax=hi, fill=qname, col=qname, group=qname), alpha=0.1, col=NA) +
        ggplot2::geom_line(ggplot2::aes(y=md, col=qname, group=qname)) +
        ggplot2::geom_line(ggplot2::aes(y=y, linetype=qname), size=1) +
        ggplot2::scale_colour_manual(
          name=sprintf("Simulated Percentiles\nMedian (lines) %s%% CI (areas)", 100*vpc$conf.level),
          values=color,
          breaks=qlvls,
          labels=qlbls) +
        ggplot2::scale_fill_manual(
          name=sprintf("Simulated Percentiles\nMedian (lines) %s%% CI (areas)", 100*vpc$conf.level),
          values=color,
          breaks=qlvls,
          labels=qlbls) +
        ggplot2::scale_linetype_manual(
          name="Observed Percentiles\n(black lines)",
          values=linetype,
          breaks=qlvls,
          labels=qlbls) +
        ggplot2::guides(
          fill=ggplot2::guide_legend(order=2),
          colour=ggplot2::guide_legend(order=2),
          linetype=ggplot2::guide_legend(order=1))
    }
  } else {
    g <- ggplot2::ggplot(vpc$strat)
  }
  
  
  g <- g + eval(parse(text = paste0(custom.theme, "()"))) +
    ggplot2::theme(
      legend.key.width=ggplot2::unit(2, "lines"),
      legend.position=legend.position) +
    ggplot2::labs(x=xlab, y=ylab)
  
  if (show.points) {
    points.dat <- copy(vpc$obs)
    if (isTRUE(vpc$predcor)) {
      if(isTRUE(vpc$loess.ypc)) {
        points.dat[, y := l.ypc]
      } else {
        points.dat[, y := ypc]
      }
    }
    if (show.binning) {
      reorder2 <- function(y, x) {
        y <- stats::reorder(y, x)
        (1:nlevels(y))[y]
      }
      points.dat[, color := reorder2(factor(bin), x), by=vpc$strat]
      points.dat[, color := factor(color)]
      points.dat <- points.dat[!(blq|alq)]
      g <- g + ggplot2::geom_point(data=points.dat, ggplot2::aes(x=x, y=y, color=color), size=1, alpha=0.4, show.legend=FALSE) +
        ggplot2::scale_color_brewer(palette="Set1")
    } else {
      points.dat <- points.dat[!(blq|alq)]
      g <- g + ggplot2::geom_point(data=points.dat, ggplot2::aes(x=x, y=y), size=1, alpha=0.4)
    }
  }
  
  if (show.boundaries) {
    if(is.null(vpc$rqss.obs.fits)) {
      if (!is.null(vpc$strat)) {
        boundaries <- bininfo(vpc)[, .(x=sort(unique(c(xleft, xright)))), by=names(vpc$strat)]
      } else {
        boundaries <- bininfo(vpc)[, .(x=sort(unique(c(xleft, xright))))]
      }
      if (show.binning) {
        g <- g + ggplot2::geom_vline(data=boundaries, ggplot2::aes(xintercept=x), size=ggplot2::rel(0.5), col="gray80") + 
          ggplot2::theme(panel.grid=ggplot2::element_blank())
      }
      g <- g + ggplot2::geom_rug(data=boundaries, ggplot2::aes(x=x), sides="t", size=1)
    }
  }
  
  if(facet){
    if (!is.null(vpc$strat)) {
      g <- g + ggplot2::facet_grid(as.formula(paste("qname ~", paste0(names(vpc$strat), collapse = " + "), sep = " ")), scales=facet.scales, as.table = FALSE)
    } else {
      g <- g + ggplot2::facet_grid(qname ~ ., scales=facet.scales, as.table =FALSE )
    }
  } else {
    if (!is.null(vpc$strat)) {
      g <- g + ggplot2::facet_grid(vpc$strat.formula, scales=facet.scales)
    }
  }
  
  } else {
    g <- ggplot(vpc$stats, aes(x = xbin)) +
      geom_ribbon(aes(ymin = lo, ymax = hi, fill = pname, col = pname, group = pname), alpha = 0.1, col = NA) +
      geom_line(aes(y = md, col = pname, group = pname)) +
      geom_line(aes(y = y, linetype = pname), size = 1) +
      geom_point(aes(x = xbin, y = y) ) +
      ylab("Observed/simulate probabilities and associated 95% CI") +
      xlab("TIME") +
      scale_colour_manual(name = "Simulated \nMedian (lines) 95% CI (areas)", breaks = levels(vpc$stats$pname), values = .get_colors(length(levels(vpc$stats$pname))), labels = levels(vpc$stats$pname)) +
      scale_fill_manual(name = "Simulated \nMedian (lines) 95% CI (areas)", breaks = levels(vpc$stats$pname), values = .get_colors(length(levels(vpc$stats$pname))), labels = levels(vpc$stats$pname)) +
      scale_linetype_manual(name = "Observed \nMedian (lines)", breaks = levels(vpc$stats$pname), values = .get_lines(length(levels(vpc$stats$pname))), labels = levels(vpc$stats$pname)) +
      guides(fill = guide_legend(order = 2), colour = guide_legend(order = 2), linetype = guide_legend(order = 1)) +
      eval(parse(text = paste0(custom.theme, "()"))) +
      ggplot2::theme(
        legend.text = element_text(size = 8),
        legend.position=legend.position,
        legend.spacing=unit(.1, "cm"),
        legend.direction = "horizontal",
        legend.key.size = unit(.55, "cm")) 

    if(facet){
      if (!is.null(vpc$strat)) {
        g <- g + ggplot2::facet_grid(as.formula(paste(paste0(names(vpc$strat), collapse = " + "), "~", "pname", sep = " ")), scales=facet.scales, as.table = TRUE)
      } else {
        g <- g + ggplot2::facet_grid(~ pname, scales=facet.scales, as.table = FALSE)
      }
    } else {
      if (!is.null(vpc$strat)) {
        g <- g + ggplot2::facet_wrap(names(vpc$strat), scales=facet.scales)
      }
    }
    
    
  }
  
  g
}


.get_colors <- function(n){
  if(n <= 1){
    stop("Levels must be greater than 1")
  }
  if(n == 2){
    colors <- c("green", "blue")
  } else if(n == 3){
    colors <- c("green", "blue", "red")
  } else if(n == 4){
    colors <- c("green", "blue", "red", "orange")
  } else if(n == 5){
    colors <- c("green", "blue", "red", "orange", "purple")
  } else if(n == 6){
    colors <- c("green", "blue", "red", "orange", "purple", "yellow")
  }
  
  colors
}






.get_lines <- function(n){
  if(n <= 1){
    stop("Levels must be greater than 1")
  }
  if(n == 2){
    lines <- c("solid", "dashed")
  } else if(n == 3){
    lines <- c("solid", "dashed", "dotted")
  } else if(n == 4){
    lines <- c("solid", "dashed", "dotted", "dotdash")
  } else if(n == 5){
    lines <- c("solid", "dashed", "dotted", "dotdash", "longdash")
  } else if(n == 6){
    lines <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
  }
  
  lines
}




