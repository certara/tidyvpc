#' Plot a \code{tidyvpcobj}
#'
#' Use ggplot2 graphics to plot and customize the appearance of VPC.
#'
#' @param x A \code{tidyvpcobj}.
#' @param facet Set to \code{TRUE} to facet plot by quantile (continuous VPC) or
#' category (categorical VPC).
#' @param show.points Should the observed data points be plotted?
#' @param show.boundaries Should the bin boundary be displayed?
#' @param show.stats Should the VPC stats be displayed?
#' @param show.binning Should the binning be displayed by coloring the observed data points by bin?
#' @param xlab A character label for the x-axis.
#' @param ylab A character label for the y-axis.
#' @param color A character vector of colors for the percentiles, from low to high.
#' @param linetype A character vector of line type for the percentiles, from low to high.
#' @param point.alpha Numeric value specifying transparency of points.
#' @param point.size Numeric value specifying size of point.
#' @param point.shape Character one of \code{"circle", "circle-fill", "diamond", "diamond-fill",
#'  "square", "square-fill", "triangle-fill" , "triangle")}. Defaults to \code{"circle-fill"}.
#' @param point.stroke Numeric value specifying size of point stroke.
#' @param ribbon.alpha Numeric value specifying transparency of ribbon.
#' @param legend.position A character string specifying the position of the legend. Options are
#' \code{"top", "bottom", "left", "right"}.
#' @param facet.scales A character string specifying the \code{scales} argument to use for faceting. Options
#' are \code{"free", "fixed"}.
#' @param custom.theme A character string specifying theme from ggplot2 package.
#' @param censoring.type A character string specifying additional blq/alq plots to include. Only applicable if
#'  \code{\link{censoring}} was performed. 
#' @param censoring.output A character string specifying whether to return percentage of blq/alq plots as an
#' arranged \code{"grid"} or as elements in a \code{"list"}. Only applicable if \code{censoring.type != "none"}.
#' @param ... Additional arguments for \code{\link[egg]{ggarrange}} e.g., \code{ncol} and \code{nrow}.
#' Only used if \code{censoring.type != "none"} and \code{censoring.output == "grid"}.
#' @return A \code{ggplot} object.
#' @seealso
#' \code{ggplot}
#' @export
plot.tidyvpcobj <- function(x,
                            facet = FALSE,
                            show.points=TRUE,
                            show.boundaries=TRUE,
                            show.stats=!is.null(x$stats),
                            show.binning=isFALSE(show.stats),
                            xlab=NULL, ylab=NULL,
                            color=c("red", "blue", "red"),
                            linetype=c("dotted", "solid", "dashed"),
                            point.alpha = 0.4,
                            point.size = 1,
                            point.shape = "circle-fill",
                            point.stroke = 1,
                            ribbon.alpha = 0.1,
                            legend.position="top",
                            facet.scales="free",
                            custom.theme = "ggplot2::theme_bw", 
                            censoring.type = c("none", "both", "blq", "alq"),
                            censoring.output = c("grid", "list"),
                            ...) {

  xbin <- lo <- hi <- qname <- md <- y <- xleft <- xright <- ypc <- l.ypc <- bin <- blq <- alq <- pname <-  NULL
  . <- list

  point_shape_vec <-  c("circle" = 1, "circle-fill" = 19,  "diamond" = 5, "diamond-fill" = 18,
                        "square" = 0, "square-fill" = 15,  "triangle-fill" = 17, "triangle" = 2)
  if(!point.shape %in% names(point_shape_vec))
    stop(paste0("point.shape must be one of ", paste0(names(point_shape_vec), collapse = ", ")))

  point.shape <- as.numeric(point_shape_vec[names(point_shape_vec) == point.shape])

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
        ggplot2::geom_ribbon(ggplot2::aes(ymin=lo, ymax=hi, fill=qname, col=qname, group=qname), alpha=ribbon.alpha, col=NA) +
        ggplot2::geom_line(ggplot2::aes(y=md, col=qname, group=qname)) +
        ggplot2::geom_line(ggplot2::aes(y=y, linetype=qname), linewidth=1) +
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
          linetype=ggplot2::guide_legend(order=1)) +
        ylab(sprintf("Observed/Simulated probabilities and associated %s%% CI", 100*vpc$conf.level)) +
        xlab("TIME")
    } else {
      g <- ggplot2::ggplot(vpc$stats, ggplot2::aes(x = xbin)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin=lo, ymax=hi, fill=qname, col=qname, group=qname), alpha=ribbon.alpha, col=NA) +
        ggplot2::geom_line(ggplot2::aes(y=md, col=qname, group=qname)) +
        ggplot2::geom_line(ggplot2::aes(y=y, linetype=qname), linewidth=1) +
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
          linetype=ggplot2::guide_legend(order=1)) +
        ylab(sprintf("Observed/Simulated probabilities and associated %s%% CI", 100*vpc$conf.level)) +
        xlab("TIME")
    }
  } else {
    g <- ggplot2::ggplot(vpc$strat)
  }

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
      g <- g + ggplot2::geom_point(data=points.dat, ggplot2::aes(x=x, y=y, color=color),
                                   size=point.size, alpha=point.alpha, shape = point.shape, stroke = point.stroke, show.legend=FALSE) +
        ggplot2::scale_color_brewer(palette="Set1")
    } else {
      points.dat <- points.dat[!(blq|alq)]
      g <- g + ggplot2::geom_point(data=points.dat, ggplot2::aes(x=x, y=y),
                                   size=point.size, shape = point.shape, stroke = point.stroke, alpha=point.alpha)
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
        g <- g + ggplot2::geom_vline(data=boundaries, ggplot2::aes(xintercept=x), linewidth=ggplot2::rel(0.5), col="gray80") +
          ggplot2::theme(panel.grid=ggplot2::element_blank())
      }
      g <- g + ggplot2::geom_rug(data=boundaries, ggplot2::aes(x=x), sides="t", linewidth=1)
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
      if(length(as.list(vpc$strat.formula)) == 3) {
        g <- g + ggplot2::facet_grid(vpc$strat.formula, scales=facet.scales)
      } else {
        g <- g + ggplot2::facet_wrap(names(vpc$strat), scales=facet.scales)
      }
    }
  }

  } else {
    if(vpc$vpc.method$method == "binless"){
      g <- ggplot(vpc$stats, aes(x = x)) +
        geom_ribbon(aes(ymin = lo, ymax = hi, fill = pname, col = pname, group = pname), alpha = ribbon.alpha, col = NA) +
        geom_line(aes(y = md, col = pname, group = pname)) +
        geom_line(aes(y = y, linetype = pname), linewidth = 1) +
        geom_point(aes(x = x, y = y), size = point.size, alpha = point.alpha, shape = point.shape, stroke = point.stroke) +
        ylab(sprintf("Observed/Simulated probabilities and associated %s%% CI", 100*vpc$conf.level)) +
        xlab("TIME") +
        scale_colour_manual(name = sprintf("Simulated \nMedian (lines) %s%% CI (areas)",100*vpc$conf.level) , breaks = levels(vpc$stats$pname), values = .get_colors(length(levels(vpc$stats$pname))), labels = levels(vpc$stats$pname)) +
        scale_fill_manual(name = sprintf("Simulated \nMedian (lines) %s%% CI (areas)",100*vpc$conf.level), breaks = levels(vpc$stats$pname), values = .get_colors(length(levels(vpc$stats$pname))), labels = levels(vpc$stats$pname)) +
        scale_linetype_manual(name = "Observed \nMedian (lines)", breaks = levels(vpc$stats$pname), values = .get_lines(length(levels(vpc$stats$pname))), labels = levels(vpc$stats$pname)) +
        guides(fill = guide_legend(order = 2), colour = guide_legend(order = 2), linetype = guide_legend(order = 1))
    } else {
    g <- ggplot(vpc$stats, aes(x = xbin)) +
      geom_ribbon(aes(ymin = lo, ymax = hi, fill = pname, col = pname, group = pname), alpha = ribbon.alpha, col = NA) +
      geom_line(aes(y = md, col = pname, group = pname)) +
      geom_line(aes(y = y, linetype = pname), linewidth = 1) +
      geom_point(aes(x = xbin, y = y), size = point.size, alpha = point.alpha, shape = point.shape, stroke = point.stroke) +
      ylab(sprintf("Observed/Simulated probabilities and associated %s%% CI", 100*vpc$conf.level)) +
      xlab("TIME") +
      scale_colour_manual(name = sprintf("Simulated \nMedian (lines) %s%% CI (areas)",100*vpc$conf.level), breaks = levels(vpc$stats$pname), values = .get_colors(length(levels(vpc$stats$pname))), labels = levels(vpc$stats$pname)) +
      scale_fill_manual(name = sprintf("Simulated \nMedian (lines) %s%% CI (areas)",100*vpc$conf.level), breaks = levels(vpc$stats$pname), values = .get_colors(length(levels(vpc$stats$pname))), labels = levels(vpc$stats$pname)) +
      scale_linetype_manual(name = "Observed \nMedian (lines)", breaks = levels(vpc$stats$pname), values = .get_lines(length(levels(vpc$stats$pname))), labels = levels(vpc$stats$pname)) +
      guides(fill = guide_legend(order = 2), colour = guide_legend(order = 2), linetype = guide_legend(order = 1))
    }

    if(facet){
      if (!is.null(vpc$strat)) {
        g <- g + ggplot2::facet_grid(as.formula(paste(paste0(names(vpc$strat), collapse = " + "), "~", "pname", sep = " ")), scales=facet.scales, as.table = TRUE, labeller = label_both)
      } else {
        g <- g + ggplot2::facet_grid(~ pname, scales=facet.scales, as.table = FALSE, labeller = label_both)
      }
    } else {
      if (!is.null(vpc$strat)) {
        g <- g + ggplot2::facet_wrap(names(vpc$strat), scales=facet.scales, label = label_both)
      }
    }


  }
  
  # add theme
  g <- g + eval(parse(text = paste0(custom.theme, "()"))) +
    tidyvpc_theme(legend.position = legend.position)
  
  # blq/alq plot
  censoring.type <- match.arg(censoring.type)
  censoring.output <- match.arg(censoring.output)
  grid_args <- as.list(substitute(list(...)))

  if (vpc.type == "continuous" && censoring.type != "none") {
    g_blq <- g_alq <- NULL
    
    if (censoring.type %in% c("both", "blq")) {
      g_blq <- plot_censored(vpc, type = "blq", facet.scales) +
        eval(parse(text = paste0(custom.theme, "()"))) +
        tidyvpc_theme(legend.position = legend.position)
    }
    
    if (censoring.type %in% c("both", "alq")) {
      g_alq <- plot_censored(vpc, type = "alq", facet.scales) +
        eval(parse(text = paste0(custom.theme, "()"))) +
        tidyvpc_theme(legend.position = legend.position)
    }
    
    grid_list <-
      c(list(g, g_blq,g_alq),
      grid_args)
    grid_list <-
      grid_list[!sapply(grid_list, function(x)
        is.null(x) || is.symbol(x))]
    
    if (censoring.output == "grid") {
      g <- do.call(egg::ggarrange, grid_list)
    } else {
      g <- setdiff(grid_list, grid_args)
    }
  }

  g
}


plot_censored <-
  function(x,
           type = c("blq", "alq"),
           facet.scales = c("free", "fixed"),
           ...) {
    stopifnot(inherits(x, "tidyvpcobj"))
    hi <- lo <- md <- xbin <- y <- NULL
    
    type <- match.arg(type)

    df_name <- paste0("pct", type)
    df <- x[[df_name]]
    if (is.null(df)) {
      stop(
        df_name,
        " data.frame was not found in tidyvpcobj. Use `censoring()` to create censored data for plotting ",
        type,
        "data."
      )
    }
    
    g <- ggplot(df)
    
    if (!is.null(x$strat)) {
      if (length(as.list(x$strat.formula)) == 3) {
        g <- g + ggplot2::facet_grid(x$strat.formula, scales = facet.scales)
      } else {
        g <- g + ggplot2::facet_wrap(names(x$strat), scales = facet.scales)
      }
    }
    
    g <- g +
      geom_ribbon(aes(x = xbin, ymin = lo, ymax = hi),
                  fill = "red",
                  alpha = .2) +
      geom_line(aes(x = xbin, y = md, color = "simulated")) +
      geom_line(aes(x = xbin, y = y, color = "observed")) +
      ggplot2::scale_colour_manual(
        name = paste0(
          "Percentage of ",
          toupper(type),
          sprintf("\nMedian (lines) %s%% CI (areas)",
                  100 * vpc$conf.level)
        ),
        values = c(simulated = "red",
                   observed = "black")
      ) +
      labs(x = "TIME", y = paste0("% ", toupper(type)))
    
    return(g)
  }


.get_colors <- function(n){
  stopifnot(n > 1 && n < 11)

  colors <- c("#59A14FE6","#4E79A7E6", "#E15759E6", "#F28E2BE6",
              "#B07AA1E6", "#EDC948E6", "#FF9DA7E6", "#9C755FE6",
              "#BAB0ACE6")

  colors[1:n]
}



.get_lines <- function(n){
  stopifnot(n > 1 && n < 11)

  lines <- c("solid", "dashed", "dotted", "dotdash", "longdash",
               "twodash", "solid", "dashed", "dotted", "dotdash")

  lines[1:n]
}

tidyvpc_theme <-  function(legend.position) {
  ggplot2::theme(
    legend.text = ggplot2::element_text(size = 6),
    legend.position = legend.position,
    legend.spacing = unit(.1, "cm"),
    legend.direction = "horizontal",
    legend.key.size = unit(.5, "cm")
  )
}
