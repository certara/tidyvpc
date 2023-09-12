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
#' @param custom.theme A custom ggplot2 theme supplied either as a character string, function, or object of class \code{"theme"}.
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
                            custom.theme = NULL,
                            censoring.type = c("none", "both", "blq", "alq"),
                            censoring.output = c("grid", "list"),
                            ...) {

  xbin <- lo <- hi <- qname <- md <- y <- xleft <- xright <- ypc <- l.ypc <- bin <- blq <- alq <- pname <-  NULL
  . <- list

  vpc <- x

  vpc_type <- vpc$vpc.type

  if(is.null(vpc_type)) vpc_type <- "continuous"

  if(vpc_type == "continuous"){
    g <-
      plot_continuous(
        vpc,
        show.stats,
        show.points,
        show.boundaries,
        show.binning,
        ribbon.alpha,
        color,
        linetype,
        facet,
        facet.scales,
        point.size, point.shape, point.stroke, point.alpha
      )
  } else {
    g <-
      plot_categorical(
        vpc,
        ribbon.alpha,
        facet,
        facet.scales,
        point.size,
        point.shape,
        point.stroke,
        point.alpha
      )


  }

    # add theme
  if (is.null(custom.theme)) {
    g <- g + ggplot2::theme_bw() + tidyvpc_theme(legend.position = legend.position)
  } else if (is.character(custom.theme)) {
    g <- g + eval(parse(text = paste0(custom.theme, "()")))
  } else if (is.function(custom.theme)) {
    g <- g + custom.theme()
  } else if (inherits(custom.theme, "theme")) {
    g <- g + custom.theme
  }

  # add labels
  if (is.null(xlab)) {
    xlab <- "TIME"
  }

  if (is.null(ylab)) {
    ylab <-
      sprintf("Observed/Simulated probabilities and associated %s%% CI",
              100 * vpc$conf.level)
    if (isTRUE(vpc$predcor)) {
      ylab <- ifelse(length(ylab) == 0,
                     "Prediction Corrected",
                     paste0(ylab, "\nPrediction Corrected"))
    }
  }

  g <- g + ggplot2::xlab(xlab)
  g <- g + ggplot2::ylab(ylab)


  # blq/alq plot
  censoring.type <- match.arg(censoring.type)
  censoring.output <- match.arg(censoring.output)
  grid_args <- as.list(substitute(list(...)))

  if (vpc_type == "continuous" && censoring.type != "none") {
    g_blq <- g_alq <- NULL

    if (censoring.type %in% c("both", "blq")) {
      g_blq <-
        plot_censored(
          vpc,
          type = "blq",
          facet.scales,
          custom.theme,
          legend.position,
          show.points,
          show.boundaries,
          show.binning
        )
    }

    if (censoring.type %in% c("both", "alq")) {
      g_alq <-
        plot_censored(
          vpc,
          type = "alq",
          facet.scales,
          custom.theme,
          legend.position,
          show.points,
          show.boundaries,
          show.binning
        )
    }

    grid_list <-
      c(list(g, g_blq,g_alq),
      grid_args)
    grid_list <-
      grid_list[!sapply(grid_list, function(x)
        is.null(x) || is.symbol(x))]

    if (censoring.output == "grid") {
      #Return egg
      g <- do.call(egg::ggarrange, grid_list)
    } else {
      #Return list
      g <- setdiff(grid_list, grid_args)
    }
  }

  g
}

#' Expand single-value vpc groups to a finite width so that they show up with `geom_ribbon()`
#'
#' @param vpc The vpc object
#' @return A data frame of the vpc$strat possibly with additional rows for
#'   single-value groups
#' @noRd
expand_vpc_stats_single_value <- function(vpc, xvar, width = 0.0001) {
  d_vpc_stats <- vpc$strat
  if (!is.null(vpc$strat)) {
    d_vpc_stats <-
      dplyr::grouped_df(vpc$stats, vars = names(vpc$strat)) %>%
      dplyr::mutate(
        n_xvar = length(unique(!!sym(xvar)))
      )
    mask_n1 <- d_vpc_stats$n_xvar == 1
    if (any(mask_n1)) {
      d_vpc_stats_single <- d_vpc_stats[mask_n1, ]
      d_vpc_stats_single_low <- d_vpc_stats_single_high <- d_vpc_stats_single
      d_vpc_stats_single_low[[xvar]] <- d_vpc_stats_single_low[[xvar]] - width/2
      d_vpc_stats_single_high[[xvar]] <- d_vpc_stats_single_high[[xvar]] + width/2

      d_vpc_stats <-
        dplyr::bind_rows(
          d_vpc_stats[!mask_n1, ],
          d_vpc_stats_single_low,
          d_vpc_stats_single_high
        )
    }
  }
  d_vpc_stats
}

plot_continuous <-
  function(vpc,
           show.stats,
           show.points,
           show.boundaries,
           show.binning,
           ribbon.alpha,
           color,
           linetype,
           facet,
           facet.scales,
           point.size,
           point.shape,
           point.stroke,
           point.alpha) {
    alq <-bin <- blq <- hi <-l.ypc <-lo <- md <- pname <- qname <- x <- xleft <- xright <- y <- ypc <- NULL
    . <- list
    method <- vpc$vpc.method$method
    qlvls <- levels(vpc$stats$qname)
    qlbls <- paste0(100 * as.numeric(sub("^q", "", qlvls)), "%")
    point_shape_vec <- .get_point_shapes()
    if (!point.shape %in% names(point_shape_vec))
      stop(paste0("point.shape must be one of ", paste0(names(point_shape_vec), collapse = ", ")))
    point.shape <-
      as.numeric(point_shape_vec[names(point_shape_vec) == point.shape])

    if (method == "binning") {
      xvar <- "xbin"
    } else {
      xvar <- "x"
    }

    if (show.stats) {
      d_vpc_stats <- expand_vpc_stats_single_value(vpc = vpc, xvar = xvar)
      g <-
        ggplot2::ggplot(d_vpc_stats, ggplot2::aes(x = !!sym(xvar))) +
        ggplot2::geom_ribbon(
          ggplot2::aes(
            ymin = lo,
            ymax = hi,
            fill = qname,
            col = qname,
            group = qname
          ),
          alpha = ribbon.alpha,
          col = NA
        ) +
        ggplot2::geom_line(ggplot2::aes(y = md, col = qname, group = qname)) +
        ggplot2::geom_line(ggplot2::aes(y = y, linetype = qname), linewidth =
                             1) +
        ggplot2::scale_colour_manual(
          name = sprintf(
            "Simulated Percentiles\nMedian (lines) %s%% CI (areas)",
            100 * vpc$conf.level
          ),
          values = color,
          breaks = qlvls,
          labels = qlbls
        ) +
        ggplot2::scale_fill_manual(
          name = sprintf(
            "Simulated Percentiles\nMedian (lines) %s%% CI (areas)",
            100 * vpc$conf.level
          ),
          values = color,
          breaks = qlvls,
          labels = qlbls
        ) +
        ggplot2::scale_linetype_manual(
          name = "Observed Percentiles\n(black lines)",
          values = linetype,
          breaks = qlvls,
          labels = qlbls
        ) +
        ggplot2::guides(
          fill = ggplot2::guide_legend(order = 2),
          colour = ggplot2::guide_legend(order = 2),
          linetype = ggplot2::guide_legend(order = 1)
        )
    } else {
      g <- ggplot2::ggplot(vpc$strat)
    }

    if (show.points) {
      points.dat <- copy(vpc$obs)
      if (isTRUE(vpc$predcor) && method == "binless") {
        points.dat[, y := l.ypc]
      } else if (isTRUE(vpc$predcor)) {
        points.dat[, y := ypc]
      }
      if (show.binning) {
        reorder2 <- function(y, x) {
          y <- stats::reorder(y, x)
          (1:nlevels(y))[y]
        }
        points.dat[, color := reorder2(factor(bin), x), by = vpc$strat]
        points.dat[, color := factor(color)]
        points.dat <- points.dat[!(blq | alq)]
        g <-
          g + ggplot2::geom_point(
            data = points.dat,
            ggplot2::aes(x = x, y = y, color = color),
            size = point.size,
            alpha = point.alpha,
            shape = point.shape,
            stroke = point.stroke,
            show.legend = FALSE
          ) +
          ggplot2::scale_color_brewer(palette = "Set1")
      } else {
        points.dat <- points.dat[!(blq | alq)]
        g <-
          g + ggplot2::geom_point(
            data = points.dat,
            ggplot2::aes(x = x, y = y),
            size = point.size,
            shape = point.shape,
            stroke = point.stroke,
            alpha = point.alpha
          )
      }
    }

    if (show.boundaries && method == "binning") {
      if (!is.null(vpc$strat)) {
        boundaries <-
          bininfo(vpc)[, .(x = sort(unique(c(xleft, xright)))), by = names(vpc$strat)]
      } else {
        boundaries <- bininfo(vpc)[, .(x = sort(unique(c(xleft, xright))))]
      }
      if (show.binning) {
        g <-
          g + ggplot2::geom_vline(
            data = boundaries,
            ggplot2::aes(xintercept = x),
            linewidth = ggplot2::rel(0.5),
            col = "gray80"
          ) +
          ggplot2::theme(panel.grid = ggplot2::element_blank())
      }
      g <-
        g + ggplot2::geom_rug(
          data = boundaries,
          ggplot2::aes(x = x),
          sides = "t",
          linewidth = 1
        )
    }

    if (facet) {
      if (!is.null(vpc$strat)) {
        g <-
          g + ggplot2::facet_grid(as.formula(paste(
            "qname ~", paste0(names(vpc$strat), collapse = " + "), sep = " "
          )),
          scales = facet.scales,
          as.table = FALSE)
      } else {
        g <-
          g + ggplot2::facet_grid(qname ~ ., scales = facet.scales, as.table = FALSE)
      }
    } else {
      if (!is.null(vpc$strat)) {
        if (length(as.list(vpc$strat.formula)) == 3) {
          g <- g + ggplot2::facet_grid(vpc$strat.formula, scales = facet.scales)
        } else {
          g <- g + ggplot2::facet_wrap(names(vpc$strat), scales = facet.scales)
        }
      }
    }
    g
  }


plot_categorical <-
  function(vpc,
           ribbon.alpha,
           facet,
           facet.scales,
           point.size,
           point.shape,
           point.stroke,
           point.alpha) {

    y <- md <- pname <- hi <- lo <- NULL

    method <- vpc$vpc.method$method
    if (method == "binning") {
      xvar <- "xbin"
    } else {
      xvar <- "x"
    }

    point_shape_vec <- .get_point_shapes()
    if (!point.shape %in% names(point_shape_vec))
      stop(paste0("point.shape must be one of ", paste0(names(point_shape_vec), collapse = ", ")))
    point.shape <-
      as.numeric(point_shape_vec[names(point_shape_vec) == point.shape])

    g <- ggplot(vpc$stats, aes(x = !!sym(xvar))) +
      geom_ribbon(
        aes(
          ymin = lo,
          ymax = hi,
          fill = pname,
          col = pname,
          group = pname
        ),
        alpha = ribbon.alpha,
        col = NA
      ) +
      geom_line(aes(y = md, col = pname, group = pname)) +
      geom_line(aes(y = y, linetype = pname), linewidth = 1) +
      geom_point(
        aes(x = !!sym(xvar), y = y),
        size = point.size,
        alpha = point.alpha,
        shape = point.shape,
        stroke = point.stroke
      ) +
      scale_colour_manual(
        name = sprintf(
          "Simulated \nMedian (lines) %s%% CI (areas)",
          100 * vpc$conf.level
        ) ,
        breaks = levels(vpc$stats$pname),
        values = .get_colors(length(levels(vpc$stats$pname))),
        labels = levels(vpc$stats$pname)
      ) +
      scale_fill_manual(
        name = sprintf(
          "Simulated \nMedian (lines) %s%% CI (areas)",
          100 * vpc$conf.level
        ),
        breaks = levels(vpc$stats$pname),
        values = .get_colors(length(levels(vpc$stats$pname))),
        labels = levels(vpc$stats$pname)
      ) +
      scale_linetype_manual(
        name = "Observed \nMedian (lines)",
        breaks = levels(vpc$stats$pname),
        values = .get_lines(length(levels(vpc$stats$pname))),
        labels = levels(vpc$stats$pname)
      ) +
      guides(
        fill = guide_legend(order = 2),
        colour = guide_legend(order = 2),
        linetype = guide_legend(order = 1)
      )

    if (facet) {
      if (!is.null(vpc$strat)) {
        g <-
          g + ggplot2::facet_grid(
            as.formula(paste(
              paste0(names(vpc$strat), collapse = " + "), "~", "pname", sep = " "
            )),
            scales = facet.scales,
            as.table = TRUE,
            labeller = label_both
          )
      } else {
        g <-
          g + ggplot2::facet_grid(
            ~ pname,
            scales = facet.scales,
            as.table = FALSE,
            labeller = label_both
          )
      }
    } else {
      if (!is.null(vpc$strat)) {
        g <-
          g + ggplot2::facet_wrap(names(vpc$strat), scales = facet.scales, label = label_both)
      }
    }

    return(g)

  }


plot_censored <-
  function(vpc,
           type = c("blq", "alq"),
           facet.scales = c("free", "fixed"),
           custom.theme,
           legend.position,
           show.points,
           show.boundaries,
           show.binning) {

    stopifnot(inherits(vpc, "tidyvpcobj"))
    hi <- lo <- md <- xbin <- y <- NULL
    . <- list

    method <- vpc$vpc.method$method

    if(method == "binning") {
      xvar <- "xbin"
    } else {
      xvar <- "x"
    }

    type <- match.arg(type)

    df_name <- paste0("pct", type)
    df <- vpc[[df_name]]
    if (is.null(df)) {
      stop(
        df_name,
        " data.frame was not found in tidyvpcobj. Use `censoring()` to create censored data for plotting ",
        type,
        "data."
      )
    }

    g <- ggplot(df)

    if (!is.null(vpc$strat)) {
      if (length(as.list(vpc$strat.formula)) == 3) {
        g <- g + ggplot2::facet_grid(vpc$strat.formula, scales = facet.scales)
      } else {
        g <- g + ggplot2::facet_wrap(names(vpc$strat), scales = facet.scales)
      }
    }

    g <- g +
      geom_ribbon(aes(x = !!sym(xvar), ymin = lo, ymax = hi),
                  fill = "red",
                  alpha = .2) +
      geom_line(aes(x = !!sym(xvar), y = md, color = "simulated")) +
      geom_line(aes(x = !!sym(xvar), y = y, color = "observed")) +
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

    # ensure x axis is same scale given options in vpc plot that can affect xmax
    if (method == "binning" &&
        any(show.binning, show.boundaries, show.points)) {
      if (any(show.binning, show.boundaries)) {
        if (!is.null(vpc$strat)) {
          xlim_df <-
            bininfo(vpc)[, .(x = sort(unique(c(xleft, xright)))), by = names(vpc$strat)]
        } else {
          xlim_df <-
            bininfo(vpc)[, .(x = sort(unique(c(xleft, xright))))]
        }
      } else {
        if (!is.null(vpc$strat)) {
          xlim_df <-
            copy(vpc$obs)[!(blq |
                              alq)][, .(x = max(x)), by = names(vpc$strat)]
        } else {
          xlim_df <-
            copy(vpc$obs)[!(blq |
                              alq)][, .(x = max(x))]
        }
      }
      g <- g + ggplot2::geom_rug(
        data = xlim_df,
        ggplot2::aes(x = x),
        sides = "t",
        alpha = 0
      )
    }

    # add theme
    if (is.null(custom.theme)) {
      g <- g + ggplot2::theme_bw() + tidyvpc_theme(legend.position = legend.position)
    } else if (is.character(custom.theme)) {
      g <- g + eval(parse(text = paste0(custom.theme, "()")))
    } else if (is.function(custom.theme)) {
      g <- g + custom.theme()
    } else if (inherits(custom.theme, "theme")) {
      g <- g + custom.theme
    }

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

.get_point_shapes <- function(){
  point_shape_vec <-  c("circle" = 1, "circle-fill" = 19,  "diamond" = 5, "diamond-fill" = 18,
                        "square" = 0, "square-fill" = 15,  "triangle-fill" = 17, "triangle" = 2)
}

tidyvpc_theme <-  function(legend.position) {
  ggplot2::theme(
    legend.title = ggplot2::element_text(size = 10),
    legend.text = ggplot2::element_text(size = 7),
    legend.position = legend.position,
    legend.spacing = unit(.1, "cm"),
    legend.direction = "horizontal",
    legend.key.size = unit(.5, "cm")
  )
}
