################################################################################
# This file is released under the GNU General Public License, Version 3, GPL-3 #
# Copyright (C) 2021 Yohann Demont                                             #
#                                                                              #
# It is part of IFCshiny package, please cite:                                 #
#  -IFCshiny: An R Interactive Shiny Application for the Analysis of Imaging   #
#             and Conventional Flow Cytometry                                  #
#  -YEAR: 2021                                                                 #
#  -COPYRIGHT HOLDERS: Yohann Demont, Jean-Pierre Marolleau, Loïc Garçon,      #
#                      CHU Amiens                                              #
#                                                                              #
# DISCLAIMER:                                                                  #
#  -You are using this package on your own risk!                               #
#  -We do not guarantee privacy nor confidentiality.                           #
#  -This program is distributed in the hope that it will be useful, but WITHOUT#
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or        #
# FITNESS FOR A PARTICULAR PURPOSE. In no event shall the copyright holders or #
# contributors be liable for any direct, indirect, incidental, special,        #
# exemplary, or consequential damages (including, but not limited to,          #
# procurement of substitute goods or services; loss of use, data, or profits;  #
# or business interruption) however caused and on any theory of liability,     #
# whether in contract, strict liability, or tort (including negligence or      #
# otherwise) arising in any way out of the use of this software, even if       #
# advised of the possibility of such damage.                                   #
#                                                                              #
# You should have received a copy of the GNU General Public License            #
# along with IFCshiny. If not, see <http://www.gnu.org/licenses/>.             #
################################################################################


# function prepare data for plotting 
plot_prepare_data <- function(obj, graph, color_mode = c("white","black")[1], precision = c("light","full")[1],
                              trunc_labels = 38L, trans = asinh, bin, viewport = "data",
                              nticks_x = 10, nticks_y = 10, batch_mode = FALSE,
                              compute_density = TRUE, ...) {
  assert(color_mode, len = 1, alw = c("white", "black"))
  mode = c(2, 1)[c("white", "black") == color_mode]
  normalize = FALSE
  
  P = obj$pops
  R = obj$regions
  g = do.call(what=buildGraph, args=graph)
  
  # prepare nbin
  if(missing(bin)) {
    if(g$type=="histogram") {
      nbin = g$bincount
    } else {
      nbin = g$BasePop[[1]]$densitybincount
    }
    nbin = na.omit(as.integer(nbin))
    if(length(nbin)==0) nbin=ifelse(g$type=="histogram", 520, 128)
    if(nbin==0) nbin=ifelse(g$type=="histogram", 520, 128)
  } else {
    nbin = na.omit(as.integer(bin)); assert(nbin, len=1, typ="integer")
  }
  
  # prepare trans
  if(missing(trans)) trans = g$BasePop[[1]]$densitytrans 
  is_fun = inherits(trans, what="function") || !inherits(try(suppressWarnings(formals(trans)), silent = TRUE), what="try-error")
  dens_feat = numeric()
  if(length(trans) == 0) trans = "asinh"
  foo = c(g$f1, g$f2)
  if(g$type == "density" && !is_fun) foo = c(foo, trans)
  tmp = foo %in% names(obj$features)
  if(!all(tmp)) stop(paste0("trying to plot a features not found in obj$features: ",  paste0(foo[!tmp], collapse=", ")))
  
  # prepare transform
  Xtrans = g$xtrans; if(length(Xtrans) == 0) Xtrans = g$xlogrange
  Ytrans = g$ytrans; if(length(Ytrans) == 0) Ytrans = g$ylogrange
  trans_x <- parseTrans(Xtrans)
  trans_y <- parseTrans(Ytrans)
  
  # extracts graph information
  if(g$type == "histogram") {
    D = obj$features[,c("Object Number",g$f1)]
    names(D) = c("Object Number","x1")
    D[,"x2"] = applyTrans(D[,"x1"], trans_x)
  } else {
    D = obj$features[,c("Object Number",g$f1,g$f2)]
    names(D) = c("Object Number","x1","y1")
    D[,"x2"] = applyTrans(D[,"x1"], trans_x)
    D[,"y2"] = applyTrans(D[,"y1"], trans_y)
  }
  
  base_n = unlist(lapply(g$BasePop, FUN=function(x) x$name))
  reg_n = unlist(lapply(g$GraphRegion, FUN=function(x) x$name))
  shown_n = unlist(lapply(g$ShownPop, FUN=function(x) x$name))
  graph_n = unlist(lapply(g$GraphRegion, FUN=function(x) x$def))
  
  operators = c("And","Or","Not","(",")")
  displayed_n = unique(splitn(definition = g$order, all_names = c(base_n, graph_n, shown_n, "Selected Bin"), operators = operators))
  displayed_n = setdiff(displayed_n, "Selected Bin")
  displayed_r = rev(displayed_n)
  tmp = displayed_n %in% names(P)
  if(!batch_mode && !all(tmp)) stop(paste0("trying to display a population not found in obj$pops: ",  paste0(displayed_n[!tmp], collapse=", ")))
  displayed_d = sapply(displayed_n, FUN=function(x) {
    foo = P[[x]]$obj
    if(length(foo) == 0) foo = rep(FALSE, times=obj$description$ID$objcount)
    foo
  })
  
  base_o = sapply(base_n, FUN=function(x) which(displayed_n%in%x))
  base_n = base_n[order(base_o)]
  
  if(length(shown_n) == 0) {
    shown_o = NULL
    shown_n = NULL
  } else {
    shown_o = unlist(sapply(shown_n, FUN=function(x) which(displayed_n%in%x)))
    shown_n = names(shown_o)[order(shown_o)]
  }
  
  # subset data
  base = as.data.frame(sapply(base_n, FUN=function(x) {
    foo = P[[x]]$obj
    if(length(foo) == 0) foo = rep(FALSE, times=obj$description$ID$objcount)
    foo
  }), stringsAsFactors = FALSE)
  data_sub = apply(base, 1, any)
  displayed_o = c(base_o, shown_o)
  D = cbind(D, displayed_d)
  D = cbind(1:nrow(D), D)
  data = D
  D = D[data_sub, ]
  
  if(nrow(D) > 0) {
    xy_subset = rep(FALSE, nrow(D))
    if(g$maxpoints <= 1) {
      xy_subset[sample(x = nrow(D), size = g$maxpoints * nrow(D), replace = FALSE)] <- TRUE
    } else {
      xy_subset[sample(x = nrow(D), size = min(g$maxpoints, nrow(D)), replace = FALSE)] <- TRUE
    }
    D = D[xy_subset, ,drop=FALSE]
  } else {
    xy_subset = TRUE
  }
  
  # prepare limits
  Xlim = c(g$xmin, g$xmax)
  Xlim = applyTrans(Xlim, trans_x)
  if(!all(is.finite(Xlim))) Xlim = c(-1, 1)
  Ylim = c(g$ymin, g$ymax)
  Ylim = applyTrans(Ylim, trans_y)
  if(!all(is.finite(Ylim))) Ylim = c(-1, 1)
  if(viewport == "data") {
    Xlim = suppressWarnings(range(D[,"x1"], na.rm = TRUE, finite = TRUE))
    Xlim = applyTrans(Xlim, trans_x)
    Xlim = Xlim + c(-0.07,0.07)*diff(Xlim)
    if(Xlim[1] == Xlim[2]) Xlim = Xlim[1] + c(-0.07,0.07)
    if(g$type != "histogram") {
      Ylim = suppressWarnings(range(D[,"y1"], na.rm = TRUE, finite = TRUE))
      Ylim = applyTrans(Ylim, trans_y)
      Ylim = Ylim + c(-0.07,0.07)*diff(Ylim)
      if(Ylim[1] == Ylim[2]) Ylim = Ylim[1] + c(-0.07,0.07)
    }
  }
  if(viewport == "max") {
    regx = sapply(reg_n, FUN=function(r) {
      reg = R[[r]] 
      coords = reg[["x"]]
      return(c(reg$cx, coords))
    })
    Xlim = suppressWarnings(range(c(D[,"x1"], regx), na.rm = TRUE, finite = TRUE))
    Xlim = applyTrans(Xlim, trans_x)
    Xlim = Xlim + c(-0.07,0.07)*diff(Xlim)
    if(Xlim[1] == Xlim[2]) Xlim = Xlim[1] + c(-0.07,0.07)
    if(g$type != "histogram") {
      regy = sapply(reg_n, FUN=function(r) {
        reg = R[[r]] 
        coords = reg[["y"]]
        return(c(reg$cy, coords))
      })
      Ylim = suppressWarnings(range(c(D[,"y1"], regy), na.rm = TRUE, finite = TRUE))
      Ylim = applyTrans(Ylim, trans_y)
      Ylim = Ylim + c(-0.07,0.07)*diff(Ylim)
      if(Ylim[1] == Ylim[2]) Ylim = Ylim[1] + c(-0.07,0.07)
    }
  }
  if(viewport == "ideas") {
    if(Xlim[1] == Xlim[2]) Xlim = Xlim[1] + c(-0.07,0.07)
    D[D[,"x2"] < Xlim[1], "x2"] <- Xlim[1] # D = D[(D[,"x2"] >= Xlim[1]) & (D[,"x2"] <= Xlim[2]), ]
    D[D[,"x2"] > Xlim[2], "x2"] <- Xlim[2] #
    if(g$type != "histogram") {
      D[D[,"y2"] < Ylim[1], "y2"] <- Ylim[1] # D = D[(D[,"x2"] >= Xlim[1]) & (D[,"x2"] <= Xlim[2]), ]
      D[D[,"y2"] > Ylim[2], "y2"] <- Ylim[2] #
      if(Ylim[1] == Ylim[2]) Ylim = Ylim[1] + c(-0.07,0.07)
    }
  }
  
  xtop = NULL
  if(is_fun) {
    dens_feat = obj$features[data_sub,,drop=FALSE][xy_subset,,drop=FALSE]
  } else {
    if((length(g$BasePop[[base_o[1]]][["densitylevel"]]) == 0) || (g$BasePop[[base_o[1]]][["densitylevel"]] == "")) {
      xtop = trans
    } 
    dens_feat = obj$features[data_sub,,drop=FALSE][xy_subset,trans,drop=TRUE]
    dens_ran = suppressWarnings(range(dens_feat, na.rm = TRUE))
    dens_feat = (dens_feat-dens_ran[1])/diff(dens_ran)
  }
  switch(g$type, 
         "density" = {
           colramp = colorRampPalette(colConv(g$BasePop[[base_o[1]]][c("densitycolorsdarkmode","densitycolorslightmode")][[mode]]))
           if(nrow(D) > 0 && compute_density) {
             col = densCols(x=structure(D[,"x2"], features=dens_feat), y=D[,"y2"], colramp=colramp, nbin=nbin, transformation=trans)
           } else{
             col = "transparent"
           }
           pch = "."
           args = list(xtop = xtop, 
                       colramp = colramp, 
                       col = col,
                       pch = pch)
         },
         "scatter" = {
           col = sapply(displayed_r, FUN=function(p) P[[p]][c("color","lightModeColor")][[mode]])
           pch = sapply(displayed_r, FUN=function(p) P[[p]]$style)
           args = list(col = col, 
                       pch = pch)
         },
         "histogram" = {
           type = "count"
           if(as.logical(g$freq)) type = "percent"
           smooth = g$histogramsmoothingfactor > 0
           br = lattice::do.breaks(Xlim, nbin)
           Ylim = lapply(displayed_n, FUN = function(p) {
             sub_ = as.vector(D[, tail(which(colnames(D) == p),1), drop = TRUE])
             x = D[sub_,"x2"]
             if(length(x) == 0) return(c(NA_real_, NA_real_))
             if(normalize) return(c(0,1)) # never happen
             h = hist_constr(x, br, include.lowest=TRUE, right=TRUE, plot=FALSE)
             yy = val_constr(x, h, type = type)
             return(suppressWarnings(range(yy, na.rm=TRUE, finite=TRUE)))
           })
           Ylim = suppressWarnings(range(unlist(Ylim), na.rm=TRUE, finite = TRUE))
           # if(viewport == "max") {
           regy = sapply(reg_n, FUN=function(r) {
             reg = R[[r]] 
             coords = reg[["y"]]
             return(c(reg$cy, coords))
           })
           Ylim = suppressWarnings(range(c(Ylim, unlist(regy)), na.rm=TRUE, finite = TRUE))
           Ylim = Ylim + c(-0.07,0.07)*diff(Ylim)
           # }
           col = sapply(displayed_n, FUN=function(p) P[[p]][c("color","lightModeColor")][[mode]])
           fill = sapply(displayed_o, FUN = function(p) g$BasePop[[p]]$fill=="true")
           lty = sapply(displayed_o, FUN = function(p) c(1,2,3,4,6)[match(g$BasePop[[p]]$linestyle, c("Solid","Dash","Dot","DashDot","DashDotDot"))])
           args = list(col = col,
                       fill = fill,
                       lty = lty)
         })
  return(list(data = D[,-1],
              scales = list(trans_x = Xtrans, Xlim=Xlim,
                            trans_y = Ytrans, Ylim=Ylim,
                            nbin=nbin),
              axes = list(x = c(base_axis_constr(lim = Xlim, trans = Xtrans, nint = nticks_x),text=g$xlabel),
                          y = c(base_axis_constr(lim = Ylim, trans = Ytrans, nint = nticks_y),text=g$ylabel)),
              args = c(list(type = g$type, color_mode = color_mode, nbin = nbin, displayed = displayed_r, order = displayed_o, main = g$title), args)))
}

batch_stats <- function(batch, pop = "All", feat, method = c("wilcox","t","none")[3]) {
  if(missing(pop)) pop = "All"
  if(missing(feat)) feat = names(batch[[1]]$features)
  L = length(batch)
  N = names(batch)
  if(length(N) == 0) {
    N = as.character(seq_len(L))
    names(batch) = N
  }
  feat_1 = feat
  if(L > 1) for(i in 2:L) { feat = intersect(feat, names(batch[[i]]$features)) }
  dat <- lapply(1:L, FUN = function(i_batch) {
    k = 1 + (length(batch[[i_batch]]$description$FCS) == 0)
    pat = c("^(?!.*FS|.*SS|.*LOG).*$", "^Intensity|^Bright Detail Intensity|^Uncompensated")
    fun = c("asinh","smoothLinLog")
    d = batch[[i_batch]]$features[batch[[i_batch]]$pops[[pop]]$obj, feat, drop = FALSE]
    if(nrow(d) == 0) return(matrix(NA, nrow=1, ncol=1+length(feat), dimnames = list(NULL, c(feat,""))))
    int_to_tra = colnames(d)[grepl(pat[k], colnames(d))]
    if(length(int_to_tra) != 0) d[, int_to_tra] <- sapply(int_to_tra, FUN = function(i_trans) do.call(what = fun[k], args = list(x = d[,i_trans])))
    cbind(d, rep(i_batch, nrow(d)))
  })
  dat2 <- do.call(rbind.data.frame, args = dat)
  
  med = apply(dat2, 2, median, na.rm = TRUE)
  avg = apply(dat2, 2, mean, na.rm = TRUE)
  std = apply(dat2, 2, sd, na.rm = TRUE)
  if(ncol(dat2) <= 1) return(matrix(numeric(), nrow=length(N), ncol=0, dimnames=list(N,NULL)))
  
  X <- unlist(lapply(dat,FUN = function(d) {
    sapply(colnames(d)[-ncol(d)], FUN = function(i_col) {
      mean(d[, i_col], na.rm=TRUE)
    })
  }))
  
  zscore <- unlist(lapply(dat,FUN = function(d) {
    sapply(colnames(d)[-ncol(d)], FUN = function(i_col) {
      (mean(d[, i_col], na.rm=TRUE) - avg[i_col]) / std[i_col]
    })
  }))
  
  fold_avg <- unlist(lapply(dat,FUN = function(d) {
    sapply(colnames(d)[-ncol(d)], FUN = function(i_col) {
      mean(d[, i_col], na.rm=TRUE) / avg[i_col]
    })
  }))
  
  fold_med <- unlist(lapply(dat,FUN = function(d) {
    sapply(colnames(d)[-ncol(d)], FUN = function(i_col) {
      median(d[, i_col], na.rm=TRUE) / med[i_col]
    })
  }))
  
  p = rep(NA_real_, times=length(feat)*L)
  if(method != "none") {
    test = paste0(method,".test")
    p <- as.vector(t(sapply(1:L, FUN = function(i_batch) {
      d = dat[[i_batch]]
      compare = dat2[,ncol(dat2)] != i_batch
      sapply(colnames(d)[-ncol(d)], FUN = function(i_col) {
        foo = try(suppressWarnings(do.call(what = test, args = list(x=d[, i_col,drop=TRUE], y=dat2[compare, i_col,drop=TRUE]))$p.value), silent = TRUE)
        if(inherits(foo, "try-error")) return(NA_real_)
        foo
      })
    })))
  }
  return(structure(array(c(X,zscore,fold_avg,fold_med,p),
                         dim=c(length(feat),L,5),
                         dimnames=list(feat,N,c("X","zscore","fold_avg","fold_med","p"))),
                   "method"=method,
                   "pop"=pop))
}

# function to create volcano plot with plotly
# so as to visualize difference best discriminating feature for a single same pop of each members of a batch
plotly_batch_volcano <- function(a, fold="mean", height=NULL) {
  Log2 = function(x) {
    sapply(x, FUN = function(i) {
      if(is.na(i)) return(i)
      if(i < 0) return(-1*log2(-i))
      return(log2(i))
    })
  }
  w = switch(fold,"mean"="fold_avg","median"="fold_med",NA_character_)
  d = dim(a)
  dn = dimnames(a)
  xori = as.vector(a[,,w])
  yori = as.vector(a[,,"p"])
  ss = !is.na(xori) & !is.na(yori)
  xori = xori[ss]
  yori = yori[ss]
  xx = Log2(xori)
  yy = -log10(yori)
  sp = rep(dn[[2]], times = d[1])[ss]
  text = rep(dn[[1]], times = d[2])[ss]
  xran = range(xx, na.rm = TRUE, finite = TRUE)
  yran = range(c(yy, -log10(0.05)), na.rm = TRUE, finite = TRUE)
  p <- plotly::plot_ly(x = xx, y = yy, split = sp, text = text, 
                       customdata = paste0("<br>x:",xori,",<br>y:",yori),
                       type="scatter", mode="markers", height=height,
                       marker = list(size = 5, width = 2),
                       hovertemplate = "<i><b>%{text}</b></i>%{customdata}<extra></extra>")
  p <- p %>% plotly::layout(title=list(text="Volcano plot",x = 0),
                            xaxis = list(title = "log2 fold change", type="linear", range = c(c(-1.07,1.07)*max(abs(xran))),zeroline=TRUE),
                            yaxis = list(title= "-log10 p value", type="linear", range = c(0,1.07*max(yran))),
                            shapes = list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = -log10(0.05), y1 = -log10(0.05),
                                          line = list(dash="dash"), text="p value = 0.05"),
                            annotations = list(#list(text="pval = 0.001", xref="paper", x=0.05, y=-log10(0.001), yshift = 10, showarrow=FALSE),
                              #list(text="pval = 0.01", xref="paper", x=0.05, y=-log10(0.01), yshift = 10, showarrow=FALSE),
                              list(text="pval = 0.05", xref="paper", x=0.05, y=-log10(0.05), yshift = 10, showarrow=FALSE)))
  spp <- unique(sp) # check for empty category
  if(length(spp) != length(dn[[2]])) {
    for(i in 1:length(dn[[2]])) {
      if(!any(dn[[2]][i] %in% spp)) {
        p$x$attrs[[p$x$cur_data]]$x = c(p$x$attrs[[p$x$cur_data]]$x, 'null')
        p$x$attrs[[p$x$cur_data]]$y = c(p$x$attrs[[p$x$cur_data]]$y, 'null')
        p$x$attrs[[p$x$cur_data]]$text = c(p$x$attrs[[p$x$cur_data]]$text, "")
        p$x$attrs[[p$x$cur_data]]$customdata = c(p$x$attrs[[p$x$cur_data]]$customdata, "")
        p$x$attrs[[p$x$cur_data]]$split = c(p$x$attrs[[p$x$cur_data]]$split, dn[[2]][i])
      }
    }
  }
  p
}

# function to create heatmap with plotly
# so as to visualize difference between every (selected) features for a single same pop of each members of a batch
plotly_batch_heatmap <- function(a, what="zscore", dendro = FALSE, height=NULL, ...) {
  dots = list(...)
  dots = dots[!names(dots) %in% c("x", "row_dend_left", "plot_method", "main", "height", "custom_hovertext")]
  m = a[,,what]
  if(all(is.na(m))) return(NULL)
  if((length(m) != 0) && dendro && requireNamespace("heatmaply", quietly = TRUE)) {
    # heatmaply does not handle NA/NaN/Inf so we replace them with 0
    foo = which(!is.finite(m),arr.ind = TRUE)
    apply(foo, 1, FUN = function(x) m[x[1],x[2]] <<- 0)
    # modify rownames and hover
    n = rownames(m)
    n = paste(sprintf(paste0("%0",nchar(length(n)),"i"),1:length(n)), n, sep = ": ")
    rownames(m) <- center_short(n, max = 14)
    lab = outer( paste0("row: <i>", rownames(m),"<i>"), paste0("col: <b>", colnames(m), "</b>"), FUN = function(X,Y) paste(Y,X,sep="<br>"))
    lab = matrix(paste(lab, "<br>val: ", m, sep = ""), ncol = ncol(m))
    # plot it
    do.call(what = heatmaply::heatmaply,
            args = c(dots, list(x=m, height=height,
                                row_dend_left=TRUE, 
                                custom_hovertext = lab,
                                plot_method="plotly",
                                main="Batch Heatmap")))
  } else {
    plotly::plot_ly(x=colnames(m), y=rownames(a), z=m, height=height, type="heatmap") %>% layout(title=list(text="Batch Heatmap",x = 0))
  }
}

# function to create violin or ridge plot with plotly
# so as to visualize difference between one features for a single same pop of each members of a batch
plotly_batch_violin <- function(batch, pop = "All", feat = "Object Number", trans = "P",
                                space = 2*length(batch)-1, height = NULL,
                                type = c("violin", "ridge")[1],
                                points = c("all","outliers","none")[2]) {
  if(missing(pop)) pop = "All"
  if(missing(feat)) feat = "Object Number"
  L = length(batch)
  N = names(batch)
  N = sort(names(batch))
  if(length(N) == 0) {
    N = as.character(seq_len(L))
    names(batch) = N
  }
  batch = batch[N]
  S = seq_len(L)
  trans_ = parseTrans(trans)
  dat <- lapply(1:L, FUN = function(i_batch) {
    V = applyTrans(batch[[i_batch]]$features[batch[[i_batch]]$pops[[pop]]$obj, feat], trans_)
    V = V[is.finite(V)]
    cbind.data.frame("V1"=V, "V2"=rep(N[i_batch], length(V)))
  })
  is_empty = sapply(dat, FUN = function(d) length(d[, 1])) == 0
  dat <- as.data.frame(do.call(rbind, dat), stringsAsFactors = FALSE, check.names = FALSE)
  vlim = suppressWarnings(range(dat$V1))
  vaxis = base_axis_constr(vlim, trans = trans)
  if(any(is_empty)) dat = rbind(dat, cbind(V1=rep(0, times = sum(is_empty)), V2=N[is_empty]))
  switch(type,
         "violin" = {
           p <- plotly::plot_ly(x = dat[,2], y = dat[, 1], split = dat[,2],
                                height=height,
                                points = points,
                                box = list(visible = TRUE),
                                meanline = list(visible = TRUE),
                                type = "violin")
           p <- p %>% plotly::style(pointpos=0) %>%
             plotly::layout(title=list(text=feat,x = 0),
                            yaxis = list(title = "", type="linear",
                                         tickvals = vaxis$at,
                                         ticktext = vaxis$labels,
                                         fixedrange =TRUE, autorange = TRUE, zeroline=FALSE),
                            xaxis = list(type="category", autorange = TRUE, ticktext=S, tickvals=N))
         }, ridge = {
           p <- plotly::plot_ly(y = dat[,2], x = dat[, 1], split = dat[,2], height = height, type = "violin")
           p <- p %>% plotly::style(orientation="h", side="positive", width=space, points=FALSE) %>%
             plotly::layout(title=list(text=feat,x = 0),
                            xaxis = list(title = "", type="linear", 
                                         tickvals = vaxis$at,
                                         ticktext = vaxis$labels,
                                         autorange = TRUE, zeroline=FALSE),
                            yaxis = list(type="category", fixedrange =TRUE, autorange = TRUE, ticktext=S, tickvals=N))
         })
  if(any(is_empty)) { # check for empty category
    for(i in 1:L) {
      if(is_empty[i]) {
        p$x$data[[i]]$visible="legendonly"
        p$x$data[[i]][[c("x","y")[1 + (type=="violin")]]] = 'null'
      }
    }
  }
  p
}

# function to plot batch 3D stack
plotly_batch_stack <- function(batch, g, viewport = "data", pt_size = 2, alpha = 0.5, fill = TRUE, height = NULL, ...) {
  dots = list(...)
  L = length(batch)
  N = names(batch)
  N = sort(names(batch))
  if(length(N) == 0) {
    N = as.character(seq_len(L))
    names(batch) = N
  }
  batch = batch[N]
  assert(viewport, len = 1, alw = c("ideas","data","max"))
  # see https://plotly.com/python/marker-style/#custom-marker-symbols
  map_style_plotly = function(style) {
    set1 = c("circle", "x", "cross", 
             "circle-open", "diamond-open", "square-open", 
             "triangle-up-open", "diamond", "square", 
             "triangle-up")
    set2 = c(20, 4, 3, 1, 5, 0, 2, 18, 15, 17)
    style_ = suppressWarnings(as.integer(style))
    foo = style_ %in% set2
    if(any(foo)) {
      bar = sapply(style_[foo], FUN = function(x) which(set2 %in% x))
      style[foo] <- set1[bar]
    }
    return(style)
  }
  map_color_plotly = function(color, alpha=0.8) {
    col = map_color(color = color, toR = TRUE)
    apply(col2rgb(color,alpha=FALSE), 2, FUN = function(x) paste0("'rgba(",paste0(c(x,alpha),collapse=','),")'"))
  }
  map_lty_plotly = function(lty) {
    set1 =   c("solid","dash","dot","dashdot","dashdotdot")
    set2 = c(1,2,3,4,5)
    lty_ = suppressWarnings(as.integer(lty))
    foo = lty_ %in% set2
    if(any(foo)) {
      bar = sapply(lty_[foo], FUN = function(x) which(set2 %in% x))
      lty[foo] <- set1[bar]
    }
    return(lty)
  }
  
  dat <- lapply(1:L, FUN = function(i_batch) {
    do.call(what = plot_prepare_data, args = c(list(obj = batch[[i_batch]], graph = g, viewport = viewport), dots))
  })
  
  dat2 <- as.data.frame(do.call(rbind, args = lapply(dat, FUN = function(d) d$data)), stringsAsFactors = FALSE, check.names = FALSE)
  switch(viewport,
         "ideas" = {
           Xlim = suppressWarnings(range(c(dat2[, "x1"], g$xmin, g$xmax), na.rm = TRUE, finite = TRUE))
           Xlim = applyTrans(Xlim, parseTrans(dat[[1]]$scales$trans_x))
           if(g$type != "histogram") {
             Ylim = suppressWarnings(range(c(dat2[, "y1"], g$ymin, g$ymax), na.rm = TRUE, finite = TRUE))
             Ylim = applyTrans(Ylim, parseTrans(dat[[1]]$scales$trans_y))
           } 
         },
         { # default i.e. data or max
           Xlim = suppressWarnings(range(c(dat2[, "x2"]), na.rm = TRUE, finite = TRUE))
           if(g$type != "histogram") Ylim = suppressWarnings(range(c(dat2[, "y2"]), na.rm = TRUE, finite = TRUE))
         })
  Xlim = Xlim + c(-0.07,0.07)*diff(Xlim)
  if(Xlim[1] == Xlim[2]) Xlim = Xlim[1] + c(-0.07,0.07)
  if(g$type == "histogram") Ylim = c(0,0)
  Ylim = Ylim + c(-0.07,0.07)*diff(Ylim)
  if(Ylim[1] == Ylim[2]) Ylim = Ylim[1] + c(-0.07,0.07)
  
  p = plotly::plot_ly(height = height)
  canfill = fill && requireNamespace(package = "decido", quietly = TRUE)
  for(i_batch in 1:L) {
    displayed = dat[[i_batch]]$args$displayed
    ll = length(displayed)
    if(nrow(dat[[i_batch]]$data) == 0) {
      p <- p %>% add_trace(x = 0, y = 0, z = N[i_batch], name = N[i_batch], visible='legendonly')
      w = p$x$cur_data == names(p$x$attrs)
      p$x$attrs[w][[i_batch]]$x <- 'null'
      p$x$attrs[w][[i_batch]]$y <- 'null'
      p$x$attrs[w][[i_batch]]$z <- 'null'
    } else {
      switch(dat[[i_batch]]$args$type,
             "histogram" = {
               type = "count"
               if(as.logical(g$freq)) type = "percent"
               normalize = FALSE
               br = lattice::do.breaks(Xlim, dat[[i_batch]]$args$nbin)
               REV = displayed[rev(dat[[i_batch]]$args$order)]
               for(disp in REV) {
                 sub_ = dat[[i_batch]]$data[, tail(which(colnames(dat[[i_batch]]$data) == disp),1), drop=TRUE]
                 d = dat[[i_batch]]$data[sub_, ,drop = FALSE]
                 jit = seq(from = i_batch - 0.1, to = i_batch + 0.1, length.out = ll)
                 ID = which(REV == disp)
                 if(nrow(d) == 0) {
                   yy = NULL
                   p <- p %>% plotly::add_trace(x = 0, y = 0, z = N[i_batch], name = disp, legendgroup = N[i_batch], visible='legendonly')
                   w = p$x$cur_data == names(p$x$attrs)
                   p$x$attrs[w][[i_batch]]$x <- 'null'
                   p$x$attrs[w][[i_batch]]$y <- 'null'
                   p$x$attrs[w][[i_batch]]$z <- 'null'
                 } else {
                   # compute density
                   x = d[, "x2"]
                   h <- hist_constr(x, br, include.lowest=TRUE, right=TRUE, plot=FALSE)
                   xx=val_constr(x, h, "mids")
                   yy=density(x, n=length(br)-1, na.rm=TRUE, from=min(br), to=max(br))$y
                   yy[xx<min(x, na.rm=TRUE)]=0
                   yy[xx>max(x, na.rm=TRUE)]=0
                   yy=yy/max(yy)*max(val_constr(x, h, type))
                   yy = c(yy,0)
                   # use only non repetitive points
                   aa = rle(yy)
                   yyy = aa$v; yyy = c(0,yyy,0);
                   xxx = br[cumsum(aa$l)]; xxx = c(xxx[1],xxx,xxx[length(xxx)])
                   # draw lines
                   p <- p %>% plotly::add_trace(x = xxx, y = yyy,
                                                z = jit[ID],
                                                colors = dat[[i_batch]]$args$col,
                                                color = disp,
                                                line=list(width=4,
                                                          dash = map_lty_plotly(dat[[i_batch]]$args$lty[disp])),
                                                name = disp,
                                                text = paste(N[i_batch],disp,sep="<br>"),
                                                hovertemplate = "x: %{x}<br>y: %{y}<extra>%{text}</extra>",
                                                showlegend = !canfill,
                                                legendgrouptitle = list(text=N[i_batch]),
                                                legendgroup = N[i_batch],
                                                type = "scatter3d",
                                                mode = "lines") 
                   # fill lines
                   if(canfill) {
                     # run earcut polygon triangulation to build mesh
                     idx = decido::earcut(cbind(xxx,yyy))
                     # JS indexing start at 0
                     ijk = matrix(idx, 3) - 1
                     # add indexing for z
                     ijk = rbind(ijk, rep(0, ncol(ijk)))
                     # creates a colorscale
                     K = matrix(c(0,1,rep(map_color_plotly(dat[[i_batch]]$args$col[disp], alpha), 2)),
                                ncol = 2, byrow = FALSE)
                     p <- p %>% plotly::add_mesh(x=xxx, y=yyy, z = rep(jit[ID], length(xxx)),
                                                 i = ijk[1,], j = ijk[2,], k = ijk[3,],
                                                 showlegend = TRUE,
                                                 alphahull = -1,
                                                 delaunayaxis = "z",
                                                 opacity = alpha,
                                                 cauto = FALSE,
                                                 autocolorscale = FALSE,
                                                 showscale = FALSE,
                                                 intensity = 1,
                                                 colorscale = K,
                                                 hoverinfo = "skip",
                                                 name = disp, 
                                                 legendgrouptitle = list(text=N[i_batch]),
                                                 legendgroup = N[i_batch],
                                                 type = "mesh3d")
                     
                   }
                 }
                 Ylim = suppressWarnings(range(Ylim, yy))
               }
               if(Ylim[1] == Ylim[2]) Ylim = Ylim[1] + c(-0.07,0.07)
             },
             "scatter"= {
               for(disp in displayed) {
                 sub_ = dat[[i_batch]]$data[, tail(which(colnames(dat[[i_batch]]$data) == disp),1), drop=TRUE]
                 d = dat[[i_batch]]$data[sub_, ,drop = FALSE]
                 if(nrow(d) == 0) {
                   p <- p %>% plotly::add_trace(x = 0, y = 0, z = N[i_batch], name = disp, legendgroup = N[i_batch], visible='legendonly')
                   w = p$x$cur_data == names(p$x$attrs)
                   p$x$attrs[w][[i_batch]]$x <- 'null'
                   p$x$attrs[w][[i_batch]]$y <- 'null'
                   p$x$attrs[w][[i_batch]]$z <- 'null'
                 } else{
                   jit = seq(from = i_batch - 0.1, to = i_batch + 0.1, length.out = ll)
                   p <- p %>% plotly::add_trace(x = d[, "x2"],
                                                y = d[, "y2"], 
                                                z = jitter(jit[which(dat[[i_batch]]$args$displayed == disp)]), 
                                                symbols = map_style_plotly(dat[[i_batch]]$args$pch),
                                                symbol = disp,
                                                colors = dat[[i_batch]]$args$col,
                                                color = disp,
                                                marker=list(size=pt_size),
                                                text = paste0(N[i_batch],"<br>",disp,"<br>",d[, "Object Number"]),
                                                hoverinfo = "all",
                                                hovertemplate = "x: %{x}<br>y: %{y}<extra>%{text}</extra>",
                                                ids = d[, "Object Number"],
                                                name = disp,
                                                legendgrouptitle = list(text=N[i_batch]),
                                                legendgroup = N[i_batch],
                                                type = "scatter3d",
                                                mode="markers")
                 }
               }
             },
             "density"= {
               p <- p %>% plotly::add_trace(x = dat[[i_batch]]$data[, "x2"],
                                            y = dat[[i_batch]]$data[, "y2"], 
                                            z = jitter(rep(i_batch, nrow(dat[[i_batch]]$data))), 
                                            marker=list(size=pt_size,
                                                        symbol = map_style_plotly(dat[[i_batch]]$args$pch[displayed[1]]),
                                                        color = dat[[i_batch]]$args$col),
                                            text = paste0(N[i_batch],"<br>",displayed[1],"<br>",dat[[i_batch]]$data[, "Object Number"]),
                                            hoverinfo = "all",
                                            hovertemplate = "x: %{x}<br>y: %{y}<extra>%{text}</extra>",
                                            ids = dat[[i_batch]]$data[, "Object Number"],
                                            name = N[i_batch],
                                            type = "scatter3d",
                                            mode="markers")
             })
    }
  }
  # tips to get current camera position
  # $('.plotly .plot-container')[0].parentNode._fullLayout.scene._scene.getCamera()
  xaxis = base_axis_constr(Xlim, trans = dat[[1]]$scales$trans_x)
  yaxis = base_axis_constr(Ylim, trans = dat[[1]]$scales$trans_y)
  Zlim = c(1,L)+c(-0.1,0.1) # due to jitter z is extended
  Zlim = Zlim + c(-0.07,0.07)*diff(Zlim) # add 0.07
  title = dat[[1]]$args$main
  if(g$type == "density" && (length(dat[[1]]$args$xtop) != 0)) title = paste0(title,"<br><sup>",dat[[1]]$args$xtop,"</sup>")
  p <- p %>% plotly::layout(title=list(text=title,x = 0, y = 1, xref="paper", yref="paper"),
                            scene=list(aspectmode="cube",
                                       camera=list(up=list(x=-0.05, y=-1, z=-0.1),
                                                   eye=list(x=-4/3, y=-1/3, z=2.5)),
                                       yaxis = list(title = center_short(dat[[1]]$axes$y$text, 20),
                                                    range= rev(Ylim), zeroline=FALSE,
                                                    tickmode = "array", type="linear",
                                                    tickvals = yaxis$at,
                                                    ticktext = yaxis$labels),
                                       xaxis = list(title = center_short(dat[[1]]$axes$x$text, 20), 
                                                    range = rev(Xlim), zeroline=FALSE,
                                                    tickmode = "array", type="linear",
                                                    tickvals = xaxis$at,
                                                    ticktext = xaxis$labels),
                                       zaxis = list(title = "batch", type="linear",
                                                    range = rev(Zlim), zeroline=FALSE,
                                                    tickmode = "array",
                                                    tickvals = 1:L,
                                                    ticktext = as.character(1:L))),
                            legend = list(title = center_short(N, 20), groupclick = "toggleitem"))
  p
}