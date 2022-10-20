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

# Create a rgl toggle checkbox
# I used the same structure as in shiny
# except that I did not include width param but it can be passed thanks to ...
Toggle3D <- function(inputId,
                     label,
                     value = FALSE,
                     widgetId = NULL,
                     objIds = NULL,
                     ...) {
  dots = list(...)
  onchg = HTML(paste(sep = ";\n",
                     "{",
                     "var wdg = document.getElementById(this.getAttribute('widgetId'))",
                     "if(wdg == null) return null",
                     "var rgl = wdg.rglinstance",
                     "if(rgl == null) return null",
                     "var ids = this.getAttribute('objIds')",
                     "if(ids == null) return null",
                     "ids = ids.split(',')",
                     "if(ids[0] === '') return null", # should handle null value but remains to be checked
                     "for(var obj, id, i = 0; i < ids.length; i++) {",
                     "id = parseInt(ids[i])",
                     "obj = rgl.scene.objects[id]",
                     "if(obj == null) continue",
                     "if(this.checked) {",
                     "rgl.addToSubscene(id, rgl.subsceneid)",
                     "} else {",
                     "rgl.delFromSubscene(id, rgl.subsceneid)",
                     "}",
                     "}",
                     "return rgl.drawScene()",
                     "}"))
  chk = tags$input(id = inputId, type = "checkbox", onchange = onchg, widgetId = widgetId, objIds = objIds, class = "rgl_toggle")
  if(!is.null(value) && value) chk$attribs$checked <- "checked"
  args = c(dots, list(tags$div(class = "checkbox",tags$label(chk,tags$span(label)))))
  do.call(what = tags$div, args = args)
}

# Update a rgl toggle checkbox
updateToggle3D <- function(session = shiny::getDefaultReactiveDomain(),
                           inputId,
                           label = NULL,
                           value = NULL,
                           widgetId = NULL,
                           objIds = NULL) {
  message <- list(eleId = inputId, label = label, value = value, widgetId = widgetId, objIds = unname(unlist(objIds)))
  message <- message[sapply(message, length) != 0]
  session$sendCustomMessage("updateToggle3D", message)
}

# function to clear plot3D; it removes javascript from scene and then run clear3d()
my_clear3d <- function() {
  shinyjs::runjs(code = "var wdg = document.getElementById('plot_3D');
                  if(wdg != null) {
                    var rgl = wdg.rglinstance;
                    if(rgl != null) {
                      delete rgl.scene.javascript;
                    }
                  }")
  rgl::clear3d()
}

#' @title create_obj3D
#' @description
#' Function to create a 3 dimensional object for further plotting with plot_obj3D()
#' @param data a data.frame
#' @param dims columns from 'data' used to generate 3D object.
#' Integer or Character vector of columns names from 'data'. Default is 1:3.
#' if length(dims) < 3, 1st element will be repeated to fill missing dimension(s).
#' @param xlab,ylab,zlab names of the x, y , z axis respectively. Default is "x", "y" and "z".
#' @param clust a vector specifying clusterization of 'data'.
#' Default is missing, in such a case if 'color' is also missing "Gray".
#' @param symbol symbol used for pch for each unique value of 'clust'. Default is  c(20, 4, 3, 1, 5, 0, 2, 18, 15, 17).
#' @param color a vector of color for each unique value of 'clust'.
#' Default is to use, IFC::paletteIFC(x = "palette_R") will be used.
#' @param level parameter to pass to rgl::ellipse3d(). Default is 0.68.
#' @expand coeeficient to apply to expand the bounding box of the data. Default is 1.1
#' @param pt_size desired point size. Default is 10.
#' @param ... other arguments to be passed.
#' @export
create_obj3D = function(data, dims = 1:3,
                        xlab ="x", ylab = "y", zlab = "z",
                        clust, symbol = c(20, 4, 3, 1, 5, 0, 2, 18, 15, 17), color = IFC::paletteIFC(x = "palette_R"),
                        level = 0.68, expand = 1.1, pt_size = 10, ...) {
  if(missing(data)) stop("'data' can't be missing")
  data = as.data.frame(data)
  ids = data[, "Object Number"]
  if(missing(clust)) clust = factor(rep("unk", nrow(data), label = "unk", levels = "unk"))
  if(!is.factor(clust)) clust = as.factor(clust)
  col = as.character(clust)
  pch = as.character(clust)
  
  if(!identical(sort(levels(clust)), sort(unique(names(color))))) {
    if(nlevels(clust) == length(unique(color))) {
      names(color) <- levels(clust)
    } else {
      color = rep_len(color, length.out = nlevels(clust))
      names(color) <- levels(clust)
    }
  }
  for(i in levels(clust)) {
    col[clust == i] <- color[i]
  }
  col[is.na(col)] <- "Grey"
  
  if(!identical(sort(levels(clust)), sort(unique(names(symbol))))) {
    if(nlevels(clust) == length(unique(symbol))) {
      names(symbol) <- levels(clust)
    } else {
      symbol = rep_len(symbol, length.out = nlevels(clust))
      names(symbol) <- levels(clust)
    }
  }
  for(i in levels(clust)) {
    pch[clust == i] <- symbol[i]
  }
  pch[is.na(pch)] <- 20
  
  SUB <- !is.na(clust)
  dims = as.character(dims)
  if(anyNA(suppressWarnings(as.integer(dims)))) {
    dims_in = lapply(dims, FUN=function(x) grep(paste0("^",x,"$"), names(data)))
    tmp = sapply(dims_in, length) == 0
    if(any(tmp)) stop(paste0("Can't find 'dims' [",paste0(dims[tmp], collapse = ","),"] in 'data' names"))
    dims = sapply(dims_in, FUN=function(x) x[1])
  } else {
    dims = as.integer(dims)
    tmp = dims %in% (1:ncol(data))
    if(!all(tmp)) stop(paste0("Can't find 'dims' [",paste0(dims[!tmp], collapse = ","),"] in 'data' names"))
  }
  while(length(dims) < 3) {
    dims = c(dims, dims[1])
  }
  data = cbind(data[, dims], "Object Number" = ids)
  
  xlim3d = range(data[,1], na.rm = TRUE)
  ylim3d = range(data[,2], na.rm = TRUE)
  zlim3d = range(data[,3], na.rm = TRUE)
  ran3d = list(x=xlim3d, y=ylim3d, z=zlim3d)
  radius = max(sapply(ran3d, diff))
  ran3d = c(ran3d, list(radius=radius / 100, size=0, col = "white", lit=FALSE))
  pts3d = by(cbind(data[SUB, 1:4], clust[SUB]), clust[SUB], FUN=function(d)
    list(x=d[,1], y=d[,2], z=d[,3], id=d[,4], pch = symbol[levels(clust)==unique(d[, 5])], color=color[levels(clust)==unique(d[, 5])], lit=FALSE))
  dim_cov = ((by(data[SUB, 1:3], clust[SUB], FUN=function(x) cov(x))))
  dim_centers = do.call(what = "cbind", args = lapply(1:3, FUN=function(i) {by(data[SUB,i], clust[SUB], FUN=function(x) {median(x, na.rm = TRUE)})}))
  ell3d = suppressWarnings(lapply(names(dim_cov), FUN=function(i) return(list(x = dim_cov[[i]], centre=dim_centers[i,], smooth=FALSE, level=level, col=color[levels(clust)==i]))))
  txt3d = lapply(names(dim_cov), FUN=function(i) return(list(x=dim_centers[i,1],y=dim_centers[i,2],z=dim_centers[i,3], texts=i, col=color[levels(clust)==i], cex = 1, adj=c(0.5,0.5), lit=FALSE, forceClipregion=FALSE)))
  ans = list(data = data, color = color, clust = clust, radius = radius, pt_size = pt_size,
             pts3d = pts3d, ell3d = ell3d, txt3d = txt3d, ran3d = ran3d,
             xlim3d = xlim3d, ylim3d = ylim3d, zlim3d = zlim3d,
             xlab=xlab, ylab=ylab, zlab=zlab,
             expand = expand)
  attr(ans, "class") <- "obj3D"
  return(ans)
}

#' @title plot_axis3D
#' @description
#' Function to create axis in a 3D graph
#' @param obj3D a 3D object as created by create_obj3D(). Default is missing.
#' @param which which axis to create. Default is c("x", "y", "z").
#' @param box whether to create a box or not. Default is TRUE.
#' @param label whether to draw axis name
#' @param line_color color of axes / box lines. Default is "black".
#' @param label_color color of axes labels. Default is "black".
#' @param tick_color color of axes ticks. Default is "black".
#' @param ... other arguments to be passed.
#' @export
plot_axis3D <- function(obj3D, which = c("x", "y", "z"), box = TRUE,
                        label = TRUE,  expand = TRUE,
                        line_color = "black", label_color = "black", tick_color = "black", ...) {
  if(missing(obj3D)) stop("'obj3D' can't be missing")
  if(class(obj3D) != "obj3D") stop("'obj3D' is not of class 'obj3D'")
  lim = list(x = obj3D$xlim3d,
             y = obj3D$ylim3d,
             z = obj3D$zlim3d)
  if(expand) lim = sapply(lim, simplify = FALSE, USE.NAMES = TRUE, FUN = function(x) x + (obj3D$expand - 1) * diff(x) / 2 * c(-1,1))
  pos = c(lim$x[1], lim$y[1], lim$z[1])
  foo = sapply(which, simplify = FALSE, USE.NAMES = TRUE, FUN = function(axe) {
    tck = axis3d(edge = axe, pos = pos, col = tick_color, lit = FALSE)
    args = list(x = lim$x[1], y = lim$y[1], z = lim$z[1], col = line_color, lit = FALSE)
    args[[axe]] = c(args[[axe]], lim[[axe]][2])
    line_in = do.call(what = rgl.lines, args = args)
    edges = list(c(2,2,2),
                 c(1,2,2),
                 c(2,1,2),
                 c(2,2,1))
    if(box) {
      line_out = lapply(edges, FUN = function(edg) {
        args = list(x = lim$x[edg[1]],
                    y = lim$y[edg[2]],
                    z = lim$z[edg[3]], col = line_color, lit = FALSE)
        args[[axe]] = lim[[axe]]
        return(do.call(what = rgl.lines, args = args))
      })
    } else {
      line_out = list()
    }
    if(label) {
      lab = mtext3d(text = obj3D[[paste0(axe, "lab")]], edge = axe, pos = pos,
                    col = label_color, line = 1.5, lit = FALSE)
    } else {
      lab = list()
    }
    return(list(tck = tck, line_in = line_in, line_out = line_out, lab =lab))
  })
  return(list(tck = sapply(foo, simplify = FALSE, USE.NAMES = TRUE, FUN = function(x) x$tck),
              lin = unlist(c(lapply(foo, FUN = function(x) x$line_in), lapply(foo, FUN = function(x) x$line_out))),
              lab = sapply(foo, simplify = FALSE, USE.NAMES = TRUE, FUN = function(x) x$lab)))
}

#' @title plot_obj3D
#' @description
#' Function to visualize a 3D object constructed by create_obj3D() using \pkg{rgl}.
#' @param obj3D a 3D object as created by create_obj3D(). Default is missing.
#' @param scaling scaling factor. Default is 1.7
#' @param draw_pts whether to draw axis. Default is TRUE.
#' @param draw_pts whether to draw points. Default is TRUE.
#' @param draw_ell whether to draw ellipses. Default is FALSE.
#' @param draw_txt whether to draw text labels. Default is FALSE.
#' @param useNULL whether to use the null graphics device. Default is TRUE.
#' @param ... other arguments to be passed.
#' @export
plot_obj3D = function(obj3D, scaling = 1.7,
                      draw_axs = TRUE, draw_pts = TRUE, draw_ell = FALSE, draw_txt = FALSE,
                      useNULL = TRUE, ...) {
  if(missing(obj3D)) stop("'obj3D' can't be missing")
  if(class(obj3D) != "obj3D") stop("'obj3D' is not of class 'obj3D'")
  
  obj3D$scaling = scaling
  dots = list(...)
  devs = rgl.dev.list()
  dev = integer()
  if(useNULL) {
    dev = devs[names(devs) == "null"]
  } else {
    dev = devs[names(devs) == "wgl"]
  }
  if(length(dev) == 0) open3d(useNULL = useNULL)
  dev = dev[length(dev)]
  rgl.set(dev, silent = TRUE)
  clear3d()
  
  # create almost invisible spheres at each vertex of the 3D plot
  do.call(what = points3d, args = obj3D$ran3d)
  # set plot a cube
  asp = aspect3d(1,1,1)[[1]]
  names(asp) = c("x", "y", "z")
  view3d(theta = 0, phi = 0, fov = 60, zoom = 1/1.7)
  
  # change and record observer
  # INFO
  viewdepth = sum(sort(sapply(obj3D$ran3d[c("x", "y", "z")], diff), decreasing = TRUE) * c(1, 0.5, 0.25))
  viewdepth_max = scaling * sum(rep(obj3D$radius, 3) * c(1, 0.5, 0.25))
  obj3D$zoom = viewdepth/viewdepth_max
  obj3D$pt_ratio = viewdepth/1000
  
  # create axes
  axes = list()
  if(draw_axs) axes = do.call(what = plot_axis3D, args = list(obj3D = obj3D, box = TRUE))
  
  # create points
  pts = list()
  if(draw_pts) pts = lapply(obj3D$pts3d, FUN=function(i) { try(do.call(what = pch3d, args = c(list(radius = 4 * obj3D$pt_size * obj3D$pt_ratio), i[setdiff(names(i), "id")])), silent = TRUE) })
  
  # create ellipse
  ell = list()
  if(draw_ell) ell = suppressWarnings(lapply(obj3D$ell3d, FUN=function(i) {
    foo = try(do.call(what = ellipse3d, args = i), silent = TRUE)
    if(inherits(x = foo, what = "try-error")) return(NULL)
    return(shade3d(do.call(what = ellipse3d, args = i), alpha = 0.3, lit = FALSE))
  }))
  
  # create txt
  txt = list()
  if(draw_txt) txt = lapply(obj3D$txt3d, FUN=function(i) { do.call(what = text3d, args = i) })
  
  xx = obj3D$ran3d$x[1] + diff(obj3D$ran3d$x)/2
  yy = obj3D$ran3d$y[1] + diff(obj3D$ran3d$y)/2
  zz = obj3D$ran3d$z[1] + diff(obj3D$ran3d$z)/2
  hid = list()
  hid = text3d(x = xx, y = yy, z = zz, texts = "Creating 3D", alpha = 0, lit = FALSE)
  
  # creates hover for use with setUserCallbacks
  hov = list()
  if(packageVersion("rgl") >= '0.106.19') {
    hov$pts = pch3d(x=xx, y=yy, z=zz,
                    polygon_offset = 0, radius=4 * obj3D$pt_size * obj3D$pt_ratio,
                    pch = 13, color = "#F5AA2200", alpha=1, lit=FALSE)
    hov$txt = text3d(x=xx, y=yy, z=zz, texts = "-1",
                     offset = 1, pos = 4,
                     alpha = 0, lit = FALSE)
    hov$img = sprites3d(x=xx, y=yy, z=zz,
                        polygon_offset = 1, fixedSize = TRUE, radius = scaling,
                        color = "grey", alpha = 0, lit = FALSE,
                        textype = "rgb", texture = system.file(package = "rgl", "textures", "rgl2.png"))
    
    rglbegin <- local({function(x, y) {}})
    rglupdate <- local({function(x, y) {}})
    rglend <- local({function(x, y) {}})
    
    js <- sprintf(' var idverts = %s, idtext = %i, idimg = %i, idpts = %i, closest = ["",""], subid = %i;
     window.rglbegin = function(x, y) {
       return; 
     }
     window.rglupdate = function(x, y) {
       var obj, i, newdist, dist = Infinity, pt, pt_adj, newclosest, objid = - 1;
       var subscene = this.getObj(subid);
       if(Object.values(subscene.par3d.mouseMode).indexOf("selecting") < 0) {
       x = x/this.canvas.width;
       y = y/this.canvas.height;
       for(var [key, value] of Object.entries(idverts)) {
         obj = this.getObj(parseInt(value));
         //if(obj != null)
         for(i = 0; i < obj.vertices.length; i++) {
           pt = obj.vertices[i].concat(1);
           pt = this.user2window(pt, subid);
           pt[0] = x - pt[0];
           pt[1] = y - pt[1];
           pt[2] = 0;
           newdist = rglwidgetClass.vlen(pt);
           if (newdist < dist) {
             dist = newdist;
             newclosest = [key, i + ""];
             objid = parseInt(value);
           }
         }
       }
       if((objid > 0) && ((newclosest[0] !== closest[0]) || (newclosest[1] !== closest[1]))) {
         closest = newclosest
         var txt = this.getObj(idtext);
         var img = this.getObj(idimg);
         var pts = this.getObj(idpts);
         obj = this.getObj(objid);
         pt = obj.vertices[closest[1]];

         // convert to normalized coords
         var v = pt.concat(1);
         var w = rglwidgetClass.multVM(v, this.prmvMatrix);
         var xx = w[0]/w[3], yy = w[1]/w[3]
         //var zz = w[2]/w[3];

         // convert to pixel unit
         xx = 0.5 * this.vp.width  * (xx + 1) + this.vp.x;
         yy = 0.5 * this.vp.height * (1 - yy) - this.vp.y;
         //zz = (1 + zz) * 0.5;

         // offset x and y
         xx = xx + 25;
         yy = yy + 30;
         //zz = zz;

         // convert back to normalized coords
         xx = (xx - this.vp.x) * 2 / this.vp.width  - 1;
         yy = 1 - (yy + this.vp.y) * 2 / this.vp.height;
         //zz = zz * 2 - 1;

         xx = xx * w[3];
         yy = yy * w[3];
         //zz = zz * w[3];

         // convert to data unit
         this.prmvMatrix.invert();
         pt_adj = rglwidgetClass.multVM([xx, yy, w[2], w[3]], this.prmvMatrix);

         pts.vertices[0] = txt.vertices[0] = pt;
         img.vertices[0] = pt_adj.slice(0, 3);
         this.prmvMatrix.invert();
         pts.colors[0][3] = img.colors[0][3] = txt.colors[0][3] = 1;
         pts.initialized = false;
         
         Shiny.onInputChange("rgl_3D_hover", { closest:newclosest, x:x, y:y, coords: { ori:pts.vertices[0], translated:pt_adj }, hover:{text:idtext, image:idimg, point:idpts }} );
       }
       }
     };
     window.rglend = function() {
       var txt = this.getObj(idtext);
       var img = this.getObj(idimg);
       var pts = this.getObj(idpts);
       pts.colors[0][3] = img.colors[0][3] = txt.colors[0][3] = 0;
       pts.initialized = img.initialized = txt.initialized = false;
       this.drawScene();
     }', toJSON(pts), hov$txt, hov$img, hov$pts, subsceneInfo()$id)
    
    scene <- scene3d(minimal = FALSE)
    if("javascript" %in% names(scene)) scene$javascript <- ""
    setUserCallbacks(0, begin = "rglbegin", update = "rglupdate", end = "rglend", javascript = js, scene = scene, applyToDev = TRUE)
  }
  view3d(theta = 0, phi = 0, fov = 60, zoom = obj3D$zoom)
  observer3d(0, 0, viewdepth_max, auto = FALSE)
  clear3d(type = c("lights","bboxdeco","background","material"))
  return(invisible(list(obj3D = obj3D, axes = axes, pts = pts, ell = ell, txt = txt, hov = hov, hid = hid, dev = dev)))
}

#' @title plotly_obj3D
#' @description
#' Function to visualize a 3D object constructed by create_obj3D() using \pkg{plotly}.
#' @param obj3D an obj3D object created by create_obj3D().
#' @param xlab,ylab,zlab names of the x, y , z axis respectively. Default is missing.
#' If not missing it will not use value stored in obj3D$xlab, obj3D$ylab, obj3D$zlab.
#' @param fileName path to file containing images. Default is NULL.
#' @param selection channels to embed when 'fileName' is provided. Default is 4.
#' @param force_width force_width parameter of IFC::objectParam(). Default is FALSE.
#' @param size size parameter of IFC::objectParam(). Default is c(0,0)
#' @param write_to write_to parameter of IFC::objectParam(). Default is "image.png",
#' @param ... other arguments to be passed to IFC::objectParam().
#' @export
plotly_obj3D <- function(obj3D, xlab, ylab, zlab, fileName = NULL,
                         selection = 4, force_width = FALSE, size = c(0,0), write_to = "image.png", ...) {
  dots = list(...)
  dat = do.call(what = rbind, args = c(list(make.row.names = FALSE), lapply(names(obj3D$pts3d), FUN = function(pop) {
    p = obj3D$pts3d[[pop]]
    data.frame(x = p$x, y = p$y, z = p$z, id = p$id, text = rep(pop, length(p$x)))
  })))
  col = unlist(lapply(obj3D$pts3d, FUN = function(p) p$color))
  names(col) = names(obj3D$pts3d)
  pch = unlist(lapply(obj3D$pts3d, FUN = function(p) p$pch))
  names(pch) = names(obj3D$pts3d)
  plotly_args = list(data = dat, x = ~x, y = ~y, z = ~z,
                     customdata = ~id, text = ~text,
                     color = ~text, colors = col, 
                     symbol = ~text, symbols = pch)
  if(length(fileName) != 0) {
    param_extra = names(dots) %in% c("objects", "base64_att")
    if("objects" %in% names(dots)) warning("'objects' from input will not be used", call. = FALSE, immediate. = TRUE)
    if("base64_att" %in% names(dots)) warning("'base64_att' from input will not be used", call. = FALSE, immediate. = TRUE)
    dots = dots[!param_extra]
    if(selection %in% c("all", "none")) stop("'selection' can not be set to \"all\" nor \"none\"")
    if(length(selection) != 1) stop("'selection' should be of length 1")
    dat$meta = unlist(do.call(what = ExtractImages_toBase64,
                              args = c(dots, list(fileName = fileName, objects = as.integer(dat$id),
                                                  selection = selection, force_width = FALSE, size = c(0,0), write_to = "image.png",
                                                  base64_att = "style='position:absolute; top:5px; left:5px; z-index:100;' class='IFC_displayed_cell'"))))
    plotly_args = c(plotly_args, list(meta = dat$meta))
  }
  fig <- do.call(what = plotly::plot_ly, args = plotly_args)
  fig <- fig %>% plotly::add_markers(hovertemplate = "%{text}<extra>%{customdata}</extra>")
  xlab <- ifelse(missing(xlab), obj3D$xlab, xlab)
  ylab <- ifelse(missing(ylab), obj3D$ylab, ylab)
  zlab <- ifelse(missing(zlab), obj3D$zlab, zlab)
  fig <- fig %>% plotly::layout(scene = list(xaxis = list(title = xlab),
                                             yaxis = list(title = ylab),
                                             zaxis = list(title = zlab)))
  if("meta" %in% names(dat)) {
    fig <- fig %>% htmlwidgets::onRender("
    function(el, x) {
      el.on('plotly_hover', function(d) {
        var to_remove = el.querySelector('.IFC_displayed_cell');
        if(to_remove != null) to_remove.remove();
        el.insertAdjacentHTML('beforeend', d.points[0].meta);
      });
      el.on('plotly_unhover', function(d) {
        var to_remove = el.querySelector('.IFC_displayed_cell');
        if(to_remove != null) to_remove.remove();
      });
    }")
  }
  suppressWarnings(fig)
}