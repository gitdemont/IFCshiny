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

output$infos_save_btn <- downloadHandler(
  filename = function() {
    if(length(react_dat()$info$in_use) == 0) {
      ans = "infos.txt"
    } else {
      ans = paste0(remove_ext(basename(react_dat()$fileName)), "_infos.txt")
    } 
    ans = specialr(ans)
    ans
  },
  content = function(file) {
    sink(file = file)
    mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", msg = "Exporting file information", reset = FALSE)
    tryCatch({
      if(length(react_dat()$info$in_use) == 0) {
        print("")
      } else {
        cat(sep = "\n",
            paste0("input: ", input$file$name),
            paste0("date: " , react_dat()$info$date),
            paste0("instrument: " , react_dat()$info$instrument),
            paste0("sw_raw: " , react_dat()$info$sw_raw),
            paste0("sw_process: " , react_dat()$info$sw_process),
            paste0("fileName: ", react_dat()$fileName),
            paste0("fileName_image: ", react_dat()$fileName_image),
            "\nMerged_rif: ", react_dat()$info$Merged_rif,
            "\nMerged_cif: ", react_dat()$info$Merged_cif,
            paste0("\nobjcount: ", react_dat()$info$objcount),
            paste0("magnification: ", react_dat()$info$magnification),
            paste0("high-gain: ", react_dat()$info$evmode),
            paste0("in_use: ", paste0(sprintf("%0.02i", which(react_dat()$info$in_use)), collapse = ",")),
            paste0("brightfield: ", ifelse(react_dat()$info$found, paste0(sprintf("%0.02i", which(react_dat()$info$brightfield$channel)), collapse = ","), react_dat()$info$brightfield)),
            "\nIllumination:")
        print(react_dat()$info$illumination)
        cat("\nCompensation:\n")
        print(react_dat()$info$CrossTalkMatrix)
        cat("\nDisplay:\n")
        tmp = grepl("^name$|^color$|^physicalChannel$|^xmin$|^xmax$|^xmid$|^ymid$|^scalemin^|^scalemax$", colnames(param_react$param$channels))
        print(param_react$param$channels[, tmp])
        cat("\nMasks:\n")
        print(react_dat()$description$masks[, c("name","def")])
      }
    },
    error = function(e) {
      mess_global("exporting info", msg = e$message, type = "error", duration = 10)
    },
    finally = {
      sink()
      mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", reset = TRUE)
    })
  })
output$ML_save_btn <- downloadHandler(
  filename = function() {
    ans = paste0(remove_ext(basename(react_dat()$fileName)), "_", model_react$name, ".", input$ML_save_type)
    ans = specialr(ans)
    ans
  },
  content = function(file) {
    mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", msg = "Exporting DAF with model", reset = FALSE)
    tmpfile = file.path(session_react$dir, basename(file))
    tryCatch({
      if(!model_react$success) stop("please re-run model")
      
      # retrieve features used for modeling
      feat = grep("^Object Number$|^clust$", names(model_react$train), invert = TRUE, value = TRUE)
      
      pred = NULL
      sub1 = !apply(model_react$data[, feat], 1, anyNA)
      clust = rep_len(NA, length.out = nrow(obj_react$obj$features))
      switch(model_react$name, 
             "em" = {
               pred = clust
               pred[sub1] = as.character(predict(object = model_react$fit, newdata = model_react$data[sub1, feat])$classification)
             },
             "svm" = {
               pred = clust
               pred[sub1] = as.character(predict(object = model_react$fit, newdata = model_react$data[sub1, feat]))
             },
             "xgb" = {
               prob = predict(model_react$fit, newdata = as.matrix(model_react$data[sub1, feat]), reshape = TRUE)
               pred = clust
               pred[sub1] = as.character(factor(levels(model_react$data$clust)[apply(prob, 1, which.max)], levels = unique(model_react$data$clust)))
             },
             "lda" = {
               proj_lda = predict(object = model_react$fit, newdata = model_react$data[sub1, feat])
               pred = clust
               pred[sub1] = as.character(proj_lda$clas)
             },
             "flowsom" = {
               proj_som = FlowSOM::NewData(model_react$fit, new(structure("flowFrame", package="flowCore"), exprs = as.matrix(model_react$data[sub1, feat])),
                                  spillover = FALSE, transform = FALSE, scale = FALSE)
               if(nlevels(model_react$train$clust) > 1) {
                 map = by(model_react$fit$map$mapping[, 1], model_react$train$clust, FUN = function(x) {
                   proj_som$map$mapping[, 1] %in% x
                 })
                 map = sapply(map, FUN = function(x) x)
                 N = colnames(map)
                 pred = clust
                 pred[sub1] = apply(map, 1, FUN = function(x) {
                   foo = which(x)
                   if(length(foo) == 1) return(N[foo])
                   return(NA)
                 })
               } 
             })
      
      pops = list(buildPopulation(name = "ML_subset", type = "C",
                                  definition = paste0(model_react$pops, collapse = "|Or|"),
                                  color = "White", lightModeColor = "Black"))
      if(length(pred) != 0) {
        if(all(setdiff(na.omit(unique(pred)), "") %in% as.character(levels(model_react$train$clust)))) {
          pops = c(pops, lapply(levels(model_react$train$clust), FUN = function(i_pop) {
            buildPopulation(name = paste0("ML_", model_react$name, "_", i_pop),
                            type = "T", 
                            lightModeColor = obj_react$obj$pops[[i_pop]]$lightModeColor,
                            color = obj_react$obj$pops[[i_pop]]$color,
                            obj = !is.na(pred) & pred == i_pop)
          }),
          lapply(levels(model_react$train$clust), FUN = function(i_pop) {
            buildPopulation(name = paste0("ML_train_", model_react$name, "_", i_pop),
                            type = "T",
                            lightModeColor = obj_react$obj$pops[[i_pop]]$lightModeColor,
                            color = obj_react$obj$pops[[i_pop]]$color,
                            obj = model_react$data[, "Object Number"] %in% model_react$train[model_react$train$clust == i_pop, "Object Number"])
          }),
          lapply(levels(model_react$train$clust), FUN = function(i_pop) {
            buildPopulation(name = paste0("ML_test_", model_react$name, "_", i_pop),
                            type = "T",
                            lightModeColor = obj_react$obj$pops[[i_pop]]$lightModeColor,
                            color = obj_react$obj$pops[[i_pop]]$color,
                            obj = model_react$data[, "Object Number"] %in% model_react$test[model_react$test$clust == i_pop, "Object Number"])
          }),
          lapply(levels(model_react$train$clust), FUN = function(i_pop) {
            buildPopulation(name = paste0("ML_mismatch_", model_react$name, "_predicted_", i_pop),
                            type = "T",
                            lightModeColor = obj_react$obj$pops[[i_pop]]$lightModeColor,
                            color = obj_react$obj$pops[[i_pop]]$color,
                            obj = !is.na(pred) & pred == i_pop &
                              (model_react$data[, "Object Number"] %in% model_react$test[model_react$test$clust != i_pop, "Object Number"]))
          }))
        } 
      }
      
      # add image values in obj_react (if not already extracted)
      if((length(react_dat()$description$FCS) == 0) && (getFileExt(react_dat()$fileName) != "daf") && (length(obj_react$obj$images) == 0)) {
        obj_react$obj$images <- getImagesValues(fileName = react_dat()$fileName, offsets = obj_react$obj$offsets,
                                                display_progress = TRUE, fast = TRUE, session = session)
      }
      
      foo = obj_react$obj
      if(length(param_react$param$channels) !=0) {
        foo$description$Images <- param_react$param$channels
      }
      tryCatch({
        # add temporary ML pops in obj_react
        foo <- suppressWarnings(data_add_pops(foo, pops, display_progress = TRUE, session = session))
        switch(input$ML_save_type, 
               "daf" = {
                 suppressWarnings(data_to_DAF(obj = foo, write_to = tmpfile, overwrite = TRUE,
                                              fullname = !input$use_example, viewing_pop = "ML_subset", binary = TRUE, 
                                              display_progress = TRUE, session = session))
                 if(!file.rename(from = tmpfile, to = file)) file.copy(from = tmpfile, to = file)
               },
               "fcs" = {
                 ExportToFCS(obj = foo, write_to = tmpfile, overwrite = TRUE, delimiter = "/", "$SPILLOVER" = convert_spillover(comp_react$spillover))
                 if(!file.rename(from = tmpfile, to = file)) file.copy(from = tmpfile, to = file)
               },
               "zip" = {
                 tmpdr = session_react$dir
                 short = short_name(react_dat()$fileName)
                 if(length(react_dat()$description$FCS) == 0) {
                   files = suppressWarnings(data_to_DAF(obj = foo, 
                                                        write_to = file.path(tmpdr, paste0("%s_", model_react$name,".%e")),
                                                        overwrite = TRUE,
                                                        fullname = !input$use_example, viewing_pop = "ML_subset", binary = TRUE, 
                                                        display_progress = TRUE, session = session))
                 } else {
                   files = ExportToFCS(obj = foo, write_to = file.path(tmpdr, paste0(short, "_", model_react$name,".fcs")), overwrite = TRUE, delimiter = "/", "$SPILLOVER" = convert_spillover(comp_react$spillover))
                   files = c(files, writeGatingStrategy(obj = foo, write_to = file.path(tmpdr, paste0(short, "_", model_react$name,".xml")), overwrite = TRUE, display_progress = TRUE, session = session))
                 }
                 model = list()
                 for(i in names(model_react)) model[[i]] = model_react[[i]]
                 saveRDS(model, file = file.path(tmpdr, paste0(short, "_", model_react$name,".rds")))
                 files = c(files, file.path(tmpdr, paste0(short, "_", model_react$name,".rds")))
                 to_rm = files
                 files = short_path(files, tmpdr)
                 olddir = getwd()
                 setwd(tmpdr)
                 pb = newPB(session = session, title = basename(react_dat()$fileName), label = "zipping files", min = 0, max = length(files))
                 tryCatch({
                   suppressMessages(zip::zip(zipfile = file, files = files[1], compression_level = 0, recurse = TRUE))
                   if(length(files) > 1) sapply(2:length(files), FUN = function(i_file) {
                     setPB(pb, value = i_file, title = basename(react_dat()$fileName), label = "zipping files")
                     suppressMessages(zip::zip_append(zipfile = file, files = files[i_file], compression_level = 0, recurse = TRUE))
                   })
                 },
                 error = function(e) {
                   stop(e$message, call. = FALSE)
                 },
                 finally = {
                   endPB(pb)
                   setwd(olddir)
                   file.remove(to_rm)
                 })
               })
      }, error = function(e) {
        stop(e$message)
      }, finally = {
        
      })
    },
    error = function(e) {
      mess_global("exporting ML", msg = e$message, type = "error", duration = 10)
    },
    finally = {
      mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", reset = TRUE)
    })
  })
output$comp_save_btn <- downloadHandler(
  filename = function() {
    ans = specialr(paste0(remove_ext(basename(react_dat()$fileName)), "-comp.", input$comp_save_type))
    ans
  },
  content = function(file) {
    mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", "Exporting compensation matrix", reset = FALSE)
    tmpfile = file.path(session_react$dir, basename(file))
    tryCatch({
      info = react_dat()$info
      control_args = list()
      if(info$XIF_test == 0) control_args = lapply(info$Merged_rif, FUN = function(f) xml_new_node(name = "control", attrs = list(name = f)))
      control_args = c(control_args, list(xml_new_node(name = "mergedcontrol", attrs = list(name = ""))))
      
      foo = comp_react$spillover
      max_channels = max(which(info$in_use))
      n_cam = ifelse(max_channels <= 6, 1, 2)
      Mat = sapply(1:(n_cam*6), FUN = function(i_col) {
        tmp = i_col == info$Images$physicalChannel
        bar = rep(0, 6 * n_cam)
        if(any(tmp)) {
          bar[info$Images$physicalChannel] <- foo[, tmp]
        } else {
          bar[i_col] <- 1
        }
        return(bar)
      })
      colnames(Mat) <- sprintf("Ch%02i", 1:(n_cam*6))
      
      switch(input$comp_save_type, 
             "txt" = write.table(x = Mat, file = tmpfile, append = FALSE, row.names = FALSE, col.names = colnames(Mat), na = "", sep = "\t", dec = "."),
             "csv" = write.table(x = Mat, file = tmpfile, append = FALSE, row.names = FALSE, col.names = colnames(Mat), na = "", sep = ",", dec = "."),
             "xlsx" = write.xlsx(x = Mat, file = tmpfile),
             "ctm" = {
               MeanMat = matrix(0, ncol(Mat), nrow(Mat))
               diag(MeanMat) <- 1
               
               doc = xml_new_node(name = "Matrix", .children= list(
                 xml_new_node(name = "ControlFiles", .children = control_args),
                 xml_new_node(name = "MatrixValues", attrs = list(scatter = c("-1", "0")[1], 
                                                                  values = paste0(t(Mat), collapse = "|"),
                                                                  slopes = paste0(rep(0, ncol(Mat) * nrow(Mat)), collapse = "|"),
                                                                  xintercepts = paste0(rep(0, ncol(Mat) * nrow(Mat)), collapse = "|"), 
                                                                  yintercepts = paste0(rep(0, ncol(Mat) * nrow(Mat)), collapse = "|"),
                                                                  errors = paste0(rep(0, ncol(Mat) * nrow(Mat)), collapse = "|"))),
                 xml_new_node(name = "MeanMatrixValues", attrs = list(values = paste0(MeanMat, collapse = "|"))),
                 xml_new_node(name = "CalculationType", attrs = list(type = "Best fit"))))
               xml2::write_xml(x = doc, file = tmpfile)
             }
      )
      if(!file.rename(from = tmpfile, to = file)) file.copy(from = tmpfile, to = file)
    },
    error = function(e) {
      mess_global(title = "exporting compensation", msg = e$message, type = "error", duration = 10)
    },
    finally = {
      mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", reset = TRUE)
    })
  })
output$daf_save_btn <- downloadHandler(
  filename = function() {
    ans = paste0(remove_ext(basename(react_dat()$fileName)), ".", input$daf_save_type)
    ans = specialr(ans)
    ans
  },
  content = function(file) {
    mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", msg = paste0("Exporting ", ifelse(length(react_dat()$description$FCS) == 0, "DAF", "FCS")), reset = FALSE)
    tmpfile = file.path(session_react$dir, basename(file))
    tryCatch({
      # add image values in obj_react (if not already extracted)
      if((length(react_dat()$description$FCS) == 0) && (getFileExt(react_dat()$fileName) != "daf") && (length(obj_react$obj$images) == 0)) {
        obj_react$obj$images <- getImagesValues(fileName = react_dat()$fileName, offsets = obj_react$obj$offsets,
                                                display_progress = TRUE, fast = TRUE, session = session)
      }
      
      foo = obj_react$obj
      if(length(param_react$param$channels) !=0) {
        foo$description$Images <- param_react$param$channels
      }
      tryCatch({
        switch(input$daf_save_type, 
               "daf" = {
                 suppressWarnings(data_to_DAF(obj = foo, write_to = tmpfile, overwrite = TRUE,
                                              fullname = !input$use_example, viewing_pop = "ML_subset", binary = TRUE, 
                                              display_progress = TRUE, session = session))
                 if(!file.rename(from = tmpfile, to = file)) file.copy(from = tmpfile, to = file)
               },
               "fcs" = {
                 ExportToFCS(obj = foo, write_to = tmpfile, overwrite = TRUE, delimiter = "/", "$SPILLOVER" = convert_spillover(comp_react$spillover))
                 if(!file.rename(from = tmpfile, to = file)) file.copy(from = tmpfile, to = file)
               },
               "zip" = {
                 files = c(ExportToFCS(obj = foo, write_to = file.path(tmpdr, paste0(short,".fcs")), overwrite = TRUE, delimiter = "/", "$SPILLOVER" = convert_spillover(comp_react$spillover)),
                           writeGatingStrategy(obj = foo, write_to = file.path(tmpdr, paste0(short,".xml")), overwrite = TRUE, display_progress = TRUE, session = session))
                 to_rm = files
                 files = short_path(files, tmpdr)
                 olddir = getwd()
                 setwd(tmpdr)
                 pb = newPB(session = session, title = basename(react_dat()$fileName), label = "zipping files", min = 0, max = length(files))
                 tryCatch({
                   suppressMessages(zip::zip(zipfile = file, files = files[1], compression_level = 0, recurse = TRUE))
                   if(length(files) > 1) sapply(2:length(files), FUN = function(i_file) {
                     setPB(pb, value = i_file, title = basename(react_dat()$fileName), label = "zipping files")
                     suppressMessages(zip::zip_append(zipfile = file, files = files[i_file], compression_level = 0, recurse = TRUE))
                   })
                 },
                 error = function(e) {
                   stop(e$message, call. = FALSE)
                 },
                 finally = {
                   endPB(pb)
                   setwd(olddir)
                   file.remove(to_rm)
                 })
               })
      }, error = function(e) {
        stop(e$message)
      }, finally = {
        
      })
    },
    error = function(e) {
      mess_global(paste0("exporting ",ifelse(length(react_dat()$description$FCS) == 0, "DAF", "FCS")), msg = e$message, type = "error", duration = 10)
    },
    finally = {
      mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", reset = TRUE)
    })
  })
output$graph_save_btn <- downloadHandler(
  # TODO add hovering tool and image export to plot 3D
  filename = function() {
    ans = paste0(remove_ext(basename(react_dat()$fileName)),
                 "_x[",
                 trunc_string(input$plot_x_feature,20),
                 ifelse(input$plot_type=="1D","", paste0("]y[",trunc_string(input$plot_y_feature, 20))))
    ans = paste0(ans, ifelse(input$plot_type=="3D",paste0("]z[",trunc_string(input$plot_y_feature, 20),"]"),"]"))
    if(react_dat()$info$found) ans = ans = paste0(ans, "(Ch",input$plot_3D_draw_chan,")")
    ans = specialr(paste0(ans,".",input$graph_save_type))
    ans
  },
  content = function(file) {
    mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", msg = paste0("Exporting ",input$plot_type," plot"), reset = FALSE)
    tmpfile = file.path(session_react$dir, basename(file))
    if(input$plot_type == "3D") {
      tryCatch({
        imgs = character()
        if(react_dat()$info$found) {
          fileName = react_dat()$fileName_image
        } else {
          fileName = NULL
        }
        saveWidget(widget = plotly_obj3D(obj3D = plot_react$plot$plot$obj3D,
                                         xlab = input$plot_x_feature, ylab = input$plot_y_feature, zlab = input$plot_z_feature,
                                         fileName = fileName,
                                         selection = as.integer(input$plot_3D_draw_chan),
                                         session = session,
                                         offsets = react_dat()$offsets, 
                                         full_range = "full_range" %in% input$chan_force,
                                         force_range = "force_range" %in% input$chan_force),
                   file = tmpfile, selfcontained = TRUE)
        if(!file.rename(from = tmpfile, to = file)) file.copy(from = tmpfile, to = file)
      },
      error = function(e) {
        mess_global(title = "exporting graph", msg = e$message, type = "error", duration = 10)
      },
      finally = {
        mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", reset = TRUE)
      })
    } else {
      pdf(tmpfile, paper = "a4", onefile = TRUE, pagecentre = TRUE, family = "serif", useDingbats = FALSE)
      tryCatch({
        foo = plot_react$plot$plot
        stats = plot_react$plot$stats
        stats = stats[,!grepl("Qu", colnames(stats)),drop=FALSE]
        foo_lay = matrix(c(rep(1,9), c(NA,2,NA)), nrow=4, ncol=3, byrow = TRUE)
        foo$vp <- viewport(x=0.5, y=0.5)
        tab = tableGrob(format(stats, scientific=FALSE, digits=5), theme = ttheme_default(base_size=4, base_family = "serif"))
        tab$vp <- viewport(x=0.5, y=unit(1,"npc") - 0.5*sum(tab$heights))
        grid.arrange(arrangeGrob(foo,
                                 tab,
                                 layout_matrix = foo_lay, respect = TRUE),
                     top=basename(react_dat()$fileName), newpage = TRUE)
      },
      error = function(e) {
        mess_global(title = "exporting graph", msg = e$message, type = "error", duration = 10)
      },
      finally = {
        dev.off(which = dev.cur())
        if(!file.rename(from = tmpfile, to = file)) file.copy(from = tmpfile, to = file)
        mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", reset = TRUE)
      })
    }
  })
output$report_save_btn <- downloadHandler(
  filename = function() {
    ans = specialr(paste0(remove_ext(basename(react_dat()$fileName)), "_report.pdf"))
    ans
  },
  content = function(file) {
    mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", msg = "Exporting graphs", reset = FALSE)
    tmpfile = file.path(session_react$dir, basename(file))
    tryCatch({
      suppressMessages(ExportToReport(obj = obj_react$obj, write_to = tmpfile, onepage = TRUE, display_progress = TRUE, session = session))
      if(!file.rename(from = tmpfile, to = file)) file.copy(from = tmpfile, to = file)
    },
    error = function(e) {
      mess_global(title = "exporting report", msg = e$message, type = "error", duration = 10)
    },
    finally = {
      mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", reset = TRUE)
    })
  })
output$network_save_btn <- downloadHandler(
  filename = function() {
    ans = specialr(paste0(remove_ext(basename(react_dat()$fileName)), "_network.", input$network_save_type))
    ans
  },
  content = function(file) {
    mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", msg = "Exporting population network", reset = FALSE)
    tmpfile = file.path(session_react$dir, basename(file))
    tryCatch({
      switch(input$network_save_type, 
             "html" = { 
               visSave(popsNetwork(obj_react$obj, session = session), file = tmpfile)
             },
             "txt" = {
               sink(file = tmpfile)
               tryCatch({
                 do.call(what = "cat", args = c(lapply(obj_react$obj$pops, FUN = function(p) {
                   typ = c("Base", "Boolean", "Graphical", "Tagged")[c("B", "C", "G", "T") %in% p$type]
                   col = p[c("color", "lightModeColor")]
                   ret = c(p$name, typ) 
                   if (typ == "Boolean") 
                     ret = c(ret, paste0("definition:", paste0(p$split, collapse = " ")))
                   if (typ == "Graphical") {
                     ret = c(ret, obj_react$obj$regions[[p$region]]$type)
                     ret = c(ret, paste0("x: ", p$fx))
                     foo = "x"
                     if (!is.null(p$fy)) {
                       ret = c(ret, paste0("y: ", p$fy))
                       foo = c(foo, "y")
                     }
                     ret = c(ret, 
                             paste0(foo, collapse = "\t"),
                             apply(do.call(cbind, args = obj_react$obj$regions[[p$region]][c(foo)]), 1, FUN = function(r) paste0(r, collapse = "\t")))
                   }
                   ret = paste0(ret, collapse = "\n")
                 }), list(sep = "\n\n")))
               },
               error = function(e) {
                 stop(e$message)
               },
               finally = {
                 sink()
               })
             })
      if(!file.rename(from = tmpfile, to = file)) file.copy(from = tmpfile, to = file)
    },
    error = function(e) {
      mess_global(title = "exporting network", msg = e$message, type = "error", duration = 10)
    },
    finally = {
      mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", reset = TRUE)
    })
  })
output$pop_indices_save_btn <- downloadHandler(
  filename = function() {
    ans = specialr(paste0(remove_ext(basename(react_dat()$fileName)), ifelse(input$pop_indices_save_type == "xml", "_gating.", "_indices."), input$pop_indices_save_type))
    ans
  },
  content = function(file) {
    mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", "Exporting population indices", reset = FALSE)
    tmpfile = file.path(session_react$dir, basename(file))
    tryCatch({
      oo = rep(NA, as.integer(obj_react$obj$description$ID$objcount))
      idx = do.call(what = "cbind", args = lapply(obj_react$obj$pops, FUN = function(p) {
        foo = which(p$obj)
        oo[foo] <- foo - 1
        return(sort(oo, na.last = TRUE))
      }))
      switch(input$pop_indices_save_type, 
             "txt" = write.table(x = idx, file = tmpfile, append = FALSE, row.names = FALSE, col.names = names(obj_react$obj$pops), na = "", sep = "\t", dec = "."),
             "csv" = write.table(x = idx, file = tmpfile, append = FALSE, row.names = FALSE, col.names = names(obj_react$obj$pops), na = "", sep = ",", dec = "."),
             "xlsx" = write.xlsx(x = idx, file = tmpfile),
             "xml" = writeGatingStrategy(obj = obj_react$obj, write_to = tmpfile, overwrite = TRUE, display_progress = TRUE, verbose = FALSE, session = session)
      )
      if(!file.rename(from = tmpfile, to = file)) file.copy(from = tmpfile, to = file)
    },
    error = function(e) {
      mess_global(title = "exporting population", msg = e$message, type = "error", duration = 10)
    },
    finally = {
      mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", reset = TRUE)
    })
  })
output$cells_save_btn <- downloadHandler(
  filename = function() {
    ans = specialr(paste0(remove_ext(basename(react_dat()$fileName)), "_", input$population, ".zip"))
    ans
  },
  content = function(file) {
    mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", msg = "Exporting images", reset = FALSE)
    tryCatch({
      tmpdr = session_react$dir
      param = param_react$param
      param$overwrite = TRUE
      switch(input$cells_save_type,
             "npy" = {
               size = c(48,48)
               msg = c()
               if((length(input$chan_height) == 0) || (input$chan_height == 0) ){
                 msg = c(msg, "Current image height value is 0 and has been automatically set to 48 px for the export.")
               } else {
                 size[1] <- input$chan_height
               }
               if((length(input$chan_width) == 0) || (input$chan_width == 0) ) {
                 msg = c(msg, "Current image width value is 0 and has been automatically set to 48 px for the export.")
               } else {
                 size[2] <- input$chan_width
               }
               if(length(msg) != 0) {
                 mess_global(title = "exporting to numpy", msg = c(msg, "You can change it by going into image options."), type = "info", duration = 10)
               }
               files = unlist(suppressWarnings(ExportToNumpy(display_progress = TRUE, param = param, python = Sys.getenv("PYTHON_PATH"),
                                                             size = size, force_width = FALSE,
                                                             objects = popsGetObjectsIds(obj_react$obj, pop = input$population),
                                                             image_type = "img", export = "file",
                                                             write_to = paste0(tmpdr, "/%s/%s_", input$population, "_Ch[%c].npy"),
                                                             # write_to = file.path(tmpdr, "%s_Ch[%c].npy"),
                                                             mode = "gray", overwrite = TRUE, 
                                                             offsets = react_dat()$offsets, session = session)))
             },
             "xif" = {
               files = unlist(suppressWarnings(ExportToXIF(fileName = param$fileName_image,
                                                           objects = popsGetObjectsIds(obj_react$obj, pop = input$population),
                                                           write_to = paste0(tmpdr, "/%s/%s_", input$population,".%e"),
                                                           overwrite = TRUE, 
                                                           display_progress = TRUE,
                                                           offsets = react_dat()$offsets, session = session)))
             },
             { files = unlist(suppressWarnings(ExtractImages_toFile(display_progress = TRUE, param = param, 
                                                                    objects = popsGetObjectsIds(obj_react$obj, pop = input$population),
                                                                    write_to = paste0(tmpdr, "/%s/",input$population,"/%c/%s_%c_%o.",input$cells_save_type),
                                                                    offsets = react_dat()$offsets, session = session))) }
      )
      to_rm = files
      files = short_path(files, tmpdr)
      olddir = getwd()
      setwd(tmpdr)
      pb = newPB(session = session, title = basename(param$fileName_image), label = "zipping files", min = 0, max = length(files))
      tryCatch({
        suppressMessages(zip::zip(zipfile = file, files = files[1], compression_level = 0, recurse = TRUE))
        if(length(files) > 1) sapply(2:length(files), FUN = function(i_file) {
          setPB(pb, value = i_file, title = basename(param$fileName_image), label = "zipping files")
          suppressMessages(zip::zip_append(zipfile = file, files = files[i_file], compression_level = 0, recurse = TRUE))
        })
      },
      error = function(e) {
        stop(e$message, call. = FALSE)
      },
      finally = {
        endPB(pb)
        setwd(olddir)
        file.remove(to_rm)
      })
    },
    error = function(e) {
      mess_global(title = "exporting images", msg = e$message, type = "error", duration = 10)
    },
    finally = {
      mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", reset = TRUE)
    })
  })
output$example_save_btn <- downloadHandler(
  filename = function() {
    ans = specialr(paste0(remove_ext(basename(react_dat()$fileName)), ".zip"))
    ans
  },
  content = function(file) {
    mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", msg = "Exporting example files", reset = FALSE)
    tmpdr = session_react$dir
    olddir = getwd()
    suppressWarnings(dir.create(file.path(tmpdr, "example")))
    files = list.files(path = system.file(package = "IFCdata", "extdata"), all.files = FALSE, full.names = TRUE, include.dirs = FALSE)
    suppressWarnings(file.copy(from = files, to = file.path(tmpdr, "example", basename(files)), recursive = TRUE))
    files = list.files(path = file.path(tmpdr, "example"), all.files = FALSE, full.names = TRUE, recursive = TRUE)
    to_rm = files
    files = short_path(files, tmpdr)
    setwd(tmpdr)
    tryCatch({
      pb = newPB(session = session, title = basename(react_dat()$fileName_image), label = "zipping files", min = 0, max = length(files))
      tryCatch({
        suppressMessages(zip::zip(zipfile = file, files = files[1], compression_level = 0, recurse = TRUE))
        if(length(files) > 1) sapply(2:length(files), FUN = function(i_file) {
          setPB(pb, value = i_file, title = basename(react_dat()$fileName_image), label = "zipping files")
          suppressMessages(zip::zip_append(zipfile = file, files = files[i_file], compression_level = 0, recurse = TRUE))
        })
      },
      error = function(e) {
        stop(e$message, call. = FALSE)
      },
      finally = {
        endPB(pb)
        setwd(olddir)
        unlink(to_rm, recursive = TRUE, force = TRUE)
      })
    },
    error = function(e) {
      mess_global(title = "exporting example files", msg = e$message, type = "error", duration = 10)
    },
    finally = {
      mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", reset = TRUE)
    })
  })
output$features_save_btn <- downloadHandler(
  filename = function() {
    ans = specialr(paste0(remove_ext(basename(react_dat()$fileName)), "_features.", input$features_save_type))
    ans
  },
  content = function(file) {
    mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", msg = "Exporting features", reset = FALSE)
    tmpfile = file.path(session_react$dir, basename(file))
    tryCatch({
      all_pops = do.call(what = cbind, args = lapply(obj_react$obj$pops, FUN = function(p) p$obj))
      colnames(all_pops) = names(obj_react$obj$pops)
      df = cbind(obj_react$obj$features[, setdiff(names(obj_react$obj$features), colnames(all_pops))], all_pops)
      switch(input$features_save_type, 
             "csv" = write.csv(x = df, file = tmpfile),
             "xlsx" = write.xlsx(x = df, file = tmpfile),
             "fcs" = ExportToFCS(obj = foo, write_to = tmpfile, overwrite = TRUE, delimiter = "/", "$SPILLOVER" = convert_spillover(comp_react$spillover))
             )
      if(!file.rename(from = tmpfile, to = file)) file.copy(from = tmpfile, to = file)
    },
    error = function(e) {
      mess_global(title = "exporting features", msg = e$message, type = "error", duration = 10)
    },
    finally = mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", reset = TRUE))
  })
output$stats_save_btn <- downloadHandler(
  filename = function() {
    ans = specialr(paste0(remove_ext(basename(react_dat()$fileName)), "_", specialr(trunc_string(input$stats_feature)), ".", input$stats_save_type))
    ans
  },
  content = function(file) {
    mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", msg = "Exporting stats", reset = FALSE)
    tmpfile = file.path(session_react$dir, basename(file))
    tryCatch({
      stats = extractStats(obj = obj_react$obj, feat_name = input$stats_feature, trans = input$stats_transform)
      switch(input$stats_save_type, 
             "csv" = write.csv(x = stats, file = tmpfile),
             "xlsx" = write.xlsx(x = stats, file = tmpfile))
      if(!file.rename(from = tmpfile, to = file)) file.copy(from = tmpfile, to = file)
    },
    error = function(e) {
      mess_global(title = "exporting stats", msg = e$message, type = "error", duration = 10)
    },
    finally = {
      mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", reset = TRUE)
    })
  })
output$logs_save_btn <- downloadHandler(
  filename = function() {
    "LOGS.txt"
  },
  content = function(file) {
    mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", msg = "Exporting logs", reset = FALSE)
    tryCatch({
      file.copy(from = file.path(session_react$dir, "LOGS.txt"), to = file)
    },
    error = function(e) {
      mess_global("exporting logs", msg = e$message, type = "error", duration = 10)
    },
    finally = {
      mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", reset = TRUE)
    })
  })