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

obs_img <- list(
  observeEvent(input$channels, suspended = TRUE,  {
    if(length(as.numeric(input$channels)) == 1) {
      disable(selector = paste0("#channels input[type='checkbox'][value='",input$channels,"']"))
    } else {
      for(i in param_react$param$channels$physicalChannel) enable(selector = paste0("#channels input[type='checkbox'][value='",sprintf("%02i",i),"']"))
    }
    param_react$param$selection = as.numeric(input$channels)
    param_react$param$chan_to_extract = which(param_react$param$channels$physicalChannel %in% param_react$param$selection)
    param_react$param$chan_to_keep = as.character(param_react$param$channels$physicalChannel[param_react$param$chan_to_extract])
  }),
  observeEvent(input$chan_width, suspended = TRUE,  {
    if(is.null(input$chan_width)) return(NULL)
    if(!is.finite(input$chan_width)) {
      updateNumericInput(session = session, inputId = "chan_width", value = 0)
      return(NULL)
    }
    if(as.numeric(input$chan_width) != 0) {
      param_react$param$size[2] <- as.numeric(input$chan_width)
    } else {
      param_react$param$size[2] <- as.numeric(param_react$param$channelwidth)
    }
  }),
  observeEvent(input$chan_height, suspended = TRUE, {
    if(is.null(input$chan_height)) return(NULL)
    if(!is.finite(input$chan_height)) {
      updateNumericInput(session = session, inputId = "chan_height", value = 0)
      return(NULL)
    }
    param_react$param$size[1] <- as.numeric(input$chan_height)
  }),
  observeEvent(input$chan_sel, suspended = TRUE, {
    if((length(input$chan_sel) == 0) || (input$chan_sel == "")) return(NULL)
    selected = which(param_react$param$channels$physicalChannel %in% as.numeric(input$chan_sel))
    updateSliderInput(session = session, inputId = "chan_range", value = c(param_react$param$channels$xmin[selected],param_react$param$channels$xmax[selected]))
    updateSliderInput(session = session, inputId = "chan_view", value = c(param_react$param$channels$scalemin[selected],param_react$param$channels$scalemax[selected]))
    updateCheckboxGroupInput(session = session, inputId = "chan_force", selected = c("full_range","force_range")[c(param_react$param$channels$full_range[selected], param_react$param$channels$force_range[selected])])
    updateTextInput(session = session, inputId = "chan_name", value = param_react$param$channels$name[selected])
    colourpicker::updateColourInput(session = session, inputId = "chan_color", value = tolower(param_react$param$channels$color[selected]))
  }),
  observeEvent(input$reset_name, suspended = TRUE,  {
    if(is.null(input$reset_name)) return(NULL)
    selected = which(param_react$param$channels$physicalChannel %in% as.numeric(input$chan_sel))
    name = obj_react$back$description$Images[selected,"name"]
    param_react$param$channels$name[selected] <- name
    updateTextInput(session = session, inputId = "chan_name", value = param_react$param$channels$name[selected])
  }),
  observeEvent(input$reset_color, suspended = TRUE, {
    if(is.null(input$reset_color)) return(NULL)
    selected = which(param_react$param$channels$physicalChannel %in% as.numeric(input$chan_sel))
    color = obj_react$back$description$Images[selected,"color"]
    param_react$param$channels$color[selected] <- color
    named_color = c(rgb2hsv(col2rgb(color))); names(named_color) = color; named_color
    param_react$param$colors[[selected]] <- named_color
    colourpicker::updateColourInput(session = session, inputId = "chan_color", value = tolower(param_react$param$channels$color[selected]))
  }),
  observeEvent(input$reset_gamma, suspended = TRUE, {
    if(is.null(input$reset_gamma) | input$reset_gamma == "") return(NULL)
    selected = which(param_react$param$channels$physicalChannel %in% as.numeric(input$chan_sel))
    param_react$param$channels$xmin[selected] <- as.numeric(obj_react$back$description$Images[selected,"xmin"])
    param_react$param$channels$xmax[selected] <- as.numeric(obj_react$back$description$Images[selected,"xmax"])
    param_react$param$channels$xmid[selected] <- as.numeric(obj_react$back$description$Images[selected,"xmid"])
    param_react$param$channels$ymid[selected] <- as.numeric(obj_react$back$description$Images[selected,"ymid"])
    updateSliderInput(session = session, inputId = "chan_gamma_x", value = param_react$param$channels$xmid[selected])
    updateSliderInput(session = session, inputId = "chan_gamma_y", value = param_react$param$channels$ymid[selected])
  }),
  observeEvent(input$reset_range, suspended = TRUE,  {
    if(is.null(input$reset_range) | input$reset_range == "") return(NULL)
    selected = which(param_react$param$channels$physicalChannel %in% as.numeric(input$chan_sel))
    param_react$param$channels$xmin[selected] <- as.numeric(obj_react$back$description$Images[selected,"xmin"])
    param_react$param$channels$xmax[selected] <- as.numeric(obj_react$back$description$Images[selected,"xmax"])
    param_react$param$channels$xmid[selected] <- as.numeric(obj_react$back$description$Images[selected,"xmid"])
    param_react$param$channels$ymid[selected] <- as.numeric(obj_react$back$description$Images[selected,"ymid"])
    updateSliderInput(session = session, inputId = "chan_gamma_x", 
                      min = param_react$param$channels$xmin[selected], 
                      max = param_react$param$channels$xmax[selected], 
                      value = param_react$param$channels$xmid[selected])
    updateSliderInput(session = session, inputId = "chan_range",
                      value = c(param_react$param$channels$xmin[selected],param_react$param$channels$xmax[selected]))
  }),
  observeEvent(input$reset_view, suspended = TRUE, {
    if(is.null(input$reset_view) | input$reset_view == "") return(NULL)
    selected = which(param_react$param$channels$physicalChannel %in% as.numeric(input$chan_sel))
    param_react$param$channels$scalemin[selected] <- as.numeric(obj_react$back$description$Images[selected,"scalemin"])
    param_react$param$channels$scalemax[selected] <- as.numeric(obj_react$back$description$Images[selected,"scalemax"])
    updateSliderInput(session = session, inputId = "chan_view", value = c(param_react$param$channels$scalemin[selected],param_react$param$channels$scalemax[selected]))
  }),
  observeEvent(input$chan_name, suspended = TRUE, {
    if(is.null(input$chan_name)) return(NULL)
    selected = which(param_react$param$channels$physicalChannel %in% as.numeric(input$chan_sel))
    if(length(selected) == 0) return(NULL)
    name <- input$chan_name
    if(length(name) == 0 || (name == "")) {
      showFeedbackDanger(session=session,inputId="chan_name",text="empty")
      return(NULL)
    } else {
      if(grepl(pattern = "|", x = name, fixed = TRUE)) {
        showFeedbackDanger(session=session,inputId="chan_name",text="invalid character")
        return(NULL)
      } else {
        hideFeedback(session=session,inputId="chan_name")
      }
    }
    param_react$param$channels$name[[selected]] <- name
  }),
  observeEvent(input$chan_color, suspended = TRUE,  {
    if(is.null(input$chan_color)) return(NULL)
    selected = which(param_react$param$channels$physicalChannel %in% as.numeric(input$chan_sel))
    if(length(selected) == 0) return(NULL)
    color <- input$chan_color
    param_react$param$channels$color[selected] <- color
    named_color = c(rgb2hsv(col2rgb(color))); names(named_color) = color; named_color
    param_react$param$colors[[selected]] <- named_color
  }),
  observeEvent(input$chan_view, suspended = TRUE,  {
    if(is.null(input$chan_view)) return(NULL)
    selected = which(param_react$param$channels$physicalChannel %in% as.numeric(input$chan_sel))
    if(length(selected) == 0) return(NULL)
    param_react$param$channels$scalemin[selected] <- as.numeric(input$chan_view[1])
    param_react$param$channels$scalemax[selected] <- as.numeric(input$chan_view[2])
    updateSliderInput(session = session, inputId = "chan_view", value = c(param_react$param$channels$scalemin[selected],param_react$param$channels$scalemax[selected]))
  }),
  observeEvent(input$chan_range, suspended = TRUE, {
    if(is.null(input$chan_range)) return(NULL)
    selected = which(param_react$param$channels$physicalChannel %in% as.numeric(input$chan_sel))
    if(length(selected) == 0) return(NULL)
    param_react$param$channels$xmin[selected] <- as.numeric(input$chan_range[1])
    param_react$param$channels$xmax[selected] <- as.numeric(input$chan_range[2])
    param_react$param$channels$xmid[selected] <- floor((param_react$param$channels$xmax[selected] - param_react$param$channels$xmin[selected])/2 + param_react$param$channels$xmin[selected])
    param_react$param$channels$ymid[selected] <- 127
    updateSliderInput(session = session, inputId = "chan_gamma_x", value = param_react$param$channels$xmid[selected], min = as.numeric(input$chan_range[1]), max = as.numeric(input$chan_range[2]))
    updateSliderInput(session = session, inputId = "chan_gamma_y", value = 127)
    param_react$param$channels$gamma[selected] <- 1 #computeGamma(unlist(param_react$param$channels[selected,c("xmin", "xmax", "xmid", "ymid") ]))
  }),
  observeEvent(input$chan_gamma_x, suspended = TRUE,  {
    if(is.null(input$chan_gamma_x)) return(NULL)
    selected = which(param_react$param$channels$physicalChannel %in% as.numeric(input$chan_sel))
    if(length(selected) == 0) return(NULL)
    param_react$param$channels$xmid[selected] <- as.numeric(input$chan_gamma_x)
    param_react$param$channels$gamma[selected] <- computeGamma(unlist(param_react$param$channels[selected,c("xmin", "xmax", "xmid", "ymid") ]))
  }),
  observeEvent(input$chan_gamma_y, suspended = TRUE,  {
    if(is.null(input$chan_gamma_y)) return(NULL)
    selected = which(param_react$param$channels$physicalChannel %in% as.numeric(input$chan_sel))
    if(length(selected) == 0) return(NULL)
    param_react$param$channels$ymid[selected] <- as.numeric(input$chan_gamma_y)
    param_react$param$channels$gamma[selected] <- computeGamma(unlist(param_react$param$channels[selected,c("xmin", "xmax", "xmid", "ymid")]))
  }),
  observeEvent(input$chan_type, suspended = TRUE,  {
    if((length(input$chan_sel) == 0) || (input$chan_sel == "")) return(NULL)
    selected = which(param_react$param$channels$physicalChannel %in% as.numeric(input$chan_sel))
    DT_react$loaded = FALSE
    if(input$chan_type == "msk") {
      param_react$param = param_react$back
      param_react$param$channels$add_noise <- rep(FALSE, length(input$chan_sel))
      param_react$param$channels$full_range <- rep(FALSE, length(input$chan_sel))
      param_react$param$channels$force_range <- rep(FALSE, length(input$chan_sel))
      reset(id = "chan_force")
      updateSliderInput(session = session, inputId = "chan_range", min = 0, max = 3, value = c(0,3))
      updateSliderInput(session = session, inputId = "chan_view", min = 0, max = 3, value = c(0,3))
      updateSliderInput(session = session, inputId = "chan_gamma_x", min = 0, max = 3, value = 1)
      updateSliderInput(session = session, inputId = "chan_gamma_y", value = 127)
      disable(id = "chan_force")
    } else {
      # param_react$param = param_react$back
      updateSliderInput(session = session, inputId = "chan_range", min = 0, max = 4095,
                        value = c(param_react$back$channels$xmin[selected],param_react$back$channels$xmax[selected]))
      updateSliderInput(session = session, inputId = "chan_view",  min = 0, max = 4095,
                        value = c(param_react$back$channels$scalemin[selected],param_react$back$channels$scalemax[selected]))
      updateSliderInput(session = session, inputId = "chan_gamma_x", min = 0, max = 4095, value = param_react$back$channels$xmid[selected])
      updateSliderInput(session = session, inputId = "chan_gamma_y", value = param_react$back$channels$ymid[selected])
      if(any(unlist(param_react$back$channels[selected, c("full_range", "force_range")]))) {
        updateCheckboxGroupInput(session = session, inputId = "chan_force", selected = c("full_range", "force_range")[unlist(param_react$back$channels[selected, c("full_range", "force_range")])])
      } else {
        reset(id = "chan_force") 
      }
      enable(id = "chan_force")
    }
  }),
  observeEvent(input$chan_force, suspended = TRUE, ignoreNULL = FALSE, {
    if(input$chan_sel == "") return(NULL)
    selected = which(param_react$param$channels$physicalChannel %in% as.numeric(input$chan_sel))
    param_react$param$channels$full_range[selected] <- "full_range" %in% input$chan_force
    param_react$param$channels$force_range[selected] <- "force_range" %in% input$chan_force
    if(any(unlist(param_react$param$channels[selected, c("full_range", "force_range")])) || (input$chan_type == "msk")) {
      disable("reset_color")
      disable("reset_gamma")
      disable("reset_range")
      disable("reset_view")
      disable("chan_view")
      disable("chan_range")
      disable("chan_gamma_x")
      disable("chan_gamma_y")
    } else {
      enable("reset_color")
      enable("reset_gamma")
      enable("reset_range")
      enable("reset_view")
      enable("chan_view")
      enable("chan_range")
      enable("chan_gamma_x")
      enable("chan_gamma_y")
    }
  }),
  observeEvent(input$img_close, suspended = TRUE, {
    foo = obj_react$obj
    if(length(param_react$param$channels) !=0) {
      # check if a channel name has been changed
      # if so object is modified
      N = names(foo$description$Images)
      sapply(1:length(param_react$param$channels$physicalChannel), FUN = function(i) {
        foo$description$Images[foo$description$Images$physicalChannel == param_react$param$channels$physicalChannel[i], N] <<- param_react$param$channels[i, N]
      })
      if(!all(foo$description$Images$name == obj_react$obj$description$Images$name)) {
        obj_react$obj = data_redefine(foo, 
                                      redefine_features_def(foo$features_def,
                                                            foo$description$masks,
                                                            images = obj_react$obj$description$Images,
                                                            force_default = TRUE,
                                                            new_images_names = foo$description$Images$name))
        # report is reinitialized
        runjs(sprintf("Shiny.setInputValue('report_recover', %i)", ifelse(length(input$report_recover)==0, 0L, input$report_recover + 1L)))
      }
    }
    runjs("Shiny.onInputChange('img_manager_visible', false)")
  }))