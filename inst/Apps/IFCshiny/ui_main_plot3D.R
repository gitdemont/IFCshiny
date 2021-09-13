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

list(hidden(tags$div(id = "plot_3D_placeholder",
                tags$div(id = "plot_3D_options",
                         style = "z-index:3;",
                         tags$div(style = "position:relative; display: inline-block; vertical-align: -30px; width:100px;",
                                  actionButton(inputId = "plot_type_3D_option01", label = "Resample")),
                         tags$div(style = "position:relative; display: inline-block; vertical-align: top; width:200px;",
                                  sliderInput(inputId = "plot_type_3D_option03", label = "Density %", min = 0, max = 100, value = 100)),
                         tags$div(style = "position:relative; display: inline-block; vertical-align: top; width:200px;",
                                  sliderInput(inputId = "plot_type_3D_option02", label = "Point size", min = 1, max = 10, value = 2))
                ),
                tags$div(id = "plot_3D_controls",
                         style = "z-index:2;",
                         tags$div(style = "position:relative; display: inline-block; vertical-align: top; width: 50px;",
                                  Toggle3D(inputId = "plot_3D_draw_axes", label = "axes", value = TRUE)),
                         tags$div(style = "position:relative; display: inline-block; vertical-align: top; width: 50px;",
                                  Toggle3D(inputId = "plot_3D_draw_points", label = "pts", value = TRUE)),
                         tags$div(style = "position:relative; display: inline-block; vertical-align: top; width: 50px;",
                                  Toggle3D(inputId = "plot_3D_draw_ell", label = "ell", value = FALSE)),
                         tags$div(style = "position:relative; display: inline-block; vertical-align: top; width: 50px;",
                                  Toggle3D(inputId = "plot_3D_draw_txt", label = "txt", value = FALSE)),
                         hidden(tags$div(id = "plot_3D_img_ctrl",
                                         style = "position:relative; display: inline-block; vertical-align: top;",
                                         tags$div(style = "position:relative; display: inline-block; vertical-align: top; width: 60px;",
                                                  selectInput("plot_3D_draw_chan", label = NULL, choices = sprintf("%02i",1:12), multiple = FALSE)))),
                         tags$div(style = "position:relative; display: inline-block; vertical-align: top; width: 100px;",
                                  selectInput("plot_3D_mouse_ctrl", label = NULL, choices = c("trackball", "selecting"), selected = "trackball", multiple = FALSE)),
                         tags$div(style = "position:relative; display: inline-block; vertical-align: top; width: 190px;",
                                  actionButton("plot_3D_create_pop", label = "Create pop from selection", value = FALSE))),
                tags$div(style = "z-index:1;",
                         rglwidgetOutput(outputId = "plot_3D", width = 512, height = 512))))
)