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

list(tags$div(class = "plot_sel_tools",
              tags$div(class = "plot_sel_init",
                       style="position:relative; vertical-align:sub; display:inline-block",
                       tags$div(style="position:relative; vertical-align:sub; display:inline-block", class = "plot_tool",
                                "data-toggle"="tooltip", "data-placement"="top", "data-html"="true", "title"="stop drawing region & allow selection",
                                actionButton(inputId = "plot_sel_init", label = NULL, icon("mouse-pointer", lib = "font-awesome", verify_fa=FALSE)))),
              tags$div(class = "plot_sel_1D",
                       style="position:relative; vertical-align:sub; display:inline-block",
                       tags$div(style="position:relative; vertical-align:sub; display:inline-block", class = "plot_tool",
                                "data-toggle"="tooltip", "data-placement"="top", "data-html"="true", "title"="draw line",
                                actionButton(inputId = "plot_sel_line", label = NULL, icon("arrows-alt-h", lib = "font-awesome", verify_fa=FALSE)))),
              tags$div(class = "plot_sel_2D",
                       style="position:relative; vertical-align:sub; display:inline-block",
                       tags$div(style="position:relative; vertical-align:sub; display:inline-block", class = "plot_tool",
                                "data-toggle"="tooltip", "data-placement"="top", "data-html"="true", "title"="draw rectangle",
                                actionButton(inputId = "plot_sel_rectangle", label = NULL, icon("vector-square", lib = "font-awesome", verify_fa=FALSE))),
                       tags$div(style="position:relative; vertical-align:sub; display:inline-block", class = "plot_tool",
                                "data-toggle"="tooltip", "data-placement"="top", "data-html"="true", "title"="draw polygon",
                                actionButton(inputId = "plot_sel_polygon", label = NULL, icon("draw-polygon", lib = "font-awesome", verify_fa=FALSE))),
                       tags$div(style="position:relative; vertical-align:sub; display:inline-block", class = "plot_tool",
                                "data-toggle"="tooltip", "data-placement"="top", "data-html"="true", "title"="draw polygon with lasso selection",
                                actionButton(inputId = "plot_sel_lasso", label = NULL, icon("hand-point-up", lib = "font-awesome", verify_fa=FALSE))),
                       tags$div(style="position:relative; vertical-align:sub; display:inline-block", class = "plot_tool",
                                "data-toggle"="tooltip", "data-placement"="top", "data-html"="true", "title"="draw ellipse",# style = "transform: scale(1.5,1);",
                                actionButton(inputId = "plot_sel_ellipse", label = NULL, icon("circle", lib = "font-awesome", verify_fa=FALSE)))),
              tags$div(class = "plot_sel_edit",
                       style="position:relative; vertical-align:sub; display:inline-block",
                       tags$div(style="position:relative; vertical-align:sub; display:inline-block", class = "plot_tool",
                                "data-toggle"="tooltip", "data-placement"="top", "data-html"="true", "title"="edit, mouse over label and click to select",
                                actionButton(inputId = "plot_sel_edit", label = NULL, icon("edit", lib = "font-awesome", verify_fa=FALSE)))),
              tags$div(class = "plot_sel_remove",
                       style="position:relative; vertical-align:sub; display:inline-block",
                       tags$div(style="position:relative; vertical-align:sub; display:inline-block", class = "plot_tool",
                                "data-toggle"="tooltip", "data-placement"="top", "data-html"="true", "title"="remove, mouse over label and click to select",
                                actionButton(inputId = "plot_sel_remove", label = NULL, icon("trash-alt", lib = "font-awesome", verify_fa=FALSE)))),
              tags$div(class = "plot_sel_zoom",
                       style="position:relative; vertical-align:sub; display:inline-block",
                       tags$div(style="position:relative; vertical-align:sub; display:inline-block", class = "plot_tool",
                                "data-toggle"="tooltip", "data-placement"="top", "data-html"="true", "title"="zoomin",
                                actionButton(inputId = "plot_sel_zoomin", label = NULL, icon("search-plus", lib = "font-awesome", verify_fa=FALSE))),
                       tags$div(style="position:relative; vertical-align:sub; display:inline-block", class = "plot_tool",
                                "data-toggle"="tooltip", "data-placement"="top", "data-html"="true", "title"="reset zoom",
                                actionButton(inputId = "plot_sel_zoomreset", label = NULL, icon("search", lib = "font-awesome", verify_fa=FALSE)))),
              tags$div(class = "plot_sel_add",
                       style="position:relative; vertical-align:sub; display:inline-block",
                       tags$div(style="position:relative; vertical-align:sub; display:inline-block", class = "plot_tool",
                                "data-toggle"="tooltip", "data-placement"="top", "data-html"="true", "title"="add to report",
                                actionButton(inputId = "plot_sel_add", label = NULL, icon("share-square", lib = "font-awesome", verify_fa=FALSE)))),
              tags$div(class = "plot_sel_stack",
                       style="position:relative; vertical-align:sub; display:inline-block",
                       tags$div(style="position:relative; vertical-align:sub; display:inline-block", class = "plot_tool",
                                "data-toggle"="tooltip", "data-placement"="top", "data-html"="true", "title"="view in batch",
                                actionButton(inputId = "plot_sel_stack", label = NULL, icon("layer-group", lib = "font-awesome", verify_fa=FALSE)))))
)