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

list(
  hidden(tags$div(id="example_save",
                  tags$hr(),
                  tags$div(tags$h4("Download example files")),
                  tags$div(tags$div(style = "display:inline-block; vertical-align:baseline; width:80%",
                                    radioButtons(inputId = "example_save_type", label = "download as:", choices = c("zip"), selected = "zip", inline = TRUE)),
                           tags$div(style = "display:inline-block; vertical-align:text-bottom; width:10%",
                                    downloadButton("example_save_btn", label = "", icon("download")))),
                  verbatimTextOutput(outputId = "example_saved_msg"))
  ),
  hidden(tags$div(id="infos_save",
                  tags$hr(),
                  tags$div(tags$h4("Download Infos")),
                  tags$div(tags$div(style = "display:inline-block; vertical-align:baseline; width:80%",
                                    radioButtons(inputId = "infos_save_type", label = "download as:", choices = c("txt"), selected = "txt", inline = TRUE)),
                           tags$div(style = "display:inline-block; vertical-align:text-bottom; width:10%",
                                    downloadButton("infos_save_btn", label = "", icon("download")))),
                  verbatimTextOutput(outputId = "infos_saved_msg"),
                  tags$hr(),
                  tags$div(tags$h4("Download DAF")),
                  tags$div(tags$div(style = "display:inline-block; vertical-align:baseline; width:80%",
                                    radioButtons(inputId = "daf_save_type", label = "download as:", choiceNames = c("daf","fcs","fcs + xml"), choiceValues = c("daf","fcs","zip"), selected = "daf", inline = TRUE)),
                           tags$div(style = "display:inline-block; vertical-align:text-bottom; width:10%",
                                    downloadButton("daf_save_btn", label = "", icon("download")))),
                  verbatimTextOutput(outputId = "daf_saved_msg"))
  ),
  hidden(tags$div(id="graph_save",
                  tags$hr(),
                  tags$div(tags$h4("Download Single Graph")),
                  tags$div(
                    tags$div(tags$div(style = "display:inline-block; vertical-align:baseline; width:80%",
                                      radioButtons(inputId = "graph_save_type", label = "download as:", choices = c("pdf", "html"), selected = "pdf", inline = TRUE)),
                             tags$div(style = "display:inline-block; vertical-align:text-bottom; width:10%",   
                                      downloadButton("graph_save_btn", label="", icon("download")))),
                    verbatimTextOutput(outputId = "graph_saved_msg")))
  ),
  hidden(tags$div(id="features_save",
                  tags$hr(),
                  tags$div(tags$h4("Download Features")),
                  tags$div(tags$div(style = "display:inline-block; vertical-align:baseline; width:80%",
                                    radioButtons(inputId = "features_save_type", label = "download as:", choices = c("csv", "xlsx", "fcs"), selected = "csv", inline = TRUE)),
                           tags$div(style = "display:inline-block; vertical-align:text-bottom; width:10%",
                                    downloadButton("features_save_btn", label="",icon("download")))),
                  verbatimTextOutput(outputId = "features_saved_msg"))
  ),
  hidden(tags$div(id="stats_save",
                  tags$hr(),
                  tags$div(tags$h4("Download Pops Stats")),
                  tags$div(tags$div(style = "display:inline-block; vertical-align:baseline; width:80%",
                                    radioButtons(inputId = "stats_save_type", label = "download as:", choices = c("csv", "xlsx"), selected = "csv", inline = TRUE)),
                           tags$div(style = "display:inline-block; vertical-align:text-bottom; width:10%",
                                    downloadButton("stats_save_btn", label="",icon("download")))),
                  verbatimTextOutput(outputId = "stats_saved_msg"))
  ),
  hidden(tags$div(id="cells_save",
                  tags$hr(),
                  tags$div(tags$h4("Download Images")),
                  tags$div(tags$div(style = "display:inline-block; vertical-align:baseline; width:80%",
                                    "data-toggle"="tooltip", "data-placement"="top", "data-html"="true", "title"="Select the export format for images/masks export.",
                                    radioButtons(inputId = "cells_save_type", label = "download as:", choices = c("tiff", "png", "bmp", "jpg", "xif", "npy"), #[1:ifelse(Sys.info()[["user"]] == "shiny",5,6)],
                                                 selected = "tiff", inline = TRUE)),
                           tags$div(style = "display:inline-block; vertical-align:text-bottom; width:10%",
                                    downloadButton("cells_save_btn", label="",icon("download")))),
                  verbatimTextOutput(outputId = "cells_saved_msg"),
                  hidden(wellPanel(tags$div(tags$p(id = "drop zone",
                                                   class = "addable",
                                                   # style = "height:300px; width:300px;",
                                                   ondrop = 'drop(event)',"Drop your cells on me"),
                                            tags$div(id = "display zone")))))
  ),
  hidden(tags$div(id="logs_save",
                  tags$div(tags$h4("Download Logs")),
                  tags$div(tags$div(style = "display:inline-block; vertical-align:baseline; width:80%",
                                    radioButtons(inputId = "logs_save_type", label = "download as:", choices = c("txt"), selected = "txt", inline = TRUE)),
                           tags$div(style = "display:inline-block; vertical-align:text-bottom; width:10%",
                                    downloadButton("logs_save_btn", label = "", icon("download")))),
                  verbatimTextOutput(outputId = "logs_saved_msg"))
  )
)