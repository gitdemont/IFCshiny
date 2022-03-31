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

# we set up value to check if license was already accepted
runjs("Shiny.setInputValue('license_decline', null)")

observeEvent(input$credits, {
  add_log("credits")
  if(length(input$license_decline) == 0) runjs(code = "$('.app_dashboard_panel').hide()")
  tryCatch({
    get_dep <- function(pkg, pkgs_installed = utils::installed.packages(), which = c("Imports", "Suggests")) gsub("^(.*) \\(.*$", "\\1", setdiff(trimws(unlist(unname(tryCatch(strsplit(pkgs_installed[pkg,which], ", |,\\n"), error = function(e) return(NULL))))), ""))
    pkgs_installed = installed.packages()
    pkgs = get_dep("IFCshiny", pkgs_installed, c("Imports", "Suggests"))
    pkgs = pkgs[pkgs %in% rownames(pkgs_installed)]
    pkgs = setdiff(pkgs, c(get_dep("IFC", pkgs_installed, "Imports"), pkgs[grepl("^Part of R ", pkgs_installed[pkgs, "License"])]))
    
    if(requireNamespace("IFCdata", quietly = TRUE)) {
      pkgs = setdiff(pkgs, get_dep("IFCdata", pkgs_installed, "Imports"))
      IFCdata_args = list(tags$div(style = "display: list-item;",
                                   tags$p(style = "font-weight: bold;",
                                          sprintf("IFCdata, under %s", pkgs_installed["IFCdata", "License"])),
                                   tags$span(citation("IFCdata")$textVersion),
                                   tags$p("")))
    } else {
      IFCdata_args = list()
    }
    if(requireNamespace("IFCip", quietly = TRUE)) {
      pkgs = setdiff(pkgs, get_dep("IFCip", pkgs_installed, "Imports"))
      IFCip_args = list(tags$div(style = "display: list-item;",
                                   tags$p(style = "font-weight: bold;",
                                          sprintf("IFCip, under %s", pkgs_installed["IFCip", "License"])),
                                   tags$span(citation("IFCip")$textVersion),
                                   tags$p("")))
    } else {
      IFCip_args = list()
    }
    pkgs_args = lapply(pkgs, FUN = function(pkg) {
      if(!(pkg %in% row.names(pkgs_installed))) return(NULL)
      tags$div(style = "display: list-item;",
               tags$p(style = "font-weight: bold;",
                      sprintf("%s, under %s", pkg, pkgs_installed[pkg, "License"])),
               tags$span(suppressWarnings(citation(pkg)$textVersion)),
               tags$p(""))
    })
    if(length(input$license_decline) == 0) {
      footer = tagList(
        actionButton(inputId = "license_decline", label = "Decline"),
        tags$button(type = "button", class = "btn btn-default", 
                    `data-dismiss` = "modal", onclick = "{ $('.app_dashboard_panel').show(); }",
                    "Accept")
      )
    } else {
      footer = tagList(
        tags$button(type = "button", class = "btn btn-default", 
                    `data-dismiss` = "modal", onclick = "{ $('.app_dashboard_panel').show(); }",
                    "Already accepted")
      )
    }
    tryCatch({
      showModal(do.call(what = modalDialog,
                        args = c(list(title = "IFCshiny app credits",
                                      size = "l",
                                      easyClose = length(input$license_decline) != 0,
                                      footer = footer,
                                      tags$p(tags$b("IFCshiny: An R Interactive Shiny Application for the Analysis of Imaging and Conventional Flow Cytometry.")),
                                      tags$p(tags$b("This application has been written by Yohann Demont\u00a9, YEAR: 2021, GPL-3.")),
                                      tags$p("It allows to read, visualize, analyse and export data from and to Imaging and Conventional Flow Cytometry files.\n Users will also be able to apply supervised and unsupervised machine learning algorithms to fit their data"),
                                      tags$hr(),
                                      tags$p(tags$b("It represents a GUI (Graphic User Interface) of 'IFC' package. Although much more can be done with this package, it allows to simplify users experience through 'shiny'. Provided example files are part of 'IFCdata' package")),
                                      tags$div(style = "display: list-item;",
                                               tags$p(style = "font-weight: bold;",
                                                      sprintf("IFC, under %s",  pkgs_installed["IFC", "License"])),
                                               tags$span(citation("IFC")$textVersion),
                                               tags$p("")),
                                      IFCdata_args,
                                      IFCip_args,
                                      pkgs_args,
                                      list(tags$div(tags$hr(),
                                                    tags$div(style = "display: list-item;",
                                                             tags$p(style = "font-weight: bold;",
                                                                    sprintf("R, under %s",  getRversion())),
                                                             tags$span(citation()$textVersion)),
                                                    tags$p(""))),
                                      list(tags$hr(),
                                           tags$p(tags$b("JAVASCRIPT Libraries")),
                                           tags$div(style = "display: list-item;",
                                                    tags$span("Report layout is produced thanks to"),
                                                    tags$a(target="_blank", href="https://github.com/haltu/muuri/", "muuri, version 0.9.2,"),
                                                    tags$span("under MIT")),
                                           tags$div(style = "display: list-item;",
                                                    tags$span("Interaction with 1D and 2D plot(s) to create and edit gate(s) is done thanks to"),
                                                    tags$a(target="_blank", href="https://github.com/taye/interact.js/", "interact, version 1.10.0,"),
                                                    tags$span("under MIT"))
                                      )),
                                 list(tags$hr(),
                                      tags$div(tags$p(tags$b("DISCLAIMER:"))),
                                      tags$div(tags$p("- You are using this IFCshiny application on your OWN RISK!"),
                                               tags$p("- We do not guarantee PRIVACY nor CONFIDENTIALITY."),
                                               tags$p("- This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. In no event shall the copyright holders or contributors be liable for any direct, indirect, incidental, special, exemplary, or consequential damages (including, but not limited to, procurement of substitute goods or services; loss of use, data, or profits; or business interruption) however caused and on any theory of liability, whether in contract, strict liability, or tort (including negligence or otherwise) arising in any way out of the use of this software, even if advised of the possibility of such damage.")
                                      )))))
    }, error = function(e) {
      removeModal()
    })
  }, error = function(e) {
    mess_global(title = "copyrights", msg = e$message, type = "error")
    runjs(code = "$('.app_dashboard_panel').show()")
  })
  observeEvent(input$license_decline, once = TRUE, {
    session$reload()
  })
})