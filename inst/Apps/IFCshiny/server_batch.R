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

output$violin_plot <- renderPlotly({
  if((input$navbar != "tab7") ||
     (input$navbar_batch != "Violin") ||
     (length(obj_react$batch) == 0)) return(NULL)
  runjs("document.getElementById('msg_busy_txt2').innerText = 'updating violin plot';")
  runjs("document.getElementById('msg_busy_ctn2').style.display = 'block';")
  tryCatch({plotly_batch_violin(obj_react$batch, 
                                pop = input$plot_batch_population,
                                feat = input$plot_batch_feature,
                                trans = input$plot_batch_feature_transform,
                                type = "violin",
                                height = input$plot_batch_height,
                                points = input$violin_points)
  }, error = function(e) {
    mess_global(title = "batch violin", msg = e$message, type = "error", duration = 10)
    return(NULL)
  },
  finally = {
    runjs("document.getElementById('msg_busy_ctn2').style.display = 'none';")
  })
})
output$ridge_plot <- renderPlotly({
  if((input$navbar != "tab7") ||
     (input$navbar_batch != "Ridge") ||
     (length(obj_react$batch) == 0)) return(NULL)
  runjs("document.getElementById('msg_busy_txt2').innerText = 'updating ridge plot';")
  runjs("document.getElementById('msg_busy_ctn2').style.display = 'block';")
  tryCatch({plotly_batch_violin(obj_react$batch, 
                                pop = input$plot_batch_population,
                                feat = input$plot_batch_feature,
                                trans = input$plot_batch_feature_transform,
                                type = "ridge",
                                space = input$ridge_space,
                                height = input$plot_batch_height,
                                points = "none")
  }, error = function(e) {
    mess_global(title = "batch ridge", msg = e$message, type = "error", duration = 10)
    return(NULL)
  },
  finally = {
    runjs("document.getElementById('msg_busy_ctn2').style.display = 'none';")
  })
})
output$volcano_plot <- renderPlotly({
  if((input$navbar != "tab7") ||
     (input$navbar_batch != "Volcano") ||
     (length(obj_react$stats) == 0)) return(NULL)
  runjs("document.getElementById('msg_busy_txt2').innerText = 'updating volcano plot';")
  runjs("document.getElementById('msg_busy_ctn2').style.display = 'block';")
  m = switch(input$volcano_method,"wilcoxon"="wilcox","t-test"="t")
  if(attr(obj_react$stats, "method") != m ||
     attr(obj_react$stats, "pop") != input$plot_batch_population) {
    obj_react$stats = batch_stats(obj_react$batch, pop=input$plot_batch_population, method=m)
  }
  tryCatch({plotly_batch_volcano(obj_react$stats, fold = input$volcano_fold, height = input$plot_batch_height)  }, error = function(e) {
    mess_global(title = "batch volcano", msg = e$message, type = "error", duration = 10)
    return(NULL)
  },
  finally = {
    runjs("document.getElementById('msg_busy_ctn2').style.display = 'none';")
  })
})
output$heatmap_plot <- renderPlotly({
  if((input$navbar != "tab7") ||
     (input$navbar_batch != "Heatmap") ||
     (length(obj_react$stats) == 0)) return(NULL)
  runjs("document.getElementById('msg_busy_txt2').innerText = 'updating heatmap';")
  runjs("document.getElementById('msg_busy_ctn2').style.display = 'block';")
  w = switch(input$heatmap_what,"zscore"="zscore","mean"="fold_avg","median"="fold_med")
  if(attr(obj_react$stats, "pop") != input$plot_batch_population) {
    obj_react$stats = batch_stats(obj_react$batch, pop=input$plot_batch_population, method="none")
  }
  tryCatch({plotly_batch_heatmap(obj_react$stats,
                                 height = input$plot_batch_height,
                                 what = w,
                                 dendro = input$heatmap_dendro == "yes")
  }, error = function(e) {
    mess_global(title = "batch heatmap", msg = e$message, type = "error", duration = 10)
    return(NULL)
  },
  finally = {
    runjs("document.getElementById('msg_busy_ctn2').style.display = 'none';")
  })
})
output$stack_plot <- renderPlotly({
  if((input$navbar != "tab7") ||
     (input$navbar_batch != "Stack") ||
     (length(obj_react$batch) == 0)) return(NULL)
  runjs("document.getElementById('msg_busy_txt2').innerText = 'updating batch 3D stack';")
  runjs("document.getElementById('msg_busy_ctn2').style.display = 'block';")
  tryCatch({
    plotly_batch_stack(obj_react$batch, 
                       g = plot_react$g,
                       batch_mode = TRUE,
                       height = input$plot_batch_height)
  }, error = function(e) {
    mess_global(title = "batch 3D stack", msg = e$message, type = "error", duration = 10)
    return(NULL)
  },
  finally = {
    runjs("document.getElementById('msg_busy_ctn2').style.display = 'none';")
  })
})

obs_batch = list(
  observeEvent(input$navbar_batch, suspended = TRUE, ignoreInit = FALSE, {
    if(input$navbar != "tab7") return(NULL)
    hideElement("batch_feature")
    hideElement("violin_controls")
    hideElement("ridge_controls")
    hideElement("volcano_controls")
    hideElement("heatmap_controls")
    hideElement("stack_controls")
    switch(input$navbar_batch, 
           "Violin" = {
             showElement("batch_feature")
             showElement("violin_controls")
           }, 
           "Ridge" = {
             showElement("batch_feature")
             showElement("ridge_controls")
           }, 
           "Volcano" = {
             showElement("volcano_controls")
           },
           "Heatmap" = {
             showElement("heatmap_controls")
           },
           "Stack" = {
             showElement("stack_controls")
           })
  }),
  observeEvent(input$plot_batch_height, suspended = TRUE, ignoreInit = FALSE, ignoreNULL = TRUE, {
    runjs(code=sprintf("$('.plot_batch,.plot_batch>.html-widget').height(%i)", input$plot_batch_height))
  })
)