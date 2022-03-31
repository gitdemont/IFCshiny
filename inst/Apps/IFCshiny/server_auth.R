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

# create authentification variable
res_auth = NULL

# depending if we are on shinyapps or not we enable / disable some features
if(.passphrase == "") {
  .no_cores <- max(1, min(.max_cores, ifelse(requireNamespace("parallel", quietly = TRUE), parallel::detectCores(), 1)))
  runjs(code = "$('.app_dashboard_panel').show()")
  # allow dev mode
  showElement("dev")
  runjs(code = "document.getElementById('msg_busy_ctn2').style.display = 'none';")
  observeEvent(once = TRUE, input$webgl_available , {
    if(!input$webgl_available) {
      updateRadioButtons(session=session, inputId="plot_type", choices=c("1D", "2D"), selected="1D", inline = TRUE)
      mess_global(title = "WebGL", 
                  msg = c("Your browser does not support WebGL",
                          "Some features are not available",
                          "- 3D graphs"),
                  type = "warning", duration = 10, where = "toast-bottom-right")
    } 
  })
} else {
  res_auth <- secure_server(
    check_credentials = check_credentials(
      db = .path_to_db,
      passphrase = .passphrase
    ),
    timeout = 0,
    inputs_list = list(group = list(fun = "selectInput",
                                    args = list(choices = c("all", "restricted"),
                                                multiple = TRUE,
                                                selected = c("all", "restricted")))),
    session = session
  )
  runjs(code = "$('.app_dashboard_panel').hide()")
  .no_cores <- max(1, min(.max_cores, ifelse(requireNamespace("parallel", quietly = TRUE), parallel::detectCores(), 1)))
  runjs(code = "document.getElementById('msg_busy_ctn2').style.display = 'none';")
}

if(!requireNamespace("parallel", quietly = TRUE) ||
   !requireNamespace("doParallel", quietly = TRUE) ||
   !requireNamespace("foreach", quietly = TRUE) ||
   .no_cores <= 1 || Sys.getenv('SHINY_PORT') != "") { 
  updateMaterialSwitch(session=session, inputId = "use_parallelization", value = FALSE)
  # disable("use_parallelization")
}

# depending if we use authentication we manage session time + number on allowed users connected
# in addition if in authentication mode we start the app with credits
# an admin will always be able to connect once
observeEvent(res_auth , {
  if(length(res_auth$user) == 0) return(NULL)
  # to limit the number of users 
  if(!as.logical(res_auth$admin) && (length(getShinyOption("connected_users")) >= .max_users) && !(res_auth$user %in% getShinyOption("connected_users"))) {
    mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", msg = "", reset = FALSE)
    mess_global(title = "Session log-in", 
                msg = c("There are too many users already connected",
                        "Please reconnect later"),
                type = "error", duration = 10)
    runjs(code = "$('.app_dashboard_panel').hide()")
    
    # here there are already xxx users connected reaching the max number of allowed concomitant connection
    # so we set a 10 seconds timer to show a message saying this 
    # before disconnecting the current session
    runjs(code ="setTimeout(function(sec = 10) {
      var foo = sec * 1000;
      setInterval(function() {
        foo = foo - 1000;
        var m = parseInt(foo/60000);
        var s = parseInt((foo%60000) / 1000);
        document.getElementById('timer').innerText = 'disconnection in ' + ('00' + m).slice(-2) + ':' + ('00' + s).slice(-2); }, 1000);
        setTimeout(function() { history.go(0); }, sec * 1000 );
      }, 100);")
    delay(ms = 10000, session$reload())
  } else {
    if(res_auth$user %in% getShinyOption("connected_users")) {
      mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", msg = "", reset = FALSE)
      mess_global(title = "Session log-in", 
                  msg = c("You are already logged",
                          "Please disconnect from other place"),
                  type = "error", where = "toast-bottom-full-width", duration = 10)
      runjs(code = "$('.app_dashboard_panel').hide()")
      
      # here user is already connected in another session
      # so we set a 10 seconds timer  to show a message saying this 
      # before disconnecting him from the current session
      runjs(code ="setTimeout(function(sec = 10) {
      var foo = sec * 1000;
      setInterval(function() {
        foo = foo - 1000;
        var m = parseInt(foo/60000);
        var s = parseInt((foo%60000) / 1000);
        document.getElementById('timer').innerText = 'disconnection in ' + ('00' + m).slice(-2) + ':' + ('00' + s).slice(-2); }, 1000);
        setTimeout(function() { history.go(0); }, sec * 1000 );
      }, 100);")
      delay(ms = 10000, session$reload())
    } else {
      # here max number of user is not reached and user is not already connected elsewhere
      # so we say hello, we test features availability
      mess_global(title = "Session log-in", 
                  msg = paste0("Hello ", res_auth$user),
                  type = "success", duration = 10, where = "toast-bottom-right")
      # we show credits
      observeEvent(once = TRUE, input$webgl_available , {
        if(!input$webgl_available) {
          updateRadioButtons(session=session, inputId="plot_type", choices=c("1D", "2D"), selected="1D", inline = TRUE)
          mess_global(title = "WebGL", 
                      msg = c("Your browser does not support WebGL",
                              "Some features are not available",
                              "- 3D graphs"),
                      type = "warning", duration = 10, where = "toast-bottom-right")
        } 
      })
      # we register user name so as to test if he will not connect once again 
      shinyOptions(connected_users = c(getShinyOption("connected_users"), res_auth$user))
      onSessionEnded(fun = function(user = isolate(res_auth$user)) {
        shinyOptions(connected_users = setdiff(getShinyOption("connected_users"), user))
      }, session = session)
      # we allow xxx seconds of use before automatically disconnect the user
      # except if user is an admin
      if(!(as.logical(res_auth$admin) || as.logical(res_auth$privileged))) {
        click("credits")
        runjs(code =sprintf("setTimeout(function(sec = %i) {
      var foo = sec * 1000;
      setInterval(function() {
        foo = foo - 1000;
        var m = parseInt(foo/60000);
        var s = parseInt((foo%%60000) / 1000);
        document.getElementById('timer').innerText = 'remaining time ' + ('00' + m).slice(-2) + ':' + ('00' + s).slice(-2); }, 1000);
        setTimeout(function() { history.go(0); }, sec * 1000 );
      }, 100);", .max_time))
      } else {
        runjs(code = "$('.app_dashboard_panel').show()")
      }
    }
  }
})