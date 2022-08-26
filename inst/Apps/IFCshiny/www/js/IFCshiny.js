/*
  This file is released under the GNU General Public License, Version 3, GPL-3
  Copyright (C) 2021 Yohann Demont                                              
                                                                                
  It is part of IFCshiny package, please cite:                                  
   -IFCshiny: An R Interactive Shiny Application for the Analysis of Imaging    
              and Conventional Flow Cytometry                                   
   -YEAR: 2021                                                                  
   -COPYRIGHT HOLDERS: Yohann Demont, Jean-Pierre Marolleau, Loïc Garçon,       
                       CHU Amiens                                               
                                                                                
  DISCLAIMER:                                                                   
  -You are using this package on your own risk!                                 
  -We do not guarantee privacy nor confidentiality.                             
  -This program is distributed in the hope that it will be useful, but WITHOUT  
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or         
  FITNESS FOR A PARTICULAR PURPOSE. In no event shall the copyright holders or  
  contributors be liable for any direct, indirect, incidental, special,         
  exemplary, or consequential damages (including, but not limited to,           
  procurement of substitute goods or services; loss of use, data, or profits;   
  or business interruption) however caused and on any theory of liability,      
  whether in contract, strict liability, or tort (including negligence or       
  otherwise) arising in any way out of the use of this software, even if        
  advised of the possibility of such damage.                                    
                                                                                
  You should have received a copy of the GNU General Public License             
  along with IFCshiny. If not, see <http://www.gnu.org/licenses/>.              
*/

IFCshiny = {
  // function for draggagle target;
  dragging : function(event) {
    //console.log("dragging: " + event.target.id);
    Shiny.onInputChange("ele_dragging", event.target.id);
    event.dataTransfer.setData("text", event.target.id);
    event.target.style.opacity = "0.4";
    return null;
  },
  
  dragstart : function(event) {
    event.dataTransfer.setData("text", event.target.id);
    event.target.style.opacity = "0.4";
    return null;
  },
            
  dragend : function(event) {
    event.target.style.opacity = "1";
    return null;
  },
  
  // functions for drop target;
  dragenter : function(event) {
    if(event.target.classList.contains("droppable") ) {
      event.target.style.border = "3px dotted red";
    }
    return null;
  },
  
  dragover : function(event) {
    event.preventDefault();
    if(event.target.classList.contains("droppable") ) {
      event.target.style.border = "3px dotted red";
    }
    if(event.target.classList.contains("movable")) {
      var bbox = event.target.getBoundingClientRect();
      if(event.clientY - bbox.y - (bbox.height/2) > 0 ) {
   	    event.target.style["border-bottom"] = "4px outset cornflowerblue";
        event.target.style["border-top"] = "1px outset black";
      } else {
        event.target.style["border-top"] = "4px outset cornflowerblue";
        event.target.style['border-bottom'] = "1px outset black";
      }
    }
    Shiny.onInputChange("ele_hover", event.target.id);
    return null;
  },
  
  dragleave : function(event) {
    if(event.target.classList.contains("droppable") ) {
      event.target.style.border = "";
    }
    if(event.target.classList.contains("movable")) {
      var target = event.target;
      target.style["border-bottom"] = "1px outset black";
      target.style["border-top"] = "1px outset black";
    }
    return null;
  },

  move : function(event) {
    event.preventDefault();
    var data = event.dataTransfer.getData("text");
    var node = document.getElementById(data);
    if(event.target.style["border-bottom"] !== "1px outset black") {
      event.target.style["border-bottom"] = "1px outset black";
      event.target.parentNode.insertBefore( document.getElementById(data), event.target.nextSibling);
    } else {
      event.target.style["border-top"] = "1px outset black";
      event.target.parentNode.insertBefore( document.getElementById(data), event.target);
    }
    //console.log("moved: " + node.id);
    Shiny.onInputChange("ele_moved", node.id);
    var children = event.target.parentNode.children;
    var ids = [];
    for (var i = 0; i < children.length; i++) {
      ids.push(children[i].id);
    }
    //console.log("moved: " + ids);
    Shiny.onInputChange("ele_order", ids);
    return null;
  },
  
  // report listeners
  report_dblclick : function(event) {
    console.log("report_graph_dblclick: " + event.target.getAttribute('data-id'));
    Shiny.onInputChange("report_graph_dblclick", event.target.getAttribute('data-id'));
    return null;
  },
  report_close : function(event) {
    console.log("report_close: " + event.target.parentNode.parentNode.firstElementChild.getAttribute('data-id'));
    Shiny.onInputChange("report_close", event.target.parentNode.parentNode.firstElementChild.getAttribute('data-id'));
    return null;
  },
  // @param { string } message - elementId
  getOffset : function(message) {
	var offset = $(message).position();
	if(offset == null) return null;
	return Shiny.onInputChange('IFCshiny_getOffset_ret', { ele: message, top: offset.top, left: offset.left, width: $(message).outerWidth(), height: $(message).outerHeight(), ratio: Math.round(window.devicePixelRatio * 100) / 100 });
},

// @param { string } message - elementId to refresh
refresh : function(message) {
  var ele = document.getElementById(message);
  if(ele == null) return null;
  var old = ele.style.display;
  ele.style.display = "none";
  ele.style.display = old;
  return null;
},

// @param { string } message - elementId
reach : function(message) {
	var ele = document.getElementById(message);
	if(ele == null) return undefined;
	ele.focus();
	ele.scrollIntoView();
},

// @param { string } message - a message not used
init_grid : function(message) {
  if(IFCshiny.hasOwnProperty('grid')) {
    if(Shiny.shinyapp.$inputValues.report_recover != null) Shiny.onInputChange('report_recover', Shiny.shinyapp.$inputValues.report_recover + 1);
    return;
  }
  IFCshiny.grid = new Muuri('.report_grid', {
    dragEnabled: true,
    dragAutoScroll: {
      targets: [],
      handle: null,
      threshold: 50,
      safeZone: 0.2,
      speed: Muuri.AutoScroller.smoothSpeed(1000, 2000, 2500),
      sortDuringScroll: true,
      smoothStop: false,
      onStart: null,
      onStop: null },
    layoutOnResize: false,
    layoutDuration: 100,
    // the layout function has been tweaked to prevent empty tiles from being 
    layout: function (grid, layoutId, items, width, height, callback) {
      var layout = { id: layoutId, items: items, slots: [], styles: {} };
      if(items.length < 1) return callback(layout);
      var margin, ncol, nrow;
      width = parseInt(width, 10);
      margin = items[0].getMargin();
      var ml = parseInt(margin.left, 10), 
          mr = parseInt(margin.right, 10),
          mt = parseInt(margin.top, 10),
          mb = parseInt(margin.bottom, 10);
      ncol = (width-5) / (parseInt(items[0].getWidth(),10) + ml + mr);
      nrow = (height-5) / (parseInt(items[0].getHeight(),10) + mt + mb);
      //  if(Shiny.shinyapp.$inputValues.report_recover != null) Shiny.onInputChange('report_recover', Shiny.shinyapp.$inputValues.report_recover + 1);
      var keepon = true, item_swap = 0, i_item = 0, i_row = 0, i_col = 0, itemA, itemB, ele;
      for(i_row = 0; i_row <= nrow; i_row++) {
        keepon = true;
        for(i_col = 0; i_col < ncol; i_col++) {
          // console.log('r: ' + i_row + ', c:' + i_col + ' i:' + i_item);
          if(i_item < items.length) {
            ele = items[i_item].getElement();
            if(ele.classList.contains('empty_tile')) {
              // console.log('r: ' + i_row + ', c:' + i_col + ' i:' + i_item +' e: '+ ele.id);
              keepon = false;
              item_swap = i_item;
              itemA = items[i_item];
            }
            if((!keepon) && !(ele.classList.contains('empty_tile'))) {
              // console.log(item_swap + ',' + i_item);
              itemB = items[i_item];
              items[i_item] = itemA;
              items[item_swap] = itemB;
              if(i_row != 0) i_row--;
              i_item = i_row * ncol;
              break;
            } else {
              i_item++;
            }
          }
        }
      }
      layout.items = items;
      i_item = 0;
      var item, w, h;
      for (i_row = 0; i_row < nrow; i_row++) {
        for (i_col = 0; i_col < ncol; i_col++) {
          if(i_item < items.length) {
            item = items[i_item];
            w = parseInt(item.getWidth(), 10) + ml + mr;
            h = parseInt(item.getHeight(), 10) + mt + mb;
            layout.slots.push(w * i_col, h * i_row);
          }
        }
        i_item++;
      }
      return callback(layout);
    },
    dragSort: true,
    dragSortHeuristics: {
      sortInterval: 10,
    },
    dragStartPredicate: function (item, event) {
      if(item.getElement().classList.contains('empty_tile')) return false;
      return Muuri.ItemDrag.defaultStartPredicate(item, event);
    },
    dragSortPredicate: function (item) {
      var result = Muuri.ItemDrag.defaultSortPredicate(item, { action: 'move', threshold: 40 } );
      if(result === null) return null;
      return result;
    },
    dragRelease: {
      duration: 0,
      easing: 'ease-out',
      useDragContainer: true },
    dragCssProps: {
      touchAction: 'none',
      userSelect: 'none',
      userDrag: 'none',
      tapHighlightColor: 'rgba(0, 0, 0, 0)',
      touchCallout: 'none',
      contentZooming: 'non' },
    dragPlaceholder: {
      enabled: false,
      createElement: null,
      onCreate: null,
      onRemove: null }
  });
  // on layout changes we pass the layout info to R
  // so that we can compute the new graph positions
  IFCshiny.grid.on('add', function (items) {
    Shiny.onInputChange('report_ready', false);
  });
  IFCshiny.grid.on('remove', function (items) {
    Shiny.onInputChange('report_ready', false);
  });
  IFCshiny.grid.on('layoutEnd', function (items) {
    var out = {};
    for(var i = 0; i < items.length; i++) {
      var item = items[i];
      out[i] = { 'id': item.getElement().getAttribute('data-id'), 'position': item.getPosition() };
    }
    Shiny.onInputChange('report_ready', false);
    Shiny.onInputChange('report_layout', out);
  });
  // we also modify opacity of empty item during drag action so that they can be easily identified
  IFCshiny.grid.on('dragInit', function (item, event) {
    var ele = item.getElement();
    var emtpy_img = ele.parentElement.querySelectorAll('.empty_tile>img');
    emtpy_img.forEach(x => x.style.opacity = 0.9);
    // TODO tweak on drag to allow grid expansion on grid leave
    /*var W = $('.report_grid').width();
    var H = $('.report_grid').height();
    $('.report_grid').mouseleave(function(e) {
      var ele = item.getElement();
      var pos = $(ele).position();
      if(pos.left > W) {
        document.getElementById('report_col_add').click();
      }
      if(pos.top > H) {
        document.getElementById('report_row_add').click();
      }
    });*/
  });
  IFCshiny.grid.on('dragEnd', function (item, event) {
    var ele = item.getElement();
    var emtpy_img = ele.parentElement.querySelectorAll('.empty_tile>img');
    emtpy_img.forEach(x => x.style.opacity = 1);
    //$('.report_grid').off('mouseleave');
  });
},

//@param { Object } message - Object with :,
// -{ string } eleId elementId of the rgl toggle checkbox,
// -{ number } width width of the elementId,
// -{ number } height height of the elementId,
// -{ number } i_width height of the subitem,
// -{ number } i_height width of the subitem,
// -{ bool } blink wether to do modify class for blinking,
// -{ bool } layout wether to trigger layout computation,
resize_grid : function(message) {
  var ele = document.getElementById(message.eleId);
  ele.style.width = message.width + 'px';
  ele.style.height = message.height + 'px';
  var items = ele.querySelectorAll('.muuri-item');
  items.forEach(x => {
    x.style.width = message.i_width + 'px';
    x.style.height = message.i_height + 'px';
  })
  if(message.blink) {
    ele.classList.add('resize_grid');
    setTimeout(function() { ele.classList.remove('resize_grid'); }, 1000);
  }
  if(message.layout) if(IFCshiny.hasOwnProperty('grid')) IFCshiny.grid.refreshItems().layout();
},

// @param { string } message - elementId of the rgl plot
get3DSelection : function(message){
	var ele = document.getElementById(message);
  if(ele == null) return Shiny.onInputChange('IFCshiny_get3DSelection_ret', { rand: Math.random(), obj: [] });
  if(ele.rglinstance == null) return Shiny.onInputChange('IFCshiny_get3DSelection_ret', { rand: Math.random(), obj: [] });
	return Shiny.onInputChange('IFCshiny_get3DSelection_ret', { rand: Math.random(), obj: ele.rglinstance.scene.crosstalk.selection });
},

// @param { string } message - elementId of the rgl plot
clear3DBrush : function(message) {;
  var ele = document.getElementById(message);
  if(ele == null) return null;
  if(ele.rglinstance == null) return null;
  ele.rglinstance.clearBrush(null);
  ele.rglinstance.recordSelection(0);
},

//@param { Object } message - Object with :,
// -{ string } eleId elementId of the rgl toggle checkbox,
// -{ string } widgetId elementId of the rgl widget to control,
// -{ string } objIds rglIds to show/hide,
// -{ string } value whether to show / hide,
// -{ string } label name of the widget ,
updateToggle3D : function(message) {
  var ele = document.getElementById(message.eleId);
  if(ele == null) return null;
  if(!ele.classList.contains('rgl_toggle')) return null;
  var changed = false;
  if(message.hasOwnProperty('value')) {
    if(ele.checked != message.value) {
      changed = true;
      ele.checked = message.value;
    }
  }
  if(message.hasOwnProperty('widgetId')) {
    if(ele.getAttribute('widgetId') != message.widgetId) {
      changed = true;
      ele.setAttribute('widgetId', message.widgetId);
    }
  }
  if(message.hasOwnProperty('objIds')) {
    if(ele.getAttribute('objIds') != message.objIds) {
      changed = true;
      ele.setAttribute('objIds', message.objIds);
    }
  }
  if(message.hasOwnProperty('label')) {
    var lab = $(ele).parent().find('span');
    if(lab.text() != message.label) {
      changed = true;
      lab.text(message.label);
    }
  }
  if(changed) $(ele).trigger('change');
},

//@param { Object } message - Object with :,
// -{ string } widgetId elementId of the rgl widget to control,
// -{ string } mode - name of mode
// -{ number } button - button number (0 to 4)
// -{ number } stayActive - if truthy, don't clear brush
changeMouseMode: function(message) {
  var wdg = document.getElementById(message.widgetId);
  if(wdg == null) return undefined;
  var rgl = wdg.rglinstance;
  if(rgl == null) return undefined;
  rgl.setMouseMode(message.mode, message.button, rgl.subsceneid, message.stayActive);
},

hover3D: function(message) {
  var wdg = document.getElementById(message.widgetId);
  if(wdg == null) return undefined;
  var mousemoveHandler = function(event) {
    var rgl = this.rglinstance;
    var subid = rgl.subsceneid;
    var subscene = rgl.getObj(subid);
        if(!rgl.hover_wait & !rgl.drawing) {
    if(!rgl.hover_sel) rgl.hover_sel = '-1';
    var mouse, viewport, rect, objids, m_dist, sel = [];
    rect = rgl.canvas.getBoundingClientRect();
    mouse = { x: event.clientX - rect.left, y: event.clientY - rect.top };
    rgl.select.subscene = subid;
    viewport = rgl.vp;
    rgl.setmvMatrix(subid);
    rgl.setprMatrix(subid);
    rgl.setprmvMatrix();
    objids = subscene.objects; 
    m_dist = Number.POSITIVE_INFINITY;
    for (var i = 0, j, id, keys, obj; i < objids.length; i++) {
      id = objids[i];
      j = rgl.scene.crosstalk.id.indexOf(id);
      if(j >= 0) {
        keys = rgl.scene.crosstalk.key[j];
        obj = rgl.getObj(id);
        for (var k = 0, x, y, z, v, w, dist; k < keys.length; k++) {
          v = [].concat(obj.vertices[k]).concat(1.0);
          w = rglwidgetClass.multVM(v, rgl.prmvMatrix);
          x = w[0]/w[3];
          y = w[1]/w[3];
          z = w[2]/w[3];
          z = (1 + z) * 0.5;
          x = 0.5 * viewport.width  * (x + 1) + viewport.x;
          y = 0.5 * viewport.height * (1 - y) - viewport.y;
          dist = Math.sqrt(Math.pow(mouse.x - x, 2.0) + Math.pow(mouse.y - y, 2.0));
          if(z > 0 && z < 1) {
            if(dist < m_dist) {
              m_dist = dist;
              sel = [j,keys[k],x,y,v[0],v[1],v[2]];
            }
          }
        }
      }
    }
    if(sel.length > 0) {
      if(sel[1] !== rgl.hover_sel) {
        var to_remove = this.querySelector('.plot3D-hover');
        if(to_remove !== null) to_remove.remove();
        var txt = !message.with_img | !rgl.scene.meta ? "" : rgl.scene.meta[sel[1]];
        this.insertAdjacentHTML('afterbegin', "<div style='position:fixed; z-index:100; left:" + (sel[2] + rect.left) + "px; top:" + (sel[3] + rect.top) + "px; transform:translate(5px, 5px);' class='plot3D-hover'>" + txt + "<p style='position:absolute; top:-15px; left:0; background-color: black; color: white;'>" + sel[1] + "</p><div style='display: block; position:absolute; transform:translate(-10px,-10px); width:0; height:0; border-style:solid; border-width:5px; border-radius:5px; border-color:#f5aa22;'></div></div>");
        var div = this.querySelector('.plot3D-hover')
        Shiny.onInputChange('rgl_3D_hover', { x:sel[2], y:sel[3], text:sel[1], vertex:{x:sel[4],y:sel[5],z:sel[6] } } );
        rgl.hover_sel = sel[1];
      }
    }
  }
    return undefined;
  };
  wdg.addEventListener('wheel', function( e ) {
    var to_remove = this.querySelector('.plot3D-hover');
    if(to_remove !== null) to_remove.remove();
  });
  wdg.addEventListener('mouseup', function( e ) {
    wdg.rglinstance.hover_wait = false;
  });
  wdg.addEventListener('mousedown', function( e ) {
    var to_remove = this.querySelector('.plot3D-hover');
    if(to_remove !== null) to_remove.remove();
    wdg.rglinstance.hover_wait = true;
  }, true);
  wdg.addEventListener('mouseleave', function( e ) {
    wdg.removeEventListener('mousemove', mousemoveHandler);
    var to_remove = this.querySelector('.plot3D-hover');
    if(to_remove !== null) to_remove.remove();
  });
  wdg.addEventListener('mouseover', function( e ) {
    var rgl = this.rglinstance;
    if(rgl == null) return undefined;
    var subid = rgl.subsceneid;
    if(subid == null) return undefined;
    if(Object.keys(rgl.scene.objects).indexOf("" + subid) < 0) return undefined;
    var subscene = rgl.getObj(subid);
    if(subscene == null) return undefined;
    if(Object.values(subscene.par3d.mouseMode).indexOf("selecting") > -1) return undefined;
    wdg.addEventListener('mousemove', mousemoveHandler);
  });
},

computeCheckboxSelection : function(message){
	var selected = [];
	if(Shiny.shinyapp.$inputValues[''+ message.inputId +''] != null) selected = [].concat(Shiny.shinyapp.$inputValues[''+ message.inputId +'']); // use former input$cell_selected if exists
  selected = selected.concat($(''+ message.selector + ':checked').map(function() { return $(this).data('value') }).get()); // append values that were not stored but selected
  if($(''+ message.selector +'').length != 0) selected = selected.filter(function(x) { return !($(''+ message.selector +'').filter(function() { return !this.checked } ).map(function() { return $(this).data('value') }).get().includes(x)) }); // remove values that are stored but deselected
  selected = [...new Set(selected)];
  //Shiny.shinyapp.$inputValues[''+ message.inputId +''] = selected;
  Shiny.onInputChange(message.inputId, selected);
  return selected;// remove duplicates
},

updateSelection : function(message) {
  return Shiny.onInputChange(message.inputId, IFCshiny.computeCheckboxSelection(message));
},

overwriteSelection : function(message) {
  var selected = [];
  var alw = [].concat(message.allowed);
  if(Shiny.shinyapp.$inputValues[''+ message.inputId +''] != null) {
    selected = Shiny.shinyapp.$inputValues[''+ message.inputId +''] // remove values that are not allowed
    var alw = [].concat(message.allowed);
    selected = alw.filter(x => selected.includes(x));
    selected =  [...new Set(selected)];
  } 
  Shiny.onInputChange(message.inputId, selected);
  return selected; 
},

modalGetSelection : function(message){
	return Shiny.onInputChange('IFCshiny_modalGetSelection_ret', { rand: Math.random(), obj: IFCshiny.computeCheckboxSelection(message), from: message.from });
},

getTaggedSelection : function(message){
	return Shiny.onInputChange('IFCshiny_getTaggedSelection_ret', { rand: Math.random(), obj: IFCshiny.computeCheckboxSelection(message) });
},

modifyTaggedSelection : function(message){
	return Shiny.onInputChange('IFCshiny_modifyTaggedSelection_ret', { rand: Math.random(), obj: IFCshiny.computeCheckboxSelection(message) });
},

// shape edition: anchor removal
remove_anchor : function(event) {
  event.preventDefault();
  var ele = event.target;
  var par = ele.parentNode;
  var shape = par.querySelector('.shape');
  if(shape.tagName === 'polygon') {
    var anchors = par.getElementsByClassName('anchor');
    if(anchors.length <= 3) return null;
    par.removeChild(ele);
    var x_anc = new Array(anchors.length);
    var y_anc = new Array(anchors.length);
    var newpoints = '';
    for(var i = 0; i < anchors.length; i++) {
      x_anc[i] = parseFloat(anchors[i].getAttribute('cx'));
      y_anc[i] = parseFloat(anchors[i].getAttribute('cy'));
      newpoints = newpoints + x_anc[i] + ',' + y_anc[i] + ' ';
    }
    shape.setAttribute('points', newpoints.slice(0, -1));
    var label = par.querySelector('.label');
    Shiny.onInputChange('shape_selected', { name: par.id, init: true, x: x_anc, y: y_anc, cx: label.getAttribute('x'), cy: label.getAttribute('y')} );
  }
  return null;
},

// shape drawing: line, rectangle, ellipse and lasso
draw_shape : function(event) {
  event.preventDefault();
  var bkg = event.target.parentNode.querySelector('.draw_shape');
  if(bkg == null) return null;
  var shape = bkg.firstElementChild;
  if(shape == null) return null;
  var bbox = bkg.getBoundingClientRect();
  
  var ratiox = shape.getAttribute('data_ratiox');
  var ratioy = shape.getAttribute('data_ratioy');
  var xlim1 = parseFloat(shape.getAttribute('data_left')  / ratiox);
  var xlim2 = parseFloat(shape.getAttribute('data_right') / ratiox);
  var ylim1 = parseFloat(shape.getAttribute('data_top')   / ratioy);
  var ylim2 = parseFloat(shape.getAttribute('data_bottom')/ ratioy);
  var x1 = parseFloat(shape.getAttribute('data_x'));
  var y1 = parseFloat(shape.getAttribute('data_y'));
  
  var x = Math.max(Math.min(xlim2, event.clientX - bbox.left), xlim1);
  var y = Math.max(Math.min(ylim2, event.clientY - bbox.top ), ylim1);
  
  switch(shape.tagName) {
    case 'rect':
      var newx = Math.min(x, x1);
      var newy = Math.min(y, y1);
      shape.setAttribute('x', newx);
      shape.setAttribute('y', newy);
      shape.setAttribute('width', Math.max(Math.abs(newx - x1), Math.abs(x - x1)));
      shape.setAttribute('height', Math.max(Math.abs(newy - y1), Math.abs(y - y1)));
      Shiny.onInputChange('shape_param', { tool:'rect', init:true, x1: x1, y1: y1, x2: x, y2: y} );
    break;
    
    case 'line':
      shape.setAttribute('y1', y);
      shape.setAttribute('x2', x);
      shape.setAttribute('y2', y);
      Shiny.onInputChange('shape_param', { tool:'line', init:true, x1: x1, y1: y1, x2: x, y2: y} );
    break;
    
    case 'ellipse':
      var rx = Math.min(Math.min(x1 - xlim1, xlim2 - x1), Math.abs(x - x1));
      var ry = Math.min(Math.min(y1 - ylim1, ylim2 - y1), Math.abs(y - y1));
      shape.setAttribute('rx', rx);
      shape.setAttribute('ry', ry);
      Shiny.onInputChange('shape_param', { tool:'ellipse', init:true, x1: x1 - rx, y1: y1 - ry, x2: x1 + rx, y2: y1 + ry} );
    break;
    
    case 'polygon':
      var lastx = parseFloat(shape.getAttribute('data_lastx'));
      var lasty = parseFloat(shape.getAttribute('data_lasty'));
      if(((x - lastx) * (x - lastx) + (y - lasty) * (y - lasty)) > 225) {
        shape.setAttribute('data_lastx', x);
        shape.setAttribute('data_lasty', y);
        shape.setAttribute('points', shape.getAttribute('points') + " " + x + "," + y);
        Shiny.onInputChange('shape_param', { tool:'lasso', init:true, points: shape.getAttribute('points')} );
      }
    break;
  }
  return null;
},

// shape drawing: polygon
draw_path : function(event) {
  event.preventDefault();
  var bkg = event.target.parentNode.querySelector('.draw_shape');
  if(bkg == null) return null;
  var shape = bkg.firstElementChild;
  if(shape == null) return null;
  var bbox = bkg.getBoundingClientRect();
  
  var ratiox = shape.getAttribute('data_ratiox');
  var ratioy = shape.getAttribute('data_ratioy');
  var xlim1 = parseFloat(shape.getAttribute('data_left')  / ratiox);
  var xlim2 = parseFloat(shape.getAttribute('data_right') / ratiox);
  var ylim1 = parseFloat(shape.getAttribute('data_top')   / ratioy);
  var ylim2 = parseFloat(shape.getAttribute('data_bottom')/ ratioy);
  
  var x = Math.max(Math.min(xlim2, event.clientX - bbox.left), xlim1);
  var y = Math.max(Math.min(ylim2, event.clientY - bbox.top), ylim1);
  
  shape.setAttribute('d', shape.getAttribute('d').slice(0, -1) + "L" + x + "," + y + "Z");
  Shiny.onInputChange('shape_param', { tool:'polygon', init:true, d: shape.getAttribute('d')} );
  return null;
}
}

Shiny.addCustomMessageHandler("getOffset", IFCshiny.getOffset );
Shiny.addCustomMessageHandler('refresh', IFCshiny.refresh );
Shiny.addCustomMessageHandler('reach', IFCshiny.reach );
Shiny.addCustomMessageHandler('init_grid', IFCshiny.init_grid );
Shiny.addCustomMessageHandler('resize_grid', IFCshiny.resize_grid );
Shiny.addCustomMessageHandler("get3DSelection", IFCshiny.get3DSelection );
Shiny.addCustomMessageHandler('clear3DBrush', IFCshiny.clear3DBrush );
Shiny.addCustomMessageHandler('updateToggle3D', IFCshiny.updateToggle3D);
Shiny.addCustomMessageHandler('changeMouseMode', IFCshiny.changeMouseMode );
Shiny.addCustomMessageHandler('hover3D', IFCshiny.hover3D );
Shiny.addCustomMessageHandler("updateSelection", IFCshiny.updateSelection );
Shiny.addCustomMessageHandler("overwriteSelection", IFCshiny.overwriteSelection );
Shiny.addCustomMessageHandler("modalGetSelection", IFCshiny.modalGetSelection );
Shiny.addCustomMessageHandler("getTaggedSelection", IFCshiny.getTaggedSelection );
Shiny.addCustomMessageHandler("modifyTaggedSelection", IFCshiny.modifyTaggedSelection );

// shape edition: position modification
interact('.shape.draggable').draggable({
  modifiers: [
    interact.modifiers.restrictRect({
      restriction: '.shapes_clippath'
    })
  ],
  cursorChecker () {
    // don't set a cursor for drag actions
      return null;
    },
  listeners: {
    start (event) {
      Shiny.onInputChange('shape_moving', event.target.parentNode.id);
      console.log(event.type + ': ' + event.target.parentNode.parentNode.parentNode.parentNode.id + ', '+ event.target.tagName + '[' + event.target.parentNode.id + ']');
    },
    move (event) {
    $(event.target).css('cursor','grabbing');
    var ele = event.target;
    var sib = $(ele).siblings();
    var anchors = sib.filter('.anchor');
    var clip = $(ele.parentNode.parentNode.children).filter('clipPath').children();
    var minx = parseFloat(clip.attr('x'));
    var miny = parseFloat(clip.attr('y'));
    var maxx = minx + parseFloat(clip.attr('width'));
    var maxy = miny + parseFloat(clip.attr('height'));
    var dx = event.dx;
    var dy = event.dy;
    var i;
    for(i = 0; i < anchors.length; i++) {
    var newx = parseFloat(anchors[i].getAttribute('cx')) + event.dx;
    var newy = parseFloat(anchors[i].getAttribute('cy')) + event.dy;
    if(newx > maxx) {
    dx = parseFloat(anchors[i].getAttribute('cx')) - maxx;
    } else {
    if(newx < minx) dx = parseFloat(anchors[i].getAttribute('cx')) - minx;
    }
    if(newy > maxy) {
    dy = parseFloat(anchors[i].getAttribute('cy')) - maxy;
    } else {
    if(newy < miny) dy = parseFloat(anchors[i].getAttribute('cy')) - miny;
    }
    }
    for(i = 0; i < anchors.length; i++) {
    anchors[i].setAttribute('cx', parseFloat(anchors[i].getAttribute('cx')) + dx);
    anchors[i].setAttribute('cy', parseFloat(anchors[i].getAttribute('cy')) + dy);
    }
    switch(ele.tagName) {
    case 'rect':
        ele.setAttribute('x', Math.min(parseFloat(anchors[0].getAttribute('cx')), parseFloat(anchors[1].getAttribute('cx'))));
        ele.setAttribute('y', Math.min(parseFloat(anchors[0].getAttribute('cy')), parseFloat(anchors[1].getAttribute('cy'))));
    break;
        
    case 'polygon':
        var newpoints = '';
        for(i = 0; i < anchors.length; i++) {
        newpoints += parseFloat(anchors[i].getAttribute('cx')) + ',' + parseFloat(anchors[i].getAttribute('cy')) + ' ';
        }
        ele.setAttribute('points', newpoints.slice(0, -1));
    break;
    
    case 'ellipse':
        ele.setAttribute('cx', (parseFloat(anchors[0].getAttribute('cx')) + parseFloat(anchors[1].getAttribute('cx')))/2);
        ele.setAttribute('cy', (parseFloat(anchors[0].getAttribute('cy')) + parseFloat(anchors[1].getAttribute('cy')))/2);
        break;
        
    case 'line':
        ele.setAttribute('x1', parseFloat(anchors[0].getAttribute('cx')));
        ele.setAttribute('x2', parseFloat(anchors[1].getAttribute('cx')));
        ele.setAttribute('y1', parseFloat(anchors[0].getAttribute('cy')));
        ele.setAttribute('y2', parseFloat(anchors[1].getAttribute('cy')));
        break;
    }
    
    },
    end (event) {
    Shiny.onInputChange('shape_moving', null);
    var ele = event.target;
    var sib = $(ele).siblings();
    var anchors = sib.filter('.anchor');
    var label = sib.filter('.label')[0];
    var x_anc = new Array(anchors.length);
    var y_anc = new Array(anchors.length);
    for(var i = 0; i < anchors.length; i++) {
      x_anc[i] = parseFloat(anchors[i].getAttribute('cx'));
      y_anc[i] = parseFloat(anchors[i].getAttribute('cy'));
    }
    Shiny.onInputChange('shape_selected', { name:event.target.parentNode.id, init:true, x: x_anc, y: y_anc, cx: label.getAttribute('x'), cy: label.getAttribute('y') } );
    console.log(event.type + ': ' + event.target.parentNode.parentNode.parentNode.parentNode.id + ', ' + event.target.tagName + '[' + event.target.parentNode.id + ']');
    }
  }
});

// shape edition: label position modification
interact('.label.draggable').draggable({
  modifiers: [
    interact.modifiers.restrictRect({
      restriction: '.shapes_clippath'
    })
  ],
  cursorChecker () {
    // don't set a cursor for drag actions
      return null;
    },
  listeners: {
    start (event) {
      Shiny.onInputChange('shape_moving', event.target.parentNode.id);
      console.log(event.type + ': ' + event.target.parentNode.parentNode.parentNode.parentNode.id + ', '+ event.target.tagName + '[' + event.target.parentNode.id + ']');
    },
    move (event) {
    $(event.target).css('cursor','grabbing');
    var ele = event.target;
    var bbox = ele.getBoundingClientRect();
    var clip = $(ele.parentNode.parentNode.children).filter('clipPath').children();
    var minx = parseFloat(clip.attr('x'));
    var miny = parseFloat(clip.attr('y')) + bbox.height;
    var maxx = minx + parseFloat(clip.attr('width')) - bbox.width;
    var maxy = miny + parseFloat(clip.attr('height')) - bbox.height;
    var newx = parseFloat(ele.getAttribute('x')) + event.dx;
    var newy = parseFloat(ele.getAttribute('y')) + event.dy;
    newx = newx > maxx ? maxx : newx;
    newx = newx < minx ? minx : newx;
    newy = newy > maxy ? maxy : newy;
    newy = newy < miny ? miny : newy;
    ele.setAttribute('x', newx);
    ele.setAttribute('y', newy);
    },
    end (event) {
    Shiny.onInputChange('shape_moving', null);
    var ele = event.target;
    var sib = $(ele).siblings();
    var anchors = sib.filter('.anchor');
    var x_anc = new Array(anchors.length);
    var y_anc = new Array(anchors.length);
    for(var i = 0; i < anchors.length; i++) {
      x_anc[i] = parseFloat(anchors[i].getAttribute('cx'));
      y_anc[i] = parseFloat(anchors[i].getAttribute('cy'));
    }
    Shiny.onInputChange('shape_selected', { name:event.target.parentNode.id, init:true, x: x_anc, y: y_anc, cx: ele.getAttribute('x'), cy: ele.getAttribute('y') } );
    console.log(event.type + ': ' + event.target.parentNode.parentNode.parentNode.parentNode.id + ', ' + event.target.tagName + '[' + event.target.parentNode.id + ']');
    }
  }
});

// shape edition: anchor position modification
interact('.anchor.draggable').draggable({
      modifiers: [
      interact.modifiers.restrictRect({
        restriction: '.shapes_clippath'
      })
    ],
  listeners: {
    start (event) {
      Shiny.onInputChange('shape_moving', event.target.parentNode.id);
      console.log(event.type + ': ' + event.target.parentNode.parentNode.parentNode.parentNode.id + ', ' + event.target.id + '[' + event.target.parentNode.id + ']');
    },
    move (event) {
    $(event.target).css('cursor','grabbing');
    var ele = event.target;
    var par = $(ele).parent();
    var kids = par.children();
    var sib = $(ele).siblings();
    var shape = sib.filter('.shape');
    var anchors = kids.filter('.anchor');
    var next_anchors = sib.filter('.anchor');
    var clip = $(ele.parentNode.parentNode.children).filter('clipPath').children();
    var minx = parseFloat(clip.attr('x'));
    var miny = parseFloat(clip.attr('y'));
    var maxx = minx + parseFloat(clip.attr('width'));
    var maxy = miny + parseFloat(clip.attr('height'));
    
    var x = parseFloat(ele.getAttribute('cx'));
    var y = parseFloat(ele.getAttribute('cy'));
    var xx = x + event.dx;
    var yy = y + event.dy;
    var i;
    if(xx > maxx) {
    xx = maxx;
    } else {
    if(xx < minx) xx = minx;
    }
    if(yy > maxy) {
    yy = maxy;
    } else {
    if(yy < miny) yy = miny;
    }
    ele.setAttribute('cx', xx);
    ele.setAttribute('cy', yy);
    
    switch(shape.prop('tagName')) {
    case 'rect':
        var x_anc = new Array(next_anchors.length);
        var y_anc = new Array(next_anchors.length);
        for(i = 0; i < next_anchors.length; i++) {
        x_anc[i] = parseFloat(next_anchors[i].getAttribute('cx'));
        y_anc[i] = parseFloat(next_anchors[i].getAttribute('cy'));
        }
        var newx = Math.min(xx, x_anc[0]);
        var newy = Math.min(yy, y_anc[0]);
        shape.attr('x', newx);
        shape.attr('y', newy);
        shape.attr('width', Math.max(Math.abs(newx - x_anc[0]), Math.abs(xx - x_anc[0])));
        shape.attr('height', Math.max(Math.abs(newy - y_anc[0]), Math.abs(yy - y_anc[0])));
    break;
    
    case 'polygon':
        var newpoints = '';
        for(i = 0; i < anchors.length; i++) {
        newpoints += parseFloat(anchors[i].getAttribute('cx')) + ',' + parseFloat(anchors[i].getAttribute('cy')) + ' ';
        }
        shape.attr('points', newpoints.slice(0, -1));
    break;
    
    case 'ellipse':
        var x_anc = new Array(anchors.length);
        var x_sum = 0;
        var y_anc = new Array(anchors.length);
        var y_sum = 0;
        for(i = 0; i < anchors.length; i++) {
        x_anc[i] = parseFloat(anchors[i].getAttribute('cx'));
        x_sum += x_anc[i];
        y_anc[i] = parseFloat(anchors[i].getAttribute('cy'));
        y_sum += y_anc[i];
        }
        shape.attr('cx', x_sum/anchors.length);
        shape.attr('cy', y_sum/anchors.length);
        shape.attr('rx', Math.max(x_anc[1], x_anc[0]) - x_sum/anchors.length);
        shape.attr('ry', Math.max(y_anc[1], y_anc[0]) - y_sum/anchors.length);
    break;
    
    case 'line':
        next_anchors.attr('cy', yy);
        shape.attr('y1', yy);
        shape.attr('y2', yy);
        shape.attr('x1', parseFloat(anchors[0].getAttribute('cx')));
        shape.attr('x2', parseFloat(anchors[1].getAttribute('cx')));
    break;
    }
    },
    end (event) {
    var ele = event.target;
    
    var par = $(ele).parent();
    var kids = par.children();
    var anchors = kids.filter('.anchor');
    var label = kids.filter('.label')[0];
    var x_anc = new Array(anchors.length);
    var y_anc = new Array(anchors.length);
    for(var i = 0; i < anchors.length; i++) {
      x_anc[i] = parseFloat(anchors[i].getAttribute('cx'));
      y_anc[i] = parseFloat(anchors[i].getAttribute('cy'));
    }
    Shiny.onInputChange('shape_moving', null);
    Shiny.onInputChange('shape_selected', { name:event.target.parentNode.id, init:true, x: x_anc, y: y_anc, cx: label.getAttribute('x'), cy: label.getAttribute('y')} );
    console.log(event.type + ': ' + event.target.parentNode.parentNode.parentNode.parentNode.id + ', ' + event.target.id + '[' + event.target.parentNode.id + ']');
    }
  }
});
