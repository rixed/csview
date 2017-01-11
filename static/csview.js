// JS functions for csview

/* For each graph, get the current t1 and t2 */

function foreach(a, f) {
  for (var i = 0; i < a.length; i++) {
    f(a[i]);
  }
}

function Graph(obj) {
  that = this;
  that.obj = obj;
  that.width = Number(obj.width);
  var url = obj.src;
  var a = document.createElement('a');
  a.href = url;
  that.url_prefix = a.origin + a.pathname +'?';

  var params = a.search.slice(1). // removes initial '?'
               split('&');
  foreach(params, function(p) {
    var vs = p.split('=', 2);
    if (vs[0] == 'g') {
      that.idx = Number(vs[1]);
    } else if (vs[0] == 't1') {
      that.t1 = Number(vs[1]);
    } else if (vs[0] == 't2') {
      that.t2 = Number(vs[1]);
    }
  });
}
Graph.prototype.src = function () {
  var search = 'g='+ this.idx +'&t1='+ this.t1 +'&t2='+ this.t2;
  return this.url_prefix + search;
}

var graphs = [];
var redrawTimeout = 200; // ms

function init() {
  // Get some in fo about the graphs that are present on this page

  for (var i = 0; ; i++) {
    var obj = document.getElementById("graph_" + i);
    if (null == obj) break;
    var graph = new Graph(obj);
    graphs[graph.idx] = graph;
  }
  // The more graphs, the slower the timeout. TODO: adjust dynamically!
  redrawTimeout *= graphs.length;

  // Install mouse handlers for dragging

  foreach(graphs, function(g) {
    /* First of all drag n drop needs to be disabled otherwise we
     * would stop receiving events once the image is in drag mode: */
    g.obj.ondragstart = function () { return false; }

    g.obj.onmousedown = function (ev) {
      startDragX = ev.clientX;
      stopDragX = startDragX;
      dragG = g;
    };
    g.obj.onmouseup = function (ev) {
      stopDragX = ev.clientX;
      if (dragTimeoutId != null) {
        clearTimeout(dragTimeoutId);
        dragTimeoutId = null;
      }
      doScroll();
      startDragX = null;
      stopDragX = null;
      dragG = null;
    };
    g.obj.onmousemove = function (ev) {
      stopDragX = ev.clientX;
      if (dragTimeoutId == null) {
        dragTimeoutId = setTimeout(doScroll, redrawTimeout);
      }
    };

    /* Zoom in/out */
    g.obj.onwheel = function (ev) {
      if (ev.target.id != g.obj.id) return;
      if (ev.ctrlKey) {
        ev.preventDefault();
        ev.stopImmediatePropagation();
      }
      if (zoomG != g) {
        zoomG = g;
        zoom = 0;
      }
      if ('wheelDelta' in ev) {
        zoom += ev.wheelDelta;
      } else if ('deltaZ' in ev) {
        zoom += ev.deltaZ;
      }
      if (zoomTimeoutId == null) {
        zoomTimeoutId = setTimeout(doZoom, redrawTimeout);
      }
    };
  });
}

var startDragX, stopDragX; // where the mouse drag started
var dragG;  // graph to scroll
var dragTimeoutId;
var zoomG;
var zoom;
var zoomTimeoutId;

function doScroll() {
  dragTimeoutId = null;

  if (null == dragG ||
      null == startDragX ||
      null == stopDragX ||
      startDragX == stopDragX) return;
  
  var dx = stopDragX - startDragX;
  var dt_tot = dragG.t2 - dragG.t1;
  var dt = - dt_tot * dx / (0.8 * dragG.width); // account for the tick marks
  dragG.t1 += dt;
  dragG.t2 += dt;
  dragG.obj.src = dragG.src();
  startDragX = stopDragX;
}

function doZoom() {
  zoomTimeoutId = null;

  if (null == zoomG ||
      zoom == 0) return;

  var mid_t = (zoomG.t1 + zoomG.t2) * 0.5;
  var dt = (zoomG.t2 - zoomG.t1) * 0.5;
  var zoomRatio = 0.0001;
  var new_dt = zoom > 0 ?
    dt / (1 + zoom * zoomRatio) :
    dt * (1 - zoom * zoomRatio);
  zoomG.t1 = mid_t - new_dt;
  zoomG.t2 = mid_t + new_dt;
  zoom = 0;
  zoomG.obj.src = zoomG.src();
}

