
   function isDef(v) { return typeof v != 'undefined'; }

// Initialize handling of keyboard and mouse events on a canvas:

   function initEventHandlers(canvas) {
      function getHandle(canvas) { return window[canvas.id]; }

      var handle = getHandle(canvas);
      handle.mouseX = 0;
      handle.mouseY = 0;
      handle.mousePressed = false;

      canvas.onkeydown = function(event) {
          var handle = getHandle(this);
	  if (isDef(handle.keyDown)) {
              event = event || window.event;
	      handle.keyDown(event.keyCode);
          }
      }

      canvas.onkeyup = function(event) {
          var handle = getHandle(this);
	  if (isDef(handle.keyUp)) {
              event = event || window.event;
	      handle.keyUp(event.keyCode);
          }
      }

      canvas.onkeypress = function(event) {
          var handle = getHandle(this);
	  if (isDef(handle.keyPress)) {
              event = event || window.event;
	      handle.keyPress(event.keyCode);
          }
      }

      canvas.onmousedown = function(event) {
          var handle = getHandle(this);
          var r = event.target.getBoundingClientRect();
          handle.mouseX = event.clientX - r.left;
          handle.mouseY = event.clientY - r.top;
	  handle.mousePressed = true;

          if ( handle.mouseX >= 0 && handle.mouseX <= r.right - r.left &&
	       handle.mouseY >= 0 && handle.mouseY <= r.bottom - r.top &&
	       isDef(handle.mouseDown))
	     handle.mouseDown(handle.mouseX, handle.mouseY);
      };

      canvas.onmouseup = function(event){ // Mouse is released
          var handle = getHandle(this);
          var r = event.target.getBoundingClientRect();
          handle.mouseX = event.clientX - r.left;
          handle.mouseY = event.clientY - r.top;
	  handle.mousePressed = false;

          if (isDef(handle.mouseUp))
             handle.mouseUp(handle.mouseX, handle.mouseY);
      }

      canvas.onmousemove = function(event) { // Mouse is moved
          var handle = getHandle(this);
          var r = event.target.getBoundingClientRect();
          handle.mouseX = event.clientX - r.left;
          handle.mouseY = event.clientY - r.top;

	  if (handle.mousePressed) {
	     if (isDef(handle.mouseDrag))
	         handle.mouseDrag(handle.mouseX, handle.mouseY);
          }
	  else if (handle.mouseX >= 0 && handle.mouseX <= r.right - r.left &&
	           handle.mouseY >= 0 && handle.mouseY <= r.bottom - r.top &&
	           isDef(handle.mouseMove))
	     handle.mouseMove(handle.mouseX, handle.mouseY);
      }
   }

// Wrapper for matrix functions.

   function push() { _g.save(); }
   function pop() { _g.restore(); }
   function identity() { _g.setTransform(1,0,0,0,1,0); }
   function translate(x, y) { _g.translate(x, y); }
   function rotate(a) { _g.rotate(a); }
   function scale(x, y) { if (! isDef(y)) y = x; _g.scale(x,y); }

// Math utilities

   var PI = Math.PI;
   function cos(t) { return Math.cos(t); }
   function dot(a, b) { return a[0]*b[0] + a[1]*b[1] + a[2]*b[2]; }
   function floor(t) { return Math.floor(t); }
   function lerp(t, a, b) { return a + t * (b - a); }
   function sCurve(t) { return t * t * (3 - t - t); }
   function sin(t) { return Math.sin(t); }

// Wrappers for drawing functions.

   function lineWidth(w) {
      if (isDef(w))
         _g.lineWidth = w;
      return _g.lineWidth;
   }

   function arrow(ax, ay, bx, by, r) {
      if (! isDef(r))
         r = 10;

      var angle = Math.atan2(bx - ax, by - ay);
      var x = r * Math.cos(angle), y = r * Math.sin(angle);

      _g.beginPath();
      _g.moveTo(ax, ay);
      _g.lineTo(bx, by);
      _g.stroke();

      _g.moveTo(bx - x - y, by + y - x);
      _g.lineTo(bx, by);
      _g.lineTo(bx + x - y, by - y - x);
      _g.stroke();
   }

   function line(ax, ay, bx, by) {
      _g.beginPath();
      _g.moveTo(ax, ay);
      _g.lineTo(bx, by);
      _g.stroke();
   }

   function color(red, grn, blu) {
      _g.strokeStyle = _g.fillStyle = _color(red, grn, blu);
   }

   function fill(red, grn, blu) {
      _g.fillStyle = _color(red, grn, blu);
   }

   function _color(red, grn, blu) {
      return typeof ! isDef(grn) ? red : "rgba(" + red + "," + grn + "," + blu + ",255)";
   }

   function drawPolygon(p, x, y, r, isOpen) {
      makePath(p, x, y, r, isOpen);
      _g.stroke();
   }

   function fillPolygon(p, x, y, r) {
      makePath(p, x, y, r);
      _g.fill();
   }

   function drawRect(x, y, w, h) {
      makeRectPath(x, y, w, h);
      _g.stroke();
   }

   function fillRect(x, y, w, h) {
      makeRectPath(x, y, w, h);
      _g.fill();
   }

   function drawOval(x, y, w, h, n) {
      makeOvalPath(x, y, w, h, n);
      _g.stroke();
   }

   function fillOval(x, y, w, h, n) {
      makeOvalPath(x, y, w, h, n);
      _g.fill();
   }

   function drawDiamond(x, y, w, h) {
      makeDiamondPath(x, y, w, h);
      _g.stroke();
   }

   function fillDiamond(x, y, w, h) {
      makeDiamondPath(x, y, w, h);
      _g.fill();
   }

   function drawOctagon(x, y, w, h) {
      makeOctagonPath(x, y, w, h);
      _g.stroke();
   }

   function fillOctagon(x, y, w, h) {
      makeOctagonPath(x, y, w, h);
      _g.fill();
   }

   function makeRectPath(x, y, w, h) {
      makePath([ [x,y],[x+w,y], [x+w,y+h], [x,y+h] ]);
   }

   function makeDiamondPath(x, y, w, h) {
      makePath([ [x,y+h/2],[x+w/2,y], [x+w,y+h/2],[x+w/2,y+h] ]);
   }

   function makeOctagonPath(x, y, w, h) {
      var x1 = x+w/4, x2 = x+3*w/4, x3 = x + w,
          y1 = y+h/4, y2 = y+3*h/4, y3 = y + h;
      makePath([ [x,y1], [x1,y], [x2,y], [x3,y1], [x3,y2], [x2,y3], [x1,y3], [x,y2] ]);
   }

   function makeOval(x, y, w, h, n, angle0, angle1) {
      if (! isDef(n))
         n = 32;
      if (! isDef(angle0))
         angle0 = 0;
      if (! isDef(angle1))
         angle1 = 2 * Math.PI;

      var xy = new Array();
      for (var i = 0 ; i < n ; i++) {
         var theta = angle0 + (angle1 - angle0) * i / (n-1);
	 xy[i] = [x + w/2 + w/2 * Math.cos(theta),
                  y + h/2 - h/2 * Math.sin(theta)];
      }
      return xy;
   }

   function makeOvalPath(x, y, w, h, n) {
      makePath(makeOval(x, y, w, h, n));
   }

   function makePath(p, x, y, r, isOpenPath) {
      if (! isDef(x)) x = 0;
      if (! isDef(y)) y = 0;
      if (! isDef(r)) r = 0;
      var n = p.length;
      _g.beginPath();
      if (r == 0) {
         if (! isOpenPath)
            _g.moveTo(x + p[n-1][0], y + p[n-1][1]);
         for (i = 0 ; i < n ; i++)
            _g.lineTo(x + p[i][0], y + p[i][1]);
      }
      else {
         var s = Math.sin(r);
         var c = Math.cos(r);
         if (! isOpenPath)
            _g.moveTo(x + c*p[n-1][0] + s*p[n-1][1],
                      y - s*p[n-1][0] + c*p[n-1][1]);
         for (i = 0 ; i < n ; i++)
            _g.lineTo(x + c*p[i][0] + s*p[i][1],
                      y - s*p[i][0] + c*p[i][1]);
      }
   }

   function textWidth(str) {
      return _g.measureText(str).width;
   }

   function textHeight(value) {
      if (isDef(value))
         _g.textHeight = value;
      return _g.textHeight;
   }

   function text(message, x, y, alignX, alignY) {
      if (! isDef(alignX))
         alignX = 0;
      if (! isDef(alignY))
         alignY = 1;
      _g.font = _g.textHeight + 'pt Calibri';
      _g.fillText(message, x - alignX * textWidth(message), y + (1-alignY) * textHeight());
   }

   function width() { return _g.canvas.width; }
   function height() { return _g.canvas.height; }

// Utility functions.

   function gStart() {
      var c = document.getElementsByTagName("canvas");
      for (var i = 0 ; i < c.length ; i++)
          if (c[i].getAttribute("data-render") != "gl")
	     startCanvas(c[i].id);
   }

   function startCanvas(name) {

      window.requestAnimFrame = (function(callback) {
      return window.requestAnimationFrame ||
             window.webkitRequestAnimationFrame ||
             window.mozRequestAnimationFrame ||
             window.oRequestAnimationFrame ||
             window.msRequestAnimationFrame ||
             function(callback) { window.setTimeout(callback, 1000 / 60); }; })();
      var _canvas = document.getElementById(name);
      var g = _canvas.getContext('2d');
      g.textHeight = 12;
      g.lineCap = "round";
      g.lineJoin = "round";
      g.canvas = _canvas;
      g.name = name;

      initEventHandlers(_canvas);

      if (isDef(window[g.name].setup)) {
         _g = g;
         _g.clearRect(0, 0, _g.canvas.width, _g.canvas.height);
         window[g.name].setup();
      }

      tick(g);
   }

   // Continually refresh a canvas, to support animation:

   var tick = function(g) {
      if (isDef(window[g.name].animate)) {
         time = ((new Date()).getTime() - _startTime) / 1000.0;
         _g = g;
         _g.clearRect(0, 0, _g.canvas.width, _g.canvas.height);
         window[g.name].animate();
         requestAnimFrame(function() { tick(g); });
      }
   }

   // Replace the text of an html element:

   function replaceText(id, newText) {
      document.getElementById(id).firstChild.nodeValue = newText;
   }

   // Set the document's background color:

   function setBackgroundColor(color) {
      document.body.style.background = color;
   }

   // Give "text-like" style to all the buttons of a document:

   function textlike(tagtype, textColor, hoverColor, pressColor) {
      var buttons = document.getElementsByTagName(tagtype);
      for (var i = 0 ; i < buttons.length ; i++) {
         var b = buttons[i];
         b.onmousedown = function() { this.style.color = pressColor; };
         b.onmouseup   = function() { this.style.color = hoverColor; };
         b.onmouseover = function() { this.style.color = hoverColor; };
         b.onmouseout  = function() { this.style.color = textColor; };
         b.style.border = '0px solid black';
         b.style.outline = '0px solid black';
         b.style.margin = 0;
         b.style.padding = 0;
         b.style.color = textColor;
         b.style.fontFamily = 'Helvetica';
         b.style.fontSize = '12pt';
         b.style.backgroundColor = document.body.style.background;
      }
   }

   // Object that makes a button cycle through a set of choices:

   function choice(id,      // id of the button's html tag
                   data) {  // data is an array of strings
      this.index = 0;
      this.data = (typeof data === 'string') ? data.split('|') : data;

      // The button that this choice object will control:

      var button = document.getElementById(id);

      // The button needs to know about this choice object:

      button.choice = this;

      // Initially, set the button's text to the first choice:

      button.firstChild.nodeValue = this.data[0];

      // Every click will set the button's text to the next choice:

      button.onclick = function() {
         var choice = this.choice;
         choice.index = (choice.index + 1) % choice.data.length;
	 this.firstChild.nodeValue = choice.data[choice.index];
      }
   }

   function getSpan(id) {
      return document.getElementById(id).firstChild.nodeValue;
   }

   function setSpan(id, str) {
      document.getElementById(id).firstChild.nodeValue = str;
   }

   var _g, time, _startTime = (new Date()).getTime();


