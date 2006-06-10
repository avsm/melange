
// some reason ali tse's license was causing a parse error... this code belongs to liquidx!

// -------------------------------------------------------------------------
// Check required components
// -------------------------------------------------------------------------

try {    
    if (typeof(MochiKit.Base) == 'undefined'   ||
        typeof(MochiKit.DOM) == 'undefined'    ||
        typeof(MochiKit.Color) == 'undefined'  ||
        typeof(MochiKit.Format) == 'undefined' ||
        typeof(MochiKit.Logging) == 'undefied')
    {
        throw "";    
    }
} 
catch (e) {    
    throw "canvasGraph depends on MochiKit.{Base,Color,DOM,Format,Logging}"
}

// -------------------------------------------------------------------------
// Inject Common Shortcuts we use into MochiKit.Color.Color
// -------------------------------------------------------------------------

MochiKit.Base.update(MochiKit.Color.Color.prototype, {
    lighterColor: function(alpha) {
        if (MochiKit.Base.isUndefinedOrNull(alpha)) {
            return this.blendedColor(MochiKit.Color.Color.whiteColor());
        }
        else {
            return this.blendedColor(MochiKit.Color.Color.whiteColor()).colorWithAlpha(alpha);
        }
    },

    asFillColor: function() {
        return this.lighterColor(0.5).toRGBString();
    },
        
    asStrokeColor: function() {
        return this.lighterColor(0.7).toRGBString();
    },

    asPointColor: function() {
       return this.lighterColor(0.9).toRGBString();
    }
});


// -------------------------------------------------------------------------
// Use MochiKit Style Namespace Segregation and Put CanvasGraph (and others)
// into GraphKit.
// -------------------------------------------------------------------------

if (typeof(GraphKit) == 'undefined') {
    GraphKit = {};
}

GraphKit.NAME = "GraphKit.GraphKit";
GraphKit.VERSION = "0.6";
GraphKit.__repr__ = function() {
    return "[" + this.NAME + " " + this.VERSION + "]";
};

GraphKit.toString = function() {
    return this.__repr__();
}

// -------------------------------------------------------------------------
//  Encapsulate all our utility function into it's own namespace.
// -------------------------------------------------------------------------

if (typeof(GraphKit.Misc) == 'undefined') {
    GraphKit.Misc = {};
}

GraphKit.Misc.NAME = 'GraphKit.Misc';
GraphKit.Misc.VERSION = GraphKit.VERSION;

GraphKit.Misc.__repr__ = function() {
    return "[" + this.NAME + " " + this.VERSION + "]";
};

GraphKit.Misc.toString = function() {
    return this.__repr__();
}

MochiKit.Base.update(GraphKit.Misc, {
    nearestMagnitude: function(val) {
        var magn = 1;
        if (val >= 1) {
            var xstr = parseInt(val) + "";
            var n = Math.pow(10, xstr.length - 1);
            magn = Math.round(val/n) * n;
        }
        return magn;
    },
    
    collapse: function(lst) {
        var m = MochiKit.Base;
        var biggerList = new Array();
        for (var i = 0; i < lst.length; i++) {
            biggerList = m.concat(biggerList, lst[i]);
        }
        return biggerList;
    },
    
    uniq: function(sortedList) {
        // get unique elements in list, exactly the same as unix shell's uniq.
        var m = MochiKit.Base;
        
        if (!m.isArrayLike(sortedList) || (sortedList.length < 1))
            return new Array();

        var uniq = new Array();
        var lastElem = sortedList[0];    
        uniq.push(sortedList[0]);
        for (var i = 1; i < sortedList.length; i++) {
            if (m.compare(sortedList[i], lastElem) != 0) {
                lastElem = sortedList[i];
                uniq.push(sortedList[i]);            
            }
        }
        return uniq;
    },
    
    colorScheme: function() {
        var mb = MochiKit.Base;
        var mc = MochiKit.Color
        var scheme = ["red", "orange", "yellow", "green", "cyan", "blue", "purple", "magenta", "brown", "gray"];
        
        var makeColor = function(name) {
            return mc.Color[name + "Color"]()
        };
        
        return mb.map(makeColor, scheme);
    }
});    

// ---------------------------------------------------------------------------
//  Finally the real CanvasGraph
// ---------------------------------------------------------------------------

if (typeof(GraphKit.CanvasGraph) == 'undefined') {
    GraphKit.CanvasGraph = {};
}

GraphKit.CanvasGraph.NAME = "GraphKit.CanvasGraph";
GraphKit.CanvasGraph.VERSION = GraphKit.VERSION;

GraphKit.CanvasGraph.__repr__ = function() {
    return "[" + this.NAME + " " + this.VERSION + "]";
};

GraphKit.CanvasGraph.toString = function() {
    return this.__repr__();
}


GraphKit.CanvasGraph = function (canvasElementOrName) {
    var mb = MochiKit.Base;  
    var mc = MochiKit.Color;  
    var md = MochiKit.DOM;
    
    // constructor
    this.element = md.getElement(canvasElementOrName);
    if (mb.isUndefinedOrNull(this.element)) {
        error("CanvasGraph() - canvas not found");
        return null;
    }
        
    if (!GraphKit.CanvasGraph.isSupported(canvasElementOrName)) 
        return null;
        
    // XXX: make container creation automatic?
    this.container = this.element.parentNode;
    this.context = this.element.getContext("2d");
    this.height = this.element.height;
    this.width = this.element.width;

    // public attributes

    this.padding = {top: 20, left: 30, bottom: 30, right: 20};
    
    this.xlabels = {};
    this.ylabels = {};

    this.yticks = [];
    this.xticks = [];

    this.autoAxis = true;
    this.xOriginIsZero = true;
    this.yOriginIsZero = true;    
    
    this.barChartWidth = 0.75;    
    this.fontSize = 10;
    this.xtickSeparation = 50;    
    this.ytickSeparation = 50;

    this.labelColor = mc.Color.blackColor();
    this.labelWidth = 50;

    // private attributes - we won't deliberately hide this, but don't touch
    //                      these unless you know what you're doing.
        
    this.xvmin = 0;
    this.xvmax = 0;
    this.yvmin = 0;
    this.yvmax = 0;
    
    this.values = new Array();

    // finally, some initialising
    this.lastDrawState = new Array();
    
    // make sure style is set properly on the graph object
    md.updateNodeAttributes(this.container, {"style":{
        "position": "relative",
        "width": this.element.width + "px"
    }});
    md.updateNodeAttributes(this.element, {"style":{
        "position": "relative",
        "zIndex": 1
    }});
};

GraphKit.CanvasGraph.prototype = {
    __class__: GraphKit.CanvasGraph
};

MochiKit.Base.update(GraphKit.CanvasGraph,
    {
        isSupported: function(canvasName) {
            var mb = MochiKit.Base;
            var md = MochiKit.DOM;
            var canvas = null;
            if (mb.isUndefinedOrNull(canvasName)) {
                canvas = md.CANVAS({});
            }
            else {
                canvas = md.getElement(canvasName);
            }
            
            try {
                var context = canvas.getContext("2d");
            }
            catch (e) {
                return false;
            }
            return true;
        }
    }
);    


GraphKit.CanvasGraph.prototype.setDataset = function(name, dataset) {
    var mb = MochiKit.Base;
    this.values[name] = dataset;
    var allValues = GraphKit.Misc.collapse(mb.map(itemgetter(1), mb.items(this.values)));
    
    if (this.xOriginIsZero)
        this.xvmin = 0;
    else 
    	this.xvmin = listMin(mb.map(parseFloat, mb.map(itemgetter(0), allValues)));

    if (this.yOriginIsZero)    
        this.yvmin = 0;
    else
        this.yvmin = listMin(mb.map(parseFloat, mb.map(itemgetter(1), allValues)));

    this.xvmax = listMax(mb.map(parseFloat, mb.map(itemgetter(0), allValues)));
    this.yvmax = listMax(mb.map(parseFloat, mb.map(itemgetter(1), allValues)));

};

GraphKit.CanvasGraph.prototype.setDatasetFromTable = function(name, tableElement, xcol, ycol) {
    var mb = MochiKit.Base;
    var md = MochiKit.DOM;
    var mf = MochiKit.Format;
    if (mb.isUndefinedOrNull(xcol))
        xcol = 0;
    if (mb.isUndefinedOrNull(ycol))
        ycol = 1;
        
    var rows = tableElement.tBodies[0].rows;
    var data = new Array();
    if (!mb.isUndefinedOrNull(rows)) {
        for (var i = 0; i < rows.length; i++) {
            data.push([strip(md.scrapeText(rows[i].cells[xcol])), 
                       parseFloat(mf.strip(md.scrapeText(rows[i].cells[ycol])))]);
        }
        this.setDataset(name, data);
        return true;
    }

    return false;
};


// --------------------------------------------------------------
// -- Draw Pie Charts 
// --------------------------------------------------------------

GraphKit.CanvasGraph.prototype.drawPieChart = function(dataset) {
    var mb = MochiKit.Base;
    var md = MochiKit.DOM;
    var mf = MochiKit.Format;

    var datasetname = '';
    if (typeof(dataset) == 'string') {
        datasetname = dataset;
    }
    else {
        datasetname = mb.keys(dataset)[0];
    }
        
    var context = this.context;
    var values = this.values[datasetname];
    var ytotal = 0;
    
    for (var i = 0; i < values.length; i++) {
    	ytotal += parseFloat(values[i][1]);
    }
    
    this._calcPadding();
    
    var radius = objMin((this.ymax - this.ymin), (this.xmax - this.xmin)) / 2;
    var centerx = (this.xmax - this.xmin) / 2 + this.xmin
    var centery = (this.ymax - this.ymin) / 2 + this.ymin

    var colors = mb.map(function(c) { return c.asFillColor(); }, GraphKit.Misc.colorScheme());
    var threeD = false;    

    var counter = 0.0;
    context.save();
    for (var i = 0; i < values.length; i++) {
        var fraction = values[i][1] / ytotal;
        context.beginPath();
        context.moveTo(centerx, centery);
        context.arc(centerx, centery, radius, 
                   counter * Math.PI * 2 - Math.PI * 0.5,
                   (counter + fraction) * Math.PI * 2 - Math.PI * 0.5,
                   false);
        context.lineTo(centerx, centery);
        context.closePath();
        
        context.fillStyle = colors[i%colors.length];
        context.fill();
        
        // draw label
        var sliceMiddle = (counter + fraction/2)
        var labelx = centerx + Math.sin(sliceMiddle * Math.PI * 2) * (radius + 10);
        var labely = centery - Math.cos(sliceMiddle * Math.PI * 2) * (radius + 10);
        
        var attrib = {"position": "absolute",
                      "zIndex": 11,
                      "width": this.labelWidth + "px",
                      "fontSize": this.fontSize + "px",
                      "overflow": "hidden"};
                      
        if (this.labelColor != null)
            attrib["color"] = this.labelColor.toHexString();
        else
            attrib["color"] = colors[i%colors.length].toHexString();
        
        if (sliceMiddle <= 0.25) {
            // text on top and align left
            attrib["textAlign"] = "left";
            attrib["verticalAlign"] = "top";
            attrib["left"] = labelx + "px";
            attrib["top"] = (labely - this.fontSize) + "px";
        }
        else if (sliceMiddle > 0.25 && sliceMiddle <= 0.5) {
            // text on bottom and align left
            attrib["textAlign"] = "left";
            attrib["verticalAlign"] = "bottom";     
            attrib["left"] = labelx + "px";
            attrib["top"] = labely + "px";
                   
        }
        else if (sliceMiddle > 0.5 && sliceMiddle <= 0.75) {
            // text on bottom and align right
            attrib["textAlign"] = "right";
            attrib["verticalAlign"] = "bottom"; 
            attrib["left"] = (labelx  - this.labelWidth) + "px";
            attrib["top"] = labely + "px";
        }
        else {
            // text on top and align right
            attrib["textAlign"] = "right";
            attrib["verticalAlign"] = "bottom";  
            attrib["left"] = (labelx  - this.labelWidth) + "px";
            attrib["top"] = (labely - this.fontSize) + "px";
        }
        
        var labelText = this.xlabels[values[i][0]];
        if (mb.isUndefinedOrNull(labelText))
            labelText = this.ylabels[values[i][1]];
        
        if (mb.isUndefinedOrNull(labelText))
            labelText = values[i][0];
        labelText += " (" + mf.numberFormatter("#%")(fraction) + ")"
        
        var label = DIV({"style": attrib}, labelText);
        this.xticks.push(label);
        md.appendChildNodes(this.container, label);
        
        counter += fraction;
    }
    context.restore();
    
};

// --------------------------------------------------------------
// -- Draw Line Plot
// --------------------------------------------------------------

GraphKit.CanvasGraph.prototype.drawLinePlot = function(name_color_map) {
    var mb = MochiKit.Base;
    
    this._calcPadding();
    this._calcScale();

    var context = this.context;
    var name_colors = mb.items(name_color_map);
    
    for (var c = 0; c < name_colors.length; c++) {
        var values = this.values[name_colors[c][0]];
        if (mb.isUndefinedOrNull(values))
            continue;
        
        // draw the line
        for (var i = 0; i < values.length; i++) {
            var startx = this.xmin;
            var starty = this.ymax;
            
            if ((i == 0) && (!this.xOriginIsZero)) {
                continue;
            }
            
            if (i > 0) {
                startx = Math.round(this.xmin+values[i-1][0]*this.xvscale);
                starty = Math.round(this.ymax-values[i-1][1]*this.yvscale);
            }
            
            var endx = Math.round(this.xmin + values[i][0] * this.xvscale);
            var endy = Math.round(this.ymax - values[i][1] * this.yvscale);
                
            context.save();
            context.lineWidth = 1.0;	    
            context.strokeStyle = name_colors[c][1].asStrokeColor();
            context.fillStyle = name_colors[c][1].asFillColor();

            context.beginPath();
            context.moveTo(startx, starty);
            context.lineTo(endx, endy);
            context.closePath();
            context.stroke();
	    
            // fill between y value and axis
            context.beginPath();
            context.moveTo(startx, starty);
            context.lineTo(endx, endy);
            context.lineTo(endx, this.ymax);
            context.lineTo(startx, this.ymax);            
            context.closePath();
            context.fillStyle =  name_colors[c][1].asFillColor();
    	    context.lineWidth = 0;
            context.fill();
            context.restore();
        }
    }
    
    this.lastDrawState.push({"drawLinePlot":name_color_map});

    this.drawAxis();
};

// --------------------------------------------------------------
// -- Draw Line Plot
// --------------------------------------------------------------

GraphKit.CanvasGraph.prototype.drawBarChart = function(name_color_map)  {
    var mb = MochiKit.Base;
    
    this._calcPadding();
    this._calcScale(true);

    var context = this.context;
    var name_colors = mb.items(name_color_map);
    var barWidth = 10 * this.xvscale;
    var sets = name_colors.length;
    
    // check if there are missing keys
    for (var i = 0; i < name_colors.length; i++) {
        if (mb.isUndefinedOrNull(this.values[name_colors[i][0]])) {
            error("GraphKit.CanvasGraph.drawBarChart() - given name ", 
                  name_colors[i][0], "does not have dataset associated.");
            return;
        }
    }
    
    //  find all the xvalues we need
    var setnames = mb.keys(name_color_map);
    var getxfunc = function(name) { 
        return mb.map(mb.itemgetter(0), this.values[name]); 
    };
    var getvalx = mb.bind(getxfunc, this);
    var xvalues = GraphKit.Misc.collapse(mb.map(getvalx, setnames));
    
    xvalues.sort();
    xvalues = GraphKit.Misc.uniq(xvalues);
    
    var values = new Array();
    for (var s = 0; s < setnames.length; s++) {
        var setname = setnames[s];        
        values[setname] = new Array();
        for (var i = 0; i < this.values[setname].length; i++) {
            var x = this.values[setname][i][0];
            var y = this.values[setname][i][1];
            values[setname][x] = y;
        }
    }

    //
    // work out how wide each bar should be
    //
    
    var minxdelta = 10000;
    var barWidth = 0;
    
    if (xvalues.length == 0) {
        // don't even bother with empty data sets
        return;
    }
    else if (xvalues.length == 1) {
        // we set the bar to the whole chart for single values
        minxdelta = 1;
    }
    else {
        for (var i = 1; i < xvalues.length; i++) {
            minxdelta = mb.objMin(Math.abs(xvalues[i] - xvalues[i-1]), minxdelta);
        }
    }
    
    barWidthPerX = minxdelta * this.xvscale;
    barWidth = barWidthPerX / sets * this.barChartWidth;
    
    for (var i = 0; i < xvalues.length; i++) {
        var xv = xvalues[i];
        var barLeftMostX = this.xpx(xv) + (1 - this.barChartWidth)/2 * barWidthPerX;
        
        for (var s = 0; s < setnames.length; s++) {
            var setname = setnames[s];
            var yv = values[setname][xv];
            if (mb.isUndefinedOrNull(yv))
                continue;
                
            var setColor = name_color_map[setname];
            var barX = barLeftMostX + (s * barWidth);
            var barY = this.ypx(yv);
            
            context.save();
            context.rect(barX, barY, barWidth - 1, this.ymax - barY);
            context.fillStyle = setColor.asFillColor();
            context.fill();
            
            context.rect(barX, barY, barWidth - 1, this.ymax - barY);
            context.strokeStyle = setColor.toRGBString();
            context.lineWidth = 0.5;            
            context.stroke();
            
            context.restore();
        }
    }

    this.lastDrawState.push({"drawBarChart": name_color_map});
    this.drawAxis(barWidthPerX/2, 0);

};

GraphKit.CanvasGraph.prototype.drawGrid = function(spacing, gridColor) {
    var mb = MochiKit.Base;
    var mc = MochiKit.Color;
    var context = this.context;
    
    context.save()
    if (mb.isUndefinedOrNull(gridColor)) {
        context.strokeStyle = mc.Color.lightGrayColor().asFillColor();
    }
    else {
        context.strokeStyle = gridColor.toRGBString();
    }

    context.lineWidth = 0.5;

    for (x = 0; x < this.width; x += spacing) {
        context.save();
        context.beginPath();                
        context.moveTo(x, 0);
        context.lineTo(x, this.height);
        context.closePath();
        context.stroke();
        context.restore();
    }
    
    for (y = 0; y < this.height; y += spacing) {
        context.save();
        context.moveTo(0, y);
        context.lineTo(this.width, y);
        context.closePath();
        context.stroke();
        context.restore();
    }    
    
    context.restore();
    
    this.lastDrawState.push({"drawGrid": spacing});

};

GraphKit.CanvasGraph.prototype.clear = function () {
    var md = MochiKit.DOM;

    var context = this.context;
    context.clearRect(0, 0, this.width, this.height);

    md.hideElement(this.labelbox);
    for (var i = 0; i < this.xticks.length; i++) {
        md.removeElement(this.xticks[i]);
    }        
    for (var i = 0; i < this.yticks.length; i++) {
        md.removeElement(this.yticks[i]);
    }            
    this.xticks = new Array();
    this.yticks = new Array();

};

GraphKit.CanvasGraph.prototype.redraw = function() {
    // Rough redraw to restore draw state
    var mb = MochiKit.Base;
    var states = mb.clone(this.lastDrawState);
    this.lastDrawState = new Array();
    
    if (states.length > 0) {
        this.clear();
    }

    for (var i = 0; i < states.length; i++) {
        if (states[i]["drawGrid"]) {
            this.drawGrid(states[i]["drawGrid"]);
        }
        else if (states[i]["drawBarChart"]) {
            this.drawBarChart(states[i]["drawBarChart"]);
        }
        else if (states[i]["drawLinePlot"]) {
            this.drawLinePlot(states[i]["drawLinePlot"]);
        }
    }
};


/* 

 Internal Functions

*/

// translate from x-value to x-pixel
GraphKit.CanvasGraph.prototype.xpx = function(xv) {
    return this.xmin + (xv - this.xvmin)*this.xvscale;
};

GraphKit.CanvasGraph.prototype.ypx = function(yv) {
    return this.ymax - (yv - this.yvmin)*this.yvscale;
};

GraphKit.CanvasGraph.prototype._calcPadding = function() {
    this.xmin = this.padding.left;
    this.xmax = this.width - this.padding.right;
    this.ymin = this.padding.top;
    this.ymax = this.height - this.padding.bottom;
};

GraphKit.CanvasGraph.prototype._calcScale = function(isBarChart) {
    var mb = MochiKit.Base;
    var one = 1;
    if (mb.isUndefinedOrNull(isBarChart) || !isBarChart) {
        one = 0;
    }
    
    this.xvrange = this.xvmax - this.xvmin;
    if (this.xvrange > 0)
        this.xvscale = (this.xmax - this.xmin)/(this.xvrange + one);
    else
        this.xvscale = this.xmax - this.xmin;
        
    this.yvrange = this.yvmax - this.yvmin;
    if (this.yvrange > 0) 
        this.yvscale = (this.ymax - this.ymin)/(this.yvrange + one);
    else
        this.yvscale = this.ymax - this.ymin;
};

GraphKit.CanvasGraph.prototype._calcAxis = function() {
    /* 
        Finds the nearest pretty looking steps for the x and y axis.
    */
    
    var xsteps = (this.width / this.xtickSeparation);
    var ysteps = (this.height / this.ytickSeparation);
    
    xsteps = (this.xvmax - this.xvmin) / xsteps;
    ysteps = (this.yvmax - this.yvmin) / ysteps;
    
    xsteps = GraphKit.Misc.nearestMagnitude(xsteps);
    ysteps = GraphKit.Misc.nearestMagnitude(ysteps);
    
    if (xsteps == 0)
        xsteps = 1;
    if (ysteps == 0)
        ysteps = 1;
    
    return [xsteps, ysteps];
};

GraphKit.CanvasGraph.prototype.drawAxis = function(xaxis_off, yaxis_off, xsteps, ysteps) {
    var mb = MochiKit.Base;
    var mc = MochiKit.Color;
    
    context = this.context;

    if (mb.isUndefinedOrNull(xsteps) || mb.isUndefinedOrNull(ysteps)) {
        steps = this._calcAxis();
        if (mb.isUndefinedOrNull(xsteps)) 
            xsteps = steps[0];
        if (mb.isUndefinedOrNull(ysteps))
            ysteps = steps[1];
    }

    if (mb.isUndefinedOrNull(xaxis_off)) {
        xaxis_off = 0;
    }
    if (mb.isUndefinedOrNull(yaxis_off)) {
        yaxis_off = 0;
    }

    context.save();
        context.lineWidth = 1.0;
        context.strokeStyle = mc.Color.blackColor().toRGBString();
        
        context.beginPath();       
        context.moveTo(this.xmin, this.ymin);
        context.lineTo(this.xmin, this.ymax);        
        context.closePath();
        context.stroke();
        
        context.beginPath();
        context.moveTo(this.xmin, this.ymax);        
        context.lineTo(this.xmax, this.ymax);
        context.closePath();
        context.stroke();
    context.restore();
    
    this.drawTicks(xaxis_off, yaxis_off, xsteps, ysteps);
};

GraphKit.CanvasGraph.prototype.drawTicks = function (xaxis_off, yaxis_off, xsteps, ysteps) {
    var mb = MochiKit.Base;
    var mc = MochiKit.Color;
    var md = MochiKit.DOM;

    var context = this.context;
    
    if (mb.isUndefinedOrNull(xaxis_off)) {
        xaxis_off = 0;
    }
    
    if (mb.isUndefinedOrNull(yaxis_off)) {
        yaxis_off = 0;
    }
    
    context.save();
    context.lineWidth = 0.5;
    context.strokeStyle = mc.Color.blackColor().toRGBString();

    // horiztonal ticks
    for (var xv = this.xvmin; xv <= this.xvmax; xv += xsteps) {
        var x = this.xpx(xv);
        var y = this.ymax + 7;
               
        context.beginPath();
        context.moveTo(x + xaxis_off, this.ymax + 1);
        context.lineTo(x + xaxis_off, this.ymax + 5);
        context.closePath();
        context.stroke();

        var label = null;        
        if (!mb.isUndefinedOrNull(this.xlabels[xv]))
            label = md.DIV({}, this.xlabels[xv]);
        else
            label = md.DIV({}, xv);
        
        md.updateNodeAttributes(label, {"style":
            {"position": "absolute",
             "textAlign": "center",
             "width": this.xtickSeparation + "px",
             "top": y + "px",
             "left": Math.ceil(x + xaxis_off - (this.xtickSeparation/2)) + "px",
             "fontSize": this.fontSize + "px",
             "zIndex": 10,
             "color": "black",
             "overflow": "hidden"
        }});
        
        md.appendChildNodes(this.container, label);
        this.xticks.push(label);
    }
    
    // vertical ticks
    
    for (var yv = this.yvmin; yv <= this.yvmax; yv += ysteps) {
        y = this.ypx(yv);
        x = this.xmin - 7
        context.beginPath();
        context.moveTo(this.xmin - 5, y - yaxis_off);
        context.lineTo(this.xmin - 1, y - yaxis_off);
        context.closePath();
        context.stroke();
        
        var label = null;
        if (!mb.isUndefinedOrNull(this.ylabels[yv]))
            label = md.DIV({}, this.ylabels[yv]);
        else
            label = md.DIV({}, yv);
            
        md.updateNodeAttributes(label, {"style":
            {"position": "absolute",
             "top": Math.ceil(y - this.fontSize/2) + "px",
             "left": Math.ceil(x - this.padding.left - yaxis_off) + "px",
             "fontSize": this.fontSize + "px",
             "zIndex": 10,
             "color": "black",
             "textAlign": "right",
             "width": this.padding.left + "px",
             "overflow": "hidden"
        }});
        md.appendChildNodes(this.container, label);
        this.yticks.push(label);
    }    
    
    context.restore();
};


GraphKit.EXPORT = [
    "CanvasGraph"
];

GraphKit.EXPORT_OK = [];

GraphKit.__new__ = function() {
    var m = MochiKit.Base;
    
    m.nameFunctions(this);
    
    this.EXPORT_TAGS = {
        ":common": this.EXPORT,
        ":all": m.concat(this.EXPORT, this.EXPORT_OK)
    };
};

GraphKit.__new__();
MochiKit.Base._exportSymbols(this, GraphKit);

/*

 Copyright (c) 2005, 2006 Alastair Tse <alastair@tse.id.au>

 All rights reserved.

 Redistribution and use in source and binary forms, with or without modification, are
 permitted provided that the following conditions are met:
 
  * Redistributions of source code must retain the above copyright notice, this list of
 conditions and the following disclaimer. * Redistributions in binary form must reproduce
 the above copyright notice, this list of conditions and the following disclaimer in the
 documentation and/or other materials provided with the distribution. * Neither the name
 of the <ORGANIZATION> nor the names of its contributors may be used to endorse or
 promote products derived from this software without specific prior written permission.
 
  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
 EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
 THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
 OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 
*/ 
