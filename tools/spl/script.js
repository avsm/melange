var activeBg = "#AAFFAA";
var evenBg = "#eee";
var oddBg = "#fff";
var req;

var waiting = false;
var single = false;

var scalls= new Array();
var allcalls = new Array();

function startRequest() {
    req = false;
    // branch for native XMLHttpRequest object
    if (window.XMLHttpRequest) {
        try { req = new XMLHttpRequest();
        } catch(e) { req = false; }
    // branch for IE/Windows ActiveX version
    } else if (window.ActiveXObject) {
        try { req = new ActiveXObject("Msxml2.XMLHTTP");
        } catch(e) {
            try { req = new ActiveXObject("Microsoft.XMLHTTP");
            } catch(e) { req = false; }
        }
    }
    if(req) {
        req.onreadystatechange = processReqChange;
        req.open("GET", spl_url, true);
        req.send("");
    }
    waiting=true;
}

function comp(a,b) {
    return (scalls[b] - scalls[a]);
}

var graph;

function updateStatecalls() {
    s = "";
    keys = new Array();
    for (var i in scalls) {
        keys.push(i);
    };
    skeys = keys.sort(comp);
    gvals = new Array();
    yvals = new Array();
    xaxis = 0;
    var max = 5;
    for (var i in skeys) {
        if (xaxis < max) {
            t=skeys[i];
            gvals.push([xaxis,scalls[t]]);
            yvals[scalls[t]] = t;
            xaxis = xaxis + 1;
        }
    }
    graph.clear();
    graph.setDataset("blah", gvals);
    graph.fontSize = 10;
    graph.padding = {top: 30, left: 30, bottom: 30, right: 30};
    graph.ylabels = yvals;
    graph.drawPieChart("blah");
    s = "";
    d = document.getElementById("allcalls");
    num = 15;
    if (allcalls.length < 15) num=allcalls.length;
    subar = allcalls.slice(allcalls.length-num);
    for (i=0; i<num;i++) {
        e = subar[i];
        s += e + "<br />";
    };
    d.innerHTML=s;
}

function processReqChange() {
    // only if req shows "loaded"
    if (req.readyState == 4) {
        // only if "OK"
        if (req.status == 200) {
            var cons = document.getElementById("console");
            waiting=false;
            var x = req.responseText;
            if (cons) { cons.innerHTML = "<pre>" + x + "</pre>"; }
            bits = x.split(":");
            scall = bits[0];
            if (! scalls[scall]) { scalls[scall] = 0; }
            scalls[scall]++;
            allcalls.push(scall);
            updateStatecalls();
            locs = bits[1].split(",");
            deactivateAllRows();
            for (i=0; i < locs.length; i++) {
                activateRow(locs[i]);
            }
        } else {
            alert("There was a problem retrieving the XML data:\n" + req.statusText);
        }
    }
}

function activateRow(num)
{
    x = document.getElementById("line"+num);
    if (!x) return;
    st = x.style;
    st.backgroundColor = activeBg;
}

function deactivateRow(num)
{
    x = document.getElementById("line"+num);
    if (!x) return;
    st = x.style;
    if (num % 2 == 1) {
        st.backgroundColor= evenBg;
    } else {
        st.backgroundColor= oddBg;
    }
}

function deactivateAllRows()
{
    for (i=0; i < total_lines; i++) {
        deactivateRow(i);
    }
}

function next()
{
    if (!waiting) startRequest();
}

function begin()
{
    var evn = false;
    for (i=0; i< total_lines; i++) {
        x = document.getElementById("line"+i);
        if (x && evn) {
            x.style.backgroundColor=evenBg;
        } else if (x) {
            x.style.backgroundColor=oddBg;
        }
        evn = !evn;
    };
    graph = new CanvasGraph("piechart");
    updateStatecalls();
}

function toggle(elem)
{
    divo = document.getElementById(elem);
    if (!divo) { window.alert("toggle:null "+num); return; }
    dv = divo.style.display;
    if (dv == "block") {
        divo.style.display = "none";
    } else {
        divo.style.display = "block";
    };
 }