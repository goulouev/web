//################################ ERRORS
function DoubleVal(txtID) {
    if (txtID) {
        var x = document.getElementById(txtID).value;
        if (isNumeric(x)) {
            return parseFloat(x);
        } else {
            document.getElementById(txtID).value = "NaN";
            return NaN;
        }

    }
    else {
        return NaN;
    }
}
function IntVal(txtID) {
    var x = document.getElementById(txtID).value;
    if (isNumeric(x)) {
        return parseInt(x);
    } else {
        document.getElementById(txtID).value = "NaN";
        return NaN;
    }
}
function isNumeric(n) {
    return !isNaN(parseFloat(n)) && isFinite(n);
}

//################################ ARRAYS
        function vPrint(v) {
            var n = v.length;
            var t = '';
            var i;
            for (i = 0; i <= n - 1; i++) {
                t = t + ' ' + v[i];
            }
            return t;
        }

//################################ STRINGS

function trim(str) {
    return str.replace(/^\s+|\s+$/g, '');
}

function WordsToArray(str) {
    // text line to array of words
    return ReduceWhiteSpaces(str).split(/\s/g);
}

function ReduceWhiteSpaces(str) {
    return trim(str).replace(/\s+/g, ' ');
}


function GetInts(nVars, VarLine) {
    var i;
    var v = WordsToArray(VarLine);
    //var v = VarLine.split(/\s/g);
    if (v.length >= nVars) {
        var vv = [];
        for (i = 0; i <= nVars - 1; i++) {
            vv[i] = parseInt(v[i]);
            if (isNaN(vv[i])) {
                //            alert("Entered value #"+(i+1).toString()+" is not recognized as a number.");
                return NaN;
            }
        }

    } else {
        //alert("Number of entered values is less than required (" + nVars.toString() + ").");
        return NaN;
    }
    return vv;
}

function GetFloats(nVars, VarLine) {
    var i;
    var v = WordsToArray(VarLine);
    //var v = VarLine.split(/\s/g);
    if (v.length >= nVars) {
        var vv = [];
        for (i = 0; i <= nVars - 1; i++) {
            vv[i] = parseFloat(v[i]);
            if (isNaN(vv[i])) {
                //return NaN;
                return null;
            }
        }

    } else {
        //alert("Number of entered values is less than required (" + nVars.toString() + ").")
        //return NaN;
        return null;
    }
    return vv;
}

function GetFloatFromObj(obj) {
    var x = parseFloat(obj.value);
    if (isNaN(x)) { return NaN; }
    return x;
}

function GetIntFromObj(obj) {
    var x = parseInt(obj.value);
    if (isNaN(x)) { return NaN; }
    return x;
}

function FormatNumberByLen(x, nSpc, nDgt) {
    var i;
    var g = x.toFixed(nDgt);
    var n = g.length;
    if (n >= nSpc) {
        return g;
    } else {
        for (i = n; i <= nSpc; i++) {
            g = " " + g;
        }
    }
    return g;
}

function FormatStrByLen(x, IsLeft, nSpc) {
    var i;
    var g = trim(x);
    var n = g.length;
    if (n >= nSpc) {
        return g;
    } else {
        for (i = n; i <= nSpc; i++) {
            if (IsLeft) { g = g + " "; } else { g = " " + g; }
        }
    }
    return g;
}



function WordLineCorrect(Median, TextLine) {
    var i;
    var r = "";
    var dLine = "";
    //TextLine=TextLine.replace(tab," ")
    var A = "";
    var InMark = true;
    var rr = TextLine.split("");
    for (i in rr) {
        r = rr[i];
        if (r.charCodeAt() === 32) { r = " "; }
        dLine = r;
        if (InMark) { r = " "; } else { dLine = ""; }
        if (r === " ") { InMark = true; } else { InMark = false; }
        A += dLine;
    }
    if (Median === " ") {
        return trim(A);//.trim();
    } else {
        return trim(A).replace(" ", Median);
    }
}

