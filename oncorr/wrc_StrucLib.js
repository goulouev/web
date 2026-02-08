
//  DESIGN STRUC
function node() {
    this.iType = 0;
    this.iOpt = 0;
    this.iDir = 0; // -2 if iris, 2 if cav, +/-1 if step, 0 if small node, 1 if big node
    this.iGuide = -1; // adjacent big node count in lib
    this.iAper = -1; // adjacent aper count in lib
    this.v = [0, 0, 0, 0, 0, 0];
}

node.prototype.ImportFromLine = function (LN) {
    var vs = WordsToArray(LN);
    this.iOpt = parseInt(vs[0]);
    this.iType = parseInt(vs[1]);
    var ns = vs.length - 1, i;
    for (i = 2; i <= ns; i++) {
        this.v[i - 1] = parseFloat(vs[i]);
    }
};

node.prototype.ExportToLine = function () {
    var txt = '';
    txt = txt + this.iOpt + '    ' + this.iType + FormatNumberByLen(this.v[1], 12, 6);
    txt = txt + FormatNumberByLen(this.v[2], 12, 6) + FormatNumberByLen(this.v[3], 12, 6);
    txt = txt + FormatNumberByLen(this.v[4], 12, 6) + FormatNumberByLen(this.v[5], 12, 6);
    return txt;
};

function IsDesignLine(LN) {
    if (LN) {
        var vs = WordsToArray(LN);
        var ns = vs.length - 1;
        if (ns < 6) { return false; }
        var i;
        for (i = 0; i <= ns; i++) {
            if (isNaN(parseFloat(vs[i]))) { return false; }
        }
        return true;
    } else { return false; }
}

node.prototype.SetVar = function (iSymX, iSymY, iVar, t) {
    //abs(iVar)<5
    var iX = iSymX * iSymX, iY = iSymY * iSymY, iV;
    if (iVar < 0) { iV = -iVar; iX = 1; iY = 1;} else { iV = iVar;}
        if (this.iType > 0) {
            if (iVar !== 0) {
                this.v[iVar] = t;
                if (iV === 2 && iX === 1) { this.v[4] = -t; }
                if (iV === 3 && iY === 1) { this.v[5] = -t; }
                if (iV === 4 && iX === 1) { this.v[2] = -t; }
                if (iV === 5 && iY === 1) { this.v[3] = -t; }
            }
        }
};

node.prototype.GetVar = function (iVar) {
    var iV = Math.abs(iVar);
    return this.v[iV];
};

function NumSplit(num) {
    var iSym, iV, iV0, iV1 = 0;
    if (num < 0) { iV = -num; iSym = -1; iV0 = iV; } else { iV = num; iSym = 1; iV0 = iV;}
    if (iV >= 10) { iV0 = Math.floor(iV / 10 + 0.001); iV1 = Math.floor(iV - 10 * iV0 + 0.001); }
    return [iSym * iV0, iV1];
}

function design() {
    this.iSymX = 0;
    this.iSymY = 0;
    this.iSymZ = 0;
    this.km = 0;
    this.kx = 1;
    this.ky = 0;
    this.md = 1;
    this.nx = 10;
    this.ny = 10;
    this.e = 1;
    this.tge = 0;
    this.sig = 0;
    this.nJuncs = -1;
    this.nGuides = -1;
    this.nApers = -1;
    this.sqrtE = 1;
    this.f = 0;
    this.iCalcNum = -1;
    //this.juncs = new Array();
    this.juncs = [];
    this.So = [0, 0, 0, 0, 0, 0, 0, 0];
}
function iDirJunc(a,b) {
    var aX0 = a.v[2]; var aY0 = a.v[3]; var aX1 = a.v[4]; var aY1 = a.v[5];
    var bX0 = b.v[2]; var bY0 = b.v[3]; var bX1 = b.v[4]; var bY1 = b.v[5];
    if (bX0 <= aX0 && bX1 >= aX1 && bY0 <= aY0 && bY1 >= aY1) { return 1;}
    if (bX0 >= aX0 && bX1 <= aX1 && bY0 >= aY0 && bY1 <= aY1) { return -1; }
    return 0;
}

//DESIGN DATA EXCHANGE

//        Project File Functions

design.prototype.ExportJuncsList = function(){
    var i, txt='Junc  ind  iDir iGuide iAper \r\n';
    for (i = 0; i <= this.nJuncs; i++) {
        txt=txt+i+'        '+this.juncs[i].iType+'     '+this.juncs[i].iDir+'      '+this.juncs[i].iGuide+'     '+this.juncs[i].iAper+'\r\n';
    }
    return txt;
};

design.prototype.ExportSpace = function () {
    var iSp = new Array();
    iSp[0] = this.iSymX;
    iSp[1] = this.iSymY;
    iSp[2] = this.iSymZ;
    iSp[3] = this.km;
    iSp[4] = this.kx;
    iSp[5] = this.ky;
    iSp[6] = this.md;
    iSp[7] = this.nx;
    iSp[8] = this.ny;
    return iSp;
};

function SplitProjectParts(InText) {
    var Shapka = '';
    var Schema = '';
    if (!InText) { return; }
    var DesLines = InText.match(/[^\r\n]+/g);
    var n = DesLines.length - 1;
    if (n > 4) {
        var i;
        for (i = 0; i <= n; i++) {
            var ShrtLine = DesLines[i].trim();
            if (ShrtLine){
                var vs = WordsToArray(ShrtLine);
                var nVars = vs.length - 1;
                if (nVars < 5) {
                    Shapka = Shapka + DesLines[i] + '\r\n';
                } else {
                    Schema = Schema + DesLines[i] + '\r\n';
                }
            }
        }
    }
    var tExit = [];
    tExit[0] = Shapka; tExit[1] = Schema;
    return tExit;
}

function JoinPrjParts(Shapka, Schema) {
    return Shapka + Schema;
}

//        Design Shapka Functions

design.prototype.ExportShapka = function () {
    // adds shapka info to design obj
    var txt = '';
    txt = txt + this.iSymX + '  ' + this.iSymY + '  ' + this.iSymZ + '\r\n';
    txt = txt + this.km + '  ' + this.kx + '  ' + this.ky + '\r\n';
    txt = txt + this.md + '  ' + this.nx + '  ' + this.ny + '\r\n';
    txt = txt + '1  1' + '\r\n';
    txt = txt + this.e + '  ' + this.tge + '  ' + this.sig + '\r\n';
    return txt;
};

design.prototype.ImportShapka = function (InText) {
    // gets shapka from design obj
    if (!InText) { return; }
    var DesLines = InText.match(/[^\r\n]+/g);
    var n = DesLines.length - 1;
    var i;
    var ii = -1;
    for (i = 0; i <= n; i++) {
        var ShrtLine = DesLines[i].trim();
        if (ShrtLine) {
            ii = ii + 1;
            var vs = WordsToArray(ShrtLine);
            switch (ii) {
                case 0:
                    this.iSymX = parseInt(vs[0]);
                    this.iSymY = parseInt(vs[1]);
                    this.iSymZ = parseInt(vs[2]);
                    break;
                case 1:
                    this.km = parseInt(vs[0]);
                    this.kx = parseInt(vs[1]);
                    this.ky = parseInt(vs[2]);
                    break;
                case 2:
                    this.md = parseInt(vs[0]);
                    this.nx = parseInt(vs[1]);
                    this.ny = parseInt(vs[2]);
                    break;
                case 4:
                    this.e = parseFloat(vs[0]);
                    this.tge = parseFloat(vs[1]);
                    this.sig = parseFloat(vs[2]);
                    break;
            }
        }
    }
};

design.prototype.IsValidMode = function () {
    var mod, n, m;
    mod = this.km; n = nSym(this.iSymX, this.kx); m = nSym(this.iSymY, this.ky);
    if (mod === 0) {
        if (n + m === 0) { return false; }
    } else {
        if (n * m === 0) { return false; }
    }
    return true;
};

design.prototype.InModeName = function () {
    var mod, n, m, label;
    mod = this.km; n = nSym(this.iSymX, this.kx); m = nSym(this.iSymY, this.ky);
    if (mod === 0) {
        label = 'TE';
    } else {
        label = 'TM';
    }
    return label + ' ' + n + ' ' + m;
};

design.prototype.CalcJunc = function (iJunc) {
    //throw '';
    var i=iJunc;
    Guides = [];
    Apers = [];
    var Ao = 0, Bo = 0, x0 = 0, x1 = 0, y0 = 0, y1 = 0, a = 0, b = 0;
    var x=0, d = this.juncs[i].v[1];
    var k = aox(kf(this.iUnit, this.f), this.sqrtE);
    var iDir = this.juncs[i].iDir;

        switch (this.juncs[i].iType) {
            case 3:
                x0 = this.juncs[i].v[2] - this.juncs[i - 1].v[2];
                x1 = this.juncs[i].v[4] - this.juncs[i - 1].v[2];
                y0 = this.juncs[i].v[3] - this.juncs[i - 1].v[3];
                y1 = this.juncs[i].v[5] - this.juncs[i - 1].v[3];
                Ao = this.juncs[i - 1].v[4] - this.juncs[i - 1].v[2];
                Bo = this.juncs[i - 1].v[5] - this.juncs[i - 1].v[3];
                Guides[0] = CalcNodes(Ao, Bo);
                Apers[0] = CalcApers(x0, y0, x1, y1, Ao, Bo);
                x0 = this.juncs[i].v[2] - this.juncs[i + 1].v[2];
                x1 = this.juncs[i].v[4] - this.juncs[i + 1].v[2];
                y0 = this.juncs[i].v[3] - this.juncs[i + 1].v[3];
                y1 = this.juncs[i].v[5] - this.juncs[i + 1].v[3];
                Ao = this.juncs[i + 1].v[4] - this.juncs[i + 1].v[2];
                Bo = this.juncs[i + 1].v[5] - this.juncs[i + 1].v[3];
                Apers[1] = CalcApers(x0, y0, x1, y1, Ao, Bo);
                Guides[1] = CalcNodes(Ao, Bo);
                x = CalcNode(this.kx, this.ky, x1 - x0, y1 - y0);
                this.So = RecIris(k, Guides[0], Apers[0], x, d, Guides[1], Apers[1]);
                break;
            case 0:
                Ao = this.juncs[i + iDir].v[4] - this.juncs[i + iDir].v[2];
                Bo = this.juncs[i + iDir].v[5] - this.juncs[i + iDir].v[3];
                x0 = this.juncs[i - iDir].v[2] - this.juncs[i + iDir].v[2];
                x1 = this.juncs[i - iDir].v[4] - this.juncs[i + iDir].v[2];
                y0 = this.juncs[i - iDir].v[3] - this.juncs[i + iDir].v[3];
                y1 = this.juncs[i - iDir].v[5] - this.juncs[i + iDir].v[3];
                Guides[0] = CalcNodes(Ao, Bo);
                Apers[0] = CalcApers(x0, y0, x1, y1, Ao, Bo);
                var jBY = RecStep(k, Guides[0], Apers[0]);
                x = CalcNode(this.kx, this.ky, x1 - x0, y1 - y0);
                var ySmall = yw(this.km, k, beta(k, x));
                if (iDir === 1) {
                    this.So = Shunt(ySmall, [jBY[0], jBY[1]], [jBY[2], jBY[3]]);
                } else {
                    this.So = Shunt([jBY[2], jBY[3]], [jBY[0], jBY[1]], ySmall);
                }
                break;
            case 2:
                Ao = this.juncs[i].v[4] - this.juncs[i].v[2];
                Bo = this.juncs[i].v[5] - this.juncs[i].v[3];
                x0 = this.juncs[i - 1].v[2] - this.juncs[i].v[2];
                x1 = this.juncs[i - 1].v[4] - this.juncs[i].v[2];
                y0 = this.juncs[i - 1].v[3] - this.juncs[i].v[3];
                y1 = this.juncs[i - 1].v[5] - this.juncs[i].v[3];
                var xs0 = CalcNode(this.kx, this.ky, x1 - x0, y1 - y0);
                Apers[0] = CalcApers(x0, y0, x1, y1, Ao, Bo);
                x0 = this.juncs[i + 1].v[2] - this.juncs[i].v[2];
                x1 = this.juncs[i + 1].v[4] - this.juncs[i].v[2];
                y0 = this.juncs[i + 1].v[3] - this.juncs[i].v[3];
                y1 = this.juncs[i + 1].v[5] - this.juncs[i].v[3];
                var xs1 = CalcNode(this.kx, this.ky, x1 - x0, y1 - y0);
                Apers[1] = CalcApers(x0, y0, x1, y1, Ao, Bo);
                Guides[0] = CalcNodes(Ao, Bo);
                var Ye = RecCav(k, Ao, Bo, d, Apers[0], Guides[0], Apers[1]);
                var Y0 = yw(this.km, k, beta(k, xs0));
                var Y1 = yw(this.km, k, beta(k, xs1));
                this.So = SY2(Y0, Ye, Y1);
                break;
        }
};





design.prototype.CalcFreq = function () {
    var i, Se, So;
    var mode = this.km;
    var k = aox(kf(this.iUnit, this.f), this.sqrtE);
    var iGuide = -1;
    for (i = 0; i <= this.nJuncs; i++) {
        var iAper = this.juncs[i].iAper;
        var d = this.juncs[i].v[1];
        var a = this.juncs[i].v[4] - this.juncs[i].v[2];
        var b = this.juncs[i].v[5] - this.juncs[i].v[3];
        var x = this.juncs[i].v[0];
        switch (this.juncs[i].iType) {
            case 3:
                iGuide = this.juncs[i - 1].iGuide;
                Se = RecIris(k, Guides[iGuide], Apers[iAper], x, d, Guides[iGuide + 1], Apers[iAper + 1]);
                break;
            case 2:
                iGuide = this.juncs[i].iGuide;
                var Ye = RecCav(k, a, b, d, Apers[iAper], Guides[iGuide], Apers[iAper + 1]);
                var Y0 = yw(mode, k, beta(k, this.juncs[i - 1].v[0]));
                var Y1 = yw(mode, k, beta(k, this.juncs[i + 1].v[0]));
                Se = SY2(Y0, Ye, Y1);
                break;
            case 1:
                Se = Snode(beta(k, this.juncs[i].v[0]), d);
                break;
            case 0:
                var iDir = this.juncs[i].iDir;
                var iSmall = i - iDir;
                iGuide = this.juncs[i+iDir].iGuide;
                var jBY = RecStep(k, Guides[iGuide], Apers[iAper]);
                var ySmall = yw(mode, k, beta(k, this.juncs[iSmall].v[0]));
                if (iDir === 1) {
                    Se = Shunt(ySmall, [jBY[0], jBY[1]], [jBY[2], jBY[3]]);
                } else {
                    Se = Shunt([jBY[2], jBY[3]], [jBY[0], jBY[1]], ySmall);
                }
                break;
        }
        if (i === 0) {
            So = Se;
        } else {
            So = Cascade(So, Se);
        }
    }
    this.So = So;
};

design.prototype.DesignData = function () {
    //throw '';
    var i;
    Guides=[];
    Apers=[];
    //var iSymX = this.iSymX; var iSymY = this.iSymY;
    var mod = this.md, n = this.nx, m = this.ny;
    var mdkl = this.km, k = this.kx, l = this.ky;
    var Ao = 0, Bo = 0, x0 = 0, x1 = 0, y0 = 0, y1 = 0, a = 0, b = 0;
    for (i = 0; i <= this.nJuncs; i++) {
        var iAper = this.juncs[i].iAper, iGuide = this.juncs[i].iGuide;
        if (this.juncs[i].iType === 1) {
            if (this.juncs[i].iDir === 1) {
                a = this.juncs[i].v[4] - this.juncs[i].v[2];
                b = this.juncs[i].v[5] - this.juncs[i].v[3];
                Guides[iGuide] = CalcNodes(a, b);
                this.juncs[i].v[0] = xDom(Guides[iGuide]);
            } else {
                a = this.juncs[i].v[4] - this.juncs[i].v[2];
                b = this.juncs[i].v[5] - this.juncs[i].v[3];
                this.juncs[i].v[0] = CalcNode(k, l, a, b);
            }
        } else {
            switch (this.juncs[i].iDir) {
                case -2:
                    x0 = this.juncs[i].v[2] - this.juncs[i - 1].v[2];
                    x1 = this.juncs[i].v[4] - this.juncs[i - 1].v[2];
                    y0 = this.juncs[i].v[3] - this.juncs[i - 1].v[3];
                    y1 = this.juncs[i].v[5] - this.juncs[i - 1].v[3];
                    Ao = this.juncs[i - 1].v[4] - this.juncs[i - 1].v[2];
                    Bo = this.juncs[i - 1].v[5] - this.juncs[i - 1].v[3];
                    Apers[iAper] = CalcApers(x0, y0, x1, y1, Ao, Bo);
                    x0 = this.juncs[i].v[2] - this.juncs[i + 1].v[2];
                    x1 = this.juncs[i].v[4] - this.juncs[i + 1].v[2];
                    y0 = this.juncs[i].v[3] - this.juncs[i + 1].v[3];
                    y1 = this.juncs[i].v[5] - this.juncs[i + 1].v[3];
                    Ao = this.juncs[i + 1].v[4] - this.juncs[i + 1].v[2];
                    Bo = this.juncs[i + 1].v[5] - this.juncs[i + 1].v[3];
                    Apers[iAper+1] = CalcApers(x0, y0, x1, y1, Ao, Bo);
                    this.juncs[i].v[0] = CalcNode(k, l, x1 - x0, y1 - y0);
                    break;
                case -1:
                    Ao = this.juncs[i - 1].v[4] - this.juncs[i - 1].v[2];
                    Bo = this.juncs[i - 1].v[5] - this.juncs[i - 1].v[3];
                    x0 = this.juncs[i + 1].v[2] - this.juncs[i - 1].v[2];
                    x1 = this.juncs[i + 1].v[4] - this.juncs[i - 1].v[2];
                    y0 = this.juncs[i + 1].v[3] - this.juncs[i - 1].v[3];
                    y1 = this.juncs[i + 1].v[5] - this.juncs[i - 1].v[3];
                    Apers[iAper] = CalcApers(x0, y0, x1, y1, Ao, Bo);
                    break;
                case 1:
                    Ao = this.juncs[i + 1].v[4] - this.juncs[i + 1].v[2];
                    Bo = this.juncs[i + 1].v[5] - this.juncs[i + 1].v[3];
                    x0 = this.juncs[i - 1].v[2] - this.juncs[i + 1].v[2];
                    x1 = this.juncs[i - 1].v[4] - this.juncs[i + 1].v[2];
                    y0 = this.juncs[i - 1].v[3] - this.juncs[i + 1].v[3];
                    y1 = this.juncs[i - 1].v[5] - this.juncs[i + 1].v[3];
                    Apers[iAper] = CalcApers(x0, y0, x1, y1, Ao, Bo);
                    break;
                case 2:
                    Ao = this.juncs[i].v[4] - this.juncs[i].v[2];
                    Bo = this.juncs[i].v[5] - this.juncs[i].v[3];
                    x0 = this.juncs[i - 1].v[2] - this.juncs[i].v[2];
                    x1 = this.juncs[i - 1].v[4] - this.juncs[i].v[2];
                    y0 = this.juncs[i - 1].v[3] - this.juncs[i].v[3];
                    y1 = this.juncs[i - 1].v[5] - this.juncs[i].v[3];
                    a = x1 - x0; b = y1 - y0;
                    Apers[iAper] = CalcApers(x0, y0, x1, y1, Ao, Bo);
                    x0 = this.juncs[i + 1].v[2] - this.juncs[i].v[2];
                    x1 = this.juncs[i + 1].v[4] - this.juncs[i].v[2];
                    y0 = this.juncs[i + 1].v[3] - this.juncs[i].v[3];
                    y1 = this.juncs[i + 1].v[5] - this.juncs[i].v[3];
                    a = x1 - x0; b = y1 - y0;
                    Apers[iAper+1] = CalcApers(x0, y0, x1, y1, Ao, Bo);
                    Guides[iGuide] = CalcNodes(Ao, Bo);
                    break;
            }
        }
    }
};



design.prototype.MarkJuncs = function () {
    var i = 0, n = this.nJuncs;
    for (i = 0; i <= n; i++) {
        if (this.juncs[i].iType === 1) {
            this.juncs[i].iDir = 0;
        }
    }
    //var iAper = -1, iGuide = -1;
    for (i = 0; i <= n; i++) {
        switch (this.juncs[i].iType) {
            case 0:
                var iWay = iDirJunc(this.juncs[i - 1], this.juncs[i + 1]);
                //iAper = iAper + 1; iGuide = iGuide + 1;
                this.juncs[i].iDir = iWay; //this.juncs[i].iAper = iAper; this.juncs[i].iGuide = iGuide;
                if (iWay === 1) { this.juncs[i + 1].iDir = 1; }
                if (iWay === -1) { this.juncs[i - 1].iDir = 1; }
                break;
            case 2:
                //iAper = iAper + 1; iGuide = iGuide + 1;
                this.juncs[i].iDir = 2; //this.juncs[i].iAper = iAper; this.juncs[i].iGuide = iGuide;
                //iAper = iAper + 1;
                break;
            case 3:
                //iAper = iAper + 1; iGuide = iGuide + 1;
                this.juncs[i].iDir = -2; //this.juncs[i].iAper = iAper; this.juncs[i].iGuide = iGuide;
                this.juncs[i - 1].iDir = 1;
                this.juncs[i + 1].iDir = 1;
                //iAper = iAper + 1; iGuide = iGuide + 1;
                break;
        }
    }
    // Guides and Apers Indexing
    var iAper = -1, iGuide = -1;
    for (i = 0; i <= this.nJuncs; i++) {
        if (this.juncs[i].iType === 1) {
            if (this.juncs[i].iDir === 1) {
                iGuide = iGuide + 1;
                this.juncs[i].iGuide = iGuide;
            }
        } else {
            switch (this.juncs[i].iDir) {
                case -2:
                    iAper = iAper + 1;
                    this.juncs[i].iAper = iAper;
                    iAper = iAper + 1;
                    break;
                case -1:
                    iAper = iAper + 1;
                    this.juncs[i].iAper = iAper;
                    break;
                case 1:
                    iAper = iAper + 1;
                    this.juncs[i].iAper = iAper;
                    break;
                case 2:
                    iAper = iAper + 1;
                    this.juncs[i].iAper = iAper;
                    iAper = iAper + 1;
                    iGuide = iGuide + 1;
                    this.juncs[i].iGuide = iGuide;
                    break;
            }
        }
    }
    this.nApers = iAper; this.nGuides = iGuide;
    //this.nApers = iAper; this.nGuides = iGuide;
    this.sqrtE = csqrt([this.e, this.e * this.tge]);
};

//        Design Shcema Functions

design.prototype.ImportSchema = function (InText) {
    // schema (text with juncs) to design.juncs
    if (!InText) { return; }
    var DesLines = InText.match(/[^\r\n]+/g);
    var n = DesLines.length - 1;
    if (n > 1) {
        var i;
        var iii = -1;
        for (i = 0; i <= n; i++) {
            var ShrtLine = DesLines[i].trim();
            if (IsDesignLine(ShrtLine)) {
                iii = iii + 1;
                this.juncs[iii] = new node();
                this.juncs[iii].ImportFromLine(ShrtLine);
            }
        }
        this.nJuncs = iii;
    }
};

design.prototype.ExportSchema = function () {
    // design schema (juns) to text
    var txt = '';
    var i;
    for (i = 0; i <= this.nJuncs; i++) {
        txt = txt + this.juncs[i].ExportToLine() + '\r\n';
    }
    return txt;
};


// END DESIGN STRUC

//CONVERT FUNCS



function ConvertJuncsToNodes(Juncs) {
    var nJuncs = Juncs.length - 1;
    var i, ii = -1;
    for(i = 0; i <= nJuncs; i++) {
        if (Juncs[i].iType !== 0) {
            ii = ii + 1;
        }
    }
    var nNodes = ii;
    var Nodes = [];
    ii = -1;
    for (i = 0; i <= nJuncs; i++) {
        if (Juncs[i].iType !== 0) {
            ii = ii + 1;
            Nodes[ii] = new node;
            Nodes[ii].iType = 1;
            Nodes[ii].iOpt = Juncs[i].iOpt;
            Nodes[ii].v = Juncs[i].v;
        }
    }
    return Nodes;
}
//this.iType = 0;
//this.iOpt = 0;
//this.iDir = 0; // -2 if iris, 2 if cav, +/-1 if step, 0 if small node, 1 if big node
//this.iGuide = -1; // adjacent big node count in lib
//this.iAper = -1; // adjacent aper count in lib
//this.v = [0, 0, 0, 0, 0, 0];


function ConvertNodesToIrises(Nodes) {
    var Irises = [];
    var i, nNodes = Nodes.length-1;
    //ReDim Irises(nNodes)
    for (i = 0; i <= nNodes; i++) {
        Irises[i] = new node;
        Irises[i].v = Nodes[i].v;
        Irises[i].iOpt = Nodes[i].iOpt;
        if (i > 0 && i < nNodes) {
            
            if (iDirJunc(Nodes[i - 1], Nodes[i]) === -1 && iDirJunc(Nodes[i], Nodes[i + 1]) === 1) {
                Irises[i].iType = 3;
            } else {
                Irises[i].iType = 1;
            }
        } else {
            Irises[i].iType = 1;
        }
    }

    for (i = 1; i <= nNodes - 1; i++) {
        if (Irises[i - 1].iType === 1 && Irises[i].iType === 1 && Irises[i + 1].iType === 1) {
            if (iDirJunc(Irises[i - 1], Irises[i]) === 1 && iDirJunc(Irises[i], Irises[i + 1]) === -1) {
                Irises[i].iType = 2;
            }

        }
    }

    var ii=-1, Juncs = [];
    for (i = 0; i <= nNodes - 1; i++) {
        ii = ii + 1;
        Juncs[ii] = new node;
        Juncs[ii] = Irises[i];
        if (Irises[i].iType === 1 && Irises[i + 1].iType === 1) {
            ii = ii + 1;
            Juncs[ii] = new node;
            Juncs[ii].iType = 0;
        }
    }
    Juncs[ii + 1] = new node;
    Juncs[ii + 1] = Irises[nNodes];
    Irises = null;
    return Juncs;
}

function ConvertNodesToCavities(Nodes) {
    var Irises = [];
    var i, nNodes = Nodes.length - 1;
    //ReDim Irises(nNodes)
    for (i = 0; i <= nNodes; i++) {
        Irises[i] = new node;
        Irises[i].v = Nodes[i].v;
        Irises[i].iOpt = Nodes[i].iOpt;
        if (i > 0 && i < nNodes) {

            if (iDirJunc(Nodes[i - 1], Nodes[i]) === 1 && iDirJunc(Nodes[i], Nodes[i + 1]) === -1) {
                Irises[i].iType = 2;
            } else {
                Irises[i].iType = 1;
            }
        } else {
            Irises[i].iType = 1;
        }
    }

    for (i = 1; i <= nNodes - 1; i++) {
        if (Irises[i - 1].iType === 1 && Irises[i].iType === 1 && Irises[i + 1].iType === 1) {
            if (iDirJunc(Irises[i - 1], Irises[i]) === -1 && iDirJunc(Irises[i], Irises[i + 1]) === 1) {
                Irises[i].iType = 3;
            }

        }
    }

    var ii = -1, Juncs = [];
    for (i = 0; i <= nNodes - 1; i++) {
        ii = ii + 1;
        Juncs[ii] = new node;
        Juncs[ii] = Irises[i];
        if (Irises[i].iType === 1 && Irises[i + 1].iType === 1) {
            ii = ii + 1;
            Juncs[ii] = new node;
            Juncs[ii].iType = 0;
        }
    }
    Juncs[ii + 1] = new node;
    Juncs[ii + 1] = Irises[nNodes];
    Irises = null;
    return Juncs;
}
