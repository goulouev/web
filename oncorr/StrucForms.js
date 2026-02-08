function corro() {
    //iUnit, F0, F1, Ao, Bo, At, Bt, Xt, Ac, Bc, Sc, Nc, Dc, Hc
    this.nUnit = 6;
    this.cfunit = .0209579;
    this.Fcut = 0;
    this.ind = 0;//iUnit
    this.Ao = 0;
    this.Bo = 0;
    this.At = 0;
    this.Bt = 0;
    this.Xt = 0;
    this.Nc = -1;
    this.Ac = 0;
    this.Bc = 0;
    this.Sc = 0;
    this.Xc = 0;
    this.Fr = 0;  //SYNTH, OPT
    this.FB0 = 0;  //SYNTH, OPT
    this.FB1 = 0;  //SYNTH, OPT
    this.iProto = 1;  //SYNTH
    this.Bmin = 0;  //SYNTH
    this.Bmax = 0;  //SYNTH
    this.Np = 200;  //SIM
    this.F0 = 0;  //SIM
    this.F1 = 0;  //SIM
    this.RL = 25; //OPT
    this.TR = 50; //OPT
    this.Nsteps = 10; //OPT
    this.dX = null;//OPT
    this.Xmin = null;//OPT
    this.Dc = [];
    this.Hc = [];
    this.Hmin = 0;
    this.OptD = true;
}

corro.prototype.setUnit = function () {
    if (this.ind === 0) {
        this.nUnit = 5; this.cfunit = .0209579;
        if (!this.Xmin) { this.Xmin = 0.5; }
    } else {
        this.nUnit = 6; this.cfunit = .532331;
        if (!this.Xmin) { this.Xmin = 0.02; }
    }
    if (this.Ac !== 0) {
        //Hmin defines tz Hmin=Ac/2/alfa, alfa=4 (proto=1),5(proto=2)
        this.Fcut = Math.PI / this.Ac / this.cfunit;
        if (!this.dX) { this.dX = 0.005 * this.Ac; }
        //if (this.iProto === 2) { this.Hmin = this.Ac / 10; } else { this.Hmin = this.Ac / 6;}
        if (this.iProto === 2) { this.Hmin = this.Ac / 10; } else { this.Hmin = this.Ac / 10; }
    }
};

corro.prototype.ImportBufer = function (txt) {
    var parts = txt.split("<>");
    var V = WordsToArray(parts[0]);
    this.ind = parseInt(V[0]);
    this.Ao = parseFloat(V[1]);
    this.Bo = parseFloat(V[2]);
    this.At = parseFloat(V[3]);
    this.Bt = parseFloat(V[4]);
    this.Xt = parseFloat(V[5]);
    this.Ac = parseFloat(V[6]);
    this.Bc = parseFloat(V[7]);
    this.Sc = parseFloat(V[8]);
    this.Xc = parseFloat(V[9]);
    this.Fr = parseFloat(V[10]);
    this.FB0 = parseFloat(V[11]);
    this.FB1 = parseFloat(V[12]);
    this.iProto = parseInt(V[13]);
    this.Bmin = parseFloat(V[14]);
    this.Bmax = parseFloat(V[15]);
    this.Np = parseInt(V[16]);
    this.F0 = parseFloat(V[17]);
    this.F1 = parseFloat(V[18]);
    this.Nc = parseInt(V[19]);
    this.RL = parseFloat(V[20]); //OPT
    this.TR = parseFloat(V[21]); //OPT
    this.Nsteps = parseInt(V[22]); //OPT
    this.dX = parseFloat(V[23]);//OPT
    this.Xmin = parseFloat(V[24]);//OPT

    var LNs = parts[1].replace("\r", "").split("\n");
    var ii = -1;
    var Ncc = LNs.length - 1;
    for (i = 0; i <= Ncc; i++) {
        if (trim(LNs[i]) !== "") {
            V = WordsToArray(LNs[i]);
            if (areNumbers(V) === 1) {
                ii = ii + 1;
                this.Dc[ii] = parseFloat(V[0]); this.Hc[ii] = parseFloat(V[1]);
            }
        }
    }
    this.Nc = ii;
};

corro.prototype.ExportBufer = function () {
    this.setUnit();
    var i, rs = this.ind;
    rs = rs + " " + this.Ao.toFixed(this.nUnit);
    rs = rs + " " + this.Bo.toFixed(this.nUnit);
    rs = rs + " " + this.At.toFixed(this.nUnit);
    rs = rs + " " + this.Bt.toFixed(this.nUnit);
    rs = rs + " " + this.Xt.toFixed(this.nUnit);
    rs = rs + " " + this.Ac.toFixed(this.nUnit);
    rs = rs + " " + this.Bc.toFixed(this.nUnit);
    rs = rs + " " + this.Sc.toFixed(this.nUnit);
    rs = rs + " " + this.Xc.toFixed(this.nUnit);
    rs = rs + " " + this.Fr.toFixed(4);
    rs = rs + " " + this.FB0.toFixed(4);
    rs = rs + " " + this.FB1.toFixed(4);
    rs = rs + " " + this.iProto;
    rs = rs + " " + this.Bmin.toFixed(this.nUnit);
    rs = rs + " " + this.Bmax.toFixed(this.nUnit);
    rs = rs + " " + this.Np;
    rs = rs + " " + this.F0.toFixed(4);
    rs = rs + " " + this.F1.toFixed(4);
    rs = rs + " " + this.Nc;
    rs = rs + " " + this.RL.toFixed(1); //OPT
    rs = rs + " " + this.TR.toFixed(1); //OPT
    rs = rs + " " + this.Nsteps; //OPT
    rs = rs + " " + this.dX.toFixed(this.nUnit);//OPT
    rs = rs + " " + this.Xmin.toFixed(this.nUnit);//OPT
    rs = rs + "<>";
    for (i = 0; i <= this.Nc; i++) {
        rs = rs + "     " + this.Dc[i].toFixed(this.nUnit);
        rs = rs + "     " + this.Hc[i].toFixed(this.nUnit) + "\r\n";
    }
    return rs;
};

corro.prototype.ExportCorros = function () {
    //this.setUnit();
    var i, xx = "";
    for (i = 0; i <= this.Nc; i++) {
        xx = xx + "     " + this.Dc[i].toFixed(this.nUnit);
        xx = xx + "     " + this.Hc[i].toFixed(this.nUnit) + "\r\n";
    }
    return xx;
};

corro.prototype.ImportCorros = function (txt) {
    var LNs = txt.replace("\r", "").split("\n");
    var ii = -1;
    var Ncc = LNs.length - 1;
    for (i = 0; i <= Ncc; i++) {
        if (trim(LNs[i]) !== "") {
            V = WordsToArray(LNs[i]);
            if (areNumbers(V) === 1) {
                ii = ii + 1;
                this.Dc[ii] = parseFloat(V[0]); this.Hc[ii] = parseFloat(V[1]);
            }
        }
    }
    this.Nc = ii;
    this.Dc[this.Nc + 1] = this.Dc[0];
};
//#######################################################
corro.prototype.ExportInDat = function () {
    var txt = '';
    txt = txt + this.ind + " 10 10\r\n";
    txt = txt + "2 0.050 1.000  ";
    txt = txt + this.Bmin + " " + this.Bmax + " " + this.Bmin + " " + this.iProto + " 0\r\n";
    txt = txt + this.Fr + " " + this.Fr / 4 + "\r\n";
    txt = txt + this.F0 + " " + this.F1 + " " + this.Np + "\r\n";
    txt = txt + "1   20 " + this.Xmin + " " + this.Xmin + " 0.01\r\n";
    txt = txt + "1  1  " + this.FB0 + " " + this.FB1 + " " + this.RL + " 20\r\n";
    txt = txt + "1  2  " + this.Fr + " " + this.Fr + " " + this.TR + " 0\r\n";
    return txt;
};
corro.prototype.ExportStrucDat = function () {
    var txt = '', i, N = this.nUnit;
    txt = txt + "-1" + '  ' + "1" + '  ' + "0" + '\r\n';
    txt = txt + "0" + '  ' + "0" + '  ' + "0" + '\r\n';
    txt = txt + "1" + '  ' + "10" + '  ' + "10" + '\r\n';
    txt = txt + '1  1' + '\r\n';
    txt = txt + "1" + '  ' + "0" + '  ' + "0" + '\r\n';

    txt = txt + "0" + '   ' + "1" + '   ' + junc(this.Ao / 2, this.Ao, this.Bo) + '   ' + '\r\n';
    txt = txt + "0" + '   ' + "0" + '   ' + junc(0, 0, 0) + '   ' + "0.0" + '\r\n';
    txt = txt + "0" + '   ' + "1" + '   ' + junc(this.Xt, this.At, this.Bt) + '   ' + '\r\n';
    txt = txt + "0" + '   ' + "0" + '   ' + junc(0, 0, 0) + '   ' + "0.0" + '\r\n';
    for (i = 0; i <= this.Nc; i++) {
        txt = txt + "0" + '   ' + "1" + '   ' + junc(this.Dc[i], this.Ac, this.Bc) + '   ' + '\r\n';
        var Bmax = 2 * this.Hc[i] + this.Bc;
        txt = txt + "5" + '   ' + "2" + '   ' + junc(this.Sc, this.Ac, Bmax) + '   ' + "0.0" + '\r\n';
    }
    txt = txt + "0" + '   ' + "1" + '   ' + junc(this.Dc[0], this.Ac, this.Bc) + '   ' + '\r\n';
    txt = txt + "0" + '   ' + "0" + '   ' + junc(0, 0, 0) + '   ' + "0.0" + '\r\n';
    txt = txt + "0" + '   ' + "1" + '   ' + junc(this.Xt, this.At, this.Bt) + '   ' + '\r\n';
    txt = txt + "0" + '   ' + "0" + '   ' + junc(0, 0, 0) + '   ' + "0.0" + '\r\n';
    txt = txt + "0" + '   ' + "1" + '   ' + junc(this.Ao / 2, this.Ao, this.Bo) + '   ' + '\r\n';
    return txt;
    function junc(d, a, b) {
        return d.toFixed(N) + '   ' + half(-a) + '   ' + half(-b) + '   ' + half(a) + '   ' + half(b);
    }
    function half(a) {
        var x = a / 2;
        if (a < 0) {
            return x.toFixed(N);
        } else {
            return " " + x.toFixed(N);
        }
    }
};
//#######################################################
corro.prototype.ImportBufer0 = function (txt) {
    var V = WordsToArray(txt);
    this.ind = parseInt(V[0]);
    this.Ac = parseFloat(V[1]);
    this.Fr = parseFloat(V[2]);
    this.Bmin = parseFloat(V[3]);
    this.Bmax = parseFloat(V[4]);
    this.setUnit();
};

corro.prototype.ExportBufer0 = function () {
    this.setUnit();
    var out = this.ind + " " + this.Ac.toFixed(this.nUnit) + " " + this.Fr.toFixed(4);
    out = out + " " + this.Bmin.toFixed(3) + " " + this.Bmax.toFixed(3) + " " + this.iProto;
    return out;
};

corro.prototype.ImportBufer1 = function (txt) {
    var V = WordsToArray(txt);
    this.Ao = parseFloat(V[0]);
    this.Bo = parseFloat(V[1]);
    this.At = parseFloat(V[2]);
    this.Bt = parseFloat(V[3]);
    this.Xt = parseFloat(V[4]);
    this.Ac = parseFloat(V[5]);
    this.Bc = parseFloat(V[6]);
    this.Sc = parseFloat(V[7]);
    this.FB0 = parseFloat(V[8]);
    this.FB1 = parseFloat(V[9]);
    this.F0 = parseFloat(V[8]);
    this.F1 = parseFloat(V[9]);
};

corro.prototype.ExportBufer1 = function () {
    this.setUnit();
    var buf1 = this.Ao.toFixed(this.nUnit);
    buf1 = buf1 + " " + this.Bo.toFixed(this.nUnit);
    buf1 = buf1 + " " + this.At.toFixed(this.nUnit);
    buf1 = buf1 + " " + this.Bt.toFixed(this.nUnit);
    buf1 = buf1 + " " + this.Xt.toFixed(this.nUnit);
    buf1 = buf1 + " " + this.Ac.toFixed(this.nUnit);
    buf1 = buf1 + " " + this.Bc.toFixed(this.nUnit);
    buf1 = buf1 + " " + this.Sc.toFixed(this.nUnit);
    buf1 = buf1 + " " + this.FB0.toFixed(4);
    buf1 = buf1 + " " + this.FB1.toFixed(4);
    buf1 = buf1 + " " + this.F0.toFixed(4);
    buf1 = buf1 + " " + this.F1.toFixed(4);
    return buf1;
};

corro.prototype.ImportBufer2 = function (txt) {
    var LNs = txt.replace("\r", "").split("\n");
    var ii = -1;
    var Ncc = LNs.length - 1;
    for (i = 0; i <= Ncc; i++) {
        if (trim(LNs[i]) !== "") {
            V = WordsToArray(LNs[i]);
            if (areNumbers(V) === 1) {
                ii = ii + 1;
                this.Dc[ii] = parseFloat(V[0]); this.Hc[ii] = parseFloat(V[1]);
            }
        }
    }
    this.Nc = ii;
    this.Dc[this.Nc + 1] = this.Dc[0];
};

corro.prototype.ExportBufer2 = function () {
    this.setUnit();
    var i, xx = "";
    for (i = 0; i <= this.Nc; i++) {
        xx = xx + "     " + this.Dc[i].toFixed(this.nUnit);
        xx = xx + "     " + this.Hc[i].toFixed(this.nUnit) + "\r\n";
    }
    return xx;
};

corro.prototype.ExportPars = function () {
    var PAR = [0, 0, 0, 0, 0, 0, 0, 0];
    PAR[0] = this.Nc; PAR[1] = this.Ao; PAR[2] = this.Bo;
    PAR[3] = this.At; PAR[4] = this.Ac; PAR[5] = this.Bc; PAR[6] = this.Xc;
    PAR[7] = this.Sc;
    return PAR;
};

corro.prototype.ExportVars = function () {
    var X = [], N=this.Nc;
    var i, ii=-1, NN = Math.floor((N + 1) / 2);
    if (this.OptD) {
        for (i = 0; i <= NN; i++) { ii = ii + 1; X[ii] = this.Dc[i]; }
    }
    for (i = 0; i <= N - NN; i++) { ii = ii + 1; X[ii] = this.Hc[i]; }
    X[ii + 1] = this.Bt; X[ii + 2] = this.Xt;
    return X;
};

corro.prototype.ImportVars = function (X) {
    var N = this.Nc;
    var i, ii = -1; NN = Math.floor((N + 1) / 2);
    if (this.OptD) {
        for (i = 0; i <= NN; i++) {
            ii = ii + 1;
            if (X[ii] < this.Xmin) {
                this.Dc[i] = this.Xmin; this.Dc[N + 1 - i] = this.Xmin;
            } else {
                this.Dc[i] = X[ii]; this.Dc[N + 1 - i] = X[ii];
            }
        }
    }
    for (i = 0; i <= N - NN; i++) {
        ii = ii + 1;
        if (X[ii] < this.Hmin) {
            this.Hc[i] = this.Hmin; this.Hc[N - i] = this.Hmin;
        } else {
            this.Hc[i] = X[ii]; this.Hc[N - i] = X[ii];
        }
    }
    this.Bt = X[ii+1]; this.Xt = X[ii+2];
};

//###################  STRINGS  ###########################
function isNull(x) {
    var delta = 1E-6;
    if (isNaN(x)) {
        return true;
    } else {
        if (Math.abs(x) < delta && NaN(x)) { return true; } else { return false; }
    }
}

function areNumbers(V) {
    if (!Array.isArray) {
        Array.isArray = function (arg) {
            return Object.prototype.toString.call(arg) === '[object Array]';
        };
    } if (!Array.isArray(V)) { return -1; }
    var i, N = V.length - 1;
    for (i = 0; i <= N; i++) {
        if (isNaN(V[i])) { return -1; }
    }
    return N;
}

function WordsToArray(str) {
    // text line to array of words
    return ReduceWhiteSpaces(str).split(/\s/g);
}

function ReduceWhiteSpaces(str) {
    return trim(str).replace(/\s+/g, ' ');
}

function trim(str) {
    return str.replace(/^\s+|\s+$/g, '');
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
