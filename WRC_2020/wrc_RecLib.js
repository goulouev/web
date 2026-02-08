//#####################################################
var pi = 3.1415926535897932384626433832795;
var iSpace = [0,0,0,0,0,0,0,0,0];
//   iSpace=[iSymX,iSymY,iSymZ,modDom,nDom,mDom,modMax,nMax,mMax]
//            0     1      2     3     4    5     6     7    8
// JUNC DATA MATH


function ModeNums(i) {
    var Nx = iSpace[7], Ny = iSpace[8];
    var iimod, iin, iim;
    var iiv = chet(i / (Ny + 1));
    iim = i - iiv * (Ny + 1);
    iimod = chet(iiv / (Nx + 1));
    iin = iiv - iimod * (Nx + 1);
    return [iimod, iin, iim];
}

function iModeNum(mod, n, m) {
    var Nx = iSpace[7], Ny = iSpace[8];
    return (Nx + 1) * (Ny + 1) * mod + (Ny + 1) * n + m;
}

function xDom(Guide) {
    //Gets x from Guide Array
    var ix = iModeNum(0, iSpace[4], iSpace[5]);
    return Guide[ix];
}


function chet(x) {
    if (isNaN(x)) {
        return NaN;
    }
    if (x > 0) {
        return Math.floor(x);
    }
    return Math.ceil(x);
}

function ExportModesToText(modes) {
    var i, rs = '';
    //var iMax = iModeNum(iSpace[6], iSpace[7], iSpace[8]);
    var iMax = modes.length - 1;
    for (i = 0; i <= iMax; i++) {
        var nums = ModeNums(i);
        rs = rs + i + ' ' + nums[0] + ' ' + nums[1] + ' ' + nums[2] + ' ' + modes[i] + '\r\n';
    }
    return rs;
}

function oLoss(x0, y0, x1, y1, Ao, Bo) {
    return 1 - (x1 - x0) * (y1 - y0) / Ao / Bo;
}

function CalcApers(x0, y0, x1, y1, Ao, Bo) {
    var iSymX = iSpace[0], iSymY = iSpace[1], mdkl = iSpace[3];
    var k = iSpace[4], l = iSpace[5], mod = iSpace[6], n = iSpace[7], m = iSpace[8];
    var im, ix, iy;
    var Apers = new Array();
    for (im = 0; im <= mod; im++) {
        for (ix = 0; ix <= n; ix++) {
            for (iy = 0; iy <= m; iy++) {
                var i = iModeNum(im, ix, iy);
                Apers[i] = CalcAper(mdkl, k, l, im, ix, iy, x0, y0, x1, y1, Ao, Bo);
           }
        }
    }
    return Apers;
}

function CalcNodes(a, b) {
    var iSymX = iSpace[0], iSymY = iSpace[1], n = iSpace[7], m = iSpace[8];
    var ix, iy;
    var modes = new Array();
    for (ix = 0; ix <= n; ix++) {
        for (iy = 0; iy <= m; iy++) {
            var i = iModeNum(0, ix, iy);
            modes[i] = CalcNode(ix, iy, a, b);
        }
    }
    return modes;
}

function CalcNode(n, m, a, b) {
    return xEH(iSpace[0], iSpace[1], n, m, a, b);
}

function CalcAper(mdkl, k, l, mod, n, m, x0, y0, x1, y1, Ao, Bo) {
    if (mdkl === 0 && mod === 0) {
        return aMM(iSpace[0], iSpace[1], k, l, n, m, x0, y0, x1, y1, Ao, Bo);
    }
    if (mdkl === 0 && mod === 1) {
        return aME(iSpace[0], iSpace[1], k, l, n, m, x0, y0, x1, y1, Ao, Bo);
    }
    if (mdkl === 1 && mod === 1) {
        return aMM(iSpace[0], iSpace[1], k, l, n, m, x0, y0, x1, y1, Ao, Bo);
    }
    return 0;
}
// APER JUNCS

function RecCav(k, a, b, d, A0, W, A1) {
    var u = Array(1, 0);
    var C0 = Array(0, 0), C = Array(0, 0), C1 = Array(0, 0);
    var im, ix, iy, i, ii;
    for (md = 0; md <= iSpace[6]; md++) {
        for (n = 0; n <= iSpace[7]; n++) {
            for (m = 0; m <= iSpace[8]; m++) {
                i = iModeNum(md, n, m);
                ii = iModeNum(0, n, m);
                var Aem0 = A0[i], Aem1 = A1[i];
                var xnm = W[ii];
                var gm = beta(k, xnm);
                var yt = yw(md, k, gm);
                var vm = cexp(phase(gm, d));
                var ct = xdy(xpy(u, xin2(vm)), xmy(u, xin2(vm)));
                var cs = xdy(vm, xmy(u, xin2(vm)));
                var dC0 = aox(Aem0 * Aem0, xoy(yt, ct));
                var dC = aox(Aem0 * Aem1, xoy(yt, cs));
                var dC1 = aox(Aem1 * Aem1, xoy(yt, ct));
                C0 = xpy(C0, dC0);
                C = xpy(C, dC);
                C1 = xpy(C1, dC1);
            }
        }
    }
    var y = Array(C0[0], C0[1], -2 * C[0], -2 * C[1], C1[0], C1[1]);
    return y;
}

function RecCavC(k, rm, a, b, d, o0, A0, W, o1, A1) {
    //Get rm in freq calc
    var zs = cmplx(rm, rm);
    var iSymX = iSpace[0], iSymY = iSpace[1];
    var u = Array(1, 0);
    var C0 = Array(0, 0), C = Array(0, 0), C1 = Array(0, 0);
    var im, ix, iy, i, ii;
    for (md = 0; md <= iSpace[6]; md++) {
        for (n = 0; n <= iSpace[7]; n++) {
            for (m = 0; m <= iSpace[8]; m++) {
                if (IsMode(iSymX, iSymY, md, n, m)) {
                    i = iModeNum(md, n, m);
                    var Aem0 = A0[i], Aem1 = A1[i];
                    //var gm = beta(k, xnm);
                    //nSym(iSym, n) nn=nSym(iSymX, n) mm=nSym(iSymY, m)
                    //var gm = betC(k, rm, a, b, imode, n, m);
                    var gm = betC(k, rm, a, b, md, nSym(iSymX, n), nSym(iSymY, m));
                    var yt = yw(md, k, gm);
                    var zsyn = xoy(zs, yt);
                    var cx0 = [1 + o0 * zsyn[0], o0 * zsyn[1]];
                    var cx1 = [1 + o1 * zsyn[0], o1 * zsyn[1]];
                    var cx = csqrt(xoy(cx0, cx1));
                    var vm = cexp(phase(gm, d));
                    var ct = xdy(xpy(u, xin2(vm)), xmy(u, xin2(vm)));
                    var cs = xdy(vm, xmy(u, xin2(vm)));
                    var dC0 = xdy(aox(Aem0 * Aem0, xoy(yt, ct)), cx0);
                    var dC = xdy(aox(Aem0 * Aem1, xoy(yt, cs)), cx);
                    var dC1 = xdy(aox(Aem1 * Aem1, xoy(yt, ct)), cx1);
                    C0 = xpy(C0, dC0);
                    C = xpy(C, dC);
                    C1 = xpy(C1, dC1);
                }
            }
        }
    }
    var y = Array(C0[0], C0[1], -2 * C[0], -2 * C[1], C1[0], C1[1]);
    return y;
}


function RecStep(k, W, A) {
    //iSymX,iSymY,iSymZ
    //  0    1      2
    //modDom, nDom, mDom
    //  3      4     5
    // modMax, nMax, mMax
    //  6      7      8
    //          A-aper, W-big guide, iDom=[iDomMod,nDom,mDom]
    var jB = Array(0, 0), Y = Array(0, 0);
    var im, i, ii;
    for (md = 0; md <= iSpace[6]; md++) {
        for (n = 0; n <= iSpace[7]; n++) {
            for (m = 0; m <= iSpace[8]; m++) {
                i = iModeNum(md, n, m);
                ii = iModeNum(0, n, m);
                var Aem = A[i];
                var xnm = W[ii];
                var gm = beta(k, xnm);
                var yt = yw(md, k, gm);

                if (iSpace[3] === md && iSpace[4] === n && iSpace[5] === m) {
                    Y = aox(Aem * Aem, yt);
                } else {
                    jB = xpy(jB, aox(Aem * Aem, yt));
                }

            }
        }
    }
    return([jB[0], jB[1], Y[0], Y[1]]);
}

function RecStepC(k, rm, a, b, o, A) {
    var zs = cmplx(rm, rm);
    var iSymX = iSpace[0], iSymY = iSpace[1];
    var jB = Array(0, 0), Y = Array(0, 0);
    var im, i, ii;
    for (md = 0; md <= iSpace[6]; md++) {
        for (n = 0; n <= iSpace[7]; n++) {
            for (m = 0; m <= iSpace[8]; m++) {
                if (IsMode(iSymX, iSymY, md, n, m)) {
                    i = iModeNum(md, n, m);
                    var Aem = A[i];
                    var gm = betC(k, rm, a, b, md, nSym(iSymX, n), nSym(iSymY, m));
                    var yt = yw(md, k, gm);
                    var zsyn = xoy(zs, yt);
                    var cx = [1 + o * zsyn[0], o * zsyn[1]];
                    var yto = xdy(yt, cx);
                    if (iSpace[3] === md && iSpace[4] === n && iSpace[5] === m) {
                        Y = aox(Aem * Aem, yto);
                    } else {
                        jB = xpy(jB, aox(Aem * Aem, yto));
                    }
                }
            }
        }
    }
    return [jB[0], jB[1], Y[0], Y[1]];
}

function RecIris(k, W0, A0, x, d, W1, A1) {
    var step0 = RecStep(k, W0, A0);
    var step1 = RecStep(k, W1, A1);
    var gm = beta(k, x);
    var Y = yw(iSpace[3], k, gm);
    var jB0 = [step0[0], step0[1]], Y0 = [step0[2], step0[3]];
    var S0 = Shunt(Y0, jB0, Y);
    var jB1 = [step1[0], step1[1]], Y1 = [step1[2], step1[3]];
    var S1 = Shunt(Y, jB1, Y1);
    var So = Snode(gm, d);
    return Cascade(S0, Cascade(So, S1));
}

function RecIrisC(k, rm, a0, b0, o0, A0, a, b, d, a1, b1, o1, A1) {
    var iSymX = iSpace[0], iSymY = iSpace[1], md = iSpace[3];
    var md = iSpace[3], n = iSpace[4], m = iSpace[5];
    var step0 = RecStepC(k, rm, a0, b0, o0, A0);
    var step1 = RecStepC(k, rm, a1, b1, o1, A1);

    //var step0 = RecStep(k, W0, A0);
    //var step1 = RecStep(k, W1, A1);
    var gm = betC(k, rm, a, b, md, nSym(iSymX, n), nSym(iSymY, m));
    // var gm = beta(k, x);
    var Y = yw(md, k, gm);
    var jB0 = [step0[0], step0[1]], Y0 = [step0[2], step0[3]];
    var S0 = Shunt(Y0, jB0, Y);
    var jB1 = [step1[0], step1[1]], Y1 = [step1[2], step1[3]];
    var S1 = Shunt(Y, jB1, Y1);
    var So = Snode(gm, d);
    return Cascade(S0, Cascade(So, S1));
}

// APER MATH 
function Cme(iSymX, iSymY, k, l, a, b, n, m, Ao, Bo) {
    var xKL = xEH(iSymX, iSymY, k, l, a, b);
    var xNM = xEH(iSymX, iSymY, n, m, Ao, Bo);
    if (xKL === 0 || xNM === 0) {
        return 0;
    } else {
        return 1. / xKL / xNM * cNorm(iSymX, k, a) * cNorm(iSymY, l, b) * cNorm(iSymX, n, Ao) * cNorm(iSymY, m, Bo);
    }
}

function aEE(iSymX, iSymY, k, l, n, m, x0, y0, x1, y1, Ao, Bo) {
    var a = x1 - x0;
    var b = y1 - y0;
    var xk = vn(iSymX, k, a);
    var xl = vn(iSymY, l, b);
    var xN = vn(iSymX, n, Ao);
    var xM = vn(iSymY, m, Bo);
    var C = Cme(iSymX, iSymY, k, l, a, b, n, m, Ao, Bo);
    return C * (xk * xN * vc(xk, xN, x0, x1) * wc(xl, xM, y0, y1) + xl * xM * wc(xk, xN, x0, x1) * vc(xl, xM, y0, y1));
}

function aME(iSymX, iSymY, k, l, n, m, x0, y0, x1, y1, Ao, Bo) {
    var a = x1 - x0;
    var b = y1 - y0;
    var xk = vn(iSymX, k, a);
    var xl = vn(iSymY, l, b);
    var xN = vn(iSymX, n, Ao);
    var xM = vn(iSymY, m, Bo);
    var C = Cme(iSymX, iSymY, k, l, a, b, n, m, Ao, Bo);
    return -C * (xl * xN *vc(xk, xN, x0, x1) *wc(xl, xM, y0, y1) - xk * xM *wc(xk, xN, x0, x1) *vc(xl, xM, y0, y1));
}

function aMM(iSymX, iSymY, k, l, n, m, x0, y0, x1, y1, Ao, Bo) {
    var a = x1 - x0;
    var b = y1 - y0;
    var xk = vn(iSymX, k, a);
    var xl = vn(iSymY, l, b);
    var xN = vn(iSymX, n, Ao);
    var xM = vn(iSymY, m, Bo);
    var C = Cme(iSymX, iSymY, k, l, a, b, n, m, Ao, Bo);
    return C * (xk * xN *wc(xk, xN, x0, x1) *vc(xl, xM, y0, y1) + xl * xM *vc(xk, xN, x0, x1) *wc(xl, xM, y0, y1));
}

function yNorm(iSymX, iSymY, mode, n, m, Ao, Bo) {
    //Lumped Yw correction factor by PV
    var Vx;
    var xn = vn(iSymX, n, Ao);
    var xm = vn(iSymY, m, Bo);
    var Emax = cNorm(iSymX, n, Ao) * cNorm(iSymY, m, Bo) / sqrt(xn * xn + xm * xm);
    if (mode === 0) {
        Vx = Emax * xm * Ao; Vy = Emax * xn * Bo;
    } else {
        Vx = Emax * xn * Ao; Vy = Emax * xm * Bo;
    }
    return 1. / (Vx * Vx + Vy * Vy);
}

//########################## INTEGRALS #########################

function uc(v, w, x0, x) {
    return Math.cos(v * (x - x0)) * Math.sin(w * x) - Math.sin(w * x0);
}

function vc(v, w, x0, x) {
    //  x
    //V=| cos(v(x - x0)) * cos(wx)dx
    //  x0
    return vis(v, w, x0, x) + vis(v, -w, x0, x);
}


function wc(v, w, x0, x) {
    //x
    //V=| sin(v(x - x0)) * sin(wx)dx
    //x0
    return -vis(v, w, x0, x) + vis(v, -w, x0, x);
}

function vis(v, w, x0, x) {
    var er = Math.abs((v + w) * (x - x0));
    if (er < 1e-6) {
        return 0.5 * (x - x0) * Math.cos(v * x0);
    } else {
        return 0.5 * (Math.sin(v * (x - x0) + w * x) - Math.sin(w * x0)) / (v + w);
    }
}


//#####################################################


function cNorm(iSym, n, A) {
    //cNorm=1 / sqrt(| fi(n, x) * fi(n, x)dx)
    if (n === 0 && iSym >= 0) {
        return Math.sqrt(1. / A);
    } else {
        return Math.sqrt(2. / A);
    }
}

function vn(iSym, n, A) {
    //Eigen Value Line Interval
    var iPH = 0;
    if (iSym < 0) { iPH = 1; }
    return pi * ((Math.abs(iSym) + 1) * n + iPH) / A;
}

function xEH(iSymX, iSymY, n, m, A, B) {
//Eigen Value Rec Form
    var vx = vn(iSymX, n, A);
    var vy = vn(iSymY, m, B);
    return Math.sqrt(vx * vx + vy * vy);
}

function nSym(iSym, n) {
    //Symmetry converter iSym - 1 for H - sym, 0 for No sym, 1 for E - sym
    var iPH = 0;
    if (iSym < 0) { iPH = 1; }
    return (Math.abs(iSym) + 1) * n + iPH;
}

function IsMode(iSymX, iSymY, km, kx, ky) {
    var mod, n, m;
    mod = km; n = nSym(iSymX, kx); m = nSym(iSymY, ky);
    if (mod === 0) {
        if (n + m === 0) { return false; }
    } else {
        if (n * m === 0) { return false; }
    }
    return true;
}

