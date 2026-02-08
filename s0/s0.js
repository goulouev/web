﻿var Nx, pi = 3.14159265358979, pi2 = 1.5707963267949;


//Public Sub Struc(k, N, jw, w, jC, C, So)
function Struc(k, N, jw, w, jc, c) {
    var So = [];
    var S0 = [], Bt = [];
    var i, ii;
    Nx = 20;
    So = [0, 0, 1, 0, 1, 0, 0, 0];
    for (i = 0; i <= N; i++) {
        Bt[0] = k; Bt[1] = 0; x = w[i][0];
        S0 = Snode(Bt, x);
        So = Cascade(So, S0);
        S0 = EL(k, jw[i], w[i], jc[i], c[i], jw[i + 1], w[i + 1]);
        So = Cascade(So, S0);
     }
    Bt[0] = k; Bt[1] = 0; x = w[N + 1][0];
    S0 = Snode(Bt, x);
    So = Cascade(So, S0);
     return So;
}
function Balance(So) {
    var R0, T0, SS = [0, 0];
    R0 = So[0] * So[0] + So[1] * So[1]; T0= So[2] * So[2] + So[3] * So[3];
    R1 = So[4] * So[4] + So[5] * So[5]; T1= So[6] * So[6] + So[7] * So[7];
    SS[0] =R0+T0;
    SS[1] = R1+T1;
    return R0 + " + " + T0 + " =" + SS[0] + "\n" + R1 + " + " + T1 + " =" + SS[1] + "\n";


}
function EL(k, jW0, W0, jC, C, jW1, W1) {
     var A0 = [], X0 = [], A1 = [], X1 = [];
    StripJunc(jW0[1], jW0[2], W0[1], W0[2], jC[1], jC[2], C[1], C[2], X0, A0);
    StripJunc(jW1[1], jW1[2], W1[1], W1[2], jC[1], jC[2], C[1], C[2], X1, A1);
    return Js(k, C[0], X0, A0, X1, A1);
}

function Js(k, x, X0, A0, X1, A1) {
    var S0 = new Array(4);
    var S1 = new Array(4);
    var R = 0, R0 = 0, R1 = 0;
    var N, O, O0;
    for (N = 0; N <= Nx; N++) {
        var g2 = k * k - X0[N] * X1[N];
        if (g2 < 0) {
            w = Math.sqrt(-g2);
            O0 = k / w / tanh(w * x);
            if (w * x > 15) {
                O = 0;
            } else {
                O = k / w / sinh(w * x);
            }
        } else {
            w = Math.sqrt(g2);
            O0 = -k / w / Math.tan(w * x);
            O = -k / w / Math.sin(w * x);
        }
        R0 = R0 + O0 * A0[N] * A0[N];
        R1 = R1 + O0 * A1[N] * A1[N];
        R = R + O * A0[N] * A1[N];
    }

    S0 = Spath(R0, R, R1);
    S1 = Spath(R1, R, R0);
    return [S0[0], S0[1], S0[2], S0[3], S1[2], S1[3], S1[0], S1[1]];
}
//Sub Spath(R0, R, R1, S0, S1)
//Dim y(1)
//Call Yin(R0, R, R1, y)

//M = (1 + y(0)) ^ 2 + (y(1)) ^ 2
//S0(0) = (1 - y(0) * y(0) - y(1) * y(1)) / M
//S0(1) = -2 * y(1) / M
//MR = 1 + R1 * R1
//S1(0) = R * (S0(1) + (1 - S0(0)) * R1) / MR
//S1(1) = R * (1 - S0(0) - R1 * S0(1)) / MR
//End Sub

function Spath(R0, R, R1) {
    var y = [], S = [0, 0, 0, 0];
    y = Yin(R0, R, R1);
    var M = (1 + y[0]) * (1 + y[0]) + y[1] * y[1];
    S[0] = (1 - y[0] * y[0] - y[1] * y[1]) / M;
    S[1] = -2 * y[1] / M;
    var MR = 1 + R1 * R1;
    S[2] = R * (S[1] + (1 - S[0]) * R1) / MR;
    S[3] = R * (1 - S[0] - R1 * S[1]) / MR;
    return S;
}

function Yin(R0, R, R1) {
    var y = [0, 0];
    Rs = R * R - R0 * R1; Ms = Rs * Rs + R0 * R0;
    y[0] = R * R / Ms; y[1] = (R1 * Rs - R0) / Ms;
    return y;
}

function StripJunc(is0, is1, xs0, xs1, i0, i1, X0, X1, Vx, ax) {
    var ep = 1;
    if (is0 + is1 === 0) { ep = 2; }
    var Aso = xs1 - xs0;
    var Ao = Math.sqrt(2 / ep / Aso);
    var v = pi2 * (is0 + is1) / (xs1 - xs0);
    var a = X1 - X0;
    var TS1 = Tn(Ao, is0, v, Aso);
    var TS0 = Tn(Ao, is0, v, 0);
    var RS1 = Rn(Ao, is0, v, Aso);
    var RS0 = Rn(Ao, is0, v, 0);
    var An0 = Math.sqrt(2 / a);
    var AN1 = Math.sqrt(1 / a);
    var N;
    for (N = 0; N <= Nx; N++) {
        var An = An0;
        if (N + i0 + i1 === 0) { An = AN1; }
        var mu = pi2 * (2 * N + i0 + i1) / a;
        Vx[N] = mu;
        var T1 = Tn(An, i0, mu, xs1 - X0);
        var T0 = Tn(An, i0, mu, xs0 - X0);
        var R1 = Rn(An, i0, mu, xs1 - X0);
        var R0 = Rn(An, i0, mu, xs0 - X0);
        if (mu === v) {
            if (v === 0) {
                ax[N] = T0 * TS0 * Aso;
            } else {
                ax[N] = 0.5 * (Aso * (T1 * TS1 + R1 * RS1) + (T1 * RS1 - T0 * RS0) / v);
            }
        } else {
            var D1 = v * T1 * RS1 - mu * R1 * TS1;
            var D0 = v * T0 * RS0 - mu * R0 * TS0;
            ax[N] = -(D1 - D0) / (mu * mu - v * v);
        }
    }

}

function Tn(An, i, mu, x) {
    return An * Math.cos(mu * x - pi2 * i);
}

function Rn(An, i, mu, x) {
    pi2 = 1.5707963267949;
    return An * Math.sin(mu * x - pi2 * i);
}


function Sinvert(S, Sinv) {
    var i, j;
    for (i = 0; i <= 3; i++) {
        for (j = 0; j <= 1; j++) {
            Sinv[2 * i + j] = S[2 * (3 - i) + j];
        }
    }
}


function SdB(So, RdB, TdB) {
    var R = [], T = [1];
    R[0] = So[0][0][0]; R[1] = So[0][0][1];
    T[0] = So[0][1][0]; T[1] = So[0, 1, 1];
    RdB = dB(R); TdB = dB(T);
}


function dB(x) {
    R = cabs(x);
    if (R > 1E-20) {
        return 20 * Log(R) / Log(10);
    } else {
        return -400;
    }
}

function cosh(x) {
    var y;
    y = Math.exp(x);
    return 0.5 * (y + 1 / y);
}

function sinh(x) {
    var y;
    y = Math.exp(x);
    return 0.5 * (y - 1 / y);
}

function tanh(x) {
    var y;
    if (x < 0) {
        y = Math.exp(2 * x); return (y - 1) / (y + 1);
    } else {
        y = Math.exp(-2 * x); return (1 - y) / (1 + y);
    }
}

function dB_Line(f, So) {
    var spac = "       ";
    var dBL = f.toFixed(5);
    var i;
    for (i = 0; i <= 3; i++) {
        var R = Math.sqrt(So[2 * i] * So[2 * i] + So[2 * i + 1] * So[2 * i + 1]);

        if (R < 1E-20) { R = 1E-20; }
        var dBs = 20 * Math.log(R)/Math.log(10);
        var dBL = dBL + spac + dBs.toFixed(5);
    }
    return dBL;
}

function kf(iUnit, f) {
    if (iUnit === 0) {
        return 0.0209579 * f;
    } else {
        return 0.5323312 * f;
    }
}


function phase(bt, d) {
    // -j*bt*d
    return Array(bt[1] * d, -bt[0] * d);
}

function Snode(bt, d) {
    // phase=exp(-j*beta*d)
    var ph = cexp(phase(bt, d));
    return [0, 0, ph[0], ph[1], ph[0], ph[1], 0, 0];
}
function Cascade(S0, S1) {
    var U = Array(1, 0);
    var S011 = Array(S0[0], S0[1]);
    var S012 = Array(S0[2], S0[3]);
    var S022 = Array(S0[6], S0[7]);
    var S111 = Array(S1[0], S1[1]);
    var S112 = Array(S1[2], S1[3]);
    var S122 = Array(S1[6], S1[7]);
    var RS = xmy(U, xoy(S022, S111));
    var So11 = xpy(S011, xdy(xoy(S111, xin2(S012)), RS));
    var So22 = xpy(S122, xdy(xoy(S022, xin2(S112)), RS));
    var So12 = xdy(xoy(S012, S112), RS);
    return Array(So11[0], So11[1], So12[0], So12[1], So12[0], So12[1], So22[0], So22[1]);
}




