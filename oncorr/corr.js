function tester() {
    var y0 = [1.8, 0], jy = [0, 0.98], y1 = [0.78, 0];
    var so = hu003(y0, jy, y1);
    alert(so);

}


function hu000(k, Ao, Bo, At, Bt, Xt, Nc, Ac, Bc, Sc, Dc, Hc) {

    var wK = hu006(k, Math.PI / Ac);
    var ph = [wK[0] * Dc[0], wK[1] * Dc[0]];
    var S2 = hu002(k, Ao, Bo, At, Bt, Xt, Ac, Bc);
    var S0 = Sinvert(S2);
    var S1 = hu001(wK, Nc, Ac, Bc, Sc, Dc, Hc);
    S1 = hu004(ph, S1, S0);
    return hu004(ph, S2, S1);
}

function hu001(wK, Nc, Ac, Bc, Sc, Dc, Hc) {
    var i, Shu001 = [];
    for (i = Nc; i >= 0; i--) {
        if (i === Nc) {
            Shu001 = hu007(wK, Bc, Sc, Hc[i], Hc[i]);
        } else {
            var ph=[wK[0] * Dc[i + 1], wK[1] * Dc[i + 1]];
            var SS = hu007(wK, Bc, Sc, Hc[i], Hc[i]);
            Shu001 = hu004(ph, SS, Shu001);
        }
    }
    return Shu001;
}

function hu002(k, Ao, Bo, At, Bt, Xt, Ac, Bc) {
    var wK = hu006(k, Math.PI / At);
    var ph = [wK[0] * Xt, wK[1] * Xt];
    var SS = EH(k, Ao, Bo, At, Bt);
    var So = EH(k, At, Bt, Ac, Bc);
   return hu004(ph, SS, So);
}

function hu003(Y0, jB, Y1) {
    //from WRC
    var Y = Array(Y0[0] + Y1[0] + jB[0], Y0[1] + Y1[1] + jB[1]);
    var R = xdy(Array(Y0[0] - Y1[0], Y0[1] - Y1[1]), Y);
    var dR = xdy(jB, Y);
    var nrm = xdy(csqrt(Y1), csqrt(Y0));
    var T = aox(2, xoy(xdy(Y0, Y), nrm));
    var S11 = Array(R[0] - dR[0], R[1] - dR[01]);
    var S22 = Array(-R[0] - dR[0], -R[1] - dR[01]);
    return Array(S11[0], S11[1], T[0], T[1], T[0], T[1], S22[0], S22[1]);
}

function shunt(y0, jy, y1) {
    //Dim yy0(1), yy1(1), u(1), s11(1), s22(1), s12(1), s21(1), ymy0(1), ymy1(1), ypy0(1), ypy1(1)
    var u = [1, 0];
    //alert(y0);
    var yy0 = xpy(y1, jy), yy1 = xpy(y0, jy);
    var ymy0 = xmy(y0, yy0), ypy0 = xpy(y0, yy0);
    var ymy1 = xmy(y1, yy1), ypy1 = xpy(y1, yy1);
    var S11 = xdy(ymy0, ypy0);
    var S22 = xdy(ymy1, ypy1);
    var S12 = xpy(u, S11);
    var S21 = xpy(u, S22);
    //alert([S11[0], S11[1], S12[0], S12[1], S21[0], S21[1], S22[0], S22[1]]);
    return [S11[0], S11[1], S12[0], S12[1], S21[0], S21[1], S22[0], S22[1]];
    //call Smatrix(s11, s12, s21, s22, S)
}

function Sinvert(S) {
    return [S[6], S[7], S[4], S[5], S[2], S[3], S[0], S[1]];
}

function hu004(phase, S0, S1) {
    var W = cexp([phase[1], -phase[0]]);
    var W2 = xin2(W);
    var R1 = xoy(W2, [S1[0], S1[1]]), T1 = xoy(W, [S1[2], S1[3]]), T2 = xoy(W, [S1[4], S1[5]]);
    var So = [R1[0], R1[1], T1[0], T1[1], T2[0], T2[1], S1[6], S1[7]];
    return Cascade(S0, So);
}


function hu007(wK, b, s, h0, h1) {
    //So
    var j = [0, 1], Yin = [0, 0];
    var  jopa = [0, 0];
    var S01 = term_sums(10, wK, b, s, h0, h1);
    var S0 = S01[0], S1 = S01[1];
    var jopa2 = (1 + S0[0]) * (1 + S0[0]) + S0[1] * S0[1];
    jopa[0] = (S1[0] * (1 + S0[0]) + S1[1] * S0[1]) / jopa2;
    jopa[1] = (-S1[0] * S0[1] + S1[1] * (1 + S0[0])) / jopa2;
    Yin[0] = S0[0] - S1[0] * jopa[0] + S1[1] * jopa[1];
    Yin[1] = S0[1] - S1[0] * jopa[1] - S1[1] * jopa[0];
    var EpY = [1 + Yin[0], Yin[1]];
    var EmY = [1 - Yin[0], -Yin[1]];
    var R = xdy(EmY, EpY);
    var EpR = [1 + R[0], R[1]];
    var T = xoy(jopa, EpR);
    return [R[0], R[1], T[0], T[1], T[0], T[1], R[0], R[1]];
}

function term_sums(N, wK, b, s, h0, h1) {
    // S0, S1
    var pi = Math.PI;
    var j = [0, 1], ro_bet = [0, 0], tu_bet = [0, 0], ss0 = [0, 0], ss1 = [0, 0];
    var m, mu, Am, g2, gm;
    var S0 = [], S1 = [];
    AM0 = 1 / Math.sqrt(b * (h0 + b + h1));
    AM1 = Math.sqrt(2) * AM0;
    for (m = 0; m <= N; m += 2) {
        mu = pi * m / (h0 + b + h1);
        //alert([pi * m / (h0 + b + h1),mu]);
        if (m === 0) {
            Am = AM0 * b;
        } else {
            Am = AM1 * (Math.sin(mu * (b + h0)) - Math.sin(mu * h0)) / mu;
        }

        g2 = wK[0] * wK[0] - wK[1] * wK[1] - mu * mu;
        if (g2 > 0) {
            gm = Math.sqrt(g2);
            ro_bet[0] = 0; ro_bet[1] = -0.5 / gm / Math.sin(gm * s);
            tu_bet[0] = 0; tu_bet[1] = -1. / gm / Math.tan(gm * s);
        } else {
            gm = Math.sqrt(-g2);
            ro_bet[0] = 0; ro_bet[1] = 0.5 / gm / sinh(gm * s);
            tu_bet[0] = 0; tu_bet[1] = 1. / gm / tanh(gm * s);
        }
        ss0[0] = ss0[0] + Am * Am * tu_bet[0]; ss0[1] = ss0[1] + Am * Am * tu_bet[1];
        ss1[0] = ss1[0] + Am * Am * ro_bet[0]; ss1[1] = ss1[1] + Am * Am * ro_bet[1];
    }
    S0[0] = wK[0] * ss0[0] - wK[1] * ss0[1]; S0[1] = wK[0] * ss0[1] + wK[1] * ss0[0];
    S1[0] = 2. * (wK[0] * ss1[0] - wK[1] * ss1[1]); S1[1] = 2. * (wK[0] * ss1[1] + wK[1] * ss1[0]);
    //throw ('huj');
    return [S0, S1];
}

function EH(k, a0, b0, a1, b1) {
    var Y = [0, 0], jB = [0, 0];
    var Sum0 = [0, 0], Sum1 = [0, 0];
    var Y0 = [0, 0], Sm = [0, 0];
    var u = [1, 0];
    var a, b, ai, bi;
    if (a0 <= a1 && b0 <= b1) {
        ai = a0; bi = b0; a = a1; b = b1;
        Sum0 = EH0(k, a, b, ai, bi);
        Sum1 = EH1(k, a, b, ai, bi);

       Y = Yi(k, a, b, ai, bi);
        jB[0] = Sum0[0] + Sum1[0];
        jB[1] = Sum0[1] + Sum1[1];

    } else {
        ai = a1; bi = b1; a = a0; b = b0;
        Sum0 = EH0(k, a, b, ai, bi);
        Sum1 = EH1(k, a, b, ai, bi);
         //jB[0] = Sum0[0] + Sum1[0];
        //jB[1] = Sum0[1] + Sum1[1];
        Y0 = Yi(k, a, b, ai, bi);
        //Y = Yi(k, a, b, ai, bi);
        //return hu003(Y, jB, u);
        Sm[0] = Sum0[0] + Sum1[0]; Sm[1] = Sum0[1] + Sum1[1];
        Y = xdy(u, Y0);
        jB = xoy(Sm, Y);
    }
    return hu003(u, jB, Y);
}

function Yi(k, a, b, ai, bi, Y) {
    var pi = Math.PI;
    var ko = [0, 0], kno = [0, 0], Sm = [0, 0], Y = [0, 0];
    var gg;
    xa = pi / a;
    xsi = pi / ai;
    ko = hu006(k, xsi);
    kno = hu006(k, xa);
    gg = ga(1, a, ai);
    ga2 = gg * gg;
    Sm[0] = kno[0] * ga2;
    Sm[1] = kno[1] * ga2;
    Rm = 4. * bi / (ai * a * b);
    ko2 = ko[0] * ko[0] + ko[1] * ko[1];
    Y[0] = Rm * (Sm[0] * ko[0] + Sm[1] * ko[1]) / ko2;
    Y[1] = Rm * (Sm[1] * ko[0] - Sm[0] * ko[1]) / ko2;
    return Y;
}

function EH0(k, a, b, ai, bi) {
    //Sum0
    var pi = Math.PI;
    var ko = [0, 0], kno = [0, 0], Sm = [0, 0], Sum0 = [0, 0];
    var ko2, kno2, ga2, xn, xa, xsi;
    if (Math.abs(a - ai) < 1E-6) {
        Sum0[0] = 0; Sum0[1] = 0;
    } else {
        Sm[0] = 0; Sm[1] = 0;
        var xa = pi / a;
        var xsi = pi / ai;
        ko = hu006(k, xsi);
        ko2 = ko[0] * ko[0] + ko[1] * ko[1];
        for (n = 3; n <= 21; n += 2) {
            xn = xa * n;
            kno = hu006(k, xn);
            kno2 = kno[0] * kno[0] + kno[1] * kno[1];
            var gg = ga(n, a, ai);
            ga2 = gg * gg;
            Sm[0] = Sm[0] + ga2 * kno[0];
            Sm[1] = Sm[1] + ga2 * kno[1];
        }
        var Rm = 4. * bi / (ai * a * b);
        Sum0[0] = Rm * (Sm[0] * ko[0] + Sm[1] * ko[1]) / ko2;
        Sum0[1] = Rm * (Sm[1] * ko[0] - Sm[0] * ko[1]) / ko2;
    }
    return Sum0;
}

function EH1(k, a, b, ai, bi) {
    //Sum1
    var pi = Math.PI;
    var ko = [0, 0], knm = [0, 0], Sm = [0, 0], Sum1 = [0, 0];
    var n, m;
    var xa = pi / a;
    var xb = pi / b;
    var xsi = pi / ai;
    var ko = hu006(k, xsi);
    var Nmax = 11, Mmax = 5;
    if (Math.abs(a - ai) < 1E-6) {
        Nmax = 1; Mmax = 10;
    }
    if (Math.abs(b - bi) < 1E-6) {
        Sum1[0] = 0; Sum1[1] = 0;
        return Sum1;
    } else {
        for (n = 1; n <= Nmax; n+= 2) {
            for (m = 2; m <= Mmax; m+= 2) {
                var xn = xa * n, xm = xb * m;
                var xnm = Math.sqrt(xn * xn + xm * xm);
                var knm = hu006(k, xnm);
                var gbb = ga(n, a, ai) * gb(m, b, bi);
                var Anm = (k * k - xn * xn) * gbb * gbb;
                var knm2 = knm[0] * knm[0] + knm[1] * knm[1];
                Sm[0] = Sm[0] + Anm * knm[0] / knm2;
                Sm[1] = Sm[1] - Anm * knm[1] / knm2;
            }
        }
        var Rm = 8. / (ai * bi * a * b);
        var ko2 = ko[0] * ko[0] + ko[1] * ko[1];
        Sum1[0] = Rm * (Sm[0] * ko[0] + Sm[1] * ko[1]) / ko2;
        Sum1[1] = Rm * (Sm[1] * ko[0] - Sm[0] * ko[1]) / ko2;
        return Sum1;
    }
}

function hu005(k, kc, x) {
    //phase
    var g2;
    g2 = k * k - kc * kc;
    if (g2 >= 0) {
        return [Math.sqrt(g2) * x, 0];
    } else {
        return [0, -Math.sqrt(-g2) * x];
    }
}

function hu006(k, kc) {
//betta
    var g2;
    g2 = k * k - kc * kc;
    if (g2 >= 0) {
        return [Math.sqrt(g2), 0];
    } else {
        return [0, -Math.sqrt(-g2)];
    }
}


function gb(M, b, bi) {
    var pi2 = 1.570796326795;
    var xn = pi2 * M * bi / b;
    if (Math.abs(xn) < 0.005) {
        return bi * (1. - xn * xn / 6);
    } else {
        return bi * Math.sin(xn) / xn;
    }
}


function ga(n, A, ai) {
    var pi2 = 1.570796326795;
    var afs = 0.4112335167121;
    var xn = ai * n / A;
    var dxn = 1. - xn;
    if (Math.abs(dxn) < 0.005) {
        return ai * (1. - afs * dxn * dxn) / (1. + xn);
    } else {
        return ai * Math.cos(pi2 * xn) / (1. - xn * xn) / pi2;
    }
}
function sinh(x) {
    var y = Math.exp(x);
    return 0.5 * (y - 1 / y);
}

function cosh(x) {
    var y = Math.exp(x);
    return 0.5 * (y + 1 / y);
}

function tanh(x) {
    var y;
    if (x < 0) {
        y = Math.exp(2. * x); return (y - 1) / (y + 1);
    } else {
        y = Math.exp(-2. * x); return (1 - y) / (1 + y);
    }
}
function SdB(So) {
    var R = [So[0], So[1]];
    var T = [So[2], So[3]];
    return [dB(R), dB(T)];
}
function balance(So) {
    var txt = "";
    var p0 = So[0] * So[0] + So[1] * So[1] + So[2] * So[2] + So[3] * So[3];
    var p1 = So[4] * So[4] + So[5] * So[5] + So[6] * So[6] + So[7] * So[7];
    txt = 'dB= ' + dB([So[0], So[1]]) + ' ' + dB([So[2], So[3]]) + ' balance=' + p0 + '\n';
    txt = txt + 'dB= ' + dB([So[4], So[5]]) + ' ' + dB([So[6], So[7]]) + ' balance=' + p0 + '\n';
    return txt;
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

