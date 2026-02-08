//###### SETUP FUNCS

//####################### INDAT SETUP DATA PROCESSING #####################################
function SetUpDat() {
    this.iUnit = 1;
    this.CnstNode = 0;
    this.ModesX = 10;
    this.ModesY = 10;
    this.IndFilter = 1;
    this.Att = 0.05;
    this.ResoNum = 0;
    this.Bstrt = 1;
    this.Bcntr = 2;
    this.Bend = 1;
    this.alfa = 1.5;
    this.Fc = 12.5;
    this.dF = 1.5;
    this.f0 = 10;
    this.f1 = 15;
    this.Np = 200;
    this.OptType = 1;
    this.Nopt = 20;
    this.dX = 0.02;
    this.Wmin = 0.05;
    this.Cmin = 0.05;
    this.Nspec = -1;
    this.OptSpecs = {};
}

SetUpDat.prototype.ExportProto = function () {
    return [this.Att, this.ResoNum, this.Bstrt, this.Bcntr, this.Bend, this.alfa];
};

SetUpDat.prototype.ExportToText = function () {
    var texto = "";
    texto = texto + this.iUnit + " " + this.ModesX + " " + this.ModesY + "\r\n";
    texto = texto + this.IndFilter + " " + this.Att + " " + this.ResoNum + " ";
    texto = texto + this.Bstrt + " " + this.Bcntr + " " + this.Bend + " " + this.alfa +" "+this.CnstNode +"\r\n";
    texto = texto + this.Fc + " " + this.dF + "\r\n";
    texto = texto + this.f0 + " " + this.f1 + " " + this.Np + "\r\n";
    texto = texto + this.OptType + " " + this.Nopt + " " + this.dX + " " + this.Wmin + " " + this.Cmin + "\r\n";
    for (i = 0; i <= this.Nspec; i++) {
        texto = texto + this.OptSpecs[i].join(" ") + "\r\n";
    }

    return texto;
};

SetUpDat.prototype.ImportFromText = function (InText) {
    if (!InText) { return;}
    var InDatLines = InText.match(/[^\r\n]+/g);
    var n = InDatLines.length - 1;
    if (n >= 4) {
        var i;
        var ii = -1;
        var iii = -1;
        //this.OptSpecs = {};
        for (i = 0; i <= n; i++) {
            var ShrtLine = InDatLines[i].trim();
            if (ShrtLine) {
                ii = ii + 1;
                var vs = WordsToArray(ShrtLine);
                var nParams = vs.length;
                switch (ii) {
                    case 0:
                        this.iUnit = parseInt(vs[0]); this.ModesX = parseInt(vs[1]);
                        this.ModesY = parseInt(vs[2]);
                        break;
                    case 1:
                        this.IndFilter = parseInt(vs[0]); this.Att = parseFloat(vs[1]);
                        this.ResoNum = parseFloat(vs[2]);
                        this.Bstrt = parseFloat(vs[3]); this.Bcntr = parseFloat(vs[4]);
                        this.Bend = parseFloat(vs[5]);
                        this.alfa = parseFloat(vs[6]);
                        if (nParams > 6) { this.CnstNode = parseInt(vs[7]); } else { this.CnstNode = 0; }
                        break;
                    case 2:
                        this.Fc = parseFloat(vs[0]); this.dF = parseFloat(vs[1]);
                        break;
                    case 3:
                        this.f0 = parseFloat(vs[0]); this.f1 = parseFloat(vs[1]); this.Np = vs[2];
                        break;
                    case 4:
                        this.OptType = parseInt(vs[0]); this.Nopt = parseInt(vs[1]);
                        this.dX = parseFloat(vs[2]);
                        this.Wmin = parseFloat(vs[3]); this.Cmin = parseFloat(vs[4]);
                        break;
                    default:
                        if (ii > 4) {
                            iii = iii + 1;
                            //1  1  3.5410  4.2530  30.00   40
                            var i0 = parseInt(vs[0]);
                            var i1 = parseInt(vs[1]);
                            var f0 = parseFloat(vs[2]);
                            var f1 = parseFloat(vs[3]);
                            var Tr = parseFloat(vs[4]);
                            var Np = parseInt(vs[5]);
                            this.OptSpecs[iii] = [i0, i1, f0, f1, Tr, Np];
                            this.Nspec = iii;
                        }
                }
            }
        }

    }
};

