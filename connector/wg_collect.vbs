Option Explicit
dim br, oIE, oShell
br=chr(13)&chr(10)
'call SynthesisMain()
Set oIE = CreateObject("InternetExplorer.Application")
Set oShell = CreateObject("WScript.Shell")
Call IE_PrintSetUp()
call Optimize()

Sub Optimize()
dim pi, Vm(100)
dim StrucTextFile, iSym,Eps,TgD,Sigm,Nw
dim jow(50),jw(50),w(50,5),joc(50),jc(50),c(50,6)
dim SetupTextFile,iUnit,Nx,Ny,iFilter,Att,Fc,dF,F0,F1,Np,L,t
dim rm, mode
dim Nv, V(100)
dim OptStatus,OptSpec, SpecVar, U, U0, dx, cmin, wmin, Nst, IndOpt, iCount,ii
pi=3.141592653589793236
mode=1
Call OpenFile("input.dat", SetupTextFile)
Call ReadSetup(SetupTextFile, iUnit,Nx,Ny,iFilter,Att,Fc,dF,F0,F1,Np)
OptSpec= ReadOptParams(SetupTextFile)
Call OpenFile("struc.dat", StrucTextFile)
call ReadStructure(StrucTextFile, iSym,Eps,TgD,Sigm,Nw,jow,jw,w,joc,jc,c)
SpecVar=split(OptSpec,"/")
IndOpt=CInt(SpecVar(0)): Nst=CInt(SpecVar(1)): wmin=CDbl(SpecVar(2)): cmin=CDbl(SpecVar(3)): dx=CDbl(SpecVar(4))
call WC_V(Nv,V,iSym,Nw,jow,jw,w,joc,jc,c)
for iCount=0 to Nst
Call OpStep(OptStatus,OptSpec,dx,Nv,V,iUnit,mode,eps,tgD,iSym,Nx,Ny,Nw,jow,jw,w,joc,jc,c)
U=OptStatus(0): dx=OptStatus(2)
wscript.sleep 100
Call MsgIE(iCount&"   "&U)
if iCount=0 then U0=U
'msgbox "STEP #"&iCount&" "&join(OptStatus," ")
if U < U0 then for ii=0 to Nv: Vm(ii)=V(ii): next:  end if
U0=U
next
        call V_WC(Nv,Vm,iSym,Nw,jow,jw,w,wmin,joc,jc,c,cmin)
msgbox "done"
end sub

sub SynthesisMain()
dim pi
dim StrucTextFile, iSym,Eps,TgD,Sigm,Nw
dim jow(50),jw(50),w(50,5),joc(50),jc(50),c(50,6),jV(50), Bj(50), O0(50),O1(50), G(50)
dim SetupTextFile,iUnit,Nx,Ny,iFilter,Att,Fc,dF,F0,F1,Np,L,t
dim So
dim i,f,k,rm,ko,mode,  RdB,TdB,rv, iMark,N0,N1, iOrder, xs, g2, wmin,wmax
        pi=3.141592653589793236
        Rm=0.
        mode=1
        iMark=-1
Call OpenFile("input.dat", SetupTextFile)
Call ReadSetup(SetupTextFile, iUnit,Nx,Ny,iFilter,Att,Fc,dF,F0,F1,Np)
Call OpenFile("struc.dat", StrucTextFile)
call ReadStructure(StrucTextFile, iSym,Eps,TgD,Sigm,Nw,jow,jw,w,joc,jc,c)
        for i=0 to Nw
        if Left(jow(i),1)="*" then iMark=iMark+1: jV(imark)=i: end if
        next
        if iMark > 1 then msgbox "Cannot be more than two '*' marks": exit sub: end if
msgbox iMark

        N0=jV(0)
        N1=jV(1)-1
        if jV(1)=0 then N1=Nw-1 end if
        ko=kf(iUnit,Fc)
        k=ko*sqr(eps)
        call Synthesis(iFilter,N1-N0,Att,dF/Fc,Bj)
        for i=N0 to N1
        if (jc(i)=2) or (jc(i)=5) then
        L=joc(i)
        call Rt(I,L,t,Bj(I-N0),O0(I),O1(I),1,eps,0.,0.,iSym,Nx,Ny,k,Nw,jow,jw,w,joc,jc,c)
        xs=pi/2./w(i,2)
        g2=k*k-xs*xs
        if g2 <=0 then msgbox "The Central Frequency is below cut-off": exit sub: end if
        G(i)=sqr(k*k-xs*xs)
        end if
        next
        iOrder=InputBox("Enter Resonance Order","Filter Resonance Order","0")

        for i=N0+1 to N1
        w(i,1)=(pi*iOrder+O1(i-1)+O0(i))/G(i)
        if w(i,1) < wmin then w(i,1)=wmin end if
        next
call WriteStructure(StrucTextFile, iSym,Eps,TgD,Sigm,Nw,jow,jw,w,joc,jc,c)
call SaveFile("struc.dat", StrucTextFile)       
        end sub

sub SimulationMain()
dim StrucTextFile, iSym,Eps,TgD,Sigm,Nw,jow(50),jw(50),w(50,5),joc(50),jc(50),c(50,6)
dim SetupTextFile,iUnit,Nx,Ny,iFilter,Att,Fc,dF,F0,F1,Np
dim i,f,k,rm,mode, So, RdB,TdB,rv
mode=1
Call OpenFile("input.dat", SetupTextFile)
Call ReadSetup(SetupTextFile, iUnit,Nx,Ny,iFilter,Att,Fc,dF,F0,F1,Np)
Call OpenFile("struc.dat", StrucTextFile)
call ReadStructure(StrucTextFile, iSym,Eps,TgD,Sigm,Nw,jow,jw,w,joc,jc,c)
rv=""
for i=0 to Np
f=F0+(F1-F0)*CDbl(i)/CDbl(Np)
k= kf(iUnit,f)
Rm=Rs(iUnit,k,sigm)
'call El(0,mode,eps,tgD,rm,iSym,Nx,Ny,k,Nw,jw,w,jc,c,So)
'msgbox mode&"|"&eps&"|"&tgD&"|"&rm&"|"&iSym&"|"&Nx&"|"&Ny&"|"&k&"|"&Nw
Call Struc(mode,eps,tgD,rm,iSym,Nx,Ny,k,Nw,jw,w,jc,c,So)
call SdB(So, RdB, TdB)
rv=rv&f&" "&RdB&" "&TdB&br
next
call SaveFile("out.txt", rv)
msgbox "DONE!!!"
end sub


sub ReadSetup(SetupTextFile, iUnit,Nx,Ny,iFilter,Att,Fc,dF,F0,F1,Np)
Dim TextLines,Ns,Nv,V(10)
TextLines=split(SetupTextFile, br)
Ns=ubound(TextLines)-1
Call WordExtract(TextLines(0),Nv,V): iUnit=CInt(V(0)): Nx=CInt(V(1)): Ny=CInt(V(2))
Call WordExtract(TextLines(1),Nv,V): iFilter=CInt(V(0)): Att=CDbl(V(1))
Call WordExtract(TextLines(2),Nv,V): Fc=CDbl(V(0)): dF=CDbl(V(1))
Call WordExtract(TextLines(3),Nv,V): F0=CDbl(V(0)): F1=CDbl(V(1)): Np=CInt(V(2))
end sub

function ReadOptParams(SetupTextFile)
Dim TextLine, iCount,iSpec,Ns,V(20),Nv, inText
TextLine=split(SetupTextFile, vbCrLf)
Ns=ubound(TextLine)
Call WordExtract(TextLine(4),Nv,V)
': OptType=V(0):Nopt=V(1): Wmin=V(2): Cmin=V(3): OptDx=V(4)
iSpec=-1
for iCount=5 to Ns
inText=trim(TextLine(iCount))
if IsNumeric(left(inText,1)) then
iSpec=iSpec+1
V(6+iSpec)=WordLineCorrect(" ",inText)
end if
next
V(5)=iSpec
for iCount=0 to 6+iSpec
ReadOptParams=ReadOptParams&V(iCount)&"/"
next
end function



sub ReadStructure(StrucTextFile, iSym,Eps,TgD,Sigm,Nw,jow,jw,w,joc,jc,c)
Dim TextLines,Ns,Nv,V(10),i,ii,iii
TextLines=split(StrucTextFile, br)
Ns=ubound(TextLines)-1

for i=1 to Ns :if left(TextLines(i),1)=" " then TextLines(i)="0"&TextLines(i) end if: next

Call WordExtract(TextLines(0),Nv,V)
iSym=CInt(V(0)):Eps=CDbl(V(1)):TgD=CDbl(V(2)):Sigm=CDbl(V(3))
ii=0
Nw=(Ns-1)/2
for i=0 to Nw
ii=CInt(ii)+1
Call WordExtract(TextLines(ii),Nv,V)
jow(i)=CInt(V(0)): jw(i)=CInt(V(1)): for iii=1 to 5: w(i,iii)=CDbl(V(iii+1)):Next
ii=CInt(ii)+1
Call WordExtract(TextLines(ii),Nv,V)
joc(i)=CInt(V(0)): jc(i)=CInt(V(1)): for iii=1 to 6: c(i,iii)=CDbl(V(iii+1)):Next
next
end sub

sub WriteStructure(StrucTextFile, iSym,Eps,TgD,Sigm,Nw,jow,jw,w,joc,jc,c)
Dim rLine,i,ii
rLine=iSym&" "&Eps&" "&TgD&" "&Sigm&br
for i=0 to Nw
rLine=rLine&jow(i)&" "&jw(i): for ii=1 to 5: rLine=rLine&" "&w(i,ii): next
rLine=rLine&br
if i < Nw then
rLine=rLine&joc(i)&" "&jc(i): for ii=1 to 6: rLine=rLine&" "&c(i,ii): next
rLine=rLine&br
end if
next
StrucTextFile=rLine
msgbox StrucTextFile
end sub


private sub WordExtract(TextLine,Nv,V)
dim VV,Nvv,iv,icount
VV=split(TextLine," ")
Nvv=ubound(VV)
iv=-1
for icount=0 to Nvv
if ltrim(rtrim(VV(icount)))<>"" then
iv=iv+1
V(iv)=VV(icount)
end if
next
Nv=iv
end sub

Function WordLineCorrect(Median,TextLine)
dim VV,Nvv,iv,icount,N,InMark,i,r,dLine,A
N=len(TextLine)
InMark=True
for i=1 to N
r=mid(TextLine,i,1)
if r=chr(9) then r=" "
dLine=r
if InMark and r=" " then dLine="" end if
if r=" " then InMark=True else InMark=False end if
A=A&dLine
next
if Median =" " then 
WordLineCorrect=trim(A)
else
WordLineCorrect=replace(trim(A)," ",Median)
end if
end function

sub OpenFile(FiloName, TextInside)
Dim fs, f
Set fs=CreateObject("Scripting.FileSystemObject")
Set f=fs.OpenTextFile(FiloName, 1)
TextInside=ltrim(rtrim(f.ReadAll))
f.Close
Set f=Nothing
Set fs=Nothing
end sub

Sub SaveFile(FiloName, TextInside)
Dim fs, f
set fs=Wscript.CreateObject ("Scripting.FileSystemObject")
Set f = fs.CreateTextFile(FiloName, True)
f.Write(TextInside)
Set f=Nothing
Set fs=Nothing
End sub


'###################### IE PRINTER


Sub IE_PrintSetUp()
dim sTitle, SWidth, SHeight,SWidthW,SHeightW
sTitle = "Message dialog" ' used by AppActivate

oIE.AddressBar = False
oIE.ToolBar = False
oIE.StatusBar = False
oIE.Resizable = true

oIE.Navigate("about:blank")
Do Until oIE.readyState = 4: wscript.sleep 100: Loop

SWidth = oIE.document.ParentWindow.Screen.AvailWidth
SHeight = oIE.document.ParentWindow.Screen.AvailHeight

SWidthW = oIE.document.ParentWindow.Screen.AvailWidth * .25
SHeightW = oIE.document.ParentWindow.Screen.AvailHeight * .1

oIE.document.ParentWindow.resizeto SWidthW, SHeightW
oIE.document.ParentWindow.moveto (SWidth - SWidthW)/2, (SHeight - SHeightW)/2


oIE.document.ParentWindow.document.body.style.backgroundcolor = "LightBlue"
oIE.document.ParentWindow.document.body.scroll="no"
oIE.document.ParentWindow.document.body.style.Font = "12pt 'Arial'"
'.style.borderStyle = "outset"
'.style.borderWidth = "2px"

oIE.document.Title = sTitle
oIE.Visible = True
WScript.Sleep 100
oShell.AppActivate sTitle
end sub



Sub MsgIE(sMsg)
On Error Resume Next ' Just in case the IE window is closed
If sMsg = "IE_Quit" Then
oIE.Quit
Else
oIE.Document.Body.InnerText = sMsg
oShell.AppActivate sTitle
End If
End Sub

'LIBS FOR WR_Connect
'##############################   SYNTHESIS SUBS ###########################
'OPT SUBS


'<<<<<<<<<<<<<<<<<<<<<<<<<<HUJ ZDES

        sub OpStep(OptStatus,Specs,dx,Nv,V,iUnit,mode,e,tg,iSym,Nx,Ny,N,jow,jw,w,joc,jc,c)
'        real SP(0:4,6)
        dim So 'Smatrix
        dim V0(100), rm, ix, U,U0, Umin, imin,iix,iDir, dVi,Vmin
        dim SpecVar, IndOpt,Nst, wmin, cmin
        SpecVar=split(Specs,"/")
        IndOpt=CInt(SpecVar(0)): Nst=CInt(SpecVar(1)): wmin=CDbl(SpecVar(2)): cmin=CDbl(SpecVar(3))
        rm=0.0
        for ix=0 to Nv
        V0(ix)=V(ix)
        next
        U0=Ev(Specs,Nv,V,iUnit,mode,e,tg,iSym,Nx,Ny,N,jow,jw,w,joc,jc,c)
        Umin=1000.
        imin=-1
        for ix=0 to Nv
           for iix=0 to Nv
           V(iix)=V0(iix)
           next
        for iDir=-1 to 1 step 2
        dVi=CDbl(iDir)*abs(V0(ix))*dx
        V(ix)=V0(ix)+dVi
        call V_WC(Nv,V,iSym,N,jow,jw,w,wmin,joc,jc,c,cmin)
        U=Ev(Specs,Nv,V,iUnit,mode,e,tg,iSym,Nx,Ny,N,jow,jw,w,joc,jc,c)
        if U<Umin then
        Umin=U
        imin=ix
        Vmin=V(ix)
        end if
        V(ix)=V0(ix)
        next
        next
        if Umin<U0 then
        V(imin)=Vmin
        else
        dx=dx/2.
        end if
        OptStatus=Array(U,U0,dx)
        end sub


        function Ev(Specs,Nv,V,iUnit,mode,e,tg,iSym,Nx,Ny,N,jow,jw,w,joc,jc,c)
        'IU = iUnit
        dim SpecVar
        dim Ip(4),Np(4)
        dim Fp(4,1),Ap(4) ,UV(4)
        dim So 'Smatrix
        dim k,ko,pi,cf,IndOpt,Nst,wmin,cmin,Nsp,i,isp,U,f
        dim Rv(100),rm
        dim SpecLineVar
        pi=3.141592653589793236
        rm=0.0
'        cf=1.878529603895678018
        SpecVar=split(Specs,"/")
        
        IndOpt=CInt(SpecVar(0)): Nst=CInt(SpecVar(1)): wmin=CDbl(SpecVar(2)): cmin=CDbl(SpecVar(3)): wmin=CDbl(SpecVar(2))
        Nsp=CInt(SpecVar(5))
        for i=0 to Nsp
        SpecLineVar=split(SpecVar(i+6)," ")
        Ip(i)=4*CInt(SpecLineVar(0))+2*CInt(SpecLineVar(1))-6
        Ap(i)=CDbl(SpecLineVar(2))
        Fp(i,0)=CDbl(SpecLineVar(3))
        Fp(i,1)=CDbl(SpecLineVar(4))
        Np(i)=CInt(SpecLineVar(5))
        next
        call V_WC(Nv,V,iSym,N,jow,jw,w,wmin,joc,jc,c,cmin)
        for isp=0 to Nsp
        UV(isp)=0.
           for i=0 to Np(isp)
              if Np(isp)=0 then
              f=Fp(isp,0)
              else
              f=Fp(isp,0)+(FP(isp,1)-FP(isp,0))*CDbl(i)/CDbl(Np(isp))
              end if
           ko=kf(iUnit,f)
           k=ko*sqr(e)
           call Struc(mode,e,tg,rm,iSym,Nx,Ny,k,N,jw,w,jc,c,So)
           Rv(i)=cabs(Array(So(Ip(isp)),So(Ip(isp)+1))) 
           next
        UV(isp)=20.*log(Un(IndOpt,Np(isp),Rv))/log(10.)/abs(Ap(isp))
        next
        U=-1000.
        for i=0 to Nsp
'       if(UV(i) > U)U=UV(i)
             if UV(i)>U then
             U=UV(i)
             end if
        next
        Ev=U
        end function

        function Un(Ind,N,Rv)
        dim i
        if ind=0 then
        Un=-1000.
        for i=0 to N
        if Rv(i) > Un then Un=Rv(i)
        next
        else
        Un=0.
        for i=0 to N
        Un=Un+(Rv(i))^2
        next
        Un=sqr(Un/(N+1))
        end if
        end function

        function NumOpt(Ar)
	    if IsNumeric(Ar) then
	    NumOpt=CInt(Ar)
	    else
	    NumOpt=0
	    end if
        end function

        sub V_WC(Nv,V,iSym,N,jow,jw,w,wmin,joc,jc,c,cmin)
        dim j,i
        j=-1
        for i=0 to N
        if jw(i)= 1 then
          if jow(i) >=  1 then
           j=j+1
           if abs(v(j)) < wmin then v(j)=sign(v(j))*wmin
           w(i,jow(i))=v(j)
           if iSym=1 and jow(i)=3 then w(i,4)=-v(j)
           if iSym=1 and jow(i)=4 then w(i,3)=-2.0*v(j)
          end if
        end if
        next

        for i=0 to N-1
        if (jc(i) >=  1) and (jc(i) <= 5) then
          if(joc(i) >=  1)then
            j=j+1
            if abs(v(j)) < cmin then v(j)=sign(v(j))*cmin
          if joc(i) <= 6 then c(i,joc(i))=v(j)
          end if
        end if
        next
        Nv=j
        end sub

        sub WC_V(Nv,V,iSym,N,jow,jw,w,joc,jc,c)
        dim j,i
        j=-1
        for i=0 to N
        if jw(i)=1 then
           if jow(i) >=  1 then
           j=j+1
           v(j)=w(i,jow(i))
           end if
        end if
        next
        for i=0 to N-1
        if (jc(i) >=  1) and (jc(i) <= 5) then
          if joc(i) >=  1 then
            j=j+1
            if joc(i) <= 6 then v(j)=c(i,joc(i))
          end if
        end if
        next
        Nv=j
'        if j=-1 then stop
        end sub




'=========== SYNTHESIS TAPER=
        sub TransRoot(eps,tg,rm,iSym,Nx,Ny,k,Ao,Bo,At,Bt,Dt,A,B,D,Ac,Bc,Sc,Dc)
'        common /MEDIA/eps,tg,rm
        dim R,S0,S1,gt 'complex
        dim pi,I0,I1,R0,R1,B0,B1,Np,Fs,F0,F1,i
'        real k,kf
        pi=3.141592653589793236
        Np=500
        for i=0 to Np
        Bt=B+(Bo-B)*i*1./Np
        S0=STF(eps,tg,rm,iSym,Nx,Ny,k,Ao,Bo,At,Bt)
        S1=SF(eps,tg,rm,iSym,Nx,Ny,k,At,Bt,A,B,D,Ac,Bc,Sc,Dc)
        R1=cabs(S0)-cabs(S1)
        B1=Bt
        I1=sign(R1)
        if i <>0 then
        if i0<>i1 then exit for
        end if
        R0=R1
        B0=Bt
        I0=sign(R0)
        next
        gt=sqr(k*k-(pi/2./at)^2)
        F0=Im(clog(S0))
        F1=Im(clog(S1))
        Fs=0.5*(F0+F1)
        if Fs<0. then Fs=Fs+pi
        if Fs>pi then Fs=Fs-pi
        Dt=Fs/gt
        end sub

        private function STF(eps,tg,rm,iSym,Nx,Ny,k,Ao,Bo,At,Bt)
'        common /MEDIA/eps,tg,rm
'        dim k,kf 'real
        dim Yo,Go,ki,kw,gama 'complex
        dim Yt,jBo,So,Gt 'complex
        dim pi,xt
        pi=3.141592653589793236
        xt=pi/At/2.
        gt=sqr(k*k-xt*xt)
        Yt=Array(gt/k,0.0)
        'StepEH(eps,tg,rm,iSym,Nx,Ny,k,A,B,H,Ao,Bo,Ho,Y,jB)
        call StepEH(eps,tg,rm,iSym,Nx,Ny,k,At,Bt,0.0,Ao,Bo,0.0,Yo,jBo)
        'Shunt(Y0,jB,Y1)
        So= Shunt(Yo,jBo,Yt)
        STF=Array(So(6),So(7))
        end function

        private function SF(eps,tg,rm,iSym,Nx,Ny,k,At,Bt,A,B,D,Ac,Bc,Sc,Dc)
'        common /MEDIA/eps,tg,rm
'        dim k
        dim Yo,S,Y,PH,G,gama 'complex
        dim jBt,St,Yt,Gt,Ex,SFs, g2  'complex
        dim pi,x
        pi=3.141592653589793236
        x=pi/A/2.
        g2=Array(x*x-k*k,0.)
        g=csqrt(g2)
        Y=Array(g(1)/k,-g(0)/k)
        ph=Array(g(1)*Dc,-g(0)*Dc)
'        Y=g/k
        call CavEH(eps,tg,rm,iSym,Nx,Ny,k,A, B, 0.,Ac,Bc,0.0,Sc,A, B, 0.0,Yo)
        S= JuncInfinite(Y,Yo,ph)
        call StepEH(eps,tg,rm,iSym,Nx,Ny,k,A,B,0.0,At,Bt,0.0,Yt,jBt)
        St= Shunt(Yt,jBt,Y)
'        Ex=cexp((0.,-2.)*g*D)
        SFs=Sconnect(St,ph,S)
'        SF=St(0,0)+St(0,1)*St(1,0)*S(0,0)*Ex/((1.,0.)-St(1,1)*S(0,0)*Ex)
        SF=Array(SFs(0),SFs(1))
        end function

        private function JuncInfinite(Y0,Y,Ph)
        dim E,j,w 'complex
        dim SR,S00,S01,So,cs,cs0 'complex
        E=Array(1.,0.)
        j=Array(0.,1.)
        w=cexp(Array(ph(1),-ph(0)))
        So= SY2(Y0,Y,Y0)
'        S00=So(0,0)*cexp(-j*ph)
        S00=xoy(Scatter(0,So),w)
'        S01=So(0,1)*cexp(-j*ph)
        S01=xoy(Scatter(1,So),w)
        '==================DO SUDA PRAVILNO
'        cs=(E+S01*S01-S00*S00)/S01/2.
        cs0=xdy(xpy(E,xmy(x2(S01),x2(S00))),S01)
        cs=cs0(0)/2.0
'        w=acos(cs)
'        So(0,0)=(E-S01*cexp(j*w))/S00
        SR=xdy(xmy(E,xoy(S01,Array(cs,sqr(1.0-cs*cs)))),S00)
        JuncInfinite=Array(SR(0),SR(1), 0.0,0.0, 0.0,0.0,SR(0),SR(1))
'        So(1,1)=So(0,0)
'        So(1,0)=0.
'        So(0,1)=0.
         end function
         
'========================SYNTHESIS GENERAL

        sub Rt(iCode,I,L,t,Bj,O0,O1,mode,eps,tg,rm,iSym,Nx,Ny,k,Nw,jow,jw,w,joc,jc,c)
        dim So
        dim r_tst, Np_max,F, xmin,xmax, ii, t1,t0,F1,F0,dBj,Porog,index0,index1
        dim Bmin,Bmax, tMin
        dim dt,dtMax
        F=Bj
        Porog=1.0e6
        call Bounds(I,L,xmin,xmax,Nw,jow,jw,w,joc,jc,c)
        Np_max=100
        r_tst=""
        dtMax=(xmax-xmin)/Np_max
        dt=dtMax
        t1=xmin
        for ii=0 to 5*Np_max
	    iCode=false
        t1=t1+dt 'xmin+(xmax-xmin)*ii/Np_max
        call El(I,L,t1,mode,eps,tg,rm,iSym,Nx,Ny,k,Nw,jw,w,jc,c,So)
        call BO(So,F1,O0,O1)
        if F1<=5.0 then
           dt=dtMax
           elseif F1<=20.0 then
           dt=dtMax/3
           else
           dt=dtMax/10
        end if
        dBj=abs(F1-Bj)
        if dBj<Porog then Porog = dBj: tMin=t1: end if
        index1=sgn(F1-F)
          if ii<>0 then
          if index0 <> index1 then iCode=true: exit for: end if
          end if
        t0=t1
        F0=F1
        index0=index1
        next
        if not iCode then
        t=tMin
        else
        for ii=1 to 3
        t1=t0+(t1-t0)/(F1-F0)*(F-F0)
        call El(I,L,t1,mode,eps,tg,rm,iSym,Nx,Ny,k,Nw,jw,w,jc,c,So)
        call BO(So,F1,O0,O1)
        Bmin=F1
        Bmax=F
        if abs(F-F1)/F < 1.e-5 then exit for
        next
        t=t1
        end if
        end sub

        private sub Bounds(I,L,xmin,xmax,Nw,jow,jw,w,joc,jc,c)
'        dim C(0:50,6),W(0:50,5)
'        Integer Jc(0:50),Jw(0:50)
        dim A, kod
        A=amax(w(i,2),w(i+1,2))
        if Jc(i)=2 then
          kod=1
          if L=1 then
          xmax=10.*A
          xmin=0.001*A
          kod=2
          end if
          if L=2 then
          xmax=5.*A
          xmin=amax(w(i,2),w(i+1,2))
          kod=2
          end if

          if L=3 then
          xmin=amax(w(i,3),w(i+1,3))
          xmax=xmin+1.5*A
          kod=2
          end if
        end if

        if Jc(i)=5 then
          kod=1
          if L=1 then
          xmax=10.*A
          xmin=0.001*A
          kod=2
          end if
          if L=2 then
          xmax=0.01*A
          xmin=amin(w(i,2),w(i+1,2))
          kod=2
          end if

          if L=3 then
          xmax=0.01*A
          xmin=amin(w(i,3),w(i+1,3))
          kod=2
          end if
        end if

        if Jc(i)=21 then
          kod=1
          if L=1 then
          xmax=c(i,4)
          xmin=0.001*A
          kod=2
          end if
          if L=2 then
          xmax=0.01*A
          xmin=amin(0.5*(w(i,2)+w(i+1,2)),c(i,5))
          kod=2
          end if

          if L=6 then
          xmax=c(i,6)
          xmin=0.01*A
          kod=2
          end if
        end if
        end sub

        private sub BO(So,Bj,O0,O1)
        dim R0,R1,R
 R0=Scatter(0,So)       
 R1=Scatter(3,So)       
        R=cabs(R0)
        if R>=1. then R=0.9999999 end if
        Bj= 2.*R/sqr(1.-R*R)
        O0=0.5*arg(R0)
        O1=0.5*arg(R1)
        end sub

        private sub PreFilter(index,N,Lar,G)
        dim a(80),b(80),GG(100),pi,eN,xx,be,v,i,e2,kk
        pi=3.141592653589793236
        eN=2*N
        if index = 0 then
        for  i=1 to N
        e2=2*i-1
        G(i)=2.*sin(pi*e2/eN)
        next
        end if
        if index = 1 then
        xx=Lar/17.37
        be=log(cosh(xx)/sinh(xx))
        v=sinh(be/eN)
        for  i=1 to N
        e2=2*i-1
        a(i)=sin(pi*e2/eN)
        b(i)=v*v+(sin(i*pi/N))^2
        next
        G(1)=2.*a(1)/v
        for  i=2 to N
        G(i)=4.*a(i-1)*a(i)/(b(i-1)*G(i-1))
        next
        end if
        kk=2*int(N/2.+.01)-N
        if kk = 0 then
        for  i=1 to N
        GG(i)=0.5*(G(i)+G(N+1-i))
        next
        for  i=1 to N
        G(i)=GG(i)
        next
        end if
        end sub

        sub Synthesis(index,N,Lar,Band,Bj)
'  Filter with Chebychev Amplitude Responce
'  X(i)-Normalized Reactance at fo
'  Band-dF/F, dL(i)-length of i-th resonator
        dim g(20),gk(100),pi,O,i,OO
        pi=3.141592653589793236
        if index < 2 then
        call PreFilter(index,N,Lar,G)
        O=pi*Band/2.
        gk(0)=O
        gk(N+1)=O
        for  i=1 to N
        gk(i)=G(i)
        next
        for  i=0 to N
        OO=O/sqr(Gk(i)*Gk(i+1))
        Bj(i)=(1.-OO*OO)/OO
        next
        else
'        print*,*################################################*
'        print*,*            Impedance Taper Synthesis*
'        print*,* *
'        print*,* *
'        print*,*            Polynomial B-Distribution*
'        print*,* *
'        print*,*          ENTER B_start, B_center, B_end & ALFA*
'        read(*,*) B0, B, B1, ALFA
'        print*,*################################################*
        y=sqr(-1.)
        for  i=0 to N
        x=float(2*i-N)/float(N)
        Bj(i)= yProfile(x,ALFA,B0,B,B1)
        next
        end if
         end sub

        function xProfile(x,ALFA)
        teta=sign(1.,x)
        w=1.-abs(x)
        if w <= 0. then
        wALFA=0.
        else
        wALFA=w^ALFA
        end if
        xProfile=teta*(1.-wALFA)
        end function

        function yProfile(x,ALFA,B0,B,B1)
        Bm=(B0+B1)/2.
        dB=(B1-B0)/2.
        Bc=B-Bm
        t=xProfile(x,ALFA)
        yProfile=Bm+dB*t+Bc*(1.-t*t)
        end function
        
'##############################   STRUCTURE SUBS ###########################
      Sub SymCorrect(iSym,iCount,iVar,xVar,v)
      v(iCount,iVar)=xVar
      if iSym=1 then
      if iVar=3 then v(iCount,4)=-xVar/2.0
      if iVar=4 then v(iCount,3)=-2.0*xVar
      else
      end if
      end sub

      Public Sub El(iCount,iVar,xVar,mode,eps,tg,rm,iSym,Nx,Ny,k,Nw,jw,w,jc,c,Se)
        dim Yo,jB
        dim g0,g1,Y0,Y1,ph,ki
        dim pi,A0,B0,H0,x0,S,A,B,H,A1,B1,H1,x1
        dim i
        pi=3.141592653589793238
        ki=Array(k,-0.5*k*tg)
        i=iCount
'*--------ELEMENTS-------------------------------
        select case jc(i)
        
        case 2
        call SymCorrect(iSym,i,iVar,xVar,c)
'        c(i,iVar)=xVar
        A0=w(i,2)/mode
        B0=w(i,3)
        H0=w(i,4)
        x0=pi/A0/2.
        S=c(i,1)
        A=c(i,2)/mode
        B=c(i,3)
        H=c(i,4)
        A1=w(i+1,2)/mode
        B1=w(i+1,3)
        H1=w(i+1,4)
        x1=pi/A1/2.
        call CavEH(eps,tg,rm,iSym,Nx,Ny,k,A0,B0,H0,A,B,H,S,A1,B1,H1,Yo)
g0= PropNumber(k,tg,rm,2.*a0,b0,1,1,0)
g1= PropNumber(k,tg,rm,2.*a1,b1,1,1,0)
        Y0= xdy(g0,ki)   'Y0=g0/ki
        Y1= xdy(g1,ki)   'Y0=g1/ki
        Se= SY2(Y0,Yo,Y1)
          
        case 1
        A0=w(i,2)/mode
        B0=w(i,3)
        H0=w(i,4)
        x0=pi/A0/2.
        A1=w(i+1,2)/mode
        B1=w(i+1,3)
        H1=w(i+1,4)
        x1=pi/A1/2.
g0= PropNumber(k,tg,rm,2.*a0,b0,1,1,0)
g1= PropNumber(k,tg,rm,2.*a1,b1,1,1,0)
        Y0= xdy(g0,ki)   'Y0=g0/ki
        Y1= xdy(g1,ki)   'Y0=g1/ki
        if A0<=A1 and B0<=B1 then
        call StepEH(eps,tg,rm,iSym,Nx,Ny,k,A0,B0,H0,A1,B1,H1,Y1,jB)
        else
        call StepEH(eps,tg,rm,iSym,Nx,Ny,k,A1,B1,H1,A0,B0,H0,Y0,jB)
        end if
        Se= Shunt(Y0,jB,Y1)
        case 5
        call SymCorrect(iSym,i,iVar,xVar,c)
'        c(i,iVar)=xVar
        A0=w(i,2)/mode
        B0=w(i,3)
        H0=w(i,4)
        x0=pi/A0/2.
        S=c(i,1)
        A=c(i,2)/mode
        B=c(i,3)
        H=c(i,4)
        A1=w(i+1,2)/mode
        B1=w(i+1,3)
        H1=w(i+1,4)
        x1=pi/A1/2.
        call IrisEH(eps,tg,rm,iSym,Nx,Ny,k,A0,B0,H0,A,B,H,S,A1,B1,H1,Yo)
'Call PropNumber(k,tg,rm,2.*A0,B0,1,1,0,g0)
'Call PropNumber(k,tg,rm,2.*A1,B1,1,1,0,g1)
Y0=Array(1.0,0.): Y1=Array(1.0,0.)
'        call xdy(g0,ki,Y0)   'Y0=g0/ki
'        call xdy(g1,ki,Y1)   'Y0=g1/ki
        Se= SY2(Y0,Yo,Y1)
        case else
        end select
'*-------- END ELEMENTS-------------------------------
        End Sub
        

      Public Sub Struc(mode,eps,tg,rm,iSym,Nx,Ny,k,Nw,jw,w,jc,c,So)
        dim Se,Yo,jB
        dim g0,g1,Y0,Y1,ph,ki
        dim pi,A0,B0,H0,x0,S,A,B,H,A1,B1,H1,x1
        dim i
        
        pi=3.141592653589793238
        ki=Array(k,-0.5*k*tg)
        for i=Nw-1 to 0 step -1
'*--------ELEMENTS-------------------------------
        select case jc(i)
        
        case 2
        A0=w(i,2)/mode
        B0=w(i,3)
        H0=w(i,4)
        x0=pi/A0/2.
        S=c(i,1)
        A=c(i,2)/mode
        B=c(i,3)
        H=c(i,4)
        A1=w(i+1,2)/mode
        B1=w(i+1,3)
        H1=w(i+1,4)
        x1=pi/A1/2.
        call CavEH(eps,tg,rm,iSym,Nx,Ny,k,A0,B0,H0,A,B,H,S,A1,B1,H1,Yo)
g0= PropNumber(k,tg,rm,2.*a0,b0,1,1,0)
g1= PropNumber(k,tg,rm,2.*a1,b1,1,1,0)
        Y0= xdy(g0,ki)   'Y0=g0/ki
        Y1= xdy(g1,ki)   'Y0=g1/ki
        Se= SY2(Y0,Yo,Y1)
          
        case 1
        A0=w(i,2)/mode
        B0=w(i,3)
        H0=w(i,4)
        x0=pi/A0/2.
        A1=w(i+1,2)/mode
        B1=w(i+1,3)
        H1=w(i+1,4)
        x1=pi/A1/2.
g0= PropNumber(k,tg,rm,2.*a0,b0,1,1,0)
g1= PropNumber(k,tg,rm,2.*a1,b1,1,1,0)
        Y0= xdy(g0,ki)   'Y0=g0/ki
        Y1= xdy(g1,ki)   'Y0=g1/ki
        if A0<=A1 and B0<=B1 then
        call StepEH(eps,tg,rm,iSym,Nx,Ny,k,A0,B0,H0,A1,B1,H1,Y1,jB)
        else
        call StepEH(eps,tg,rm,iSym,Nx,Ny,k,A1,B1,H1,A0,B0,H0,Y0,jB)
        end if
        Se= Shunt(Y0,jB,Y1)
        case 5
        A0=w(i,2)/mode
        B0=w(i,3)
        H0=w(i,4)
        x0=pi/A0/2.
        S=c(i,1)
        A=c(i,2)/mode
        B=c(i,3)
        H=c(i,4)
        A1=w(i+1,2)/mode
        B1=w(i+1,3)
        H1=w(i+1,4)
        x1=pi/A1/2.
        call IrisEH(eps,tg,rm,iSym,Nx,Ny,k,A0,B0,H0,A,B,H,S,A1,B1,H1,Yo)
g0= PropNumber(k,tg,rm,2.*a0,b0,1,1,0)
g1= PropNumber(k,tg,rm,2.*a1,b1,1,1,0)
'Call PropNumber(k,tg,rm,2.*A0,B0,1,1,0,g0)
'Call PropNumber(k,tg,rm,2.*A1,B1,1,1,0,g1)
Y0=Array(1.,0.):Y1=Array(1.,0.)
'        call xdy(g0,ki,Y0)   'Y0=g0/ki
'        call xdy(g1,ki,Y1)   'Y0=g1/ki
        Se= SY2(Y0,Yo,Y1)
        case else
        end select

'*--------ELEMENTS-------------------------------
        if i = Nw-1 then
        So=Se
        else
        ph=ax(w(i+1,1),g1)     'ph=g1*w(i+1,1)
        So= Sconnect(Se,ph,So)
'msgbox g1(0)&" "&g1(1)
        end if
        next
        End Sub
        
        private sub CavEH(eps,tg,rm,iSym,Nx,Ny,k,A0,B0,H0,A,B,H,S,A1,B1,H1,Yo)
        dim Aem0(1,10,10), Aem1(1,10,10)
        dim o0,o1
        call RecMain(iSym,Nx,Ny,A0,B0,H0-H,A,B,Aem0)
        call RecMain(iSym,Nx,Ny,A1,B1,H1-H,A,B,Aem1)
        o0=1.-B0*A0/B/A
        o1=1.-B1*A1/B/A
        call J1(eps,tg,rm,iSym,Nx,Ny,k,o0,Aem0,A,B,S,o1,Aem1,Yo)
        End Sub
        
        
        private sub IrisEH(eps,tg,rm,iSym,Nx,Ny,k,A0,B0,H0,A,B,H,S,A1,B1,H1,Yo)
        dim Aem0(1,10,10), Aem1(1,10,10),o0,o1
        call RecMain(iSym,Nx,Ny,A,B,H-H0,A0,B0,Aem0)
        call RecMain(iSym,Nx,Ny,A,B,H-H1,A1,B1,Aem1)
        o0=1.-B0*A0/B/A
        o1=1.-B1*A1/B/A
        call IEH(eps,tg,rm,iSym,Nx,Ny,k,o0,A0,B0,Aem0,A,B,S,o1,A1,B1,Aem1,Yo)
        End Sub

        private sub StepEH(eps,tg,rm,iSym,Nx,Ny,k,A,B,H,Ao,Bo,Ho,Y,jB)
        dim Aem(1,10,10),o
        o=1.-B*A/Bo/Ao
        call RecMain(iSym,Nx,Ny,A,B,H-Ho,Ao,Bo,Aem)
        call JEH(eps,tg,rm,iSym,Nx,Ny,k,o,Aem,Ao,Bo,Y,jB)
        End Sub
        

    private sub IEH(eps,tg,rm,iSym,Nx,Ny,k,o0,A0,B0,E0,A,B,S,o1,A1,B1,E1,Y)
' IRIS OF LARGER WGs
' normalized to y0=1, y1=1
'*       common /MEDIA/co,zo,cw,zw,tg,keff
        dim ph,Y0,jB0,Y1,jB1
        dim gs,ys,ki
        dim c,jc
        dim Ys00,Ys01,Ys11
        dim pi
        pi=3.141592653589793238
        ki=Array(k,-0.5*k*tg)
'        gs=gama(k,tg,rm,pi/A/2.,2.*A,B)
gs= PropNumber(k,tg,rm,2.*a,b,1,1,0)
        ys= xdy(gs,ki): ph=Array(gs(0)*S,gs(1)*S)
        call JEH(eps,tg,rm,iSym,Nx,Ny,k,o0,E0,A0,B0,Y0,jB0)
        call JEH(eps,tg,rm,iSym,Nx,Ny,k,o1,E1,A1,B1,Y1,jB1)
        c= xoy(ys,ctanC(ph)):jc=Array(-c(1),c(0))
        Ys00=Array(jB0(0)-jc(0),jB0(1)-jc(1))
        Ys11=Array(jB1(0)-jc(0),jB1(1)-jc(1))
        c= xdy(ys,csin(ph)): Ys01=Array(-c(1),c(0))
        Y= Ynorm(Y0,Ys00,Ys01,Ys11,Y1)
'        Y(0,0)=(jB0-j*ys*cs/sn)/Y0
'        Y(1,1)=(jB1-j*ys*cs/sn)/Y1
'        Y(0,1)=j*ys/sn/csqrt(Y0*Y1)
'        Y(1,0)=Y(0,1)
        End Sub


        private sub JEH(eps,tg,rm,iSym,Nx,Ny,k,o,Aem,A,B,Y,jB)
        dim ki,ge,gm,ze,zm,ce,cm,ye,ym
        dim n,m,nn,mm,pi

        pi=3.141592653589793238
        jB=Array(0.0,0.0): ki=Array(k,-0.5*k*tg)
        for n=0 to Nx
        nn=2*n+1
        for m=0 to Ny
        mm=(1+iSym)*m
ge= PropNumber(k,tg,rm,2.*a,b,0,nn,mm)
gm= PropNumber(k,tg,rm,2.*a,b,1,nn,mm)
        ze= xdy(ge,ki)    'ze=ge/ki
        zm= xdy(ki,gm)    'zm=ki/ge
        ze(0)=ze(0)+rm*o:ze(1)=ze(1)+rm*o 'NUZHNO ISPRAVIT' - ISPRAVIL
        zm(0)=zm(0)+rm*o:zm(1)=zm(1)+rm*o: 'NUZHNO ISPRAVIT' - ISPRAVIL
        if n = 0 and m = 0 then
        Y= inv(Aem(1,n,m)*Aem(1,n,m),zm)
        else
        Ye= inv(Aem(0,n,m)*Aem(0,n,m),ze)
        Ym= inv(Aem(1,n,m)*Aem(1,n,m),zm)
        jB(0)=jB(0)+Ye(0)+Ym(0):jB(1)=jB(1)+Ye(1)+Ym(1)
        end if
        next
        next
'        msgbox k& " "&ki(0)&"  "&ki(1)
'        msgbox k& " "&n&"  "&m
'        msgbox k& " "&nn&"  "&mm
        
        End Sub



         private sub RecMain(iSym,Nx,Ny,A,B,H,Ao,Bo,Aem)
'*  CORRECTED VERSION ERROR FOUND in if(abs(x-nu).lt.
'*       ##################  -Bo      #  Params Ao x Bo
'*                        #           #  a=Wg(1)
'*       ###############  #  -h+b     #  b=Wg(2)
'*                     #  #           #  h=Wg(3)
'*       ###############  #  -h       #  x-eigen walue
'*       ##################  -0       #  y=yc/yw
'*       0             a  Ao
'*      iSym=0 no symmetry     iSym=1 y-symmetry
	    dim tx(10),ty(10),nu(10),mu(10)
	    dim pi,y0,y1,x,f0,T0,nu0,mu0,n,m,nn,mm,xnm
	    pi=3.141592653589793238
        y0=h
        y1=b+h
        x=pi/a/2.
        f0=sqr(2./b/Bo)
        T0=2./sqr(a*Ao)
        nu0=pi/Ao/2.
        mu0=pi/Bo
for n=0 to Nx
        nu(n)=nu0*(2*n+1)
          if abs(x-nu(n)) < 0.0001/Ao then
          tx(n)=T0*a/2.
          else
          tx(n)=T0*nu(n)*cos(nu(n)*a)/(x*x-nu(n)*nu(n))
          end if
next
for m=0 to Ny
        mm=m*(1+iSym)
        mu(m)=mu0*mm
          if m = 0 then
          ty(m)=sqr(b/Bo)
          else
          ty(m)=f0*(sin(mu(m)*y1)-sin(mu(m)*y0))/mu(m)
          end if
next
        for n=0 to Nx
        for m=0 to Ny
        xnm=sqr(nu(n)*nu(n)+mu(m)*mu(m))
        Aem(0,n,m)=-x*mu(m)/xnm/nu(n)*tx(n)*ty(m)
        Aem(1,n,m)=x/xnm*tx(n)*ty(m)
        next
        next
        End Sub



        private sub J1(eps,tg,rm,iSym,Nx,Ny,k,o0,A0,A,B,S,o1,A1,Y)
' CAVITY BETWEEN JUNCTIONS
' d0 - distance between junctions
' Cavity Model Ae0,Am0 - first junction integrals
' Cavity Model Ae1,Am1 - second junction integrals
' A=Real_A/2 B=Real_B S=Real_SPACE_Between_Junctions
'VALUES OF Y - REAL, ACTUAL Y=j*Y
        dim ki,ge,gm
        dim ze,zm,ce0,ce1,cm0,cm1
        dim kw,beta
        dim C0,C1,C
        dim n,m,nn,mm,pi
        pi=3.141592653589793238
        C0=Array(0.,0.): C=C0: C1=C0
        ki=Array(k,-0.5*k*tg)
        for n=0 to Nx
        nn=2*n+1
        for m=0 to Ny
        mm=(1+iSym)*m
ge= PropNumber(k,tg,rm,2.*a,b,0,nn,mm)
gm= PropNumber(k,tg,rm,2.*a,b,1,nn,mm)
        ze= xdy(ge,ki)    'ze=ge/ki
        zm= xdy(ki,gm)    'zm=ki/ge
call AppertureCoupling(rm,ge,ze,s,o0+o1,ce0,ce1)        
call AppertureCoupling(rm,gm,zm,s,o0+o1,cm0,cm1)        
       C0(0)=C0(0)+A0(0,n,m)*A0(0,n,m)*ce0(0)+A0(1,n,m)*A0(1,n,m)*cm0(0)
       C0(1)=C0(1)+A0(0,n,m)*A0(0,n,m)*ce0(1)+A0(1,n,m)*A0(1,n,m)*cm0(1)
       C1(0)=C1(0)+A1(0,n,m)*A1(0,n,m)*ce0(0)+A1(1,n,m)*A1(1,n,m)*cm0(0)
       C1(1)=C1(1)+A1(0,n,m)*A1(0,n,m)*ce0(1)+A1(1,n,m)*A1(1,n,m)*cm0(1)
       C(0)=C(0)+A0(0,n,m)*A1(0,n,m)*ce1(0)+A0(1,n,m)*A1(1,n,m)*cm1(0)
       C(1)=C(1)+A0(0,n,m)*A1(0,n,m)*ce1(1)+A0(1,n,m)*A1(1,n,m)*cm1(1)
        next
        next
        Y=Array(C0(0),C0(1),-2.*C(0),-2.*C(1),-2.*C(0),-2.*C(1),C1(0),C1(1))
        End Sub

private sub AppertureCoupling(rm,g,z,s,omega,c0,c1)
dim ph,Corr,xo,x, a0,a1,a2,MRmZ
'xs=cexp(-j*g*s)/(u+rm/z*Omega) 
'c0=(1+x*x)/(1-x*x)/z  Reflectional Coupling
'c1=x/(1-x*x)          Through      Coupling
ph=Array(g(1)*s,-g(0)*s)
MRmZ=z(0)*z(0)+z(1)*z(1): Corr=Array(1.0+Rm*omega*z(0)/MRmZ,-Rm*omega*z(1)/MRmZ)
'TO CORRECT FOR Rm*(1+j)
xo= cexp(ph):x= xdy(xo,Corr)
a0=Array(1.0+x(0)*x(0)-x(1)*x(1),2.0*x(1)*x(0)) 
a1=Array(1.0-x(0)*x(0)+x(1)*x(1),-2.0*x(1)*x(0))
a2= xdy(a0,a1): c0= xdy(a2,z): a2= xdy(x,a1): c1= xdy(a2,z)  
End Sub





'################################  S-LIB FRAGMENT #####################


Function SLine(ind,So)
dim R,lnR,RdB,wR, LN,SP,i,j,wRdg,cW
cW=57.29577951308232088
LN=""
SP="   "
for i=0 to 3
R=Scatter(i,So)
if cabs(R)<1.e-20 then R=Array(1.e-20, R(1)=0)
lnR= clog(R)
RdB = 20. * lnR(0) / Log(10.)
wR=lnR(1): wRdg=wR*cW
select case ind
case 0
LN=LN&RdB&SP
case 1
LN=LN&R(0)&SP&R(1)&SP
case 2
LN=LN&RdB&SP&wRdg&SP

end select
next
SLine=LN
End Function


Sub SdB(So, RdB, TdB)
Dim R, T
R=Scatter(0,So):T=Scatter(1,So)
RdB = dB(R): TdB = dB(T)
End Sub

Function Balance(S)
dim R, T, dBalance, iPort
Balance=""
for iPort=0 to 2 step 2
R=Scatter(iPort,S):T=Scatter(iPort+1,S)
dBalance=(cabs(R))^2+(cabs(T))^2
Balance=Balance&"   "&dBalance
next
End Function

        Function Scatter(iPort,S)
        dim iVar
        iVar=2*iPort
        Scatter=Array(S(iVar),S(iVar+1))
        End Function

        Function Smatrix(s00,s01,s10,s11)
        Smatrix=Array(s00(0),s00(1),s01(0),s01(1),s10(0),s10(1),s11(0),s11(1))
        End Function

        private function SY2(Y0,Yo,Y1)
        dim DET,Y00,Y01,Y11
        dim YY,S00,S01,S11
        dim part0,part1,M
        'Y00=Yo(0,0)/Y0
SY2=Array(0.,0.,0.,0.,0.,0.,0.,0.)
M = Y0(0) * Y0(0) + Y0(1) * Y0(1)
Y00 = Array((Yo(0) * Y0(0) + Yo(1) * Y0(1)) / M,  (-Yo(0) * Y0(1) + Yo(1) * Y0(0)) / M)
'        Y01=Yo(0,1)/csqrt(Y0)/csqrt(Y1)
YY= csqrt(xoy(Y0,Y1))
M = YY(0) * YY(0) + YY(1) * YY(1)
Y01 = Array((Yo(2) * YY(0) + Yo(3) * YY(1)) / M,  (-Yo(2) * YY(1) + Yo(3) * YY(0)) / M)

'        Y11=Yo(1,1)/Y1
M = Y1(0) * Y1(0) + Y1(1) * Y1(1)
Y11 = Array((Yo(6) * Y1(0) + Yo(7) * Y1(1)) / M, (-Yo(6) * Y1(1) + Yo(7) * Y1(0)) / M)
'        DET=(1.,0.)+Y11+Y00+Y00*Y11-Y01*Y01
part0= xoy(Y00,Y11):part1= xoy(Y01,Y01) 'Y00*Y11=part0     Y01*Y01=part1
DET=Array(1.+Y11(0)+Y00(0)+part0(0)-part1(0), Y11(1)+Y00(1)+part0(1)-part1(1))
        'So(0,0)=((1.,0.)-Y00+Y11-Y00*Y11+Y01*Y01)/DET
YY=Array(1.0-Y00(0)+Y11(0)-part0(0)+part1(0),-Y00(1)+Y11(1)-part0(1)+part1(1)): S00= xdy(YY,DET)
        'So(1,1)=((1.,0.)+Y00-Y11-Y00*Y11+Y01*Y01)/DET
YY=Array(1.0+Y00(0)-Y11(0)-part0(0)+part1(0),Y00(1)-Y11(1)-part0(1)+part1(1)): S11= xdy(YY,DET)        
S01= ax(-2.,xdy(Y01,DET))
SY2=Smatrix(s00,s01,s01,s11)
        End Function




        private function Sconnect(S0,W,S1)
' Junction of 2 Blocks of S0 and S1 Scattering Matrixes
' W-phase between the Blocks
        dim RS, EX1,EX2,ph,ph2,SS01,R,RG,SE0,S(7)
        ph=Array(w(1),-w(0)): ph2=Array(2.*w(1),-2.*w(0))
        Ex1= cexp(ph): Ex2= cexp(ph2)
        'RS=(1.,0.)-S0(1,1)*S1(0,0)*EX2
        SS01=Array(S0(6)*S1(0)-S0(7)*S1(1),S0(6)*S1(1)+S0(7)*S1(0))
        RS= xoy(SS01,EX2):RS(0)=1.0-RS(0):RS(1)=-RS(1)
'        So(0,0)=S0(0,0)+S0(0,1)*S1(0,0)*S0(1,0)*EX2/RS
        'S0(0,1)*S1(0,0)
        SS01(0)=S0(2)*S1(0)-S0(3)*S1(1):SS01(1)=S0(2)*S1(1)+S0(3)*S1(0)
        'S0(1,0)*EX2
        SE0=Array(S0(4)*EX2(0)-S0(5)*EX2(1),S0(4)*EX2(1)+S0(5)*EX2(0))
        RG= xoy(SS01,SE0):R= xdy(RG,RS)
        S(0)=S0(0)+R(0):S(1)=S0(1)+R(1):        
        'So(1,1)=S1(1,1)+S1(1,0)*S0(1,1)                  *S1(0,1)*EX2/RS
        SS01(0)=S1(4)*S0(6)-S1(5)*S0(7):SS01(1)=S1(4)*S0(7)+S1(5)*S0(6)
        SE0=Array(S1(2)*EX2(0)-S1(3)*EX2(1),S1(2)*EX2(1)+S1(3)*EX2(0))
        RG= xoy(SS01,SE0):R= xdy(RG,RS)
        S(6)=S1(6)+R(0):S(7)=S1(7)+R(1):        
'        So(0,1)=S0(0,1)*S1(0,1)                     *EX1/RS
        SS01(0)=S0(2)*S1(2)-S0(3)*S1(3):SS01(1)=S0(2)*S1(3)+S0(3)*S1(2)
        RG= xoy(SS01,EX1):R= xdy(RG,RS)
        S(2)=R(0):S(3)=R(1)
'        So(1,0)=S1(1,0)*S0(1,0)*EX1/RS
        SS01(0)=S1(4)*S0(4)-S1(5)*S0(5):SS01(1)=S1(4)*S0(5)+S1(5)*S0(4)
        RG= xoy(SS01,EX1):R= xdy(RG,RS)
        S(4)=R(0):S(5)=R(1)
        Sconnect=Array(S(0),S(1),S(2),S(3),S(4),S(5),S(6),S(7))
        End Function

        private function Ynorm(Y0,Ys00,Ys01,Ys11,Y1)
        dim yy,Yn00,Yn01,Yn11
        yy= csqrt(xoy(Y0,Y1)): Yn01=xdy(Ys01,yy)
        Yn00= xdy(Ys00,Y0)
        Yn11= xdy(Ys11,Y1)
        Ynorm=Array(Yn00(0),Yn00(1),Yn01(0),Yn01(1),Yn01(0),Yn01(1),Yn11(0),Yn11(1))
        End Function
        
        private function Shunt(Y0,jB,Y1)
        dim Y, dY,dR,R,R0,R1,T
        Y=Array(Y0(0)+Y1(0)+jB(0),Y0(1)+Y1(1)+jB(1))
        dY=Array(Y0(0)-Y1(0),Y0(1)-Y1(1))
        R= xdy(dY, Y): dR= xdy(jB, Y)
        T=xoy(xdy(Y0,Y),csqrt(xdy(Y1,Y0))): R0=xmy(R,dR): R1=xpy(R,dR)
        Shunt=Array(R0(0),R0(1),2.*T(0),2.*T(1),2.*T(0),2.*T(1),-R1(0),-R1(1))
        End Function

        private function PropNumber(k,tg,rm,a,b,mode,n,m)
'       k is wavenumber in medium, rm is Rm/Zo, tg is tangent
'        dim complex ki,g2,j
        dim pi,xn,xm,snm,g
        dim g2(1)
        pi=3.141592653589793238
        xn=pi/a*n
        xm=pi/b*m
        g2(0)=xn*xn+xm*xm-k*k*(1-tg*tg/4):g2(1)=k*k*tg
        if rm > 0 then
        snm=wnm(k,a,b,mode,n,m)
        g2(0)=g2(0)+rm*snm
        g2(1)=g2(1)+rm*snm
        end if
        g=csqrt(g2)
        PropNumber= Array(g(1),-g(0))
        End Function



        private function wnm(k,a,b,mode,n,m)
        dim k2,n2,m2,a2,a3,b2,b3,x2,snm,e2,pi2
        pi2=9.869604401089360
        if m=0 then e2=0.5 else e2=1.0 end if
        k2=k*k
        n2=n*n
        m2=m*m
        a2=a*a
        a3=a2*a
        b2=b*b
        b3=b2*b
        x2=pi2*(n2/a2+m2/b2)
        if mode =0 then
        snm=(n2*b3+m2*a3)/(n2*b2*a+m2*a3)
        else
        snm=(1.+b/a)*x2/k2+b/a*(e2-x2/k2)*(n2*a*b+m2*a2)/(n2*b2+m2*a2)
        end if
        wnm=4.*k*snm/b
        end function


        function kf(unit,f)
        if unit=0 then
        kf=0.0209579*f
        else
        kf=0.5323312*f
        end if
        end function

        function Rs(unit,ko,sigm)
' Rs is normalized to Zo=120pi  sigma=sigm*10^7*mhos/m
        dim wi
        if sigm < 0.0001 then
        Rs=0
        else
        wi=sqr(ko/sigm)
        if unit = 0 then
        Rs=3.643e-4*wi
        else
        Rs=7.229e-5*wi
        end if
        end if
        Rs=1.3*RS
        end function


'########################### COMPLEX FUNCTION MATH

Function x2(x)
x2=Array(x(0)*x(0)-x(1)*x(1),2.*x(0)*x(1))
End Function

Function cmplx(x,y)
cmplx=Array(x,y)
End Function

Function Re(x)
Re=x(0)
End Function

Function Im(x)
Im=x(1)
End Function

Function xpy(x, y)
xpy=Array(x(0) + y(0), x(1) + y(1))
End Function

Function xmy(x, y)
xmy=Array(x(0) - y(0), x(1) - y(1))
End Function

Function xoy(x, y)
xoy=Array(x(0) * y(0) - x(1) * y(1), x(0) * y(1) + x(1) * y(0))
End Function

Function xdy(x, y)
dim M
M = y(0) * y(0) + y(1) * y(1)
xdy=Array((x(0) * y(0) + x(1) * y(1)) / M, (-x(0) * y(1) + x(1) * y(0)) / M)
End Function

Function cexp(x)
Dim a
a = Exp(x(0)): cexp=Array(a * Cos(x(1)), a * Sin(x(1)))
End Function

Function ax(a,x)
ax=Array(a*x(0),a*x(1))
End Function

Function tanC(x)
dim a0, a1(1),a2(1), p(1), tg(1)
if x(1) < 0 then
p(0)=2.0*x(1):p(1)=-2.0*x(0)
a0= cexp(p): a1(0)=1.0-a0(0): a1(1)=-a0(1): a2(0)=1.0+a0(0): a2(1)=a0(1)
else
p(0)=-2.0*x(1):p(1)=2.0*x(0)
a0= cexp(p): a1(0)=a0(0)-1.0: a1(1)=a0(1): a2(0)=a0(0)+1.0: a2(1)=a0(1)
end if
a0= xdy(a1,a2)
tanC=Array(a0(1),-a0(0))
End Function

Function ctanC(x)
dim a0, a1(1),a2(1), p(1),ctg(1)
if x(1) < 0 then
p(0)=2.0*x(1):p(1)=-2.0*x(0)
a0= cexp(p): a1(0)=1.0-a0(0): a1(1)=-a0(1): a2(0)=1.0+a0(0): a2(1)=a0(1)
else
p(0)=-2.0*x(1):p(1)=2.0*x(0)
a0= cexp(p): a1(0)=a0(0)-1.0: a1(1)=a0(1): a2(0)=a0(0)+1.0: a2(1)=a0(1)
end if
a0= xdy(a2,a1)
ctanC=Array(-a0(1),a0(0))
End Function


Function cabs(x)
cabs = Sqr(x(0) * x(0) + x(1) * x(1))
End Function

Function arg(x)
Dim up, um,pi,pi2
pi= 3.141592653589793238
pi2=1.570796326794896619
up = x(1) + x(0): um = x(1) - x(0)
If up > 0 And um > 0 Then
arg = pi2 - Atn(x(0) / x(1))
End If
If up >= 0 And um <= 0 Then
arg = Atn(x(1) / x(0))
End If
If up < 0 And um < 0 Then
arg = -pi2 - Atn(x(0) / x(1))
End If
If up <= 0 And um >= 0 Then
If x(1) >= 0 Then arg = pi + Atn(x(1) / x(0)) Else arg = -pi + Atn(x(1) / x(0))
End If
End Function


Function inv(a,x)
dim M
M=x(0)*x(0)+x(1)*x(1): inv=Array(a*x(0)/M, -a*x(1)/M)
end Function

Function conj(x)
conj=Array(x(0),-x(1))
End Function

Function clog(x)
clog=Array(Log(cabs(x)),arg(x))
End Function

Function csqrt(x)
Dim z, res, ims, sqrx(1)
z = Sqr(x(0) * x(0) + x(1) * x(1))
res = Sqr((z + x(0)) / 2.)
ims = Sqr((z - x(0)) / 2.)
sqrx(0) = res   'sqrx(1) = ims  'VYBRAT" VETV"
if x(1)<0. then sqrx(1) = -ims else sqrx(1) = ims end if
csqrt=Array(sqrx(0),sqrx(1))
End Function


Function csin(x)
csin=Array(Sin(x(0)) * ch(x(1)), Cos(x(0)) * sh(x(1)))
End Function

Function ccos(x)
ccos=Array(Cos(x(0)) * ch(x(1)), -Sin(x(0)) * sh(x(1)))
End Function

Function x_2(x)
x_2=Array(x(0) * x(0) - x(1) * x(1),2. * x(0) * x(1))
End Function

Function aox(a, x)
aox=Array(a * x(0), a * x(1))
End Function

'=================== ELEMENTARY FUNCTION MATH

Function ch(x)
Dim y
 y = Exp(x): ch = 0.5 * (y + 1. / y)
End Function

Function sh(x)
Dim y
 y = Exp(x): sh = 0.5 * (y - 1. / y)
End Function


Function sinh(x)
Dim y
y = Exp(x): sinh = 0.5 * (y - 1. / y)
End Function

Function cosh(x)
Dim y
y = Exp(x): cosh = 0.5 * (y + 1. / y)
End Function

Function tanh(x)
Dim y
If y < 0 Then
y = Exp(2. * x): tanh = (y - 1.) / (y + 1.)
Else
y = Exp(-2. * x): tanh = (1. - y) / (1. + y)
End If
End Function

  Function dB(x)
  dim R
R = cabs(x)
If R > 1E-20 Then
dB = 20. * Log(R) / Log(10.)
Else
dB = -400
End If
End Function


function amax(x,y)
if x>=y then amax=x else amax=y end if
end function

function amin(x,y)
if x<=y then amin=x else amin=y end if
end function

function sign(x)
if x>0 then
  sign=1.0
  elseif x<0 then
  sign=-1.0
  else
  x=0.0
end if
end function

        function Amax(x,y)
        Amax=y
        if x => y then Amax=x end if
        end function
        function Amin(x,y)
        Amin=x
        if y <= x then Amin=y end if
        end function

        function arch(x)
        arch=log(sqr(x*x-1.e0)+x)
        end function

