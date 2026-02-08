'####This notice must be untouched at all times.######
'
'wg_class.vbs    v. 0.01
'The latest version is available at
'http://www.goulouev.com
'
' Library of classes, subroutines and functions modelling rectangular waveguide connections.
'The library is a VBScript realization of presolved problems of scattering at uniaxial 'rectangular-to-rectangular waveguide junctions, scattering matrices cascading, filter synthesis 'methods, variational 'optimization methods, etc. 
'
'    LICENSE: GPL (the GNU General Public License)
'Copyright (c) 2002-2003 Rousslan Goulouev. All rights reserved.
'Created 2/09/2002 by Rousslan Goulouev (Web: http://www.goulouev.com )
'Last modified: 28. 1. 2008
'    This library is free software: you can redistribute it and/or modify
'    it under the terms of the GNU General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    This library is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU General Public License for more details.
'
'    You should have received a copy of the GNU General Public License
'    along with this library.  If not, see <http://www.gnu.org/licenses/>.
'###############################################################################
'########################     CLASSES   ########################################

Sub CorrectSymCW(iSym,iCount,iVar,xVar,W)
      W(iCount,iVar)=xVar
      if iSym=1 then
      if iVar=3 then W(iCount,4)=-xVar/2.0
      if iVar=4 then W(iCount,3)=-2.0*xVar
      end if
end Sub


Sub CreateVarMap(Guide,nVar,VarNorma,VarTable)
Dim i,ii
ii=-1
VarNorma=0.0
for i=0 to Guide.Nw
if Guide.jow(i) > 0 then
ii=ii+1
VarTable(ii,0)=0: VarTable(ii,1)=i: VarTable(ii,2)=CInt(Guide.jow(i))
VarNorma=VarNorma+abs(Guide.w(i,VarTable(ii,2)))
end if
next
for i=0 to Guide.Nw-1
if Guide.joc(i) > 0 then
ii=ii+1
VarTable(ii,0)=1: VarTable(ii,1)=i: VarTable(ii,2)=CInt(Guide.joc(i))
VarNorma=VarNorma+abs(Guide.c(i,VarTable(ii,2)))
end if
next
VarNorma=VarNorma/CDbl(ii+1)
nVar=ii
End Sub


Function IsSmaller(A0,B0,H0,A1,B1,H1)
Dim Ax,Ay
Ax= (A0<=A1)
Ay= (H0>=H1 and H0+B0<=H1+B1)
IsSmaller=(Ax and Ay)
End Function

Function IsSame(A0,B0,H0,A1,B1,H1)
Dim Ax,Ay
Ax= (A0=A1)
Ay= (H0=H1 and H0+B0=H1+B1)
IsSame=(Ax and Ay)
End Function

Function IsCavity(A0,B0,H0,A,B,H,A1,B1,H1)
IsCavity= IsSmaller(A0,B0,H0,A,B,H) and IsSmaller(A1,B1,H1,A0,B0,H0)
End Function

Function IsIris(A0,B0,H0,A,B,H,A1,B1,H1)
IsIris= IsSmaller(A,B,H,A0,B0,H0) and IsSmaller(A,B,H,A1,B1,H1) 
End Function

Function IsSection(A0,B0,H0,A,B,H,A1,B1,H1)
IsSection = IsSame(A0,B0,H0,A,B,H) and IsSame(A1,B1,H1,A0,B0,H0)
End Function

'#########################################################
'#########################################################
CLASS OPTIMIZER
Dim OptDelta
Dim OptMethod, OptSteps 
Dim Engine
Private VarNorma, VarMap,nVar
Private OptSpec, OptSpecCount
Private iCount, iSweep,iVarStep, iVarDir,iOptStep
Private SpecMap ', ValueVarMin
Private FreqSweep, PortSweep,SpecSweep, ValueSweep,SweepCount
Dim uValue,uValue0, uValue1
Private W0,C0,W,C, PreSetup
'Private Eng0, wg0

Private Sub Class_Initialize()
Redim VarMap(100,2)
Redim OptSpec(5,5)
Redim FreqSweep(200),PortSweep(200),SpecSweep(200),ValueSweep(200)
OptSpecCount=-1: iCount=-1: iSweep=-1: iVarStep=0:iOptStep=0: iVarDir=1
Redim W0(50,5),C0(50,6),W(50,5),C(50,6)
'uValue0=0.0:uValue1=0.0: PreSetup=true
uValue0=1.0e6:uValue1=1.0e6: PreSetup=true

'Set Engine = New Solver
End Sub

Public Property Get CurrentStep
CurrentStep= iOptStep
End Property

Public Property Get VarCount
VarCount=nVar
End Property

Public Sub SetVarMap()
Call CreateVarMap(Engine.Guides,nVar,VarNorma,VarMap)
W0=Engine.Guides.W
C0=Engine.Guides.C
End Sub

Private Sub OptSweep()
dim i,ii,iii,Np
iii=-1
for i=0 to OptSpecCount
  Np=OptSpec(i,5)-1
  if Np<=0 then Np=1
  for ii=0 to OptSpec(i,5)-1
  iii=iii+1
  FreqSweep(iii)=OptSpec(i,3)+(OptSpec(i,4)-OptSpec(i,3))*CDbl(ii)/CDbl(Np)
  PortSweep(iii)=2*OptSpec(i,0)+OptSpec(i,1)-3
  SpecSweep(iii)=OptSpec(i,2)
  next
next
SweepCount=iii
End Sub

Public Sub AddSpec(Port0,Port1,Spec,f0,f1,Np)
OptSpecCount=OptSpecCount+1
OptSpec(OptSpecCount,0)=Port0
OptSpec(OptSpecCount,1)=Port1
OptSpec(OptSpecCount,2)=Abs(Spec)
OptSpec(OptSpecCount,3)=f0
OptSpec(OptSpecCount,4)=f1
OptSpec(OptSpecCount,5)=Np
call OptSweep
End Sub

Public Sub OptStep()
Dim So,iDelta
iSweep=iSweep+1
iCount=iCount+1
if iSweep=0 then call ChangeDelta
'call engine.struc(So)
'infobox iCount&"> "&iSweep&" "&iVarStep&" "&iOptStep'&" "&iDelta&" > "&formatnumber(uValue,4)&" "&formatnumber(uValue0,4)&" "&formatnumber(uValue1,4)&" > "&formatnumber(OptDelta,4)
engine.freq = FreqSweep(iSweep)
So=Engine.NewStruc(W,C)
ValueSweep(iSweep)=cabs(Array(So(2*PortSweep(iSweep)),So(2*PortSweep(iSweep)+1)))/dBmag(SpecSweep(iSweep))
if iSweep=SweepCount then
uValue=Norma(OptMethod,SweepCount,ValueSweep)
if uValue < uValue0 then 
uValue0=uValue
W0=W: C0=C
end if
iVarStep=iVarStep+1
iSweep=-1
end if

if iVarStep>nVar then
  if PreSetup then
  OptSpecCount=-1: iCount=-1: iSweep=-1: iVarStep=0:iOptStep=0: iVarDir=1
  PreSetup=false
  else
  iDelta=iOptStep-2*int(iOptStep/2)
  'msgIE iOptStep&" "&iDelta&" > "&formatnumber(uValue,4)&" "&formatnumber(uValue0,4)&" "&formatnumber(uValue1,4)&" > "&formatnumber(OptDelta,4)
  iOptStep=iOptStep+1
  iVarDir=4*CInt(iOptStep/2.0)-2*CInt(iOptStep)+1
  iVarStep=0
  iSweep=-1
  if iDelta=0 then
  if uValue0=uValue1 then OptDelta=OptDelta/2.0
  uValue1=uValue0
  End if
  End if
end if
End Sub

Private Sub ChangeDelta()
Dim x,i,ii,iii,xVar
W=W0:C=C0
if not PreSetup then
if VarMap(iVarStep,0)=0 then
x=(1.0+OptDelta*iVarDir)*W0(VarMap(iVarStep,1),VarMap(iVarStep,2))
Call CorrectSymCW(Engine.Guides.iSym,VarMap(iVarStep,1),VarMap(iVarStep,2),x*(1.0+OptDelta*iVarDir),W)
end if
if VarMap(iVarStep,0)=1 then
x=(1.0+OptDelta*iVarDir)*C0(VarMap(iVarStep,1),VarMap(iVarStep,2))
Call CorrectSymCW(Engine.Guides.iSym,VarMap(iVarStep,1),VarMap(iVarStep,2),x,C)
end if
end if
End Sub

Public Sub OptUpdate()
Engine.Guides.W=W0
Engine.Guides.C=C0
End Sub

Private Function Norma(NormaType,N,V)
Dim Vmax,SumV,i
Vmax=V(0): SumV=0.0
for i=0 to N
if V(i) > Vmax then Vmax=V(i)
SumV=SumV+V(i)*V(i)
'SumV=SumV+V(i)
next
if NormaType=0 then
Norma=Vmax
else
Norma=sqr(SumV/(N+1))
end if
End Function

Sub RecordData
Dim rLine,i,ii,StrucTextFile
with Engine.Guides
rLine= iCount&"> "&iSweep&" "&iVarStep&" "&iVarDir&" "&iOptStep&vbCrLf
rLine=rLine&iVarStep&" "&iOptStep&formatnumber(uValue,3)&" "&formatnumber(uValue0,3)&" "&formatnumber(OptDelta,4)&vbCrLf&vbCrLf
rLine=rLine&.iSym&" "&FormatNumber(.Eps,5)&" "&FormatNumber(.tg,5)&" "&FormatNumber(.Sigm,5)&vbCrLf
for i=0 to .Nw
rLine=rLine&.jow(i)&"   "&.jw(i): for ii=1 to 5: rLine=rLine&"     "&FormatNumber(w(i,ii),5): next
rLine=rLine&vbCrLf
if i < .Nw then
if .joc(i)=-1 then
rLine=rLine&"*   "&.jc(i): for ii=1 to 6: rLine=rLine&"     "&FormatNumber(c(i,ii),5): next
else
rLine=rLine&.joc(i)&"   "&.jc(i): for ii=1 to 6: rLine=rLine&"     "&FormatNumber(c(i,ii),5): next
end if
rLine=rLine&vbCrLf
end if
next
End With
StrucTextFile=rLine
Call SaveFile("jop.dat", StrucTextFile)
End Sub

Private Function dBmag(x)
dBmag=10.0^(-abs(x/20.0))
End Function

END CLASS


'#########################################################
'#########################################################


CLASS STRUCTURE
Dim Eps,tg,sigm
Dim iSym,Nw,jow,jw,w,joc,jc,c,wmin,cmin
Dim ErrStatus

Private Sub Class_Initialize()
redim jow(50),jw(50),w(50,5),joc(50),jc(50),c(50,6)
eps=1.0: tg=0.0: sigm=0.0: 
ErrStatus=False
End Sub

Public Function SpaceRemover(StrucText)
Dim LN,N,i, text
text=replace(StrucText,chr(13),"")
SpaceRemover=""
LN=Split(text,chr(10))
N=Ubound(LN)
for i=0 to N
if Trim(LN(i))<>"" then
if Left(LN(i),2)="  " then LN(i)=WordLineCorrect(" ","0"&LN(i)) else LN(i)=WordLineCorrect(" ",Trim(LN(i)))
SpaceRemover=SpaceRemover&LN(i)&vbCrLf
end if
next
End Function

Public Sub CreateTemplate(iStruc,Vars)
dim A,B,T,Ao,Bo,Ax,Bx
'TEMPLATES  Vars=Array(N,V0,V1,V2...)
'N,A,B,T
SELECT CASE iStruc
case "1"
if ubound(Vars)<3 then exit sub
N=CInt(Vars(0)):A=CDbl(Vars(1)):B=CDbl(Vars(2)):T=CDbl(Vars(3))
Nw=N+1
for i=0 to N
jow(i)=0: jw(i)=1: w(i,1)=(A+B)/4: w(i,2)=A/2.: w(i,3)=B: w(i,4)=-iSym*B/2.: w(i,5)=0.0
joc(i)=2: jc(i)=5: c(i,1)=T: c(i,2)=A/2.: c(i,3)=B: c(i,4)=-iSym*B/2.: c(i,5)=0.0: c(i,6)=0.0
next 
jow(N+1)=0: jw(N+1)=1: w(N+1,1)=(A+B)/4: w(N+1,2)=A/2.: w(N+1,3)=B: w(N+1,4)=-iSym*B/2.: w(N+1,5)=0.0
case "2"
if ubound(Vars)<3 then exit sub
N=CInt(Vars(0)):A=CDbl(Vars(1)):B=CDbl(Vars(2)):T=CDbl(Vars(3))
Nw=N+1
for i=0 to N
jow(i)=0: jw(i)=1: w(i,1)=(A+B)/4: w(i,2)=A/2.: w(i,3)=B: w(i,4)=-iSym*B/2.: w(i,5)=0.0
joc(i)=3: jc(i)=2: c(i,1)=T: c(i,2)=A/2.: c(i,3)=B: c(i,4)=-iSym*B/2.: c(i,5)=0.0: c(i,6)=0.0
next 
jow(N+1)=0: jw(N+1)=1: w(N+1,1)=(A+B)/4: w(N+1,2)=A/2.: w(N+1,3)=B: w(N+1,4)=-iSym*B/2.: w(N+1,5)=0.0
case 4
if ubound(Vars)<4 then exit sub
N=CInt(Vars(0)):A=CDbl(Vars(1)):B=CDbl(Vars(2)):Ao=CDbl(Vars(3)):Bo=CDbl(Vars(4))
Nw=N+1
for i=0 to N
Ax=A+(Ao-A)*CDbl(i)/CDbl(Nw)
Bx=B+(Bo-B)*CDbl(i)/CDbl(Nw)
jow(i)=0: jw(i)=1: w(i,1)=A/2.: w(i,2)=Ax/2.: w(i,3)=Bx: w(i,4)=-iSym*Bx/2.: w(i,5)=0.0
joc(i)=0: jc(i)=1: c(i,1)=0.: c(i,2)=0.: c(i,3)=0.: c(i,4)=0.: c(i,5)=0.0: c(i,6)=0.0
next 
jow(N+1)=0: jw(N+1)=1: w(N+1,1)=A: w(N+1,2)=Ao/2.: w(N+1,3)=Bo: w(N+1,4)=-iSym*Bo/2.: w(N+1,5)=0.0

END SELECT

End Sub

Public Sub StrucScale(x)
Dim i,ii
for i=0 to Nw
  for ii=1 to 4
  w(i,ii)=x*w(i,ii)
  if i<Nw then c(i,ii)=x*c(i,ii)
  next
next
End Sub

Public Sub StrucFlip()
dim jov(50),jv(50),v(50,5),jou(50),ju(50),u(50,6)
Dim i,ii
for i=0 to Nw
  jv(i)=jw(Nw-i)
  jov(i)=jow(Nw-i)
  for ii=1 to 4: v(i,ii)=w(Nw-i,ii):  next
  if i<Nw then
    ju(i)=jc(Nw-i-1)
    jou(i)=joc(Nw-i-1)
    for ii=1 to 4: u(i,ii)=c(Nw-i-1,ii):  next
  end if
next
jow=jov: jw=jv: w=v
joc=jou: jc=ju: c=u
End Sub



Public Sub StrucCorrect(cVar)
Dim i,ii
for i=0 to Nw
   if jow(i)>0 then
     w(i,jow(i))=cVar*w(i,jow(i))
     if iSym=1 then
     if jow(i)=3 then w(i,4)=-0.5*w(i,3)
     if jow(i)=4 then w(i,3)=-2*w(i,4)
     end if
   end if
next
for i=0 to Nw-1
   if joc(i)>0 and (jc(i)=2 or jc(i)=5) then
     c(i,joc(i))=cVar*c(i,joc(i))
     if iSym=1 then
     if joc(i)=3 then c(i,4)=-0.5*c(i,3)
     if joc(i)=4 then c(i,3)=-2*c(i,4)
     end if
   end if
next
End Sub




Public Sub ChangeSym()
Dim i
for i=0 to Nw
if iSym=0 then
  w(i,4)=-w(i,3): w(i,3)=2*w(i,3)
  if i<Nw then c(i,4)=-c(i,3): c(i,3)=2*c(i,3)
else
  w(i,4)=0.0: w(i,3)=0.5*w(i,3)
  if jow(i)=4 then jow(i)=3
  if i<Nw then
    c(i,4)=0.0: c(i,3)=0.5*c(i,3)
    if joc(i)=4 then joc(i)=3
  end if
end if
next
if iSym=0 then iSym=1 else iSym=0
End Sub

public property Let StrucDat(StrucTextFile)
Dim TextLines,Ns,Nv,V,i,ii,iii,IsNode
if trim(StrucTextFile)="" then exit property
StrucTextFile=replace(StrucTextFile,chr(13),"")
TextLines=split(SpaceRemover(StrucTextFile), chr(10))
Ns=ubound(TextLines)
ii=-1
IsNode=true
for i=0 to Ns
if trim(TextLines(i))<>"" then
   V=Split(WordLineCorrect(" ",TextLines(i))," ")
   if ubound(V)>=6 then
      if IsNode=true then
      ii=ii+1
      if V(0)="*" then jow(ii)=-1: else jow(ii)=CInt(V(0))
      jw(ii)=CInt(V(1)): w(ii,1)=CDbl(V(2)): w(ii,2)=CDbl(V(3)): w(ii,3)=CDbl(V(4)): w(ii,4)=CDbl(V(5)): w(ii,5)=0.0
      IsNode=False
      else
      joc(ii)=CInt(V(0))
      jc(ii)=CInt(V(1)): c(ii,1)=CDbl(V(2)): c(ii,2)=CDbl(V(3)): c(ii,3)=CDbl(V(4)): c(ii,4)=CDbl(V(5)): c(ii,5)=0.0: c(ii,6)=0.0
      IsNode=True
      end if
   end if
end if
next
Nw=ii
end property

public property Get StrucDat
Dim i,ii,wMark
StrucDat=""
if Nw<=0 then exit property
for i=0 to Nw
if jow(i)=-1 then wMark="*" else wMark=jow(i)
StrucDat=StrucDat&wMark&"   "&jw(i): for ii=1 to 5: StrucDat=StrucDat&"     "&FormatNumber(w(i,ii),5): next
StrucDat=StrucDat&vbCrLf
if i < Nw then
if joc(i)=-1 then
StrucDat=StrucDat&"*   "&jc(i): for ii=1 to 6: StrucDat=StrucDat&"     "&FormatNumber(c(i,ii),5): next
else
StrucDat=StrucDat&joc(i)&"   "&jc(i): for ii=1 to 6: StrucDat=StrucDat&"     "&FormatNumber(c(i,ii),5): next
end if
StrucDat=StrucDat&vbCrLf
end if
next
end property

public Sub Correct(iType,iCount,iVar,xVar)
if iType=0 then
      w(iCount,iVar)=xVar
      if iSym=1 then
      if iVar=3 then w(iCount,4)=-xVar/2.0
      if iVar=4 then w(iCount,3)=-2.0*xVar
      else
      end if
else
      c(iCount,iVar)=xVar
      if iSym=1 then
      if iVar=3 then c(iCount,4)=-xVar/2.0
      if iVar=4 then c(iCount,3)=-2.0*xVar
      else
      end if
end if
end sub

Public Function IsTaper()
IsTaper= jw(0)=1 and jc(0)=1 and jw(1)=1 and jc(1)=1 and jw(2)=1 and jw(Nw)=1 and jc(Nw-1)=1 and jw(Nw-1)=1 and jc(Nw-2)=1 and jw(Nw-2)=1 
End Function

Public Function IsFilter()
Dim iMark, jV(50),N0,N1,i,condition
        iMark=-1
        for i=0 to Nw
        if jow(i)=-1 then iMark=iMark+1: jV(imark)=i: end if
        next
        if iMark > 1 then msgbox "Cannot be more than two '*' marks": exit function: end if
        N0=CInt(jV(0))
        N1=CInt(jV(1))-1
        if jV(1)=0 then N1=Nw-1 end if
        for i=N0 to N1
        condition=false
        if jc(i)=2 and  joc(i)=3 then condition=true
        if jc(i)=5 and  joc(i)=2 then condition=true
        if not condition then IsFilter= false: Exit function
        next
        IsFilter=true
End Function

Public Sub CollectIrises()
Dim u(100),Nwg,i,ii,v0,v,v1,ju(100),p(100,5)
ii=-1
for i=0 to Nw
ii=ii+1
u(ii)=Array(jow(i),w(i,1),w(i,2),w(i,3),w(i,4)): ju(ii)=1
if i<Nw and jc(i)<>1 then
ii=ii+1
u(ii)=Array(joc(i),c(i,1),c(i,2),c(i,3),c(i,4)): ju(ii)=1
end if
next
Nwg=ii
for i=1 to Nwg-1
v0=u(i-1):v=u(i):v1=u(i+1)
if IsSmaller(v(2),v(3),v(4),v0(2),v0(3),v0(4)) and IsSmaller(v(2),v(3),v(4),v1(2),v1(3),v1(4)) then
ju(i)=5
else
ju(i)=1
end if
next
ii=-1
for i=0 to Nwg-1
v=u(i)
if ju(i)=1 then
ii=ii+1
    p(ii,0)=1:p(ii,1)=v(0):p(ii,2)=v(1):p(ii,3)=v(2):p(ii,4)=v(3):p(ii,5)=v(4)
    if ju(i+1)=1 then 
ii=ii+1
    p(ii,0)=0:p(ii,1)=0:p(ii,2)=0:p(ii,3)=0:p(ii,4)=0:p(ii,5)=0
    end if
else
if ju(i)=5 then
ii=ii+1
    p(ii,0)=5:p(ii,1)=v(0):p(ii,2)=v(1):p(ii,3)=v(2):p(ii,4)=v(3):p(ii,5)=v(4)
end if
end if
next
ii=ii+1
v=u(Nwg)
    p(ii,0)=1:p(ii,1)=v(0):p(ii,2)=v(1):p(ii,3)=v(2):p(ii,4)=v(3):p(ii,5)=v(4)
Nwg=ii
for i=0 to Nwg
ii=int(0.5*i)
if i-2*ii=0 then
jw(ii)=CInt(p(i,0)):jow(ii)=CInt(p(i,1)):w(ii,1)=p(i,2):w(ii,2)=p(i,3):w(ii,3)=p(i,4):w(ii,4)=p(i,5)
else
if p(i,0)=0 then jc(ii)=1 else jc(ii)=CInt(p(i,0)) 
joc(ii)=CInt(p(i,1)):c(ii,1)=p(i,2):c(ii,2)=p(i,3):c(ii,3)=p(i,4):c(ii,4)=p(i,5)
end if
next
Nw=ii
'Redim u,ju,p
'u=empty: ju=empty: p=empty
End Sub

Public Sub CollectCavities()
Dim u(100),Nwg,i,ii,v0,v,v1,ju(100),p(100,5)
ii=-1
for i=0 to Nw
ii=ii+1
u(ii)=Array(jow(i),w(i,1),w(i,2),w(i,3),w(i,4)): ju(ii)=1
if i<Nw and jc(i)<>1 then
ii=ii+1
u(ii)=Array(joc(i),c(i,1),c(i,2),c(i,3),c(i,4)): ju(ii)=1
end if
next
Nwg=ii
for i=1 to Nwg-1
v0=u(i-1):v=u(i):v1=u(i+1)
if IsSmaller(v0(2),v0(3),v0(4),v(2),v(3),v(4)) and IsSmaller(v1(2),v1(3),v1(4),v(2),v(3),v(4)) then
ju(i)=2
else
ju(i)=1
end if
next
ii=-1
for i=0 to Nwg-1
v=u(i)
if ju(i)=1 then
ii=ii+1
    p(ii,0)=1:p(ii,1)=v(0):p(ii,2)=v(1):p(ii,3)=v(2):p(ii,4)=v(3):p(ii,5)=v(4)
    if ju(i+1)=1 then 
ii=ii+1
    p(ii,0)=0:p(ii,1)=0:p(ii,2)=0:p(ii,3)=0:p(ii,4)=0:p(ii,5)=0
    end if
else
if ju(i)=2 then
ii=ii+1
    p(ii,0)=2:p(ii,1)=v(0):p(ii,2)=v(1):p(ii,3)=v(2):p(ii,4)=v(3):p(ii,5)=v(4)
end if
end if
next
ii=ii+1
v=u(Nwg)
    p(ii,0)=1:p(ii,1)=v(0):p(ii,2)=v(1):p(ii,3)=v(2):p(ii,4)=v(3):p(ii,5)=v(4)
Nwg=ii
for i=0 to Nwg
ii=int(0.5*i)
if i-2*ii=0 then
jw(ii)=CInt(p(i,0)):jow(ii)=CInt(p(i,1)):w(ii,1)=p(i,2):w(ii,2)=p(i,3):w(ii,3)=p(i,4):w(ii,4)=p(i,5)
else
if p(i,0)=0 then jc(ii)=1 else jc(ii)=CInt(p(i,0)) 
joc(ii)=CInt(p(i,1)):c(ii,1)=p(i,2):c(ii,2)=p(i,3):c(ii,3)=p(i,4):c(ii,4)=p(i,5)
end if
next
Nw=ii
'Redim u,ju,p
'u=empty: ju=empty: p=empty
End Sub

Public Function HfssModel(iUnit)
Dim LN, i,iCount,un(1),dx,xL
xL=0.
un(0)="mm": un(1)="in"
LN=""
iCount=0
LN= LN & HeadVBS(iUnit)
for i=0 to Nw
dx=w(i,1)
if i=0 and dx<=0 then dx=w(i,2) 
LN=LN&AddBoxVBS(un(iUnit), iCount, -w(i,2), w(i,4), xL,2*w(i,2) , w(i,3), dx)
iCount=iCount+1: xL=xL+dx
if i<Nw then
   if jc(i)=2 or jc(i)=5 then
   dx=c(i,1)
   LN=LN&AddBoxVBS(un(iUnit), iCount, -c(i,2), c(i,4), xL,2*c(i,2) , c(i,3), dx)
   iCount=iCount+1: xL=xL+dx
   end if
end if
next
LN=LN&UniteVBS(0, iCount-1)
HfssModel=LN
End Function


Private Function HeadVBS(iUnit)
Dim LN,v
v = Chr(34)
LN = LN + "Dim oDesktop"&vbCrLf
LN = LN + "Dim oDesign"&vbCrLf
LN = LN + "Dim oEditor"&vbCrLf
LN = LN + "Dim oModule"&vbCrLf
LN = LN + "Dim oAnsoftApp"&vbCrLf
LN = LN + "Dim oProject"&vbCrLf
LN = LN + "Dim Un"&vbCrLf
LN = LN + "Un="&v&"in"&v&vbCrLf
LN = LN + "Set oAnsoftApp = CreateObject("&v&"AnsoftHfss.HfssScriptInterface"&v&")"&vbCrLf
LN = LN + "Set oDesktop = oAnsoftApp.GetAppDesktop()"&vbCrLf
LN = LN + "oDesktop.RestoreWindow"&vbCrLf
LN = LN + "Set oProject = oDesktop.GetActiveProject"&vbCrLf
LN = LN + "oDesktop.NewProject"&vbCrLf

LN = LN + "Set oAnsoftApp = CreateObject("&v&"AnsoftHfss.HfssScriptInterface"&v&")"&vbCrLf
LN = LN + "Set oDesktop = oAnsoftApp.GetAppDesktop()"&vbCrLf
LN = LN + "oDesktop.RestoreWindow"&vbCrLf
LN = LN + "DesktopProjects=oDesktop.GetProjectList"&vbCrLf
LN = LN + "nProject=ubound(DesktopProjects)"&vbCrLf
LN = LN + "nProjectName=DesktopProjects(nProject)"&vbCrLf
LN = LN + "Set oProject = oDesktop.SetActiveProject(nProjectName)"&vbCrLf
'LN = LN + "Set oProject = oDesktop.SetActiveProject("&v&"WRConnect_Model"&v&")"&vbCrLf
'LN = LN + "oProject.SaveAs "&v&"model.hfss"&v&", true"&vbCrLf
LN = LN + "oProject.InsertDesign "&v&"HFSS"&v&", "&v&"HFSSDesign1"&v&", "&v&"DrivenModal"&v&", "&v&v&vbCrLf
LN = LN + "Set oDesign = oProject.SetActiveDesign("&v&"HFSSDesign1"&v&")"&vbCrLf
LN = LN + "Set oEditor = oDesign.SetActiveEditor("&v&"3D Modeler"&v&")"&vbCrLf
if iUnit=0 then
LN = LN + "oEditor.SetModelUnits Array("&v&"NAME:Units Parameter"&v&", "&v&"Units:="&v&", "&v&"mm"&v&", "&v&"Rescale:="&v&", false)"&vbCrLf
else
LN = LN + "oEditor.SetModelUnits Array("&v&"NAME:Units Parameter"&v&", "&v&"Units:="&v&", "&v&"in"&v&", "&v&"Rescale:="&v&", false)"&vbCrLf
end if
HeadVBS = LN
End Function

Private Function AddBoxVBS(Un, i, x, y, z, dx, dy, dz)
Dim LN,v,xo,yo,zo,dxo,dyo,dzo,Bo
v = Chr(34)
If dx = 0 Or dy = 0 Or dz = 0 Then AddBoxVBS = "": Exit Function
xo = LTrim(RTrim(x)): yo = LTrim(RTrim(y)): zo = LTrim(RTrim(z))
dxo = LTrim(RTrim(dx)): dyo = LTrim(RTrim(dy)): dzo = LTrim(RTrim(dz))
Bo = "obj" + LTrim(RTrim(i))
LN = "oEditor.CreateBox Array(""NAME:BoxParameters"", ""XPosition:=""," + v + xo + Un + v + ", ""YPosition:="", " + v + yo + Un + v + ", ""ZPosition:="", " + v + zo + Un + v + ", ""XSize:="", " + v + dxo + Un + v + ", ""YSize:="", " + v + dyo + Un + v + ", ""ZSize:="", " + v + dzo + Un + v + "), Array(""NAME:Attributes"", ""Name:="", " + v + Bo + v + ", ""Flags:="", """", ""Color:="", ""(132 132 193)"", ""Transparency:="", 0, ""PartCoordinateSystem:="", ""Global"", ""MaterialName:="", ""vacuum"", ""SolveInside:="", True)" + vbCrLf
AddBoxVBS = LN
End Function

Private Function UniteVBS(i0, i1)
Dim LN,v,tool,i
v = Chr(34)
tool = ""
For i = i0 To i1
tool = tool + "obj" + LTrim(RTrim(i))
If i <> i1 Then tool = tool + ","
Next
LN = "oEditor.Unite Array(""NAME:Selections"", ""Selections:="", " + v + tool + v + "), Array(""NAME:UniteParameters"", ""KeepOriginals:="", false)"
UniteVBS = LN
End Function

Public Function DrawAuto()
Dim LN, i,iCount,xL
x0=0.
LN=""
for i=0 to Nw
dx=w(i,1)
if i=0 and dx<=0 then dx=w(i,2) 
   y0=c(i,4):z0=-c(i,2)
   y1=c(i,4)+c(i,3):z1=c(i,2)
   x1=x0+c(i,1):
   LN = LN & "box" & vbCrLf
   LN = LN & x0& "," & y0 & "," & z0& vbCrLf
   LN = LN & (x0 + dx) & "," & (y0 + dy) & "," & (z0 + dz) & vbCrLf
   x0=x1
if i<Nw then
   if jc(i)=2 or jc(i)=5 then
   y0=c(i,4):z0=-c(i,2)
   y1=c(i,4)+c(i,3):z1=c(i,2)
   x1=x0+c(i,1):
   LN = LN & "box" & vbCrLf
   LN = LN & x0& "," & y0 & "," & z0& vbCrLf
   LN = LN & (x0 + dx) & "," & y1 & "," & z1 & vbCrLf
   x0=x1
   end if
end if
next
DrawAuto=LN
End Function

END CLASS


'##################################### DEVICE

CLASS SOLVER
Dim ActiveMode, iUnit
Dim ModesX,ModesY
'dim Nw,jow,joc,w,c,jw,jc
dim guides
private rm, kSpace,kMedium,kWave, VarMap,VarNorma, nVar, BufError
Dim Prototype

Private Sub Class_Initialize()
set guides= New Structure
redim prototype(5)
ActiveMode=1: iUnit=1
End Sub

Public Property Get Errors()
Errors=BufError
End Property

Public Property Let Freq(x)
kSpace=kf(x)
kMedium=kSpace*sqr(guides.eps)
kWave=Array(kMedium,-0.5*kMedium*guides.tg)
Rm=Rs()
End Property


Public sub SynthesisTaper()
dim StrucTextFile
with guides
Dim Ao,Bo,Ho,At,Bt,Ht,Dt,A,B,H,D,Ac,Bc,Hc,Sc,Dc
Ao=.w(0,2): Bo=.w(0,3): Ho=.w(0,4): At=.w(1,2): Bt=.w(1,3): Ht=.w(1,4): Dt=.w(1,1): A=.w(2,2): B=.w(2,3): H=.w(2,4): D=.w(2,1)
Ac =.c(2,2): Bc=.c(2,3): Hc=.c(2,4): Sc=.c(2,1): Dc=.w(2,1)
Call TransRoot(Ao,Bo,Ho,At,Bt,Ht,Dt,A,B,H,D,Ac,Bc,Hc,Sc,Dc)
.w(0,2)=Ao: .w(0,3)=Bo: .w(0,4)=Ho: .w(1,2)=At: .w(1,3)=Bt: .w(1,4)=Ht: .w(1,1)=Dt: .w(2,2)=A: .w(2,3)=B: .w(2,4)=H: .w(2,1)=D
.c(2,2)=Ac : .c(2,3)=Bc: .c(2,4)=Hc: .c(2,1)=Sc: .w(2,1)=Dc
Ao=.w(.Nw,2): Bo=.w(.Nw,3): Ho=.w(.Nw,4): At=.w(.Nw-1,2): Bt=.w(.Nw-1,3): Ht=.w(.Nw-1,4): Dt=.w(.Nw-1,1)
A=.w(.Nw-2,2): B=.w(.Nw-2,3): H=.w(.Nw-2,4): D=.w(.Nw-2,1)
Ac =.c(.Nw-3,2): Bc=.c(.Nw-3,3): Hc=.c(.Nw-3,4): Sc=.c(.Nw-3,1): Dc=.w(.Nw-2,1)
Call TransRoot(Ao,Bo,Ho,At,Bt,Ht,Dt,A,B,H,D,Ac,Bc,Hc,Sc,Dc)
.w(.Nw,2)=Ao: .w(.Nw,3)=Bo: .w(.Nw,4)=Ho: .w(.Nw-1,2)=At: .w(.Nw-1,3)=Bt: .w(.Nw-1,4)=Ht: .w(.Nw-1,1)=Dt
.w(.Nw-2,2)=A: .w(.Nw-2,3)=B: .w(.Nw-2,4)=H: .w(.Nw-2,1)=D
.c(.Nw-3,2)=Ac : .c(.Nw-3,3)=Bc: .c(.Nw-3,4)=Hc: .c(.Nw-3,1)=Sc: .w(.Nw-2,1)=Dc
end with
End Sub

Public sub SynthesisMain()
dim pi
dim StrucTextFile
dim jV(50), Bj(50), O0(50),O1(50), G(50)
dim SetupTextFile,iFilter,Att,Fc,dF,F0,F1,Np,L,t
dim So,Q,SynErr
dim i,f,  RdB,TdB,rv, iMark,N0,N1, xs, g2
        pi=3.141592653589793236
        Rm=0.
        ActiveMode=1
        iMark=-1
        for i=0 to Guides.Nw
        if guides.jow(i)=-1 then iMark=iMark+1: jV(imark)=i: end if
        next
        if iMark > 1 then msgbox "Cannot be more than two '*' marks": exit sub: end if
        N0=jV(0)
        N1=jV(1)-1
        if jV(1)=0 then N1=Guides.Nw-1 end if
        call PrototypeSynthesis(N1-N0,Bj)
        for i=N0 to N1
        if (guides.jc(i)=2) or (guides.jc(i)=5) then
        L=guides.joc(i)
        call Rt(SynErr,I,L,t,Bj(I-N0),O0(I),O1(I))
        xs=pi/2./guides.w(i,2)
        g2=kSpace*kSpace-xs*xs
        if g2 <=0 then msgbox "The Central Frequency is below cut-off": exit sub: end if
        G(i)=sqr(kSpace*kSpace-xs*xs)
        end if
        next
        for i=N0+1 to N1
        if Prototype(1)<=-1 then
        guides.w(i,1)=(pi*guides.jow(i)+O1(i-1)+O0(i))/G(i)
        else
        guides.w(i,1)=(pi*Prototype(1)+O1(i-1)+O0(i))/G(i)
        end if
        if guides.w(i,1) < guides.wmin then guides.w(i,1)=guides.wmin end if
        next
        end sub

        public sub DoPhaseMatch(iMatch)
        dim So,O0(50),O1(50),F1,G(50),i,pi,xs,g2
        pi=3.141592653589793236
        
        for i=0 to guides.Nw
        if i < guides.Nw then
          call Element(i,So)
           call BO(So,F1,O0(i),O1(i))
        end if
          xs=pi/2./guides.w(i,2)
          g2=kSpace*kSpace-xs*xs
            if g2 <=0 then
            msgbox "The Central Frequency is below cut-off": exit sub
            else
            G(i)=sqr(kSpace*kSpace-xs*xs)
            end if
        next

        for i=1 to guides.Nw-1
        if iMatch<=-1 then
           guides.w(i,1)=(pi*guides.jow(i)+O1(i-1)+O0(i))/G(i)
        else
           guides.w(i,1)=(pi*iMatch+O1(i-1)+O0(i))/G(i)
        end if
        if guides.w(i,1) < guides.wmin then guides.w(i,1)=guides.wmin end if
        next
        end sub

        public sub TransMain()
        dim x(40),Rmax,i,AA,A1,A0,Y1,Y0
        Rmax= Rx(0.0,1.0)
        for i=1 to 40
        AA=0.05*i*Rmax
        call DetSteps(AA,guides.Nw-1,X)
        A1=AA
        Y1=X(guides.Nw-1)
        if  Y1 > 1.0 then exit for
        A0=A1
        Y0=Y1
        next
'1011    continue
        for i=1 to 5
        A1=A0+(A1-A0)/(Y1-Y0)*(1.-Y0)
        call DetSteps(A1,guides.Nw-1,X)
        Y1=X(guides.Nw-1)
        if  (Y1-1.0) < 1.0e-3 then exit for 'goto 1012
        next
'1012    continue
        for i=0 to guides.Nw-1
        x(i)=x(i)/Y1
        next
dim v0,v1,v2
        for i=1 to guides.Nw
        call CrossSection(X(i-1),v0,v1,v2)
        guides.w(i,2)=v0: guides.w(i,3)=v1: guides.w(i,4)=v2
        next
        end sub
        




'LIBS FOR WR_Connect
'##############################   SYNTHESIS SUBS ###########################
'OPT SUBS



        private sub DetSteps(A,N,X)
        dim Am,xo,R,i
         Am=A/2^N
         xo=0.0
         for i=0 to N
         R=A*Bimo(i,N)
         call DetStep(xo,R,X(i))
         xo=X(i)
         next
        end sub


        private sub CrossSection(x,Ax,Bx,Hx)
        Dim A0,B0,H0,A1,B1,H1
        A0=guides.w(0,2):B0=guides.w(0,3):H0=guides.w(0,4)
        A1=guides.w(guides.Nw,2):B1=guides.w(guides.Nw,3):H1=guides.w(guides.Nw,4)
        Ax=A0+(A1-A0)*x
        Bx=B0+(B1-B0)*x
        Hx=H0+(H1-H0)*x
        end sub

        private sub DetStep(xo,R,X)
        dim So,Y,jB,Ao,Bo,Ho,i,R0,R1,X0,X1
        call CrossSection(xo,Ao,Bo,Ho)
        for i=0 to 400
        x=xo+0.01*i
        R1= Rx(xo,x)
        X1=x
        if  R1 > R then exit for 'goto 1001
        X0=X1
        R0=R1
        next
'1001    continue
        R0= Rx(xo,X0)
        for i=1 to 5
        x1=x0+(R-R0)/(R1-R0)*(x1-x0)
        R1= Rx(xo,x1)
        if  abs(x1-x0) < 1.e-6 then exit for 'goto 1002
        next
'1002    continue
        x=x1
        end sub

        private function Rx(xo,x)
        dim Ao,Bo,Ho,A,B,H,So
        call CrossSection(xo,Ao,Bo,Ho)
        call CrossSection(x,A,B,H)
        if  A*B < Ao*Bo then
        So=STF(A,B,H,Ao,Bo,Ho)
        else
        So=STF(Ao,Bo,Ho,A,B,H)
        end if
        Rx=cabs(So)
        end function


        private function Bimo(m,N)
        dim C,i,xm,xN
        C=1.
        for i=m+1 to N
        xm=CDbl(i)
        xN=CDbl(i-m)
        C=C*xm/xN
        next
        Bimo=C
        end function



'=========== SYNTHESIS TAPER=
private sub TransRoot(Ao,Bo,Ho,At,Bt,Ht,Dt,A,B,H,D,Ac,Bc,Hc,Sc,Dc)
        dim R,S0,S1,gt 'complex
        dim pi,I0,I1,R0,R1,B0,B1,Np,Fs,F0,F1,i
        pi=3.141592653589793236
        Np=500
        for i=0 to Np
        Bt=B+(Bo-B)*i*1./Np
        Ht=H+(Ho-H)*i*1./Np
        S0=STF(Ao,Bo,Ho,At,Bt,Ht)
        S1=SF(At,Bt,Ht,A,B,H,D,Ac,Bc,Hc,Sc,Dc)
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
        gt=sqr(kSpace*kSpace-(pi/2./at)^2)
        F0=Im(clog(S0))
        F1=Im(clog(S1))
        Fs=0.5*(F0+F1)
        if Fs<0. then Fs=Fs+pi
        if Fs>pi then Fs=Fs-pi
        Dt=Fs/gt
        end sub

private function STF(Ao,Bo,Ho,At,Bt,Ht)
        dim Yo 'complex
        dim Yt,jBo,So,Gt 'complex
        dim pi,xt
        pi=3.141592653589793236
        xt=pi/At/2.
        gt=sqr(kSpace*kSpace-xt*xt)
        Yt=Array(gt/kMedium,0.0)
        call StepEH(At,Bt,Ht,Ao,Bo,Ho,Yo,jBo)
        So= Shunt(Yo,jBo,Yt)
        STF=Array(So(6),So(7))
        end function

private function SF(At,Bt,Ht,A,B,H,D,Ac,Bc,Hc,Sc,Dc)
        dim Yo,S,Y,PH,G,gama 'complex
        dim jBt,St,Yt,Gt,Ex,SFs, g2  'complex
        dim pi,x
        pi=3.141592653589793236
        x=pi/A/2.
        g2=Array(x*x-kSpace*kSpace,0.)
        g=csqrt(g2)
        Y=Array(g(1)/kMedium,-g(0)/kMedium)
        ph=Array(g(1)*Dc,-g(0)*Dc)
        call CavEH(A, B, H,Ac,Bc,Hc,Sc,A, B, H,Yo)
        S= JuncInfinite(Y,Yo,ph)
        call StepEH(A,B,H,At,Bt,Ht,Yt,jBt)
        St= Shunt(Yt,jBt,Y)
        SFs=Sconnect(St,ph,S)
        SF=Array(SFs(0),SFs(1))
        end function

private function JuncInfinite(Y0,Y,Ph)
        dim E,j,w 'complex
        dim SR,S00,S01,So,cs,cs0 'complex
        E=Array(1.,0.)
        j=Array(0.,1.)
        w=cexp(Array(ph(1),-ph(0)))
        So= SY2(Y0,Y,Y0)
        S00=xoy(Scatter(0,So),w)
        S01=xoy(Scatter(1,So),w)
        cs0=xdy(xpy(E,xmy(x2(S01),x2(S00))),S01)
        cs=cs0(0)/2.0
        SR=xdy(xmy(E,xoy(S01,Array(cs,sqr(1.0-cs*cs)))),S00)
        JuncInfinite=Array(SR(0),SR(1), 0.0,0.0, 0.0,0.0,SR(0),SR(1))
end function
         
'========================SYNTHESIS GENERAL

private sub Rt(iCode,I,L,t,Bj,O0,O1)
        dim So
        dim r_tst, Np_max,F, xmin,xmax, ii, t1,t0,F1,F0,dBj,Porog,index0,index1
        dim Bmin,Bmax, tMin
        dim dt,dtMax
        F=Bj
        Porog=1.0e6
        call Bounds(I,L,xmin,xmax)
        Np_max=100
        r_tst=""
        dtMax=(xmax-xmin)/Np_max
        dt=dtMax
        t1=xmin
        for ii=0 to 5*Np_max
	    iCode=false
        t1=t1+dt 'xmin+(xmax-xmin)*ii/Np_max
        call El(I,L,t1,So)
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
        call El(I,L,t1,So)
        call BO(So,F1,O0,O1)
        Bmin=F1
        Bmax=F
        if abs(F-F1)/F < 1.e-5 then exit for
        next
        t=t1
        end if
        end sub

private sub Bounds(I,L,xmin,xmax)
        dim A, kod
        A=amax(guides.w(i,2),guides.w(i+1,2))
        if guides.jc(i)=2 then
          kod=1
          if L=1 then
          xmax=10.*A
          xmin=0.001*A
          kod=2
          end if
          if L=2 then
          xmax=5.*A
          xmin=amax(guides.w(i,2),guides.w(i+1,2))
          kod=2
          end if

          if L=3 then
          xmin=amax(guides.w(i,3),guides.w(i+1,3))
          xmax=xmin+1.5*A
          kod=2
          end if
        end if

        if guides.jc(i)=5 then
          kod=1
          if L=1 then
          xmax=10.*A
          xmin=0.001*A
          kod=2
          end if
          if L=2 then
          xmax=0.01*A
          xmin=amin(guides.w(i,2),guides.w(i+1,2))
          kod=2
          end if

          if L=3 then
          xmax=0.01*A
          xmin=amin(guides.w(i,3),guides.w(i+1,3))
          kod=2
          end if
        end if

        if guides.jc(i)=21 then
          kod=1
          if L=1 then
          xmax=guides.c(i,4)
          xmin=0.001*A
          kod=2
          end if
          if L=2 then
          xmax=0.01*A
          xmin=amin(0.5*(guides.w(i,2)+guides.w(i+1,2)),guides.c(i,5))
          kod=2
          end if

          if L=6 then
          xmax=guides.c(i,6)
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

private sub PrototypeSynthesis(N,Bj)
        dim g(20),gk(100),pi,O,i,OO
        dim Q0,Q1,Lar,index,B0,B1,B,Band,xL
        pi=3.141592653589793236
        index=CInt(Prototype(0))
        B0=Prototype(2):B=Prototype(3):B1=Prototype(4):Lar=Prototype(5)
        if index < 2 then
        Band=1.0/B
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
        for  i=0 to N
        xL=CDbl(2*i-N)/CDbl(N)
        Bj(i)= yProfile(xL,Lar,B0,B,B1)
        next
        end if
end sub

private function xProfile(x,ALFA)
        dim teta,wx,wALFA
        teta=sign(x)
        wx=1.-abs(x)
        if wx <= 0. then
        wALFA=0.
        else
        wALFA=wx^ALFA
        end if
        xProfile=teta*(1.-wALFA)
end function

private function yProfile(x,ALFA,B0,B,B1)
        dim Bm,dB,Bc,t
        Bm=(B0+B1)/2.
        dB=(B1-B0)/2.
        Bc=B-Bm
        t=xProfile(x,ALFA)
        yProfile=Bm+dB*t+Bc*(1.-t*t)
end function
        
'##############################   STRUCTURE SUBS ###########################


Private Sub El(iCount,iVar,xVar,Se)
'        call SymCorrect(iCount,iVar,xVar,guides.c)
        call guides.Correct(1, iCount,iVar,xVar)
        call Element(iCount,Se)
End Sub
        
'===================
Public Sub Struc(So)
        dim Se,Yo,jB
        dim g0,g1,Y0,Y1,ph
        dim pi,A0,B0,H0,x0,S,A,B,H,A1,B1,H1,x1
        dim i
        
        pi=3.141592653589793238
        for i=Guides.Nw-1 to 0 step -1
'*--------ELEMENTS-------------------------------
        select case guides.jc(i)
        
        case 2
        A0=guides.w(i,2)/ActiveMode
        B0=guides.w(i,3)
        H0=guides.w(i,4)
        x0=pi/A0/2.
        S=guides.c(i,1)
        A=guides.c(i,2)/ActiveMode
        B=guides.c(i,3)
        H=guides.c(i,4)
        A1=guides.w(i+1,2)/ActiveMode
        B1=guides.w(i+1,3)
        H1=guides.w(i+1,4)
        x1=pi/A1/2.
        call CavEH(A0,B0,H0,A,B,H,S,A1,B1,H1,Yo)
g0= PropNumber(2.*a0,b0,1,1,0)
g1= PropNumber(2.*a1,b1,1,1,0)
        Y0= xdy(g0,kWave)   
        Y1= xdy(g1,kWave)   
        Se= SY2(Y0,Yo,Y1)
        
         
        case 1
        A0=guides.w(i,2)/ActiveMode
        B0=guides.w(i,3)
        H0=guides.w(i,4)
        x0=pi/A0/2.
        A1=guides.w(i+1,2)/ActiveMode
        B1=guides.w(i+1,3)
        H1=guides.w(i+1,4)
        x1=pi/A1/2.
g0= PropNumber(2.*a0,b0,1,1,0)
g1= PropNumber(2.*a1,b1,1,1,0)
        Y0= xdy(g0,kWave)   
        Y1= xdy(g1,kWave)
        if IsSmaller(A0,B0,H0,A1,B1,H1) then
            call StepEH(A0,B0,H0,A1,B1,H1,Y1,jB)
            elseif IsSmaller(A1,B1,H1,A0,B0,H0) then
            call StepEH(A1,B1,H1,A0,B0,H0,Y0,jB)
            else
            Y0=Array(1.0,0.):Y1=Array(1.0,0.): jB=Array(0.0,0.0)
        end if
           
'        if A0<=A1 and B0<=B1 then
'        call StepEH(A0,B0,H0,A1,B1,H1,Y1,jB)
'        else
'        call StepEH(A1,B1,H1,A0,B0,H0,Y0,jB)
'        end if
        Se= Shunt(Y0,jB,Y1)
        case 5
        A0=guides.w(i,2)/ActiveMode
        B0=guides.w(i,3)
        H0=guides.w(i,4)
        x0=pi/A0/2.
        S=guides.c(i,1)
        A=guides.c(i,2)/ActiveMode
        B=guides.c(i,3)
        H=guides.c(i,4)
        A1=guides.w(i+1,2)/ActiveMode
        B1=guides.w(i+1,3)
        H1=guides.w(i+1,4)
        x1=pi/A1/2.
        call IrisEH(A0,B0,H0,A,B,H,S,A1,B1,H1,Yo)
g0= PropNumber(2.*a0,b0,1,1,0)
g1= PropNumber(2.*a1,b1,1,1,0)
Y0=Array(1.,0.):Y1=Array(1.,0.)
        Se= SY2(Y0,Yo,Y1)
        case else
        end select

'*--------ELEMENTS-------------------------------
        if i = Guides.Nw-1 then
        So=Se
        else
        ph=ax(guides.w(i+1,1),g1)     'ph=g1*w(i+1,1)
        So= Sconnect(Se,ph,So)
        end if
        next
        End Sub
        
'============================        
Public Sub CrossStruc(So)
        dim Se,Yo,jB
        dim g0,g1,Y0,Y1,ph
        dim pi,A0,B0,H0,x0,S,A,B,H,A1,B1,H1,x1
        dim i
        pi=3.141592653589793238
        for i=Guides.Nw-1 to 0 step -1
        select case guides.jc(i)
        
        case 2
        A0=0.5*guides.w(i,3)
        B0=2*guides.w(i,2)
        H0=-guides.w(i,2)
        x0=pi/A0/2.
        S=guides.c(i,1)
        A=0.5*guides.c(i,3)
        B=2*guides.c(i,2)
        H=-guides.c(i,2)
        A1=0.5*guides.w(i+1,3)
        B1=2*guides.w(i+1,2)
        H1=-guides.w(i+1,2)
        x1=pi/A1/2.
        call CavEH(A0,B0,H0,A,B,H,S,A1,B1,H1,Yo)
g0= PropNumber(2.*a0,b0,1,1,0)
g1= PropNumber(2.*a1,b1,1,1,0)
        Y0= xdy(g0,kWave)   
        Y1= xdy(g1,kWave)   
        Se= SY2(Y0,Yo,Y1)
         
        case 1
        A0=0.5*guides.w(i,3)
        B0=2*guides.w(i,2)
        H0=-guides.w(i,2)
        x0=pi/A0/2.
        A1=0.5*guides.w(i+1,3)
        B1=2*guides.w(i+1,2)
        H1=-guides.w(i+1,2)
        x1=pi/A1/2.
g0= PropNumber(2.*a0,b0,1,1,0)
g1= PropNumber(2.*a1,b1,1,1,0)
        Y0= xdy(g0,kWave)   
        Y1= xdy(g1,kWave)
        if IsSmaller(A0,B0,H0,A1,B1,H1) then
            call StepEH(A0,B0,H0,A1,B1,H1,Y1,jB)
            elseif IsSmaller(A1,B1,H1,A0,B0,H0) then
            call StepEH(A1,B1,H1,A0,B0,H0,Y0,jB)
            else
            Y0=Array(1.0,0.):Y1=Array(1.0,0.): jB=Array(0.0,0.0)
        end if
        Se= Shunt(Y0,jB,Y1)
        case 5
        A0=0.5*guides.w(i,3)
        B0=2*guides.w(i,2)
        H0=-guides.w(i,2)
        x0=pi/A0/2.
        S=guides.c(i,1)
        A=0.5*guides.c(i,3)
        B=2*guides.c(i,2)
        H=-guides.c(i,2)
        A1=0.5*guides.w(i+1,3)
        B1=2*guides.w(i+1,2)
        H1=-guides.w(i+1,2)
        x1=pi/A1/2.
        call IrisEH(A0,B0,H0,A,B,H,S,A1,B1,H1,Yo)
g0= PropNumber(2.*a0,b0,1,1,0)
g1= PropNumber(2.*a1,b1,1,1,0)
Y0=Array(1.,0.):Y1=Array(1.,0.)
        Se= SY2(Y0,Yo,Y1)
        case else
        end select

        if i = Guides.Nw-1 then
        So=Se
        else
        ph=ax(guides.w(i+1,1),g1)     'ph=g1*w(i+1,1)
        So= Sconnect(Se,ph,So)
        end if
        next
        End Sub
        
'=====================
Public Function NewStruc(Win,Cin)
        dim Se,Yo,jB
        dim g0,g1,Y0,Y1,ph
        dim pi,A0,B0,H0,x0,S,A,B,H,A1,B1,H1,x1
        dim i
      
        pi=3.141592653589793238
        for i=Guides.Nw-1 to 0 step -1
'*--------ELEMENTS-------------------------------
        select case guides.jc(i)
        
        case 2
        A0=Win(i,2)/ActiveMode
        B0=Win(i,3)
        H0=Win(i,4)
        x0=pi/A0/2.
        S=Cin(i,1)
        A=Cin(i,2)/ActiveMode
        B=Cin(i,3)
        H=Cin(i,4)
        A1=Win(i+1,2)/ActiveMode
        B1=Win(i+1,3)
        H1=Win(i+1,4)
        x1=pi/A1/2.
        call CavEH(A0,B0,H0,A,B,H,S,A1,B1,H1,Yo)
g0= PropNumber(2.*a0,b0,1,1,0)
g1= PropNumber(2.*a1,b1,1,1,0)
        Y0= xdy(g0,kWave)   
        Y1= xdy(g1,kWave)   
        Se= SY2(Y0,Yo,Y1)
        
         
        case 1
        A0=Win(i,2)/ActiveMode
        B0=Win(i,3)
        H0=Win(i,4)
        x0=pi/A0/2.
        A1=Win(i+1,2)/ActiveMode
        B1=Win(i+1,3)
        H1=Win(i+1,4)
        x1=pi/A1/2.
g0= PropNumber(2.*a0,b0,1,1,0)
g1= PropNumber(2.*a1,b1,1,1,0)
        Y0= xdy(g0,kWave)   
        Y1= xdy(g1,kWave)
        if IsSmaller(A0,B0,H0,A1,B1,H1) then
            call StepEH(A0,B0,H0,A1,B1,H1,Y1,jB)
            elseif IsSmaller(A1,B1,H1,A0,B0,H0) then
            call StepEH(A1,B1,H1,A0,B0,H0,Y0,jB)
            else
            Y0=Array(1.0,0.):Y1=Array(1.0,0.): jB=Array(0.0,0.0)
        end if
        Se= Shunt(Y0,jB,Y1)
        case 5
        A0=Win(i,2)/ActiveMode
        B0=Win(i,3)
        H0=Win(i,4)
        x0=pi/A0/2.
        S=Cin(i,1)
        A=Cin(i,2)/ActiveMode
        B=Cin(i,3)
        H=Cin(i,4)
        A1=Win(i+1,2)/ActiveMode
        B1=Win(i+1,3)
        H1=Win(i+1,4)
        x1=pi/A1/2.
        call IrisEH(A0,B0,H0,A,B,H,S,A1,B1,H1,Yo)
g0= PropNumber(2.*a0,b0,1,1,0)
g1= PropNumber(2.*a1,b1,1,1,0)
Y0=Array(1.,0.):Y1=Array(1.,0.)
        Se= SY2(Y0,Yo,Y1)
        case else
        end select

'*--------ELEMENTS-------------------------------
        if i = Guides.Nw-1 then
        NewStruc=Se
        else
        ph=ax(Win(i+1,1),g1)     'ph=g1*w(i+1,1)
        NewStruc= Sconnect(Se,ph,NewStruc)
        end if
        next
        End Function

Public Sub Element(iNumber,Se)
        dim Yo,jB
        dim g0,g1,Y0,Y1,ph
        dim pi,A0,B0,H0,x0,S,A,B,H,A1,B1,H1,x1
        dim i
i=iNumber        
        pi=3.141592653589793238
'*--------ELEMENTS-------------------------------
        select case guides.jc(i)
        
        case 2
        A0=guides.w(i,2)/ActiveMode
        B0=guides.w(i,3)
        H0=guides.w(i,4)
        x0=pi/A0/2.
        S=guides.c(i,1)
        A=guides.c(i,2)/ActiveMode
        B=guides.c(i,3)
        H=guides.c(i,4)
        A1=guides.w(i+1,2)/ActiveMode
        B1=guides.w(i+1,3)
        H1=guides.w(i+1,4)
        x1=pi/A1/2.
        call CavEH(A0,B0,H0,A,B,H,S,A1,B1,H1,Yo)
g0= PropNumber(2.*a0,b0,1,1,0)
g1= PropNumber(2.*a1,b1,1,1,0)
        Y0= xdy(g0,kWave)   
        Y1= xdy(g1,kWave)   
        Se= SY2(Y0,Yo,Y1)
        case 1
        A0=guides.w(i,2)/ActiveMode
        B0=guides.w(i,3)
        H0=guides.w(i,4)
        x0=pi/A0/2.
        A1=guides.w(i+1,2)/ActiveMode
        B1=guides.w(i+1,3)
        H1=guides.w(i+1,4)
        x1=pi/A1/2.
g0= PropNumber(2.*a0,b0,1,1,0)
g1= PropNumber(2.*a1,b1,1,1,0)
        Y0= xdy(g0,kWave)   
        Y1= xdy(g1,kWave)   
        if IsSmaller(A0,B0,H0,A1,B1,H1) then
            call StepEH(A0,B0,H0,A1,B1,H1,Y1,jB)
            elseif IsSmaller(A1,B1,H1,A0,B0,H0) then
            call StepEH(A1,B1,H1,A0,B0,H0,Y0,jB)
            else
            Y0=Array(1.0,0.):Y1=Array(1.0,0.): jB=Array(0.0,0.0)
        end if
        Se= Shunt(Y0,jB,Y1)
        case 5
        A0=guides.w(i,2)/ActiveMode
        B0=guides.w(i,3)
        H0=guides.w(i,4)
        x0=pi/A0/2.
        S=guides.c(i,1)
        A=guides.c(i,2)/ActiveMode
        B=guides.c(i,3)
        H=guides.c(i,4)
        A1=guides.w(i+1,2)/ActiveMode
        B1=guides.w(i+1,3)
        H1=guides.w(i+1,4)
        x1=pi/A1/2.
        call IrisEH(A0,B0,H0,A,B,H,S,A1,B1,H1,Yo)
g0= PropNumber(2.*a0,b0,1,1,0)
g1= PropNumber(2.*a1,b1,1,1,0)
Y0=Array(1.,0.):Y1=Array(1.,0.)
        Se= SY2(Y0,Yo,Y1)
        case else
        end select
        End Sub

private sub CavEH(A0,B0,H0,A,B,H,S,A1,B1,H1,Yo)
        dim Aem0(1,10,10), Aem1(1,10,10)
        dim o0,o1,Nx,Ny,Nm
        Nm=0
        if IsE(A0,A) and IsE(A1,A) then Nx=0 else Nx=ModesX
        if IsH(B0,H0,B,H) and IsH(B1,H1,B,H) then Nm=1:Ny=0 else Ny=ModesY
        call RecMain(Nm,Nx,Ny,A0,B0,H0-H,A,B,Aem0)
        call RecMain(Nm,Nx,Ny,A1,B1,H1-H,A,B,Aem1)
        o0=1.-B0*A0/B/A
        o1=1.-B1*A1/B/A
        call J1(Nm,Nx,Ny,o0,Aem0,A,B,S,o1,Aem1,Yo)
        End Sub
        
private Function IsE(A0,A1)
        if A1=A0 then IsE=true else IsE=false
End Function        
private Function IsH(B0,H0,B1,H1)
        if H1=H0 and H1+B1=H0+B0 then IsH=true else IsH=false
End Function        
        
        
        
private sub IrisEH(A0,B0,H0,A,B,H,S,A1,B1,H1,Yo)
        dim Aem0(1,10,10), Aem1(1,10,10),o0,o1
        dim Nm0,Nx0,Ny0,Nm1,Nx1,Ny1
        Nm0=0
        if IsE(A0,A) then Nx0=0 else Nx0=ModesX
        if IsH(B0,H0,B,H) then Nm0=1:Ny0=0 else Ny0=ModesY
        Nm1=0
        if IsE(A1,A) then Nx1=0 else Nx1=ModesX
        if IsH(B1,H1,B,H) then Nm1=1:Ny1=0 else Ny1=ModesY
        call RecMain(Nm0,Nx0,Ny0,A,B,H-H0,A0,B0,Aem0)
        call RecMain(Nm1,Nx1,Ny1,A,B,H-H1,A1,B1,Aem1)
        o0=1.-B0*A0/B/A
        o1=1.-B1*A1/B/A
        call IEH(Nm0,Nx0,Ny0,o0,A0,B0,Aem0,A,B,S,Nm1,Nx1,Ny1,o1,A1,B1,Aem1,Yo)
        End Sub

private sub StepEH(A,B,H,Ao,Bo,Ho,Y,jB)
        dim Aem(1,10,10),o,Nx,Ny,Nm
        Nm=0
        if IsE(Ao,A) then Nx=0 else Nx=ModesX
        if IsH(Bo,Ho,B,H) then Nm=1:Ny=0 else Ny=ModesY
        o=1.-B*A/Bo/Ao
        call RecMain(Nm,Nx,Ny,A,B,H-Ho,Ao,Bo,Aem)
        call JEH(Nm,Nx,Ny,o,Aem,Ao,Bo,Y,jB)
        End Sub
        

private sub IEH(Nm0,Nx0,Ny0,o0,A0,B0,E0,A,B,S,Nm1,Nx1,Ny1,o1,A1,B1,E1,Y)
' IRIS OF LARGER WGs
' normalized to y0=1, y1=1
        dim ph,Y0,jB0,Y1,jB1
        dim gs,ys
        dim c,jc
        dim Ys00,Ys01,Ys11
        dim pi
        pi=3.141592653589793238
gs= PropNumber(2.*a,b,1,1,0)
        ys= xdy(gs,kWave): ph=Array(gs(0)*S,gs(1)*S)
        call JEH(Nm0,Nx0,Ny0,o0,E0,A0,B0,Y0,jB0)
        call JEH(Nm1,Nx1,Ny1,o1,E1,A1,B1,Y1,jB1)
        c= xoy(ys,ctanC(ph)):jc=Array(-c(1),c(0))
        Ys00=Array(jB0(0)-jc(0),jB0(1)-jc(1))
        Ys11=Array(jB1(0)-jc(0),jB1(1)-jc(1))
        c= xdy(ys,csin(ph)): Ys01=Array(-c(1),c(0))
        Y= Ynorm(Y0,Ys00,Ys01,Ys11,Y1)
        End Sub


        private sub JEH(Nm,Nx,Ny,o,Aem,A,B,Y,jB)
        dim ge,gm,ze,zm,ce,cm,ye,ym
        dim n,m,nn,mm,pi

        pi=3.141592653589793238
        jB=Array(0.0,0.0)
        for n=0 to Nx
        nn=2*n+1
        for m=0 to Ny
        mm=(1+guides.iSym)*m
ge= PropNumber(2.*a,b,0,nn,mm)
gm= PropNumber(2.*a,b,1,nn,mm)
        ze= xdy(ge,kWave)    
        zm= xdy(kWave,gm)    
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
        
        End Sub



         private sub RecMain(Nm,Nx,Ny,A,B,H,Ao,Bo,Aem)
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
        mm=m*(1+guides.iSym)
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



        private sub J1(Nm,Nx,Ny,o0,A0,A,B,S,o1,A1,Y)
' CAVITY BETWEEN JUNCTIONS
' d0 - distance between junctions
' Cavity Model Ae0,Am0 - first junction integrals
' Cavity Model Ae1,Am1 - second junction integrals
' A=Real_A/2 B=Real_B S=Real_SPACE_Between_Junctions
'VALUES OF Y - REAL, ACTUAL Y=j*Y
        dim ge,gm
        dim ze,zm,ce0,ce1,cm0,cm1
        dim kw,beta
        dim C0,C1,C
        dim n,m,nn,mm,pi
        pi=3.141592653589793238
        C0=Array(0.,0.): C=C0: C1=C0
        for n=0 to Nx
        nn=2*n+1
        for m=0 to Ny
        mm=(1+guides.iSym)*m
ge= PropNumber(2.*a,b,0,nn,mm)
gm= PropNumber(2.*a,b,1,nn,mm)
        ze= xdy(ge,kWave)    
        zm= xdy(kWave,gm)    
call AppertureCoupling(ge,ze,s,o0+o1,ce0,ce1)        
call AppertureCoupling(gm,zm,s,o0+o1,cm0,cm1)        
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

private sub AppertureCoupling(g,z,s,omega,c0,c1)
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

        private function PropNumber(a,b,imode,n,m)
'       k is wavenumber in medium, rm is Rm/Zo, tg is tangent
'        dim complex g2,j
        dim pi,xn,xm,snm,g
        dim g2(1)
        pi=3.141592653589793238
        xn=pi/a*n
        xm=pi/b*m
        g2(0)=xn*xn+xm*xm-kSpace*kSpace*(1-guides.tg*guides.tg/4):g2(1)=kSpace*kSpace*guides.tg
        if rm > 0 then
        snm=wnm(a,b,imode,n,m)
        g2(0)=g2(0)+rm*snm
        g2(1)=g2(1)+rm*snm
        end if
        g=csqrt(g2)
        PropNumber= Array(g(1),-g(0))
        End Function



        private function wnm(a,b,imode,n,m)
        dim k2,n2,m2,a2,a3,b2,b3,x2,snm,e2,pi2
        pi2=9.869604401089360
        if m=0 then e2=0.5 else e2=1.0 end if
        k2=kSpace*kSpace
        n2=n*n
        m2=m*m
        a2=a*a
        a3=a2*a
        b2=b*b
        b3=b2*b
        x2=pi2*(n2/a2+m2/b2)
        if imode =0 then
        snm=(n2*b3+m2*a3)/(n2*b2*a+m2*a3)
        else
        snm=(1.+b/a)*x2/k2+b/a*(e2-x2/k2)*(n2*a*b+m2*a2)/(n2*b2+m2*a2)
        end if
        wnm=4.*kMedium*snm/b
        end function


        function kf(f)
        if iUnit=0 then
        kf=0.0209579*f
        else
        kf=0.5323312*f
        end if
        end function

        function Rs()
' Rs is normalized to Zo=120pi  sigma=sigm*10^7*mhos/m
        dim wi
        if guides.sigm < 0.0001 then
        Rs=0
        else
        wi=sqr(kSpace/guides.sigm)
        if iUnit = 0 then
        Rs=3.643e-4*wi
        else
        Rs=7.229e-5*wi
        end if
        end if
        Rs=1.5*RS
        end function

END CLASS

        Function Scatter(iPort,S)
        dim iVar
        iVar=2*iPort
        Scatter=Array(S(iVar),S(iVar+1))
        End Function

        Function Smatrix(s00,s01,s10,s11)
        Smatrix=Array(s00(0),s00(1),s01(0),s01(1),s10(0),s10(1),s11(0),s11(1))
        End Function


Function SLine(ind,So)
dim R,lnR,RdB,wR, LN,SP,i,j,wRdg,cW
cW=57.29577951308232088
LN=""
SP="   "
for i=0 to 3
R=Scatter(i,So)
if cabs(R)<1.e-20 then R=Array(1.e-20, 0.0)
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

'########################### OTHER
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

sub InfoBox(x)
dim iSo
iSo=msgbox(x,1,"info")
if iSo=2 then wscript.quit
End Sub

