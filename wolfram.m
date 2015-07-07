BeginPackage["Funstore`"]
(* ::Package:: *)

"copyright mLabs Research Inc";

digitalRead::useage = "digitalRead[pin] returns the digital value of pin";
digitalWrite::useage = "digitalWrite[pin,value] writes the value on selected pin. value can only have two states 0 or 1";
analogRead::useage = "analogRead[pin] returns the digital value of pin";
analogWrite::useage = "analogWrite[pin,value] writes the value on selected pin. value can only have states between 0-1023";
pinMode::useage = "To be written";
setRGB::useage = "To be written";
firstText::useage = "To be written";
secondText::useage = "To be written";
clearDisplay::useage = "To be written";
Temperature::useage = "To be written";
Dweet::useage = "To be written";
Datadrop::useage = "To be written";

Begin["`Private`"]

debug =0;
"rev = GPIO.RPI_REVISION";
If[rev==2 || rev ==3, bus = smbus.SMBus(1), bus = smbus.SMBus(0)];
address = 4;
dRead =1;
dWrite =2;
aRead =3;
aWrite =4;
pMode =5;
uRead =7;
version =8;
acc_xyz =20;
rtc_getTime =30;
dht_temp =40;
ledBarInit =50;
ledBarOrient =51;
ledBarLevel =52;
ledBarSetOne =53;
ledBarToggleOne =54;
ledBarSet =55;
ledBarGet =56;
fourDigitInit =70;
fourDigitBrightness =71;
fourDigitValue =72;
fourDigitValueZeros =73;
fourDigitIndividualDigit =74;
fourDigitIndividualLeds =75;
fourDigitScore =76;
fourDigitAnalogRead =77;
fourDigitAllOn =78;
fourDigitAllOff =79;
storeColor =90;
unused = 0;
backlight=98
character=62
mode1=0
mode2=1
pwm0=2
pwm1=3
pwm2=4
ledout=8
display=128
letters=64


"function definitions";

toHexString[n_Integer] := Module[{n0=n, str = StringJoin[ToString /@(IntegerDigits[n, 16] /. Thread[Range[10, 15] -> CharacterRange["A", "F"]])]},
  str = If[Mod[Length[str], 2]==1 , StringJoin[{"0x",str}], StringJoin[{"0x0",str}]]; str]

ShellI2CCmd[address_Integer, block_List] := StringJoin[{"/usr/share/funstore/funstore/scripts/write <<EOF\n ",toHexString[First[block]],"\n", toHexString[Part[block,2]],"\n",toHexString[Part[block,3]],"\n", toHexString[Part[block,4]], "\n EOF "}]
WriteI2cBlock[address_Integer, block_List] := Module[{}, val=RunThrough[ShellI2CCmd[address, block],""]; val];


ShellI2CCmdRead[value_Integer, pin_Integer] := Import[StringJoin["!/usr/share/funstore/funstore/scripts/read <<EOF\n ",toHexString[value],"\n", toHexString[pin],"\n EOF"],"String"];
ReadI2cBlock[value_Integer, pin_Integer] := Module[{}, c = ShellI2CCmdRead[value, pin]; ToExpression[c]];


digitalRead[pin_]:=(WriteI2cBlock[address, {dRead, pin, unused, unused}]; Pause[0.1]; ReadI2cBlock[dRead, pin])
analogRead[pin_]:= (WriteI2cBlock[address, {aRead, pin, unused, unused}]; Pause[0.1]; ReadI2cBlock[aRead, pin])

ShellI2CLCD[block_List] := Import[StringJoin[{"!i2cset -y 1 ", toHexString[First[block]]," ",toHexString[Part[block,2]], " ", toHexString[Part[block,3]], " "}],"String"];
WriteI2cLCD[block_List] := Module[{}, c = ShellI2CLCD[block]; ToExpression[c]];


digitalWrite[pin_, value_]:= (WriteI2cBlock[address, {dWrite, pin, value, unused}];)
analogWrite[pin_, value_]:= (WriteI2cBlock[address, {aWrite, pin, value, unused}];)

pinMode[pin_, mode_]:= (If[mode == "OUTPUT", WriteI2cBlock[address, List[pMode, pin, 1, unused]], If[mode == "INPUT", WriteI2cBlock[address, List[pMode, pin, 0, unused]], 1], 1])


ShellI2CLCD[block_List] := StringJoin[{"i2cset -y 1 ", toHexString[First[block]]," ",toHexString[Part[block,2]], " ", toHexString[Part[block,3]], " "}]
WriteI2cLCD[block_List] := Module[{}, c = ShellI2CLCD[block]; Run[c];]

setRGB[r_,g_,b_]:=(WriteI2cLCD[{backlight,mode1,0}];
                   WriteI2cLCD[{backlight,mode2,0}];
                   WriteI2cLCD[{backlight,pwm0,b}];
                   WriteI2cLCD[{backlight,pwm1,g}];
                   WriteI2cLCD[{backlight,pwm2,r}];
                   WriteI2cLCD[{backlight,ledout,170}];)
textCommand[cmd_]:=WriteI2cLCD[{character,display,cmd}]
firstText[text_]:=(WriteI2cLCD[{character,display,1}];
                WriteI2cLCD[{character,display,15}];
                WriteI2cLCD[{character,display,56}];
                Do[Print[WriteI2cLCD[{character,letters, k}]], {k, ToCharacterCode[text]}]; )
nextLine[]:=textCommand[192];
secondText[text_]:=(textCommand[192];
                    Do[Print[WriteI2cLCD[{character,letters, k}]], {k, ToCharacterCode[text]}]; )
clearDisplay[]:=WriteI2cLCD[{character,display,1}];

Temperature[pin_]:=(a=analogRead[pin];resistance = (1023 - a) * 10000 / a; 1/(Log[resistance/10000]/4250+1/298.15)-273.15)

Dweet[type_,namevar_, var_]:=(  data=Import["/usr/share/funstore/funstore/api/InfoAPI.json"];
                                thingname=data[[3]][[1]][[2]][[2]][[2]][[2]];
                                access=data[[3]][[3]][[2]];
                                If[access=="true",
                                If[type=="digital",
                                url=StringJoin[{"https://dweet.io/dweet/for/"},thingname,{"?"},namevar,{"="},ToString[digitalRead[ToExpression[namevar]]]];, 
                                url=StringJoin[{"https://dweet.io/dweet/for/"},thingname,{"?"},namevar,{"="},ToString[analogRead[ToExpression[namevar]]]];]
                                Print[url];
                                URLExecute[url];]
                                )
Datadrop[type_,namevar_, var_]:=( data=Import["/usr/share/funstore/funstore/api/InfoAPI.json"];
                            access=data[[2]][[3]][[2]];
                            datadropid=data[[2]][[1]][[2]][[1]][[2]][[2]];
                                If[access=="true",
                                If[type=="digital",
                                url=StringJoin[{"https://datadrop.wolframcloud.com/api/v1.0/Add?bin="},datadropid,{"&"},namevar,{"="},ToString[digitalRead[ToExpression[namevar]]]];,
                                url=StringJoin[{"https://datadrop.wolframcloud.com/api/v1.0/Add?bin="},datadropid,{"&"},namevar,{"="},ToString[analogRead[ToExpression[namevar]]]];]
                                URLFetch[url,"Method"->"POST"]];
                                )
Print["Loaded Successfully"]
End[]
EndPackage[]
