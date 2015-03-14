
Program IPEG4 (input, output);

Const   rinf_c           = 1.7E+25;
        release_number   = 1.0;
        max_no_of_proc   = 20;
        max_no_of_values = 100;
        test_code        = 0;

Type    int_array     = array [1..50] of integer;
        value_array   = array[1..max_no_of_values] of real;
        table_type    = array [1..24,1..max_no_of_values] of real;
        real_5_array  = array [1..5] of real;
        real_45_array = array [1..45] of real;
        real_array2   = array [1..17,1..5] of real;
        vocab         = string[10];
        q_length      = string[80];
        strtype       = string [12];
        referent_type = string[9];
        num_type      = (real_number,integer_number);
        rarray        = array [1..10] of real;
        string_type   = string[80];
        expected_set_type = set of char;

        directory_type = Record
                         company_rec               : integer;
                         size_index                : integer;
                         reference                 : q_length;
                         lines_of_data             : integer;
                         End;

        company_type =   Record
                         process_rec               : integer;
                         company_descriptive_name  : string[80];
                         process_list              : array[1..max_no_of_proc] of strtype;
                         save1_5                   : array[1..5] of real;
                         i_quan, p_quan            : real;
                         industry_units            : string[80];
                         product_units             : string[80];
                         save6_31                  : array[6..31] of real;
                         tb                        : real;
                         save32_43                 : array[32..43] of real;
                         number_of_processes       : integer;
                         End;

        process_type =   Record
                         process_descriptive_name  : string[70];
                         save5                     : array[1..5] of real;
                         save13_14                 : array[13..14] of real;
                         save27                    : real;
                         save22_26                 : array[22..26] of real;
                         p_quans                   : real;
                         save6_7                   : array[6..7] of real;
                         process_product_units     : referent_type;
                         End;

        ipeg_bin_type =  Record
                         directory                 : directory_type;
                         company                   : company_type
                         End;



Var    user_text                                   : vocab;
       ivnames,ivunits                             : array [1..43] of vocab;
       vnames,vunits                               : array [1..23] of vocab;
       t_matrix                                    : real_array2;
       table                                       : table_type;
       values                                      : value_array;

e_vector,v_vector,c,opr,otx,bval,ins,in_t,
rpl,itc,eqr,wcap,fac,pvelp,pvecc,pvitc,pvewc,
pvdep,pvsuci,ihs                                   : real_5_array;

vmin,vmax,vdef,working,saved                       : real_45_array;

normal_input_unit,normal_output_unit,process_flag,
old_size,size,where,
n_variables,n_values,ivindex, i,j,k1               : integer;

old_company_referent,referent                      : q_length;

strg                                               : q_length;
report_title                                       : q_length;

ritce,k,capk,gcrf,b1,vd,vs,tsu,tec,phi,ye,
wkg_p_quan,wkg_i_quan,q,tcc,report_year,
old_report_year,dfe,pvsucd,rdum,f1,tb,y,i_quans,
p_quans,price,deflator,varivd,cost                 : real;

user_response                                      : char;

more_cases, more_sensitivities, first_time,
valid_entry,samis_file                             : boolean;


dir                                                : file of directory_type;
com                                                : file of company_type;
pro                                                : file of process_type;
ipe                                                : text;
ipg                                                : file of ipeg_bin_type;

dir_data                                           : directory_type;
comp_dat,saved_com                                 : company_type;
proc_dat                                           : process_type;
ipg_data                                           : ipeg_bin_type;
ipeg_input                                         : q_length;

input_str                                          : strtype;
tempa                                              : rarray;

proc_start,no_proc,idum                            : integer;
prevx1,prevx2,prevy1,prevy2                        : integer;
input_buffer                                       : q_length;
buffer_cursor                                      : integer;

{$I PCIPEG.INC}

Procedure Read_report_year;

Var ok : boolean;

Begin
ok := false;
repeat
 Ask_Question(false,'Enter report year: ');
 Get_number(0,real_number,report_year,idum);
 if (report_year < 1980) or (report_year > 1990) then
  Begin
  Clear_Message_Line;
  Writeln('The report year ',report_year:4:0,' is outside usual limits');
  Ask_Question(false,'Do you wish to retain it anyway (Y/N) ? N');
  user_response := Read_Keyboard(1,['n','N','y','Y',^M]);
  If user_response in ['y','Y'] then ok := true
  End
 Else ok := true
Until ok;
End;

Procedure Calculate_the_discount_rate(var dis_rate:real);
Begin
dis_rate := (working[20]/working[18]) + (working[19]*(1 + working[17])*
            ((working[18] - 1) / working[18]))
End;

Procedure Print_report_heading;

Var proc_name : strtype;

Begin
Writeln; writeln;
Writeln(' ':20,report_title); writeln;
Writeln(' ':15,'COMPANY: ',dir_data.reference,' ':30,'SIZE INDEX: ',dir_data.size_index:3);
If process_flag = 1 then writeln(' ':15,'PROCESS: ',comp_dat.process_list[where]:11);
Writeln
End;

Procedure Reinitialize_working_vars;

Begin
With comp_dat do
 Begin
 For i := 1 to 5 do working [i] := save1_5 [i];
 For i := 32 to 43 do working [i] := save32_43 [i];
 For i := 6 to 31 do working[i] := save6_31 [i];
 End;
wkg_p_quan := comp_dat.p_quan;
wkg_i_quan := comp_dat.i_quan;
End;

Procedure Display_indep_var_names_and_ID;

Begin
Clear_The_Screen;Lowvideo;
Writeln ('      ** Independent Variable Information **');
Normvideo;
Writeln (' |    ID      Name    |    ID      Name    |    ID       Name    | ');
Writeln (' |   ----   --------  |   ----   --------  |   ----    --------  | ');
Writeln (' |     1     Eqpt     |    16     W        |    31      L        | ');
Writeln (' |     2     Sqft     |    17     Tau      |    32      A        | ');
Writeln (' |     3     Dlab     |    18     Lambda   |    33      Uf       | ');
Writeln (' |     4     Mats     |    19     Ir       |    34      Lf       | ');
Writeln (' |     5     Util     |    20     R        |    35      T        | ');
Writeln (' |     6     El       |    21     P1       |    36      M        | ');
Writeln (' |     7     Eitcr    |    22     P2       |    37      B        | ');
Writeln (' |     8     Fl       |    23     P3       |    38      Tlf      | ');
Writeln (' |     9     Beta     |    24     D1       |    39      Xec      | ');
Writeln (' |    10     X        |    25     D2       |    40      Xfc      | ');
Writeln (' |    11     Nu       |    26     Gf       |    41      Xopr     | ');
Writeln (' |    12     Z        |    27     Ge       |    42      N        | ');
Writeln (' |    13     Rlab     |    28     Ts       |    43      Crt      | ');
Writeln (' |    14     Rutil    |    29     Tm       |    44      I.Quan   | ');
Writeln (' |    15     G        |    30     Tc       |    45      P.Quan   | ');
Writeln (' |                    |                    |    46      **All**  | ');
Writeln;
Set_Window(1,20,80,24,false);
End;

Procedure Display_dep_var_names_and_id;

Begin
Clear_The_Screen;Lowvideo;
Writeln ('      ** Dependent Variable Information **');
Normvideo;
Writeln ('      ID      Name    |    ID      Name    |    ID       Name    | ');
Writeln ('     ----   --------  |   ----   --------  |   ----    --------  | ');
Writeln ('       1     C(1)     |     9     Otx      |    17      Fac      | ');
Writeln ('       2     C(2)     |    10     Bval     |    18      Pvelp    | ');
Writeln ('       3     C(3)     |    11     Ins      |    19      Pvecc    | ');
Writeln ('       4     C(4)     |    12     Int      |    20      Pvitc    | ');
Writeln ('       5     C(5)     |    13     Rpl      |    21      Pvewc    | ');
Writeln ('       6     Price1   |    14     Itc      |    22      Pvdep    | ');
Writeln ('       7     Price2   |    15     Eqr      |    23      Pvsuci   | ');
Writeln ('       8     Opr      |    16     Wcap     |    24      **All**  | ');
Writeln;
End;

Procedure Load_process_data (displacement : integer);

Var i : integer;

Begin
reset(pro);
seek (pro,comp_dat.process_rec + displacement - 1);
read (pro,proc_dat);

With comp_dat do
   Begin
   For i := 1 to 5 do save1_5 [i] := proc_dat.save5[i];
   save6_31 [13] := proc_dat.save13_14 [13];
   save6_31 [14] := proc_dat.save13_14 [14];
   save6_31 [27] := proc_dat.save27;
   For i := 22 to 26 do save6_31 [i] := proc_dat.save22_26 [i];
   p_quan := proc_dat.p_quans;
   save6_31 [6] := proc_dat.save6_7 [6];
   save6_31 [7] := proc_dat.save6_7 [7];
   product_units := proc_dat.process_product_units
   End;
close(pro);
End;


Procedure Search_process_list (strg: strtype; Var found : boolean; Var where : integer);

Begin
where := 1;
found := false;
With comp_dat do
   Begin
   While (not found) and (where <= number_of_processes) do
      if (process_list [where] = strg) then found := true
      else where := where + 1;
   End
End;


Procedure Display_the_company_data;

Var j, line_number : integer;

Begin
Clear_The_Screen;
line_number := 2;
With comp_dat do
   Begin
   Writeln ('Company: ',dir_data.reference,'  ',company_descriptive_name);
   Writeln ('Process List = ');
   For j := 1 to number_of_processes do
      Begin
      line_number := line_number + 1;
      Writeln (' ':17,process_list [j]);
      if line_number > 19 then
         Begin
         Wait_for_keypress(2);
         line_number := 1;
         End
      End;
   line_number := line_number + 5;
   if line_number > 20 then Wait_for_keypress(2);
   Writeln;
   Writeln ('I.Quan = ':14,i_quan:15:2,'  ',industry_units);
   Writeln ('P.Quan = ':14,p_quan:15:2,'  ',product_units);
   Writeln ('Tb = ':14,tb:15:2);
   Writeln;
   Wait_for_keypress(2); Clear_The_Screen;
   writeln ('       Names',' ':9,'Values',' ':4,'Units');
   writeln ('       -----',' ':9,'------',' ':4,'-----');
   writeln;
   Set_Window(1,3,80,24,false);
   For j := 1 to 5 do Writeln (ivnames [j]:11,' = ',save1_5 [j]:15:2,'  ',ivunits [j]);
   For j := 6 to 31 do
      Begin
      Writeln (ivnames [j]:11,' = ',save6_31 [j]:15:2,'  ',ivunits [j]);
      if j = 21 then Wait_for_keypress(2);
      End;
   For j := 32 to 43 do Writeln (ivnames [j]:11,' = ',save32_43 [j]:15:2,'  ',ivunits [j]);
   Wait_for_keypress(3); Set_Window(1,1,80,24,false); Clear_The_Screen;
   End
End;


Procedure Display_the_process_data;

Var j : integer;

Begin
Clear_The_Screen;
With proc_dat do
   Begin
   Writeln ('Process: ',comp_dat.process_list [where],'  ',process_descriptive_name);
   Writeln;
   For j := 1 to 5 do Writeln (ivnames [j]:11,' = ',save5 [j]:15:2,'  ',ivunits [j]);
   For j := 6 to 7 do Writeln (ivnames [j]:11,' = ',save6_7 [j]:15:2,'  ',ivunits [j]);
   For j := 13 to 14 do Writeln (ivnames [j]:11,' = ',save13_14 [j]:15:2,'  ',ivunits [j]);
   Writeln (ivnames [27]:11,' = ',save27:15:2,'  ',ivunits[27]);
   For j := 22 to 26 do Writeln (ivnames [j]:11,' = ',save22_26 [j]:15:2,'  ',ivunits [j]);
   Writeln ('p.quan = ':14,p_quans:15:2,' ',process_product_units);
   Writeln;
   End;
Wait_for_keypress(4); Clear_The_Screen;
End;


Procedure Show_indep_vars;

Var display_list, repeated_num, illegal_num    : array [1..46] of integer;
    i, j, k, n, l, number,line_number, result  : integer;
    repeated,eoline                            : boolean;
    dummy                                      : real;

Begin
Display_indep_var_names_and_ID;
Ask_Question(true,'Input variable ID numbers you wish to display (1 - 46): ');
Load_Input_Buffer;
i := 0; k := 0; l := 0;
eoline := false;
While (not eoline) do
   Begin
   repeated := false;
   Get_numeric_str_From_Buffer(5,eoline);
   Val(input_str,number,result);
   if number in [1..46] then
      Begin
      For j := 1 to i do if number = display_list[j] then repeated := true;
      if not repeated then
         Begin
         i := i + 1;
         display_list[i] := number;
         End
      else
         Begin
         k := k + 1;
         repeated_num[k] := number;
         End
      End
   else
      Begin
      l := l + 1;
      illegal_num[l] := number;
      End;
   End;
Set_Window(1,1,80,24,false);
Clear_The_Screen;
Gotoxy(1,20);
For n := 1 to k do writeln('The ID number ',repeated_num[n]:3,' is repeated and therefore ignored');
For n := 1 to l do writeln('The ID number ',illegal_num[n]:3,' is illegal and therefore ignored');
line_number := k + l + 5;
If display_list[1] = 46 then Display_the_company_data
Else
Begin
writeln('***** CURRENT VALUE OF REQUESTED VARIABLES *****');
writeln;
with comp_dat do
   Begin
   Writeln('      NAME         VALUE    UNITS');
   Writeln('      ----         -----    -----');
   Writeln;
   for j := 1 to i do
      Begin
      line_number := line_number + 1;
      if line_number > 22 then Wait_for_keypress(6);
      k := display_list[j];
      if k in [1..5] then writeln (ivnames[k]:11,save1_5[k]:15:2,' ',ivunits[k])
      else if k in [6..31] then writeln (ivnames[k]:11,save6_31[k]:15:2,' ',ivunits[k])
      else if k in [32..43] then writeln (ivnames[k]:11,save32_43[k]:15:2,' ',ivunits[k])
      else if k = 44 then writeln ('I.Quan':11,i_quan:15:2,' ',industry_units)
      else if k = 45 then writeln ('P.Quan':11,p_quan:15:2,' ',product_units)
      else if k = 46 then Display_the_company_data
      End;
   Writeln
   End
   End
End;

{$I PCIPEG.OVR}

Procedure Initialize_the_IPEG_program;

Var
    i,k      : integer;
Begin
i := 1;
report_title := 'THE IPEG RESULTS FOLLOW:';
process_flag:=0;
samis_file := true;
n_variables := 43;
old_size := 0;
old_company_referent := ' ';
old_report_year := 0;
Clrscr;
Set_Window(25,11,55,14,false);
Textcolor(white); Textbackground(blue);
Writeln;
Writeln('WELCOME TO PC-IPEG RELEASE ',release_number:2:1);
Writeln;
Delay(3000);
Set_Window(1,1,80,24,false);
Textcolor(yellow); Textbackground(black);
Writeln ('_________________  I P E G   P R O G R A M  (release ',release_number:2:1,')  ___________________');
writeln; writeln;
Initialize_variable_names_min_max_and_default_values;
Display_Messages;
Writeln;
writeln ('Please report any program bugs to  Sil Zendejas at (818) 577-6846');
writeln;
Writeln('If you abort the program and wish to restart type "PCIPEG"');
Writeln;
writeln ('Hit "Shift-Prtsc" at any time during the run to dump the screen');
textcolor(white);
write ('HIT ANY KEY TO CONTINUE ');
   Repeat
   until keypressed;
textcolor(yellow);
End;


Procedure Execute_Command;

Var
   ok,ok2 : boolean;
Begin
   Clear_The_Screen;
   Case user_response of
      'B':
   Begin
   Repeat
   samis_file := false;
   Ask_Question(false,'Enter IPEG file name: ');
   Get_String(20,ipeg_input);
   assign(ipg,ipeg_input);
   if File_exists(ipeg_input) then
     Begin
     ok := true;
     reset(ipg);
     read(ipg,ipg_data);
     dir_data := ipg_data.directory;
     Read_report_year;
     comp_dat := ipg_data.company;
     Reinitialize_working_vars
     End
   else
     Begin
     Clear_Message_Line;
     writeln('FILE ',ipeg_input,' DOES NOT EXIST');
     Ask_Question(false,'Do you wish to create one (y/n)? N');
     user_response := Read_Keyboard(21,['n','N','y','Y',^M]);
     If user_response in ['Y','y'] then
        Begin
        Create_an_ipeg_input_file;
        ok := true;
        End
     else ok := false
     End
   Until ok;
   End;
      'A':
   Begin
     Repeat
     Ask_Question(false,'Enter the IPEG input file name: ');
     Get_String(22,ipeg_input);
     assign(ipe,ipeg_input);
     ok2 := File_exists (ipeg_input);
     if not ok2 then
        Begin
        Clear_Message_Line;
        writeln ('FILE ',ipeg_input,' DOES NOT EXIST');
        End;
     Until ok2;
     Reorganize_IPEG_input_file;
   End;
      'I': Begin
           Gotoxy(wherex + 3,wherey);
           Writeln('THIS FUNCTION IS UNVAILABLE');
           End;
      'E': Writeln('IPEG TERMINATED NORMALLY');
   End; { of Case }
   Clear_The_Screen;
End;


Procedure Calculate_pvdep;

Var s1,s2,s3,c,y4,y5,j : real;


Begin
y5:=0.0; y4:= 0.0;
s1 := (1.0 + working[15]) / (1.0 + k);
s2 := 2.0 / working[6];
s3 := 2.0 / working[8];
c := power (s1,working[6]);
j := 0;
while (j <= (working[6] - 1)) do
 Begin
 y4 := y4 + (1 - s2*j)*power(s1,j);
 j := j + 1;
 End;
j := 0;
While (j <= f1) do
 Begin
 y5 := y5 + (1 - s3*j)*power(s1,j);
 j := j + 1;
 End;
pvdep [1] := ((working[9] + k) * y4 * (1  + working[39]))/(2*(1 - c));
pvdep [2] := 0.5*(working[9] + k)*b1*y5*(1 + working[40])
End;


Procedure calculate_pvecc;

Var j,yf,dff,con_stant,d,s1,s2,y2,y3 : real;


Begin
y2 := 0; y3 := 0;
yf:= working[29] - tcc;
s1:= 1 + k;
s2:= 1 + working[15];
dff:= power(s2,-yf);
con_stant:= power(s1,yf);
d:= power(s1,ye);
j := tec + 1;
while (j <= (working[29] - 1 + 0.0000001)) do
 Begin
 y2 := y2 + power(s1,(working[29] - j))*power(s2,(j - tec));
 j := j + 1;
 End;
j := tcc + 1;
while (j <= (working[29] - 1 + 0.0000001)) do
 Begin
 y3 := y3 + power(s1,(working[29] - j))*power(s2,(j - tcc));
 j := j + 1;
 End;
 pvecc[1]:= (1 + working[39])*dfe*((d - 1) + working[11]*d + (working[11] + working[9])*y2);
 pvecc[2]:= (1 + working[40])*dff*b1*((con_stant - 1) + working[11] * con_stant
                   + y3 * (working[11] + working[9]));
End; {calculate_pvecc}

Procedure calculate_pvewc;

Var	s1,s2,df,y6,y7,h,j : real;

Begin
 y6 := 0; y7 := 0;
 s1:= 1 + k;
 s2:= 1 + working[15];
 df:= power(s2,(-working[28]));
 j := 1;
 while (j <= (working[28] - 1)) do
  Begin
  y6 := y6 + power(s2,(j-1))*(power(s1,(working[28] - j)) - 1);
  j := j + 1;
  End;
 j := 1;
 While (j <= working[28]) do
  Begin
  y7 := y7 + (working[11]*power(s2,j) + working[9]*power(s2,(j - 1))*
  power(s1,(working[28] - j)));
  j := j + 1;
  End;
 h:= (1 + working[11])*power(s1,working[28]) - 1 + working[15]*y6 + y7;
 pvewc[4] := h*df*working[16]*(1 + working[41]);
 pvewc[5]:= pvewc[4];
 pvewc[2]:= pvewc[4]*vs;
 pvewc[3]:= pvewc[4]*vd
End; {calculate pvewc}

Procedure calculate_intermediate_variables;

Var
s1,s2,s3,s4,s5,c_,d,h1: real;
                   i : integer;

Begin
For i := 1 to 5 do
   Begin
   opr [i] := 0;    otx [i] := 0;
   ins [i] := 0;    in_t [i] := 0;
   rpl [i] := 0;    bval [i] := 0;
   eqr [i] := 0;    itc [i] := 0;
   pvelp [i] := 0;  pvecc [i] := 0;
   pvitc [i] := 0;  pvdep [i] := 0;
   pvewc [i] := 0;  pvsuci [i] := 0;
   fac [i] := 0;
   End;

s1:= 1 + working[41];
s2:= 1 + working[39];
s3:= 1 - working[17];
opr[2]:= s1*vs;
opr[3]:= s1*vd;
opr[4]:= s1;
opr[5]:= s1;
fac[2]:= b1*(1 + working[40]);
For i:= 1 to 5 do
 wcap[i]:= working[16]*opr[i];
 rpl[1]:= s2/working[6];
 rpl[2]:= fac[2]/working[8];
 otx[1]:= 0.5*working[9]*s2;
 otx[2]:= working[9]*(0.5*fac[2] + wcap[2]);
 otx[3]:= working[9]*wcap[3];
 otx[4]:= working[9]*wcap[4];
 otx[5]:= otx[4];
    bval[1]:= 0.5*s2;
 bval[2]:= 0.5*fac[2];
 ins[1]:= working[11]*s2;
 ins[2]:= working[11]*(fac[2] + wcap[2]);
 ins[3]:= working[11]*wcap[3];
 ins[4]:= working[11]*wcap[4];
 ins[5]:= ins[4];
 c_ := (working[18] - 1) * working[19] / working[18];
 in_t[1]:= c_ * bval[1];
 in_t[2]:= c_ * (bval[2] + wcap[2]);
 in_t[3]:= c_ * wcap[3];
 in_t[4]:= c_ * wcap[4];
 in_t[5]:= in_t[4];
 d:= working[20]/working[18];
 eqr[1]:= d*bval[1];
 eqr[2]:= d*(bval[2] + wcap[2]);
 eqr[3]:= d*wcap[3];
 eqr[4]:= d*wcap[4];
 eqr[5]:= eqr[4];
 s4 := 1.0 - power ((1.0 + working[15]) / (1.0 + k),working[6]);
 itc[1]:= capk * ritce * s2 / s4;
 if (y <= 0) then
 	pvelp[2]:= 0
 else
    Begin
    c_ := power ((1 + k),y) -1;
    if (y < 1) then pvelp[2]:= working[21]*q*c_
    else if (k = 0) then pvelp[2]:= working[21]*q*working[9]*(y - 1)
    else pvelp[2]:= working[21]*q*(c_*(1 + working[9]/k) - working[9])
    End;
 calculate_pvecc;
 pvitc[1]:= 0.0;
 calculate_pvdep;
 calculate_pvewc;
 h1:= power((1 + k),working[28]);
 pvsucd:= phi*h1*s3*(working[28]*(working[10] - working[31]) + working[16]);
 s4:= phi*h1*working[28];
 s5:= working[42]*working[31] - 1;
 pvsuci[1]:= s4*(eqr[1] + s3*(0.5*working[12]*s2 + rpl[1] + ins[1] +
             otx[1] + in_t[1]));
 pvsuci[2]:= s4*(eqr[2] + s3*(0.5*working[12]*fac[2] + opr[2] + rpl[2] +
      ins[2] + otx[2] + in_t[2]));
 pvsuci[3]:= s4*(eqr[3] + s3*(opr[3] + ins[3] + otx[3] + in_t[3]));
 pvsuci[4]:= s4*(eqr[4] + s3*(s5 + opr[4] + ins[4] + otx[4] +
      in_t[4]));
 pvsuci[5]:= s4*(eqr[5] + s3*(s5 + opr[5] + ins[5] + otx[5] + in_t[5]));
 For i := 1 to 5 do
    Begin
    t_matrix [2,i] := opr [i];     t_matrix [3,i] := otx [i];
    t_matrix [4,i] := bval [i];    t_matrix [5,i] := ins [i];
    t_matrix [6,i] := in_t [i];    t_matrix [7,i] := rpl [i];
    t_matrix [8,i] := itc [i];     t_matrix [9,i] := eqr [i];
    t_matrix [10,i] := wcap [i];   t_matrix [11,i] := fac [i];
    t_matrix [12,i] := pvelp [i];  t_matrix [13,i] := pvecc [i];
    t_matrix [14,i] := pvitc [i];  t_matrix [15,i] := pvewc [i];
    t_matrix [16,i] := pvdep [i];  t_matrix [17,i] := pvsuci [i];
    End;
End;


Procedure Calculate_IPEG_coefficients;

Var a,con_stant,s1,s2,s3,s4 : real;
    i                       : integer;

Begin
textcolor(white+blink);
Gotoxy(1,24);
write('COMPUTING COEFFICIENTS...PLEASE WAIT');gotoxy(1,wherey);textcolor(yellow);
vd := 1 + working[13];
vs := working[14];
q := working[24];
y := working[28] + working[30];

if (y = 0) then tcc := working[29]
else Begin
     s4 := working[29] + 1 - y;
     if (fraction (s4) > 0.999) then tcc := 1 + truncate (s4)
     else tcc := truncate (s4);
     End;
k := (working[20] + (working[19] * (working[18] - 1) * (1 - working[17]))) / (working[18]);
capk := (k - working[15]) / (1 + working[15]);
a := power ((1 + capk), (- working[43]));
gcrf := capk / (1 - a);
f1 := truncate ((working[8] - 1) / 2);
b1 := working[21] * working[24] + working[22] * working[25] + working[23];
tsu := working[29] - working[28];
tec := tsu;
phi := power ((1 + working[15]),(-working[28]));
ye := working[28];
dfe := phi;
s1 := 1 - working[10];
con_stant := s1 * (1 - working[17]);
calculate_intermediate_variables;
s2 := gcrf / con_stant;
s3 := 1 - pvsucd * s2;
For i := 1 to 5 do c [i] := (((opr [i] + otx [i] + ins [i] + in_t [i] + rpl [i]
 + 0.0 * (working [12] * bval [i])) / s1) + ((eqr [i] - itc [i]) / con_stant)
 + ((capk / con_stant) * pvdep[i]) + (s2 * (pvelp [i] +
 pvecc [i] + pvitc [i] + pvewc [i] + pvsuci [i]))) / s3;
For i := 1 to 5 do t_matrix[1,i] := c[i]
End;


Procedure Obtain_independent_var_and_values;


Type one_dim_real_array = array [1..max_no_of_values] of real;

Var flag,out,out1,repeated,ok,too_many_values,
    eoline  : boolean;
    i, j, k, form, n_temp,result               : integer;
    xs, xe, deltax, lamda, new_value,number    : real;
    repeated_num                               : one_dim_real_array;

Begin
Display_indep_var_names_and_id ;
ivindex := -1;
out1 := false;
While ((ivindex < 1) or (ivindex > 45)) do
   Begin
   Ask_Question(false,'Enter the ID number of the independent variable ');
   get_number (24,integer_number,rdum,ivindex);
   End;
too_many_values := false;
repeat
   Clear_Message_Line;
   Write('Current value of ');
   If ivindex in [1..43] then
   writeln(ivnames[ivindex],' is ',working[ivindex]:15:2,' ',ivunits[ivindex])
   Else if ivindex = 44 then writeln('I.Quan is ',wkg_i_quan:15:2,' ',comp_dat.industry_units)
   Else if ivindex = 45 then writeln('P.Quan is ',wkg_p_quan:15:2,' ',comp_dat.product_units);
   Wait_for_keypress(25);
   writeln ('Choose input form 1: Type all values.');
   writeln ('                  2: Type starting value,increment,number of values.');
   writeln ('                  3: Starting value,increment,ending value.');
   Write   ('                  4: Starting value,ending value,number of values.  ');
   Val(Read_Keyboard(26,['1'..'4']),form,result);
   Set_Window(1,21,80,24,false);
   For i := 1 to 4 do writeln;

   case form of

   1: Begin
      {non-equally spaced values}
      Ask_Question(false,'Input a list of values: ');
      Load_Input_Buffer;
      i := 0; k := 0;
      n_values := 0;
      eoline := false;
      While (not eoline) do
         Begin
         repeated := false;
         Get_Numeric_Str_From_Buffer(27,eoline);
         Val(input_str,number,result);
         For j := 1 to i do if number = values[j] then repeated := true;
         if not repeated then
            Begin
            i := i + 1;
            n_values := n_values + 1;
            values[i] := number
            End
         else
            Begin
            k := k + 1;
            repeated_num[k] := number
            End
         End;
         For j := 1 to k do writeln('The value ',repeated_num[j]:10:2,' is repeated and therefore ignored');
         writeln
      End;

   2: Begin
      { x(i) = x(s) + (i - 1) * deltax }
      Ask_Question(false,'Input a starting value: ');
      Get_number(28,real_number,xs,idum);
      Ask_Question(false,'Input an increment: ');
      Get_number(29,real_number,deltax,idum);
      Ask_Question(false,'Input the number of values: ');
      Get_number(29,integer_number,rdum,n_values);
      For i := 1 to n_values do values[i] := xs + (i - 1) * deltax;
      End;

   3: Begin
      Ask_Question(false,'Input a starting value: ');
      Get_number(28,real_number,xs,idum);
      Ask_Question(false,'Input an increment: ');
      Get_number(29,real_number,deltax,idum);
      Ask_Question(false,'Input an ending value: ');
      Get_number(28,real_number,xe,idum);
      lamda := abs((xe - xs) / deltax);
      n_values := 1 + trunc(lamda);
      For i :=1 to n_values do values[i] := xs + (i - 1) * deltax;
      End;

   4: Begin
      Ask_Question(false,'Input a starting value: ');
      Get_number(28,real_number,xs,idum);
      Ask_Question(false,'Input an ending value: ');
      Get_number(28,real_number,xe,idum);
      Ask_Question(false,'Input the number of values: ');
      Get_number(29,integer_number,rdum,n_values);
      deltax := abs(xe - xs)/ (n_values - 1);
      For i :=1 to n_values do values[i] := xs + (i - 1) * deltax;
      End;
   End;
   if n_values >= max_no_of_values then
      Begin
      too_many_values := true;
      Clear_Message_Line;
      write('Number of values exceeded limit of ',max_no_of_values:3,' try again')
      End
   Else too_many_values := false;
until not too_many_values;
{Check for min max}
 For i := 1 to n_values do
  Begin
  ok := false;
 Repeat
  if (values[i] > vmax[ivindex]) or (values[i] < vmin[ivindex]) then
   Begin
   Clear_Message_Line;
   writeln('Value ',values[i]:15:2,' is outside normal limits');
   Ask_Question(false,'Do you wish to retain it anyways (Y/N) Y');
   user_response := Read_Keyboard(30,['n','N','y','Y',^M]);
   if user_response in ['n','N'] then
    Begin
    Ask_Question(false,'Enter new value: ');
    Get_number(9,real_number,values[i],idum)
    End
   Else ok := true;
   End
  Else ok := true
 Until ok;
 Table[1,i] := values[i]
 End;
Set_Window(1,1,80,24,false); Wait_for_keypress(25); Clear_The_Screen;
Ask_Question(false,'Do you wish to display the set of independent variable values (Y/N) ? Y');
user_response := Read_Keyboard(31,['n','N','y','Y',^M]);
if not (user_response in ['N','n']) then
   Begin
   Clear_The_Screen;
   If ivindex in [1..43] then writeln (' ':23,'The ',ivnames[ivindex],' independent variable values:')
   Else if ivindex = 44 then  writeln (' ':23,'The I.Quan independent variable values:')
   Else  writeln (' ':23,'The P.Quan independent variable values:');
   writeln;
   For i := 1 to n_values do writeln (' ':33,values[i]:10:2);
   End;
End;

Procedure Calculate_investment_tax_credit_rate (var itc1 : real);

Var tl: real;

Begin
tl :=  working[38] * working[6];
if  working[35] <= tl then itc1 :=  working[32]
else if  working[36] <= tl then itc1 :=  working[32] *  working[33]
else if  working[37] <= tl then itc1 :=  working[32] *  working[34]
End;

Procedure Print_the_results;

Const  values_per_line = 5;

Type int_array = array [1..23] of integer;

Var i,k,l,choice_n_values,number,result,
    values_left,temp,counter1                    : integer;
    choice,repeated_num,illegal                  : int_array;
    repeated,more_rows,eoline                    : boolean;

Procedure Print_units (how_many,k : integer);

Var i,dummy : integer;
    temp    : referent_type;


Begin
  temp := comp_dat.product_units;
  write(ivunits [ivindex]:15);
  For i := 1 to how_many do
    Begin
    if choice[k] in [6,7,2] then write('$(':9,report_year:4:0,')/')
    else if choice[k] in [1,3..5] then write(' ':15)
    else write('$(':10,report_year:4:0,')');
    k := k + 1
    End;
  k := k - how_many;
  writeln;
  write(' ':15);
  For i := 1 to how_many do
    Begin
    if choice[k] = 6 then write(temp:15)
    else if choice[k] = 7 then write(comp_dat.industry_units:15)
    else if choice[k] = 2 then write('SQ_FT.':15)
    else write(' ':15);
    k := k + 1
    End;
  Writeln
End;
Begin
Clear_Message_Line;
Writeln('Present report title is: ',report_title);
Ask_Question(false,'Do you wish to change it (Y/N) N');
user_response := Read_Keyboard(32,['n','N','y','Y',^M]);
if user_response in ['y','Y'] then
 Begin
 Ask_Question(true,'Input a report title : ');
 Get_String(33,report_title)
 End;
Wait_for_keypress(25);
Display_dep_var_names_and_id;
Ask_Question(true,'Input a list of dependent variable ID to be displayed');
Load_Input_Buffer;
{Writeln ('(enter 0 to quit, 24 for all dependent variables) ');}
i := 0; k := 0; l := 0;
choice_n_values := 0;
eoline := false;
While (not eoline)  do
      Begin
      repeated := false;
      Get_Numeric_Str_From_Buffer(27,eoline);
      Val(input_str,number,result);
      If number in [0..24] then
        Begin
        For j := 1 to i do if number = choice[j] then repeated := true;
        if not repeated then
           Begin
           i := i + 1;
           choice_n_values := choice_n_values + 1;
           choice[i] := number
           End
        else
           Begin
           k := k + 1;
           repeated_num[k] := number
           End
        End
      Else
        Begin
        l := l + 1;
        illegal[l] := number
        End
      End;
   For j := 1 to l do writeln ('The value ',illegal[j]:10,' is illegal and therefore ignored');
   For j := 1 to k do writeln ('The value ',repeated_num[j]:10,' is repeated and therefore ignored');
   If choice[1] <> 0 then
   Begin
   If choice[1] = 24 then
     Begin
     choice_n_values := 23;
     for j := 1 to 23 do choice[j] := j
     End;
   Wait_for_keypress(25);
   Clear_The_Screen;
   k := 1; counter1 := 1;
   Print_report_heading;
   values_left := choice_n_values;
   more_rows := false;
   while (values_left > 0) do
     Begin
     if values_left >= (values_per_line - 1) then
        Begin
        values_left := values_left - values_per_line + 1;
        write (ivnames [ivindex]:15);
        for i := 1 to (values_per_line - 1) do
           Begin
           write (vnames[choice[k]]:15);
           k := k +1
           End;
        writeln;
        Print_units(values_per_line - 1,k - values_per_line + 1);
        temp := counter1;
        for i := 1 to n_values do
           Begin
           if more_rows then l := temp
           else l := 1;
           write (table[1,i]:15:2);
           for j := 1 to (values_per_line - 1) do
              Begin
              write (table[choice[l] + 1,i]:15:2);
              l := l + 1;
              End;
           writeln;
           End;
        Writeln;
        counter1 := counter1 + values_per_line - 1;
        more_rows := true;
        End
     else
        Begin
        write (ivnames[ivindex]:15);
        for i := 1 to values_left do
           Begin
           write (vnames[choice[k]]:15);
           k := k + 1;
           End;
        writeln;
        Print_units (values_left,k - values_left);
        temp := counter1;
        for i := 1 to n_values do
           Begin
           if more_rows then l := temp
           else l := 1;
           write (table[1,i]:15:2);
           for j := 1 to values_left  do
              Begin
              write (table [choice[l] + 1,i]:15:2);
              l := l + 1;
              End;
           writeln;
           End;
        Writeln;
        values_left := 0;
        End
     End;
     writeln; writeln;
     End;
  End;






Procedure Search_directory (code : char; Var found : boolean);

Begin
reset(dir);
found := false;
   case code of

   'i' : Begin
         While (not eof(dir)) and (not found) do
            Begin
            read (dir,dir_data);
            if (dir_data.size_index = size) then found := true;
            End;
         End;

   'r' : Begin
         if dir_data.reference = referent then found := true
         else While (not eof(dir)) and (not found) do
            Begin
            read (dir,dir_data);
            if (size = dir_data.size_index) and (referent = dir_data.reference) then found := true;
            End
         End
   End;
close (dir);
End;


Procedure Load_company_data;

Begin
reset(com);
seek (com,dir_data.company_rec);
read (com,comp_dat);
close (com);
End;



Procedure Get_the_IPEG_inputs_for_this_case;

type str20 = string [20];

Var ch_r                     : char;
    ok,found,valid_response : boolean;

Procedure Get_industry_size;

Begin
Repeat
  Ask_Question(false,'Is the industry size index (P)revious, (N)ew or (D)efault ? D');
  ch_r := Read_Keyboard(34,['P','p','N','n','D','d',^M]);
  case ch_r of
  'P','p' : Begin
            if old_size = 0 then
               Begin
               Print_Message('No previous size exists at this time. ');
               valid_response := false;
               End
            else
               Begin
               Clear_Message_Line;
               Writeln ('The previous value ',old_size,' has been assumed. ');
               valid_response:= true;
               size := old_size;
               End;
            End;
  'N','n' : Begin
               Ask_Question(false,'Enter industry size index: ');
               Get_number(35,integer_number,rdum,size);
            old_size := size;
            search_directory ('i',found);
            if (not found) then
               Begin
               Clear_Message_Line;
               Writeln ('The response ',size,' is invalid as this value is not in the file. ');
               valid_response := false;
               End
            else valid_response:= true
            End;

  'D','d',^M : Begin
            reset(dir);
            read (dir,dir_data);
            size := dir_data.size_index;
            old_size := size;
            Clear_Message_Line;
            Writeln ('The default ',size,' has been assumed.');
            valid_response:= true;
            close (dir);
            End;
        End;
  Until valid_response;
End;

Procedure Get_company_referent;

Begin
   Repeat
   Ask_Question(false,'Is the company referent (P)revious, (N)ew or (D)efault ? D');
   ch_r := Read_Keyboard(36,['P','p','N','n','D','d',^M]);
     case ch_r of
     'P','p' : Begin
               if old_company_referent = ' ' then
                  Begin
                  Print_Message('No previous referent exists at this time.');
                  valid_response := false;
                  End
               else
                  Begin
                  referent := old_company_referent;
                  comp_dat := saved_com;
                  valid_response:= true;
                  Clear_Message_Line;
                  Writeln ('The previous company referent ',referent,' has been assumed. ');
                  End;
               End;

     'N','n' : Begin
               Ask_Question(false,'Enter the company referent: ');
               Get_String(37,referent);
               old_company_referent := referent;
               search_directory ('r',found);
               if (not found) then
                  Begin
                  Clear_Message_Line;
                  Writeln('The case defined by industry size ',size:2,' and company',
                  ' referent ',referent,' is not on file. ');
                  Ask_Question(false,'Would you like to restart (Y/N) ? N');
                  user_response := Read_Keyboard(38,['n','N','y','Y',^M]);
                  if (user_response in ['Y','y']) then Get_industry_size;
                  valid_response := false;
                  End
               else
                  Begin
                  load_company_data;
                  saved_com := comp_dat;
                  valid_response:= true;
                  End
               End;

     'D','d',^M : Begin
               referent := dir_data.reference;
               saved_com := comp_dat;
               old_company_referent := referent;
               Clear_Message_Line;
               Writeln ('The default ',referent,' has been assumed.');
               valid_response:= true;
               End;
         End;
   Until valid_response;
   Ask_Question(false,'Do you wish to display the company data (Y/N) ? N');
   user_response := Read_Keyboard(39,['n','N','y','Y',^M]);
   if (user_response in ['Y','y']) then Display_the_company_data;
End;

Procedure Get_comp_or_proc_response;


Begin
Repeat
   Ask_Question(false,'Do you wish to analyze a (C)ompany or (P)rocess only ? C');
   ch_r := Read_Keyboard(40,['C','c','P','p',^M]);
if ch_r in ['P','p'] then
   Begin
   process_flag:= 1;
   Gotoxy(1,18);
   Writeln('Select one of the following processes:');
   for i := 1 to comp_dat.number_of_processes do write (comp_dat.process_list[i]:10);

   Ask_Question(false,'Enter process name: ');
   Get_String(41,strg);
   for i := 1 to 10 do if (strg[i] in ['a'..'z']) then strg[i] := chr( ord(strg[i]) - 32);
   search_process_list (strg,found,where);
   if found then
      Begin
      load_process_data (where);
      valid_response := true;
      Ask_Question(false,'Do you wish to display the process data (Y/N) ? N');
      user_response := Read_Keyboard(42,['n','N','y','Y',^M]);
      if (user_response in ['Y','y'])  then Display_the_process_data;
      End
   else
      Begin
      Clear_Message_Line;
      Print_Message('Process '+strg+' does not exist.');
      Ask_Question(false,'Would you like to restart (Y/N) ? N');
      user_response := Read_Keyboard(43,['n','N','y','Y',^M]);
      if (user_response in ['Y','y']) then
         Begin
         Get_industry_size;
         Get_company_referent;
         End;
      valid_response := false;
      End;
      End
 else
  begin
  valid_response:= true;
  load_company_data;
  process_flag:= 0
  end
Until valid_response;
End;

Procedure Get_report_year;
Begin
   Repeat
      Ask_Question(false,'Is the report year (P)revious, (N)ew or (D)efault ? D');
      ch_r := Read_Keyboard(44,['P','p','N','n','D','d',^M]);
   case ch_r of

   'P','p' : Begin
             if old_report_Year = 0 then
                Begin
                Print_Message('No previous report year exists.');
                valid_response := false;
                End
             else
                Begin
                report_Year := old_report_Year;
                valid_response:= true;
                Clear_Message_Line;
                Writeln ('The previous value ',report_Year:4:0,' has been assumed.');
                End;
             End;

   'N','n' : Begin
             ok := true;
                Repeat
                Ask_Question(false,'Enter the report Year: ');
                get_number (45,real_number,report_year,idum);
                old_report_Year := report_Year;
                if (report_Year < comp_dat.tb) or (report_Year > comp_dat.save6_31 [29]) then
                   Begin
                   Clear_Message_Line;
                   Writeln ('Your response ',report_Year:4:0,' is outside the ',
                   comp_dat.tb:4:0,' - ',comp_dat.save6_31[29]:4:0,' range.');
                   Ask_Question(false,'Do you wish to keep it anyway (Y/N) ? N');
                   user_response := Read_Keyboard(45,['n','N','y','Y',^M]);
                   if (user_response in ['Y','y']) then ok := true
                   else ok := false
                   End
                else
                   Begin
                   valid_response:= true;
                   ok:= true
                   End;
                Until ok;
             End;

   'D','d',^M : Begin
             report_Year := comp_dat.tb;
             old_report_Year := report_Year;
             Clear_Message_Line;
             Writeln ('The default ',report_Year:4:0,' has been assumed.');
             valid_response:= true
             End;
   End;
   Until valid_response;
End;

Begin  {** get the ipeg inputs for this case **}
valid_response := true;

get_industry_size;
get_company_referent;
get_comp_or_proc_response;
get_report_Year;
End;


Begin {Main Program}

Initialize_the_IPEG_program;
user_response := interface;
While (user_response <> 'E') do
Begin
Execute_Command;
more_cases := true;
While more_cases do
   Begin
   Ask_Question(false,'Do you wish to process a case (Y/N) ? Y');
   user_response := Read_Keyboard(46,['n','N','y','Y',^M]);
   if (user_response in ['N','n']) then more_cases := false
   else
      Begin
      If not samis_file then more_cases := false;
      If samis_file then
      Begin
       Get_the_IPEG_inputs_for_this_case;
       Reinitialize_working_vars;
      End;
      Ask_Question(false,'Do you wish to display any of the saved variables (Y/N) ? N');
      user_response := Read_Keyboard(47,['n','N','y','Y',^M]);
      Calculate_the_discount_rate(cost);
      {If cost <= (working[15] + epsilon) then
      writeln('***WARNING the real cost of capital ',cost:15:2,' is not');
      writeln('realistic for this case and the ipeg results will not be meaningful');}
      if (user_response in ['Y','y']) then show_indep_Vars;
      ritce := working[7];
      Calculate_IPEG_coefficients;
      price := c[1]*working[1] + c[2]*working[2] + c[3]*working[3] + c[4]*working[4] + c[5]*working[5];
      Ask_Question(false,'Do you wish to display the transformation matrix (Y/N) ? N');
      user_response := Read_Keyboard(48,['n','N','y','Y',^M]);
      if (user_response in ['Y','y']) then Display_the_transformation_matrix;
      print_the_price;
      more_sensitivities := true;
      While more_sensitivities do
         Begin
         Ask_Question(false,'Do you wish to perform a sensitivity study (Y/N) ? Y');
         user_response := Read_Keyboard(49,['n','N','y','Y',^M]);
         if user_response in ['N','n'] then more_sensitivities := false
         else
            Begin
            Ask_Question(false,'Do you wish to reinitialize the working variables (Y/N) ? N');
            user_response := Read_Keyboard(50,['n','N','y','Y',^M]);
            if user_response in ['Y','y'] then Reinitialize_working_vars;
            Ask_Question(false,'Do you wish to change any of the working variable values (Y/N) ? N');
            user_response := Read_Keyboard(51,['n','N','y','Y',^M]);
            if (user_response in ['Y','y']) then
               Begin
               Change_working_vars;
               Display_changed_vars;
               End
            else user_response := ' ';
            Obtain_independent_var_and_values;
            if ivindex = 0 then
               Begin
               ritce := working[7];
               Calculate_IPEG_coefficients; {corresponding to the saved variables}
               price := c[1]*working[1] + c[2]*working[2] + c[3]*working[3] + c[4]*working[4] + c[5]*working[5];
               Ask_Question(false,'Do you wish to display the Transformation Matrix (Y/N)? N');
               user_response := Read_Keyboard(48,['n','N','y','Y',^M]);
               if user_response in ['Y','y'] then Display_the_transformation_matrix;
               Print_the_price;
               End
            else
               Begin
               first_time := false;
               For i :=1 to n_values do
                  Begin
                  working[ivindex] := table[1,i];
                  if ((ivindex = 6) or ((ivindex > 31) and (ivindex < 39)))
                  then calculate_investment_tax_credit_rate (ritce)
                  else ritce := working[7];
                  if (not first_time) or (ivindex > 5) then
                     Begin
                     Calculate_IPEG_coefficients;
                     first_time := true;
                     End;
                  For j :=1 to 5 do e_vector[j] := working[j];
                  For k1 :=1 to 17 do
                     Begin
                     v_vector[k1] :=0.0;
                     For j :=1 to 5 do v_vector[k1] :=v_vector[k1] + t_matrix[k1,j]*e_vector[j];
                     End;
                  deflator := power ((1.0 + working[15]), (report_year - comp_dat.tb{save6_31[29]}));
                  For j :=1 to 5 do table[j+1,i] := t_matrix[1,j];
                  table[3,i] := table[3,i]*deflator;
                  price := v_vector[1]*deflator;
                  If ivindex in [44,45] then
                     Begin
                     Clear_Message_Line;
                     Print_Message('For more meaningful results you should change the expense vector accordingly');
                     End;
                  table[7,i] := price / wkg_p_quan;
                  table[8,i] := price / wkg_i_quan;
                  If ivindex = 44 then
                     Begin
                     table[8,i] := price / table[1,i];
                     table[7,i] := table[8,i] * wkg_i_quan / wkg_p_quan;
                     End
                  Else if ivindex = 45 then
                     Begin
                     table[7,i] := price / table[1,i];
                     table[8,i] := table[7,i] * wkg_p_quan / wkg_i_quan;
                     End;
                  For j := 1 to 16 do table [j + 8,i] := v_vector [j + 1]*deflator;
                 End
              End;
           Ask_Question(false,'Do you wish to have the results displayed ? Y');
           user_response := Read_Keyboard(52,['n','N','y','Y',^M]);
           if not (user_response in ['N','n']) then Print_the_results;
           Ask_Question(false,'Do you wish to plot the sensitivity results ? N');
           user_response := Read_Keyboard(53,['n','N','y','Y',^M]);
           if user_response in ['Y','y'] then
              Begin
              Set_Window(1,1,80,25,false);
              Clrscr;
              Plot_the_results;
              Clear_The_Screen;
              End;
           End
        End
     End
  End;
  Ask_Question(false,'Do you wish to save working case (Y/N) N');
  user_response := Read_Keyboard(54,['n','N','y','Y',^M]);
  If user_response in ['Y','y'] then
  Begin
   Clear_Message_Line; Gotoxy(1,23);
   Script('The current file name is ',ipeg_input,'','');
   Ask_Question(false,'Do you wish to save working case under this file name (Y/N) N');
   user_response := Read_Keyboard(55,['n','N','y','Y',^M]);
   if not (user_response in ['Y','y']) then
   Begin
    Ask_Question(false,'Enter new file name: ');
    Get_String(55,ipeg_input)
   End;
   Assign(ipg,ipeg_input);
   Rewrite(ipg);
   ipg_data.directory := dir_data;
   With ipg_data.company do
   Begin
    for i := 1 to 5 do save1_5[i] := working[i];
    for i := 6 to 31 do save6_31[i] := working[i];
    for i := 32 to 43 do save32_43[i] := working[i];
    i_quan := wkg_i_quan; p_quan := wkg_p_quan;
    tb := comp_dat.tb;
    company_descriptive_name := comp_dat.company_descriptive_name;
    number_of_processes := comp_dat.number_of_processes;
    industry_units := comp_dat.industry_units;
    product_units := comp_dat.product_units;
    process_rec := 0; process_list[1] := 'NONE'; number_of_processes := 0;
   End;
   Write(ipg,ipg_data);
   Close(ipg);
  End;
  If samis_file then
   Begin
   Erase(com);
   Erase(dir);
   Erase(pro);
   End;
user_response := interface;
End;
Set_Window(1,1,80,25,false); Clrscr;
End.


