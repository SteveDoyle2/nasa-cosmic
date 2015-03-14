Function float (num_ber:real):real;
Begin
 float := num_ber * 1.0
End;

Function Truncate (num_ber:real):real;
Begin
truncate := trunc (num_ber) / 1.0;
End;

Function Fraction (num_ber:real):real;
Var temp : real;
Begin
temp := truncate (num_ber);
fraction := num_ber - temp;
End;

Function Power (x,y:real):real;
Begin
if y = 0 then power := 1.0
else if x <= 0 then writeln ('***ERROR: Negative base in Power Function****')
else power := exp (y * (ln (x)))
End;

Function File_exists (file_name: strtype): boolean;
Var Fil : file;
Begin
assign(Fil,file_name);
{$I-} reset (Fil) {$I+};
File_exists := (IOresult = 0)
End;

Procedure Print_Message(message : q_length);
Begin
   Gotoxy(1,23); Writeln(message);
End;

Procedure Clear_Message_Line;
Begin
   Gotoxy(1,23); Clreol;
End;

Procedure Display_Messages;

Begin
   Textbackground(blue); Textcolor(white);
   Writeln('                       SPECIAL FEATURE                              ');
   Writeln;
   Writeln('You can create an IPEG case from scratch.  To do this start by      ');
   Writeln('selecting choice "B" of the program functions menu.  Specify        ');
   Writeln('the name you want to give to the case file in reply to the request  ');
   Writeln('"Enter IPEG file name:".  Then answer "Y"es to the question         ');
   Writeln('"Do you wish to create one?"  After this, PC-IPEG will ask you      ');
   Writeln('for the values needed to uniquely identify the case.                ');
   Writeln('Most of the financial parameters have default values.  You can      ');
   Writeln('either load the IPEG case with defaults or type your own values.    ');
   Writeln('Once you have done this, you will be given an opportunity to display');
   Writeln('all values and to change any of them.                               ');
   Textbackground(black); Textcolor(yellow);
End;


Procedure Script(str1,letter,str2,str3 : string_type);
Begin
   Textcolor(yellow);Gotoxy(20,wherey);Write(str1);
   Textbackground(blue);
   Textcolor(yellow + blink); Write(letter);
   Textbackground(black);
   Textcolor(yellow); Write(str2);Gotoxy(1,wherey+1);
   Gotoxy(20,wherey);Write(str3);
End;

Procedure Set_Window(x1,y1,x2,y2 : integer; previous : boolean);
Begin
   If previous then Window(prevx1,prevy1,prevx2,prevy2)
   Else
      Begin
      prevx1 := x1; prevx2 := x2;
      prevy1 := y1; prevy2 := y2;
      Window(x1,y1,x2,y2); Gotoxy(1,1);
      End;
End;

Function Read_Keyboard(help_code : integer; expected_char : expected_set_type):char;

Type

   line_type = string[76];

   error_file_entry_type = Record
                           line : array[1..4] of line_type;
                           End;

   expected_set_type = set of char;

   register_type = record
                   ax,bx,cx,dx,di,si,ds,es,flag : integer;
                   end;

   buffer_type   = array[1..80] of char;

   trick_type    = record
                   case boolean of
                      true :(charr:char);
                      false:(reg  :integer);
                   end;
Var
   ch,response      : char;
   register         : register_type;
   buffer           : array[1..7] of buffer_type;
   trick            : trick_type;
   fil              : file of error_file_entry_type;
   fil2             : text;
   i,j              : integer;
   error_file_entry : error_file_entry_type;
   line,blank_line  : line_type;

   Procedure Erase_Lines;
   Var
      i : integer;
   Begin
      For i := 1 to 7 do
         Begin
         Gotoxy(1,i);
         Clreol;
         End;
   End;

   Procedure Save_Lines;
   Begin
      For i := 1 to 7 do
         For j := 1 to 80 do
            Begin
            Gotoxy(j,i);
            With register do
               Begin
               ax := 8 shl 8;
               bx := 0 shl 8;
               End;
            Intr($10,register);
            trick.reg := register.ax;
            buffer[i][j] := trick.charr;
            End;
   End;

   Procedure Restore_Lines;
   var
      blanks : buffer_type;
   Begin
      For i := 1 to 80 do blanks[i] := ' ';
      For i := 1 to 7 do Write(blanks);
      For i := 1 to 7 do
         Begin
         For j := 1 to 80 do
            Begin
            Gotoxy(j,i);
            trick.reg := 0;
            trick.charr := buffer[i][j];
            With register do
               Begin
               cx := 1;
               bx := bx shl 8 shr 8;
               ax := trick.reg shl 8 shr 8 or ($0A shl 8);
               End;
            Intr($10,register);
            End;
         End;
   End;

   Procedure Print_Help_Message(help_code : integer);
   Var
      car : char;
      j,l: integer;

   Begin
      Textcolor(white);
      Assign(fil,'PCIPEG.HLP');
      If (not File_Exists('PCIPEG.HLP')) then
         Begin
         Clear_Message_Line;
         Print_Message('*** THE HELP FILE <PCIPEG.HLP> IS NOT ON THE DIRECTORY NO HELP AVAILABLE');
         End
      Else
         Begin
         Reset(fil);
         Seek(fil,help_code); Read(fil,error_file_entry);
         For j := 1 to 4 do
            Begin
            For l := 1 to 76  do
               If error_file_entry.line[j][l] in [#21..#126] then Write(error_file_entry.line[j][l]);
            Writeln;
            End;
         textcolor(green);
         Write('                                            PRESS ANY KEY TO CONTINUE');
         Repeat
         Until Keypressed;
         Read(kbd,car);
         textcolor(yellow);
         End;
   End;

   Function Get_Char(help_code : integer; expected_char : expected_set_type) : Char;
   Var
      continue          : boolean;
      chr_read          : char;
      px1,px2,py1,py2   : integer;
      currentx,currenty : integer;
   Begin
      currentx := wherex;
      currenty := wherey;
      continue := true;
      While (continue) do
         Begin
         continue := false;
         Repeat
            Gotoxy(currentx,currenty);
            Read(kbd,chr_read);
            If not (chr_read in [^[,^\]) then Write(chr_read);
            If not (chr_read in (expected_char + [^[,^\])) then
               Begin
               sound(440);
               delay(100);
               nosound;
               End;
         Until chr_read in (expected_char + [^[,^\]);
         If chr_read = ^[ then
            Begin
            Writeln;
            Writeln('PROGRAM IS BEING ABORTED AT YOUR REQUEST');
            Delay(1000); Set_Window(1,1,80,25,false);
            Clrscr;Textbackground(black);
            Halt;
            End
         Else if chr_read = ^\ then
            Begin
            continue := true;
            px1 := prevx1; px2 := prevx2; py1 := prevy1; py2 := prevy2;
            Set_Window(1,1,80,7,false);
            Save_Lines;Textbackground(white);
            Erase_Lines;
            Set_Window(2,2,79,6,false); Textbackground(blue);
            Print_Help_Message(help_code);
            Set_Window(1,1,80,7,false); Textbackground(black);
            Textcolor(yellow);
            Restore_lines;
            Set_Window(px1,py1,px2,py2,false);
            End
         Else Get_Char := chr_read;
         End;
   End;

Begin

   Read_Keyboard := Get_Char(help_code,expected_char);
End;

Procedure Wait_for_keypress(help_code : integer);
Var
   char_pressed : char;
Begin
   textcolor(white);
   Gotoxy(1,24); Clreol;
   write ('HIT ANY KEY TO CONTINUE '+Chr(219)); Gotoxy(wherex-1,wherey);
   textcolor(yellow);
   Repeat
   until keypressed;
   char_pressed := Read_Keyboard(help_code,[Chr(28)..Chr(126)]);
   gotoxy (1,wherey);
End;

Procedure Get_numeric_str(help_code : integer; var eoline : boolean);
Var ch_r : char;
Begin
  ch_r := Read_Keyboard(help_code,['0'..'9','-','.']);
  input_str := ch_r;
  eoline := false;
  While (ch_r in ['0'..'9','.','e','E','-','+',^H]) do
    Begin
    ch_r := Read_Keyboard(help_code,['0'..'9','.','e','-','E',^M,^H,',',' ']);
    If ch_r = ^M then eoline := true
    Else if ch_r = ^H then
       Begin
       Write(' ');
       Gotoxy(wherex-1,wherey);
       Delete(input_str,Length(input_str),1);
       End
    Else if not (ch_r in [' ',',']) then input_str := input_str + ch_r;
    End
End;

Procedure Get_number (help_code : integer; n_type : num_type; var rnum : real; var inum : integer);
Var
   x,y,result        : integer;
   dummy,valid_entry : boolean;
Begin
   x := wherex;
   y := wherey;
   Repeat
      Gotoxy(x,y);
      Clreol;
      Get_numeric_str(help_code,dummy);
      if n_type = real_number then Val (input_str,rnum,result)
      else Val (input_str,inum,result);
      if result = 0 then valid_entry := true
      else valid_entry := false;
   Until valid_entry;
End;

Procedure Get_String(test_code : integer; var st_ring : q_length);
Var
   ch     : char;
   eoline : boolean;
Begin
  eoline := false;
  st_ring := '';
  While (not eoline) do
     Begin
     ch := Read_Keyboard(test_code,['A'..'Z','a'..'z','0'..'9','.',' ',^M,^H]);
     If not (ch in[^M,^H]) then st_ring := st_ring + ch
     Else if ch = ^H then
        Begin
        Write(' ');
        Gotoxy(wherex-1,wherey);
        Delete(st_ring,Length(st_ring),1);
        End
     Else eoline := true;
     End;
End;

Procedure Ask_Question(linefeed : boolean; question : q_length);
Begin
   If linefeed then Gotoxy(1,23)
   Else Gotoxy(1,24);Clreol;
   Textbackground(white);Textcolor(black);Write(question);
   Textbackground(black);Textcolor(yellow);
   If linefeed then Gotoxy(1,24); Clreol;
End;


Procedure Show_New_Menu;
Begin
   Textbackground(blue); Textcolor(yellow);
   Gotoxy(1,25);
   Write('FOR HELP TYPE <CTRL \>                               TO EXIT PROGRAM TYPE <ESC>');
   Textbackground(black);
End;

Procedure Clear_The_Screen;
Begin
   Set_Window(1,1,80,25,false); Clrscr; Show_New_Menu; Set_Window(1,1,80,24,false);
   Gotoxy(1,1);
End;

Procedure Load_Input_Buffer;
Var
   ch_r : char;
Begin
   buffer_cursor := 1;
   ch_r := Read_Keyboard(11,['0'..'9','.','+','-']);
   input_buffer := ch_r;
   While ch_r <> ^M do
      Begin
      ch_r := Read_Keyboard(11,['0'..'9','.','e','-','E',^M,^H,',',' ']);
      If ch_r = ^H then
         Begin
         Delete(input_buffer,Length(input_buffer),1);
         Write(' ');
         Gotoxy(wherex-1,wherey);
         End
      Else input_buffer := input_buffer + ch_r;
      End;
End;

Procedure Get_Numeric_Str_From_Buffer(help_code : integer; var eoline : boolean);
Var
   ch_r : char;
Begin
   ch_r := input_buffer[buffer_cursor];
   input_str := ch_r;
   buffer_cursor := buffer_cursor + 1;
   While ch_r in ['0'..'9','.','e','E','-','+'] do
      Begin
      ch_r := input_buffer[buffer_cursor];
      buffer_cursor := buffer_cursor + 1;
      If ch_r = ^M then eoline := true
      Else if not (ch_r in [',',' ']) then input_str := input_str + ch_r
      End;
End;