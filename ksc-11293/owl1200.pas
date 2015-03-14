program owl1200 (input,output);

(*********************************************************************)
(* TITLE:  OWL1200                                                   *)
(*                                                                   *)
(* PROGRAMMER:  Gary Lee, Richard Smith and Ted Giering              *)
(*                                                                   *)
(* PURPOSE:  To allow an IBM Personal Computer to be used as a       *)
(*           means of inputting data into VIS1ON software.  Also,    *)
(*           this program may be used to access a computer as a      *)
(*           non-intelligent terminal.                               *)
(*                                                                   *)
(* NOTES:  To date, all implemented functions work correctly.        *)
(*                                                                   *)
(* WARNINGS/LIMITATIONS:  This software runs on an IBM Personal      *)
(*           Computer, but the PC must have certain features:        *)
(*                        1.  CRT Monitor                            *)
(*                        2.  1 Single(or Double)-Sided Floppy       *)
(*                            Disk Drive (5 1/4")                    *)
(*                        3.  Serial Communications Card.            *)
(*                        4.  Color Graphics Adapter.                *)
(*                        5.  128K Memory Card                       *)
(*                                                                   *)
(* CALLING CONVENTIONS:  N/A                                         *)
(*                                                                   *)
(* CALLING PARAMETERS: N/A                                           *)
(*                                                                   *)
(* DISKETTE FORMAT:                                                  *)
(*          5 1/4" IBM-PC format                                     *)
(*          Single Sided/Double Density                              *)
(*          8 Sector - 512 Bytes per sector                          *)
(*          3 Files:       OWL1200.EXE                               *)
(*                         ROMLIB.OBJ                                *)
(*                         COMLIB.OBJ                                *)
(*                                                                   *)
(* COMPILATION PROCEDURE:                                            *)
(*          Run PAS1 of the Pascal compiler on OWL1200.              *)
(*          Run PAS2 on OWL1200.                                     *)
(*          Link the OWL1200 runfile to ROMLIB AND COMLIB.           *)
(*                                                                   *)
(* MODIFICATION HISTORY:                                             *)
(*                                                                   *)
(*                                                                   *)
(*********************************************************************)

type
  inchr=char;
  tabary=array [0..80] of boolean;
  attary=array [0..23,0..79] of byte;

{ Types used for direct access of the CRT display. }
  scrcell =			{ Layout of a single character on the screen. }
    record
      character: char;		{ Character at this position. }
      attribute: byte;		{ Attribute byte for this character. }
    end;

  colidx = 0..79;		{ Column range of screen }
  rowidx = 0..23;		{ Row range of screen }

{ Screen layout }
  scrmat = array[rowidx, colidx] of scrcell;

  scrptr = ads of scrmat;	{ Access pointer to the screen.  Must be
				  initialized to segment B8000 before
				  accessing the screen. }

var
  rate  :word;    (*  Baud rate for SETCOM                         *)
  r1    :word;    (*  Upper left row, used in scroll.              *)
  status:word;    (*  Terminal status byte                         *)
  optn1 :word;    (*  Option byte #1                               *)
  optn2 :word;    (*  Option byte #2                               *)
  parity:char;    (*  Store the type of parity to be used.         *)
  ratchr:char;    (*  Represents the current baud rate.            *)
  hstchr:inchr;   (*  Input character from the host processor.     *)
  keychr:inchr;   (*  Input character from the keyboard.           *)
  sndkey:inchr;   (*  Type of transmission desired.                *)
  i     :integer; (*  Counter variable.                            *)
  j     :integer; (*  Counter variable.                            *)
  idx   :integer; (*  Array index used in setting tab stops.       *)
  chrint:integer; (*  Numerical equivalent to an ASCII character.  *)
  hstint:integer; (*  ASCII number of host CPU character.          *)
  keyint:integer; (*  ASCII number of keyboard character.          *)
  row   :integer; (*  Single global value for row.                 *)
  col   :integer; (*  Single global value for col.                 *)
  pagrow:integer; (*  Storage for cursor position at page change.  *)
  pagcol:integer; (*  Storage for cursor position at page change.  *)
  mltflg:boolean; (*  Notes which pass keybrd is in.               *)
  newlin:boolean; (*  New Line Enable Flag                         *)
  autolf:boolean; (*  Auto Line Feed.                              *)
  scrlpg:boolean; (*  Page scrolling.                              *)
  keylck:boolean; (*  Keyboard locked.                             *)
  ucenab:boolean; (*  Upper case character enable/disable.         *)
  page2 :boolean; (*  Denotes SCREEN=PAGE 2.                       *)
  functn:boolean; (*  Notes when a keystroke is a function key.    *)
  convrs:boolean; (*  Sets conversational mode.                    *)
  graph :boolean; (*  Graphics character set.                      *)
  done  :boolean; (*  Finished with a repeat loop.                 *)
  keycall:boolean;(*  Call of writch from keybrd.                  *)
  fulscr:boolean; (*  Full screen mode.                            *)
  tabstp:tabary;  (*  Array storing current tab stops.             *)
  fldatt:attary;  (*  Array of field attributes.                   *)

  screen: scrptr;	{ Pointer to the screen. }

label 50;         (*  Re-initialition label  *)
label 60;         (*  Host input label for processing of (ESC).  *)
label 70;         (*  Keyboard input label for processing of (ESC).  *)
label 100;        (*  Exit label.  *)

procedure clrint ; extern ;

function dosint (request:word):word;extern;

function direct_io (request:word):word;extern;

(*  Gets a character from the communications port.  *)
function getchr:word; extern;

(*  This procedure clears the screen and homes the cursor.         *)
procedure clrscr; extern; 

(*  This procedure scrolls the page according to its param's.      *)
procedure scroll(r1,c1,r2,c2,range,direct:word); extern;

(*  This procedure erases all text to the end of the screen.       *)
procedure era_eop_sp; extern;

(*  This procedure erases all text to the end of the line.         *)
procedure era_eol_sp; extern;

(*  This procedure initializes the communications port of the PC.  *)
procedure initcm; extern;

(*  This procedure sets the baud rate of the communications port.  *)
procedure setcom(rate:word); extern;

(*  This procedure finds the current cursor position.              *)
function fndcur:integer ; extern;

{ This procedure initializes the screen pointer for direct access to the
  characters displayed on the CRT screen.  It should be executed before
  accessing the screen with scrget (below). }
procedure scrinit;
begin
  screen.r := 0;
  screen.s := 16#b800;
end;

{ This function returns the character displayed at (row,col) on the CRT
  screen. }
function scrget(row: rowidx; col: colidx): char;
begin
  scrget := screen^[row,col].character;
end;

(*  This procedure retrieves the current cursor location and       *)
(*    stores it in r and c.                                        *)
procedure fcursr (var r,c:integer);
var
  coord:integer;
begin;
  coord:=fndcur;
  r:=ord(hibyte(coord));
  c:=ord(lobyte(coord));
end;

(*  This procedure moves the cursor to a particular location.      *)
procedure locate(row,column:integer); extern;

(*  This function returns true if the field is protected.          *)
function protct :boolean;
begin
  protct:=(fldatt[row,col] and wrd(3)) = 2;
end;

(*  Sends a character out the communications port.                 *)
procedure putchr(outpt:char); extern;

(*  This procedure does absolutely nothing.                        *)
procedure null;
begin
end;

(*  Sends a char. as putchr does, but with a delay.                *)
procedure delay_out(outpt:char);
begin
  putchr (outpt);         (*  Get a character.                               *)
  for i:=1 to 1000 do     (*  Perform a loop 1000 times to create a delay.   *)
    null                  (*  The delay is to get the PR1ME to understand    *)
end;                      (*    the IBM.                                     *)

(*  This procedure inputs a character from the keyboard.           *)
(*    NOTE--Waits for a character.                                 *)
procedure watkey(var inpt:char);
begin
  inpt:=chr(lobyte(dosint(16#0007)));
end;

(*  This procedure inputs a character from the keyboard.           *)
(*    NOTE--Does not wait for a character.                         *)
procedure getkey(var inpt:char);
var
  inchek:integer;
begin
  inpt:=chr(0);                             (*  Initialize the character.    *)
  inchek:=ord(lobyte(dosint(16#000B)));     (*  See if a char. waiting       *)
  if inchek=255 then                        (*  If one is there, then...     *)
    begin
      inpt:=chr(lobyte(direct_io(255)));    (*  ...Get the character.        *)
      if ord(inpt)=0 then                   (*  If char is a null, then...   *)
        begin
          inpt:=chr(lobyte(direct_io(255)));(*  ...get another character...  *)
          functn:=true                      (*  ...the input is function key *)
        end
      else
        functn:=false                       (*  ...or it's not a function key*)
    end;
end;

(*  This procedure calculates the position of the cursor after 1 move to  *)
(*    the right.  When cursor reaches (23,79), the row, col and done      *)
(*    values are taken from the global variables.                         *)
procedure curcalc;
begin
  done:=false;                   (*  Initialize the stop flag.               *)
  if col=79 then                 (*  If last column...                       *)
    if row=23 then               (*  ...If last row, too...                  *)
      done:=true                 (*  ......then STOP.                        *)
    else                         (*  If not last row, but last column...     *)
      begin
        row:=row+1;              (*  ...increment the row value...           *)
        col:=0;                  (*  ...reset to first column.               *)
      end
  else                           (*  If not last column...                   *)
    col:=col+1                   (*  ...increment the column.                *)
end;

(*  This procedure increments the row value and checks for scroll.  *)
procedure rowinc (direct:word);
begin
  row:=row+1;                       (*  Increment the row value.             *)
  if row>23 then                    (*  If increment is off screen...        *)
    if scrlpg then                  (*  ...If scroll is set...               *)
      begin     
        row:=23;                    (*  ...reset row value to last row...    *)
        scroll (0,0,23,79,1,direct) (*  ...and scroll the screen up 1 row.   *)
      end
    else                            (*  ...If scroll is not set...           *)
      row:=0                        (*  ......set row to first row.          *)
end;

(*  This procedure changes the direction of a cursor movement.         *)
(*    (  left to right - up to down  )                                 *)
procedure curchng (direct:word);
begin
  if direct=1 then            (*  If direction is down...                   *)
    begin
      col:=79-col;            (*  ...use certain mathematical concepts to    *)
      row:=23-row             (*  convert the parameters to a configuration  *)
    end                       (*  of direct=0(up).                           *)
end;

(*  This procedure determines the next cursor position when moved 1 space  *)
(*    to the left or right.  direct(0=right,1=left).                       *)
procedure curpos(direct:word);
begin
  curchng (direct);          (*  Change parameters to an 'up' configuration  *)
  if col=79 then             (*  If last column...                           *)
    if newlin then           (*  ...If newlin is set...                      *)
      begin
        col:=0;              (*  ...set column to 0...                       *)
        rowinc (direct)      (*  ...and increment the row value.             *)
      end
    else                     (*  ...If not newlin...                         *)
      null                   (*  ......don't do anything.                    *)
  else                       (*  If not last column...                       *)
    col:=col+1;              (*  ...increment the column value.              *)
  curchng (direct);          (*  Reset the parameters to original config.    *)
  locate (row,col)           (*  Move the cursor to the computed location.   *)
end;

(*  This procedure causes the cursor to move up or down depending on  *)
(*    the value of direct (1=up,0=down).                              *)
procedure cursrv (direct:word);
begin
  fcursr (row,col);           (*  Determine the current cursor position.     *)
  curchng (direct);           (*  Change the direction of the parameters.    *)
  rowinc (direct);            (*  Increment(or decrement) the row value.     *)
  curchng (direct);           (*  Reset the parameter direction.             *)
  locate (row,col)            (*  Move the cursor to the computed location   *)
end;

(*  This procedure writes a character to the screen.                    *)
procedure writch (oput:char);
var
  r:integer;
  c:integer;
  bitval:integer;
begin
  fcursr (row,col);          (*  Determine the current cursot location.  *)
  r:=row;                    (*  Set the temporary row value.            *)
  c:=col;                    (*  Set the temporary column value.         *)
  for i:=2 to 4 do           (*  Perform the following 3 times.          *)
    begin
      bitval:=1;             (*  Initialize bit value.                   *)
      for j:=1 to i do       (*  Perform the following 'i' times.        *)
        bitval:=bitval*2;    (*  Make bitval equal 2 to the ith power.   *)
      case (fldatt[row,col] and wrd(bitval)) of  (*  Do one of the following *)
        4 :  null;           (*  Intensity  *)
        8 :  null;           (*  Inverse video *)
        16:  null;           (*  Security  *)
        otherwise null       (*  If not recognized, do nothing.  *)
      end
    end;
  write (oput);              (*  Write the output character to the screen.  *)
  if (not protct) and (keycall) then  (*  If writing to a non-protected     *)
                             (*  field from the keyboard only then...       *)
    fldatt[row,col]:=fldatt[row,col] or wrd(32);       (*  ...Set MDT  *)
  curpos (0);                       (*  Increment the cursor position.      *)
  if (protct) and (keycall) then    (*  If protected and from the keyboard. *)
    begin
      repeat
        curcalc;                    (*  Increment cursor position.          *)
      until ((fldatt[row,col] and wrd(64)) = 64) or (done);  (*  Loop until *)
                             (*  the first position of an unprotected field *)
                             (*  or the end of page is found.               *)
      if done then           (*  If the end of page...                      *)
        begin
          row:=r;            (*  ...reset row value to original value...    *)
          col:=c             (*  ...reset column value to original value.   *)
        end
      else                   (*  If not the end of page...                  *)
        null;                (*  ...keep current row and column values.     *)
      locate (row,col);      (*  Move the cursor to the computed location.  *)
      write (chr(7))         (*  Make the IBM beep.                         *)
    end;
(*  If not end of page and location is a field attribute then increm. cursor *)
  if ((row<>23) and (col<>79)) and (fldatt[row,col] =wrd(146)) then curpos (0)
end;

(*  This procedure displays the command menu.  *)
procedure flgmnu;
var 
  bd:integer;
begin
  locate (1,29);
  write ('IBM Personal Computer');
  locate (2,23);
  write ('OWL-1200 Emulation - Version 1.01');
  locate (5,10);
  writeln ('Flags:');
  writeln ('               A - Auto Line Feed            ',autolf);
  writeln ('               S - Scroll Enable             ',scrlpg);
  writeln ('               U - Upper Case Characters     ',ucenab);
  writeln ('               N - New Line                  ',newlin);
  locate (10,10);
  writeln ('Commands:');
  writeln ('              ^F - Exit Command Menu');
  writeln ('               I - Initialize program');
  writeln ('               R - Change Baud Rate');
  writeln ('               C - Clear the Screen');
  writeln ('               E - Exit the Program');
  case ratchr of
    '0' : bd:=110;
    '1' : bd:=150;
    '2' : bd:=300;
    '3' : bd:=600;
    '4' : bd:=1200;
    '5' : bd:=2400;
    '6' : bd:=4800;
    '7' : bd:=9600;
    otherwise bd:=0
  end;
  locate (22,10);
  write ('BAUD RATE=',bd:4);
  locate (19,10);
  write ('Enter a Command:')
end;

(*  This procedure builds a menu for SETCOM.                       *)
procedure setmnu;

var
  tmprat:char;
  bdrate:word;

  begin
    tmprat:=ratchr;           (*  Store the current baud rate.               *)
    clrscr;                   (*  Clear the screen.                          *)
    locate (1,3);
    writeln ('BAUD RATE:');
    locate (3,0);
    writeln ('     0)  110');
    writeln ('     1)  150');
    writeln ('     2)  300');
    writeln ('     3)  600');
    writeln ('     4) 1200');
    writeln ('     5) 2400');
    writeln ('     6) 4800');
    writeln ('     7) 9600');
    writeln;
    writeln;
    write ('           Select(0-7): ');
    watkey (ratchr);          (*  Input,from the keyboard, new baud rate.    *)
    if (ord(ratchr) < 48) or (ord(ratchr) > 55) then  (*  If ratchr is not   *)
                              (*  between 0 and 7...                         *)
      ratchr:=tmprat;         (*  ...don't change the baud rate.             *)
    bdrate:=wrd(ord(ratchr)-48); (*  Convert the char. to its integer value  *)
    setcom(bdrate);           (*  Set the baud rate.                         *)
    clrscr;                   (*  Clear the screen.                          *)
    flgmnu                    (*  Return to the flag menu.                   *)
  end;
    
(*  Initializes the Personal Computer.                             *)
procedure pcinit;
  begin
    initcm;                   (*  Initialize the communications port.        *)
    setcom (7);               (*  Set the baud rate to 9600.                 *)
    scrinit                   (*  Initialize the screen for input.           *)
  end;

(*  This procedure switches from the current page to the other page.  *)
procedure chpage;
begin
  if page2 then                (*  If now on page 2...                       *)
    begin     
      scroll(0,0,49,79,25,0);  (*  ...Bring page 1 up again.                 *)
      locate (pagrow,pagcol);  (*  ...Return the cursor to it's original pos.*)
      page2:=false             (*  ...Reset page flag to page 1.             *)
    end
  else                         (*  If on page 1...                           *)
    begin
      fcursr (pagrow,pagcol);  (*  ...Determine the current cursor position. *)
      scroll(0,0,49,79,25,1);  (*  ...Store page 1 on lines 25 to 49...      *)
      flgmnu;                  (*  ...Bring up the flag menu...              *)
      page2:=true              (*  ...Set page flag to page 2.               *)
     end
end;

(*  The following procedure changes the value of a boolean variable.         *)
procedure chngfl(var flag:boolean;rowpos:integer);
begin
  flag:=not flag;              (*  Set the value of FLAG to its own inverse  *)
  if page2 then                (*  If currently on the second page...        *)
    begin
      locate (rowpos,45);      (*  ...Move the cursor to the proper position *)
      era_eol_sp;              (*  ...Erase what was there...                *)
      write (flag);            (*  ...Write the value of flag to the screen..*)
      locate (19,26)
    end
end;

(*  This procedure sets the field attribute.              *)
procedure setatt(var attchr:char);
var
  curatt:word;
  tmprow:integer;
  tmpcol:integer;
  loop_cnt:integer;
begin
  curatt:=wrd(ord(attchr)) and wrd(127);  (*  Turn off the parity bit is set *)
  fcursr (row,col);                       (*  Find the cursor position       *)
  tmprow:=row;                            (*  Store the row value.           *)
  tmpcol:=col;                            (*  Store the column value.        *)
  loop_cnt:=0;                            (*  Set number of loops to 0.      *)
  repeat
    loop_cnt:=loop_cnt+1;                 (*  Increment loop counter.        *)
    fldatt[row,col]:=curatt;              (*  Set the field attribute value. *)
    if loop_cnt=2 then                    (*  If processing the second loop..*)
      if not protct then                  (*  and if not protected...        *)
        fldatt[row,col]:=fldatt[row,col] or wrd(64);  (*  ...Set blink bit.  *)

(*  NOTE - In the field attribute array, wrd(146) implies that position is a *)
(*    field attribute.  The blink bit represents the first postion of an     *)
(*    unprotected field.                                                     *)

    curcalc;                              (*  Increment the cursor position  *)
  until (fldatt[row,col]=wrd(146)) or (done); (*  loop until another field   *)
                          (*  attribute, or the end of page is found.        *)
  fldatt[tmprow,tmpcol]:=wrd(146);        (*  Call first pos. a field att.   *)
  write (' ')                             (*  Put a space on the screen.     *)
end;

(*  This procedure sets the appropriate bits in the system status byte   *)
(*    and sets appropriate bits in option1 and option2.                  *)
procedure chksys;
begin
                      (* Option Byte #1 *)
  optn1:=0;                                (*  initialize  *)
  optn1:=optn1 and wrd(127);               (*  parity  *)
  optn1:=optn1 and wrd(191);               (*  Set ETX  *)
  if newlin then                           (*  New line  *)
    optn1:=optn1 or wrd(32);
  if ucenab then                           (*  Upper Case  *)
    optn1:=optn1 or wrd(16);
  if autolf then                           (*  Auto line feed  *)
    optn1:=optn1 or wrd(8);
  if scrlpg then                           (*  Scroll  *)
    optn1:=optn1 or wrd(4);
  optn1:=optn1 or wrd(2);                  (*  Set full/partial screen  *)
  if convrs then                           (*  Set conversational mode  *)
    optn1:=optn1 or wrd(1);

                      (* Option Byte #2  *)
  optn2:=0;                                          (*  initialize  *)
  (*  parity  *)
  optn2:=optn2 or wrd(64);    (*  Set Transmission Mode (nonconversational)  *)
  optn2:=optn2 or wrd(32);
  optn2:=optn2 and wrd(239);                         (*  Set parity to  *)
  optn2:=optn2 or wrd(8);                            (*    mark.        *)
  (*  Null Suppress all = 1  *)
  optn2:=optn2 or wrd(2);                (*  Send Line Terminator Enable = 1 *)
  (*  Send Line Terminator EOT = 1, ETX = 0  *)
end;

(*  This procedure clears all unprotected fields on the screen after  *)
(*    the current cursor position.                                    *)
procedure clrunp;
var
  r:integer; (* Temporary storage for the row value.             *)
  c:integer; (* Temporary storage for the column value.          *)
begin
  fcursr (row,col);        (*  determine the current cursor location.        *)
  r:=row;                  (*  Store the row value.                          *)
  c:=col;                  (*  Store the column value.                       *)
  repeat
    if not protct then     (*  If not protected...                           *)
      begin
        locate (r,c);      (*  Move the cursor to the computed location.     *)
        write (' ');       (*  Clear that position.                          *)
        fldatt[r,c]:=0     (*  Reset the field attribute at that position.   *)
      end;
    curcalc                (*  Increment the cursor position.                *)
  until done;              (*  Loop until the end of page is found.          *)
  locate (row,col);
end;

(*  This procedure clears the current line from the current cursor  *)
(*    position to either the next field or the end of the line.     *)
procedure clrlin;
var
  tmpcol:integer;
begin
  fcursr (row,col);           (*  Determine the current cursor position.     *)
  tmpcol:=col;                (*  Store the current column position.         *)
  while tmpcol<80 do          (*  Loop until the end of line is reached.     *)
    begin
      if protct then          (*  If the current position is protected...    *)
        tmpcol:=80            (*  ...set column equal to the end of line.    *)
      else                    (*  If not protected...                        *)
        begin
          tmpcol:=tmpcol+1;   (*  ...increment the column value...           *)
          write (' ')         (*  ...Clear the position.                     *)
        end 
    end;
  locate (row,col)            (*  Move the cursor to the computed location.  *)
end;

(*  This procedure performs tabulation.  *)
procedure tabula;
var
  r:integer;
  c:integer;
begin
  fcursr (row,col);    (*  Determine the current cursor position.            *)
  r:=row;              (*  Store the row value.                              *)
  c:=col;              (*  Store the column value.                           *)
  curpos(0);           (*  Increment the cursor position.                    *)
  repeat
    curcalc            (*  Increment the cursor position.                    *)
(*  Loop until the next tabstop, unprotected field, or end of page is found  *)
  until (tabstp[col]) or ((fldatt[row,col] and wrd(64)) = 64) or (done);
  if done then         (*  If end of page...                                 *)
    begin
      row:=r;          (*  ...Reset row value to its original value...       *)
      col:=c           (*  ...Reset column value to its original value.      *)
    end;
  locate (row,col)     (*  Move the cursor to the computed location.         *)
end;                                                         

(*  This procedure locks the keyboard.  *)
procedure kblock;
begin
  fcursr (row,col);    (*  Determine the current cursor location.            *)
  locate (24,0);       (*  Move the cursor to the last line on the screen.   *)
  era_eol_sp;          (*  Erase the line.                                   *)
  write ('                               KEYBOARD LOCKED');
  keylck:=true;        (*  Set the 'locked keyboard' flag to true.           *)
  locate (row,col)     (*  Move the cursor to the computed location.         *)
end;

(*  This procedure resets the terminal to an error free state.  *)
procedure error_reset;
begin
  fcursr (row,col);    (*  Determine the current cursor location.            *)
  locate (24,0);       (*  Move the cursor to the last line on the screen.   *)
  era_eol_sp;          (*  Erase the line.                                   *)
  keylck:=false;       (*  Set the 'locked keyboard' flag to false.          *)
  locate (row,col)     (*  Move the cursor to the computed location.         *)
end;

(*  This procedure initializes the screen so there are no attribute chars.  *)
procedure clratt;
begin
  for i:=0 to 23 do      (*  Increment i from the top line to the bottom.    *)
    for j:=0 to 79 do    (*  Increment j from the first column to the last.  *)
      fldatt[i,j]:=0;    (*  Set the field attribut to 0.                    *)
  clrscr                 (*  Clear the screen.                               *)
end;

(*  This procedure clears the screen and all tab stops.  *)
procedure clrall;
begin
  clratt;                (*  Clear all attribute characters.                 *)
  error_reset;           (*  Unlock the keyboard.                            *)
  for i:=0 to 80 do      (*  Loop until i reaches the last column.           *)
    tabstp[i]:=false     (*  Clear all tab stops.                            *)
end;

(*  This procedure inserts a blank line where the cursor is when dirflg=1.  *)
(*    Deletes the line where the cursor is when dirflg=0.                   *)
procedure edline(dirwrd:word);
begin
  row:=ord(hibyte(fndcur));    (*  Find the current row value.              *)
  col:=0;                      (*  Set the column value to 0.               *)
  scroll (r1,0,23,79,1,dirwrd) (*  Scroll up or down(depending on dirwrd).  *)
end;

(*  This procedure puts the cursor on a particular row or col.  *)
(*    Note - row=1  col=0  *)
procedure curloc (dirflg:integer);
var
  mltchr:inchr;
begin
  fcursr (row,col);          (*  Determine the current cursor location.      *)
  if dirflg=1 then           (*  If the call is for a row function...        *)
    begin
      watkey (mltchr);       (*  ...Get a character from the keyboard...     *)
      row:=ord(mltchr)-32    (*  ...Convert it to an integer value.          *)
    end
  else                       (*  If the call is for a column function...     *)
    begin
      watkey (mltchr);       (*  ...Get a character from the keyboard...     *)
      col:=ord(mltchr)-32    (*  ...Convert it to an integer value.          *)
    end;
  locate (row,col)           (*  Move the cursor to the computed location.   *)
end;

(*  This procedure sets the buffer address.  *)
procedure setbuf;
var
  bufchr:char;
begin
  row:=ord(chr((getchr) and 16#007F))-32;  (*  Get the row value from host   *)
  col:=ord(chr((getchr) and 16#007F))-32;  (*  Get the column value from host*)
  locate (row,col);            (*  Move the cursor to the given location     *)
end;

(*  This procedure sends the pre-read header to the host.                    *)
procedure sndhdr;
begin
  delay_out (chr(27));
  delay_out ('R');
  delay_out (sndkey)
end;

(*  This outputs the read command header.         *)
procedure read_hdr;
begin
  fcursr (row,col);         (*  Determine the current cursor position.       *)
  delay_out(chr(1));        (*  Send a '^A' out the comm port.               *)
  delay_out(chr(row+32));   (*  Send the row value out the comm port.        *)
  delay_out(chr(col+32))    (*  Send the column value out the comm port.     *)
end;

(*  This procedure sends the unprotected data to the host.  *)
procedure readunp;
var
  scrchr:char;
  done:boolean;
begin
  row:=0;                   (*  Initialize the row value to 0.               *)
  col:=0;                   (*  Initialize the column value to 0.            *)
  repeat
    if not protct then      (*  If the position is not protected...          *)
      begin
        if (fldatt[row,col] and wrd(64)) = 64 then  (*  ...If the position is*)
                            (*    the first pos. in an unprot. field...      *)
          delay_out (chr(29));  (*  ...Send a group separator.               *)
        scrchr:=scrget(row,col);(*  Get a character from the screen.         *)
        delay_out(scrchr)   (*  Send the character out the comm port.        *)
      end;
    curcalc                 (*  Increment the cursor position.               *)
  until done;               (*  Loop until end of page is found.             *)
  delay_out (chr(3));       (*  Send a ETX to the host.                      *)
end;

(*  This procedure sends all modified data to the host.  *)
procedure readmod;
var
  scrchr:char;
begin
  read_hdr;                 (*  Output header.                               *)
  row:=0;                   (*  Initialize row value to 0.                   *)
  col:=0;                   (*  Initialize column value to 0.                *)
  repeat
    if ((fldatt[row,col] and wrd(32)) = 32) and  (*  If the pos. is modified *)
      (not protct) then     (*  and not protected.                           *)
      begin
        if (fldatt[row,col] and wrd(64)) = 64 then  (*  If pos. is first pos.*)
                            (*  in an unprotected field.                     *)
          begin
            delay_out(chr(29));     (*  Send a group separator(GS) to host.  *)
            delay_out(chr(row+32)); (*  Send the row value to the host.      *)
            delay_out(chr(col+32))  (*  Send the column value to the host.   *)
          end;
        scrchr:=scrget(row,col); (*  Get a character from the screen.        *)
        delay_out(scrchr)   (*  Send the character to the host.              *)
      end;
    curcalc                 (*  Increment the cursor position.               *)
  until done;               (*  Loop until end of page.                      *)
  delay_out(chr(3))         (*  Send a EXT to the host.                      *)
end;

(*  This procedure reads all data which appears on the screen.               *)
procedure readall;
var
  scrchr:char;
begin
  if fulscr then
    begin
      row:=0;
      col:=0
    end
  else
    fcursr (row,col);
  repeat
    scrchr:=scrget(row,col);  (*  Get a character from the screen.           *)
    delay_out(scrchr);        (*  Send the character to the host.            *)
    if newlin then
      begin
        delay_out (chr(13));
        if autolf then delay_out (chr(10))
      end;
    curcalc
  until done;
  delay_out (chr(3))
end;

(*  This procedure resets the modified data tags.                  *)
procedure re_mdt;
begin
  row:=0;               (*  Initialize the row value to 0.                   *)
  col:=0;               (*  Initialize the column value to 0.                *)
  repeat
    if not protct then  (*  If the position is not protected...              *)
      fldatt[row,col]:=fldatt[row,col] and wrd(223);  (*  ...Set bit 6 to 0. *)
    curcalc             (*  Increment the cursor position.                   *)
  until done            (*  Loop until done.                                 *)
end;

(*  This procedure prints characters to the screen.                *)
procedure host (var chrint:integer);

var
  mltchr:inchr;
  mltint:integer;
  casvar:char;
begin
  if chrint < 32 then
    case chrint of
      7 :  write (chr(chrint));
      8 :  begin                                (*  cursor left  *)
             fcursr (row,col);
             curpos(1)
           end;
      9 :  tabula;                              (*  tabulation   *)
     10 :  cursrv (0);         			(*  cursor down  *)
     11 :  cursrv (1);         			(*  cursor up    *)
     12 :  begin                                (*  cursor right *)
             fcursr (row,col);
             curpos(0)
           end;
     13 :  begin                                (*  carriage return  *)
             row:=ord(hibyte(fndcur));
             col:=0;
             if autolf then
               cursrv (0)
             else
               locate (row,col)
           end;
     14  :  graph:=true;                        (*  Alt. char. set ON.  *)
     15  :  graph:=false;                       (*  Alt. char. set OFF.  *)
     30  :  begin                               (*  Home  *)
              row:=0;
              col:=0;
              locate (row,col)
            end;
     otherwise null
    end
  else
    if mltflg then
      case chrint of
        33  :  begin                            (*  Set field attribute  *)
                 mltchr:=chr((getchr) and 16#007F);
                 setatt(mltchr)
               end;
        36  :  putchr(chr(status));             (*  Read status immediate.  *)
        37  :  putchr(chr(status));             (*  Read status when ready.  *)
        38  :  putchr(chr((optn1*256)+optn2));  (*  Read option.             *)
        40  :  kblock;                          (*  Keyboard locked  *)
        41  :  error_reset;                     (*  Unlock Keyboard  *)
        49  :  tabstp[ord(lobyte(fndcur))]:=true;(*  Set tab stop.  *)
        50  :  tabstp[ord(lobyte(fndcur))]:=false;(*  Clear tab stop.  *)
        51  :  for i:=1 to 79 do                (*  Clear all tab stops.  *)
                   tabstp[i]:=false;
        60  :  sndkey:=chr((getchr) and 16#007F);  (*  Send key override.  *)
        61  :  readall;                         (*  Read all.           *)
        62  :  readunp;                         (*  Read unprotected    *)
        63  :  readmod;                         (*  Read modified       *)
        65  :  cursrv (1);                      (*  cursor up     *)
        66  :  cursrv (0);                      (*  cursor down  *)
        67  :  begin                            (*  cursor right  *)
                 fcursr (row,col);
                 curpos(0)
               end;
        68  :  begin                            (*  cursor left  *)
                 fcursr (row,col);
                 curpos(1)
               end;
        69  :  fulscr:=true;                    (*  Set full screen mode.  *)
        70  :  fulscr:=false;                   (*  Set partial screen mode. *)
        71  :  convrs:=true;                    (*  Set conversational mode. *)
        72  :  begin                            (*  Home  *)
                 row:=0;
                 col:=0;
                 locate (row,col)
               end;
        73  :  clrlin;                          (*  Clear Line  *)
        74  :  clrunp;                          (*  Clear unprotected.  *)
        75  :  clrall;                          (*  Clear all  *)
        76  :  edline(1);                       (*  Insert line  *)
        77  :  edline(0);                       (*  Delete line  *)
        78  :  null;                            (*  Insert character  *)
        79  :  null;                            (*  Delete character  *)
        81  :  re_mdt;                          (*  Reset Mod. Data Tags  *)
        82  :  convrs:=false;                   (*  Request To Send   *)
        83  :  setbuf;                          (*  Set Buffer Address  *)
        84  :  null;                            (*  Insert cursor     *)
        85  :  null;                            (*  'Send immed. all' mode  *)
        86  :  null;                            (*  'Send immed. unpro.'  *)
        87  :  null;                            (*  Set 'send immed. mod.'  *)
        88  :  begin                            (*  position the cursor (row)*)
                 col:=ord(lobyte(fndcur));
                 mltchr:=chr((getchr) and 16#007F);
                 row:=ord(mltchr)-32;
                 locate (row,col)
               end;
        89  :  begin                            (*  Position the cursor (col)*)
                 row:=ord(hibyte(fndcur));
                 mltchr:=chr((getchr) and 16#007F);
                 col:=ord(mltchr)-32;
                 locate (row,col)
               end;
        90  :  begin                            (*  send cursor position.  *)
                 putchr (chr(row+32));
                 putchr (chr(col+32))
               end;
       otherwise
         null
      end
    else
      if graph then
        begin
(*               GRAPHICS CHARACTERS                *)
          case chrint of
            64 :  casvar:=chr(196);
            65 :  casvar:=chr(179);
            66 :  casvar:=chr(197);
            67 :  casvar:=chr(193);
            68 :  casvar:=chr(194);
            69 :  casvar:=chr(192);
            70 :  casvar:=chr(218);
            71 :  casvar:=chr(191);
            72 :  casvar:=chr(217);
            73 :  casvar:=chr(195);
            74 :  casvar:=chr(180);
            75 :  casvar:=chr(205);
            76 :  casvar:=chr(157);
            77 :  casvar:=chr(216);
            78 :  casvar:=chr(207);
            79 :  casvar:=chr(209);
            80 :  casvar:=chr(212);
            81 :  casvar:=chr(213);
            82 :  casvar:=chr(184);
            83 :  casvar:=chr(190);
            84 :  casvar:=chr(198);
            85 :  casvar:=chr(181);
            otherwise                  (*  In case of non-implemented char.  *)
              casvar:=chr(15);         (*    Output 'Snowflake'.             *)
          end;
          writch (casvar)              (*  Write character to the screen.    *)
        end
      else
        writch (chr(chrint));          (*  Write the character to the screen *)
  mltflg:=false
end;

(*  This procedure prepares characters for output to the host.     *)
procedure keybrd (var chrint:integer);

var
  mltchr:inchr;
  mltint:integer;
  casvar:char;

begin
  if (ucenab) and (not functn) then (*  Make lower case letters upper case.  *)
    if (chrint>95) and (chrint<127) then
      chrint:=chrint-32;
  if mltflg then
    case chrint of
      33 :  begin                             (*  Set field attribute  *)
              watkey(mltchr);
              setatt(mltchr)
            end;
      36 :  putchr(chr(status));              (*  Read status immediate  *)
      37 :  putchr(chr(status));              (*  Read status when ready  *)
      40 :  kblock;                           (*  Keyboard locked  *)
      41 :  error_reset;                      (*  Unlock Keyboard  *)
      49 :  tabstp[ord(lobyte(fndcur))]:=true;(*  Set tab stop.  *)
      50 :  tabstp[ord(lobyte(fndcur))]:=false;(*  Clear tab stop.  *)
      51 :  for i:=1 to 79 do                 (*  Clear all tab stops.  *)
                tabstp[i]:=false;
      60 :  watkey (sndkey);                  (*  Send key override        *)
      61 :  readall;                          (*  Read all                 *)
      62 :  readunp;                          (*  read Unprotected         *)
      63 :  readmod;                          (*  Read Modified            *)
      65 :  putchr (chr(11));                 (*  cursor up     *)
      66 :  putchr (chr(10));                 (*  cursor down   *)
      67 :  putchr (chr(12));                 (*  cursor right  *)
      68 :  putchr (chr(8));                  (*  cursor left   *)
      69 :  fulscr:=true;                     (*  Set Full Screen Mode     *)
      70 :  fulscr:=false;                    (*  Set Partial Screen Mode  *)
      71 :  convrs:=true;                     (*  Set Conversational Mode  *)
      72 :  putchr (chr(30));                 (*  Home  *)
      73 :  clrlin;                           (*  Clear Line  *) 
      74 :  clrunp;                           (*  Clear Unprotected    *)
      75 :  clratt;                           (*  Clear all  *)
      76 :  edline(1);                        (*  Insert line  *)
      77 :  edline(0);                        (*  Delete line  *)
      78 :  null;                             (*  insert character       *)
      79 :  null;                             (*  delete character       *)
      81 :  re_mdt;                           (*  Reset Modified Data Tags *)
      82 :  convrs:=false;                    (*  Request to send        *)
      85 :  null;                             (*  Set 'send immediate all  *)
      86 :  null;                             (*  Set 'send immed. unpro.' *)
      87 :  null;                             (*  Set 'send immed. mod.'   *)
      88 :  curloc(0);                        (*  Position cursor(col).  *)
      89 :  curloc(1);                        (*  Position cursor(col).  *)
      otherwise
        null
    end
  else
    if functn then
      begin
        if ((chrint>58) and (chrint<69)) or
           ((chrint>83) and (chrint<100)) or
           ((chrint>103) and (chrint<110)) then
          begin
            delay_out (chr(27));
            delay_out ('R')
          end;
        case chrint of
(*  OWL-1200  -  F1-F16 (NON-SHIFTED)  *)
          59..68 :    casvar:=chr(chrint+6);    (*  Function keys 1 - 10   *)
          94..99 :    casvar:=chr(chrint-19);   (*  Function keys 11 - 16  *)
(*  OWL-1200  -  F1-F16 (SHIFTED)  *)
          84..93 :    casvar:=chr(chrint+13);   (*  Function keys 17 - 26  *)
          104..109 :  casvar:=chr(chrint+3);    (*  Function keys 27 - 32  *)
(*  IBM Cursor control keys  *)
          71 :  casvar:=chr(30);                (*  Home  *)
          72 :  casvar:=chr(11);                (*  Cursor up  *)
          75 :  casvar:=chr(8);                 (*  Cursor left  *)
          77 :  casvar:=chr(12);                (*  Cursor right  *)
          80 :  casvar:=chr(10);                (*  Cursor down  *)
          otherwise
            casvar:=chr(0);
        end;
        if ord(casvar)>0 then
          delay_out (casvar)                    (*  Output selected case  *)
        else
          null
      end
    else
      if (not convrs) then
        begin
          keycall:=true;
          host (chrint);
          keycall:=false
        end
      else
        putchr (chr(chrint));
  mltflg:=false
end;

(*  This procedure initializes all variables of the program, sets tab    *)
(*    stops and clears the screen.                                       *)
procedure prog_init;
begin
  clrscr ;                       (*  Clear the screen  *)
  ratchr:='7';                   (*  Set baud rate to 9600 *)
  status:=0;                     (*  System status is always OK.  *)
  sndkey:='1';                    (*  Set send key to send page.  *)

(*  Initialize flags  *)

  mltflg:=false;          (*  MULTICODE flag             *)
  autolf:=false;          (*  Automatic line feed        *)
  ucenab:=false;          (*  Upper case enable/disable  *)
  page2:=false;           (*  Page flag.                 *)
  newlin:=true;           (*  New line enable/disable    *)
  scrlpg:=true;           (*  Scroll enable/disable      *)
  functn:=false;          (*  Function key flag          *)
  convrs:=true;           (*  Set conversational mode.   *)
  graph :=false;          (*  Graphics character set.    *)
  fulscr:=true;           (*  Set full screen mode.      *)

(*  Initialize the field attribute array.  *)
  for i:=0 to 23 do
    for j:=0 to 79 do
      fldatt[i,j]:=0;

(*  Set default tab stops every 8 spaces.  *)
  for i:=0 to 9 do
    for j:=0 to 7 do
      begin
        idx:=(8*i)+j;
        if j=7 then
          tabstp[idx]:=true
        else
          tabstp[idx]:=false
      end
end;

(**************************** MAIN PROCEDURE *****************************)

begin

                       (*  PROGRAM INITIALIZATION  *)

(*  Initialize the personal computer(i.e.--comm. ports and buffers)  *)
      pcinit;
50:
(*  Initialize variables, tabs stops and screen.  *)
      prog_init;

                        (*  COMMAND PROCESSING LOOP  *)
  while true do
    begin

(*  Check system status and set the status and option bytes accordingly.  *)
      chksys;

(*  Get a character from the COMM port and store it in 'hstchr'.  *)
60:
      hstchr:=chr((getchr) and 16#007F);
      hstint:=ord(hstchr);

(*  Test for an incoming command.  *)
      if hstint > 0 then 
        if (hstint=27) and (not mltflg) then
          begin
            mltflg:=true;
            goto 60
          end
        else
          host (hstint);

(*  Get a character from the keyboard  *)
70:
      getkey(keychr);
      keyint:=ord(keychr);
  
      if keyint = 6 then chpage;

(*  Test for an incoming character and keyboard status.  *)
      if page2 then
        while page2 do
          begin
            watkey(keychr);
            keyint:=ord(keychr);
            if (keyint>96) and (keyint<123) then
              keyint:=keyint-32;
            locate (20,5);
            era_eol_sp;
            case keyint of
               6 :  chpage;            (*  ^F - Change Page.      *)
              65 :  chngfl (autolf,6); (*  A - Auto Line Feed.    *)
              67 :  begin              (*  C - Clear screen.      *)
                      chpage;
                      clrscr;
                      chpage
                    end;
              69 :  goto 100;          (*  E - Exit the program.  *)
              73 :  goto 50;           (*  I - Re-Initialize.     *)
              78 :  chngfl (newlin,9); (*  N - New Line Enable.   *)
              82 :  setmnu;            (*  R - Baud rate menu.    *)
              83 :  chngfl (scrlpg,7); (*  S - Scroll.            *)
              85 :  chngfl (ucenab,8); (*  U - U/C Enable.        *)
              otherwise 
                begin
                  locate (20,5);
                  write ('Illegal choice, try again:');
                  locate (19,26);
                  era_eol_sp
                end
            end
          end
        else
          if (keyint > 0) and (not keylck) then
            if (keyint=27) and (not mltflg) then
              begin
                mltflg:=true;
                goto 70
              end
            else
              keybrd (keyint)               (*  Send 'keychr' to the host  *)
    end;
100:
clrint;				{ Disable interupts on communications port. }
clrscr
end.

