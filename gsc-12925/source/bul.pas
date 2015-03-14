  { $INCLUDE : 'compile.inc' }
  { $INCLUDE : 'vbpas.int' }
  { $INCLUDE : 'option.int' }
  { $INCLUDE : 'getparam.int' }
  { $INCLUDE : 'utility.int' }
  { $INCLUDE : 'sfpas.int' }
  { $INCLUDE : 'dialog.int' }
  { $INCLUDE : 'bulalts.int' }
  { $INCLUDE : 'ldb.int' }
  { $INCLUDE : 'execute.int' }
  { $INCLUDE : 'dspas.int' }
  { $INCLUDE : 'bulaltd.int' }
  { $INCLUDE : 'display.int' }
  { $INCLUDE : 'bulexec.int' }
  { $INCLUDE : 'sldetc.int' }

  program bul ( input, output );

     USES vbpas;

     USES option;

     USES getparam;

     USES utility;

     USES sfpas;

     USES bulalts;

     USES dialog;

     USES ldb;

     USES execute;

     USES dspas;

     USES bulaltd;

     USES display;

     USES bulexec;

     USES sldetc;

  var
      pname	       : lstring(14);
      length	       : byte;
      inkey	       : byte;
      current_node     : byte;
      current, first   : entity;
      status	       : mode;
      buffer	       : lstring(127);
      buf, msg1, msg2  : lstring(80);
      i, k	       : byte;
      j 	       : integer;


  (********************************************************)

  procedure bul_initialize;

  begin
    draw_count := 1;
    dscreen := true;
    saved := true;
    add2     := false;
    destroy := false;
    last_ins := 0;
    skipfield := [8];
    msg1 := null;
    msg2 := null;
    rtype := 2;
    first_line := node_data^[6].ytext;
    last_line  := node_data^[4].ytext;
    if last_line > node_data^[3].ytext	then
	last_line := node_data^[3].ytext;
    last_line  := last_line - 1;
    last_line := 21;
    sline := 0;
    maxpix := 0;
    top_page := nil;
    bottom_page := nil;
    if name <> null then  (* draw sample/chosen chart *)
      begin
	current := first^.up;
	for j := 1 to 10  do
	  skeleton^[j] := ord(current^.field^[j]) - 48;
	enter ( current, first);
	screen (2);
	colors (0,skeleton^[10]);
	there (0,0);
	write ('Drawing in progress...');
	redraw (1, current, first );
      end
     else (* goto format menu if not sample chart *)
      begin
	current := first^.down^.down;
	while current<> first do
	  if current^.etype in [6..8] then
	    delete ( current )
	   else current := current^.down;
	format ( inkey, current, first );
      end;
    if ( not update ) and ( name <> null ) and (not (inkey in [4,5])) then  (* reset if sample chart drawn *)
      begin
	while first <> nil  do	delete (first);
	initialize (first);
	name := null;
	current := first^.down^.down;
	while current<> first do
	  if current^.etype in [6..8] then
	    delete ( current )
	   else current := current^.down;
	msg1 := messages^[19];
	msg2 := messages^[20];
	top_page := nil;
	bottom_page := nil;
      end;
    promptupd := 1;
    first_item := first^.down^.down^.down^.down^.down;
    if first_item^.etype <> 6 then first_item := nil;
  end;

  (********************************************************)

  procedure bul_update;

  begin
    if msg1 = messages^[19]  then
       begin
	 if inkey in [4,10]  then  inkey := 255;
	 if inkey in [7,41]  then
	   begin
	     msg1 := null;
	     msg2 := null;
	   end;
       end;
	     if (draw_count = 1) and (msg1 <> messages^[19])  then
		msg2 := messages^[33];
	     if (draw_count = 0) and (msg1 <> messages^[19])  then
		msg2 := null;

    if (status <> txt) or (inkey <> 0)	then
     begin
      if inkey in [1,2,3,4,7,41]  then	draw_count := 0;
      if inkey = 8  then  draw_count := 1;
     end;
    if ((status=txt)and(length<=0)and(inkey=0))or(inkey in [22,24,27,29)  then
      if (draw_count = 1)and(msg1<>messages^[19]) then
	begin
	  case inkey of
	    22 : top_page := nil;
	    24 : begin
		   paging ( 1, j, first );
		   j := j-1;
		   paging ( 2, j, first );
		 end;
	    27 : begin
		   top_page := first;
		   while not (top_page^.up^.etype in [5..7]) do
		     top_page := top_page^.up;
		   paging ( 1, j, first );
		   paging ( 2, j, first );
		 end;
	    otherwise
	    end;
	  screen(2); colors (0,skeleton^[10]);
	  there (0,0);
	  writeln ('Drawing next page...');
	  redraw ( 1, current, first );
	  inkey := 255;
	end;
    if inkey = 0 then inkey := 255;
  end;

  (********************************************************)

  begin

     pname := 'bul.ctl';
     gtype := 'bul';
     pltype := 'sld';
     cname := 'Bullet';
     sldtype := 'sldbul';

     get_equip;
     get_parameter ( pname );


   repeat
     select;
     if not endflag then
     begin

       dsinit;
       if name <> null then load_chart ( name, first )
		 else initialize ( first );

       inkey := 0;

       bul_initialize;

       current := first;
       current_node  := current^.etype;
	 while inkey <> 1 do
	   begin
	     inkey := 0;

	     incom ( 22, msg1, msg2, -2, rtype, 0, 0, 0, 4, 0, 1, 1,
		     1, 1, 1, 1, 1, status, buffer, length, inkey );

	     bul_update;

	     perform2 ( inkey, current, current_node, first );
	     perform  ( inkey, current, current_node, first );
	   end;
    end;
    until endflag;
  end.
