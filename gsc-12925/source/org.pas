  { $INCLUDE : 'compile.inc' }
  { $INCLUDE : 'vbpas.int' }
  { $INCLUDE : 'option.int' }
  { $INCLUDE : 'getparam.int' }
  { $INCLUDE : 'utility.int' }
  { $INCLUDE : 'sfpas.int' }
  { $INCLUDE : 'dialog.int' }
  { $INCLUDE : 'ldb.int' }
  { $INCLUDE : 'execute.int' }
  { $INCLUDE : 'dspas.int' }
  { $INCLUDE : 'sldetc.int' }
  { $INCLUDE : 'orgalts.int' }
  { $INCLUDE : 'orgaltd.int' }
  { $INCLUDE : 'orgexec.int' }

  program org ( input, output );

     USES vbpas;

     USES option;

     USES getparam;

     USES utility;

     USES sfpas;

     USES dialog;

     USES ldb;

     USES execute;

     USES dspas;

     USES sldetc;

     USES orgalts;

     USES orgaltd;

     USES orgexec;

  var
      pname	: lstring(14);
      length	: byte;
      inkey	       : byte;
      current_node     : byte;
      current, first   : entity;
      status	       : mode;
      buffer	       : lstring(127);
      reply	       : char;
      i, k	       : byte;
      msg1,msg2,buf    : lstring(80);

  (********************************************************)

  procedure org_initialize;

    procedure init;
       begin
	 current := first^.down^.down;
	 while current <> first do
	    if current^.etype in [7,9,13,17,21] then delete ( current )
					 else current := current^.down;

       end;
  begin
    skipfield := [21];
    add2 := false;
    destroy := false;
    dimension[-1] := 0;
    last_ins := 0;
    dimension[0] := 0;
    color := skeleton^[10];
    for i := 1 to upper ( dimension ) do dimension[i] := -1;
    current_level := 0;
    current_sub_level := 0;
    first_line := node_data^[7].ytext;
    leftnode := 1;   lnode := 1;
    icol := 0;
    topnode := 1;    tnode := 1;
    irow := 0;
    nmids := 3;
    nlows := 3;
{   last_line  := node_data^[2].ytext;
    if last_line > node_data^[4].ytext then last_line := node_data^[4].ytext;}
    last_line := 21;
    delkey := true;
    saved := true;
    rtype := 2;
    if name <> null then
      begin
	current := first^.up;
	skeleton^[8]:=ord(current^.field^[1])-48;
	skeleton^[9]:=ord(current^.field^[2])-48;
	skeleton^[10]:=ord(current^.field^[3])-48;
	delete ( current );
      end;
    if name = null then
      begin
	init;
	 imids := 0;
	 istfs := 0;
	 format ( inkey, current, first );
      end
	 else
       begin
	 current := first^.down;
	 while not (current^.etype in [1,21] do
	   begin
	     case current^.etype of
	       7: dimension[-1] := dimension[-1] + 1;
	       9: dimension[0]	:= dimension[0] + 1;
	      13: begin
		    current_level := current_level + 1;
		    dimension[current_level] := 0;
		  end;
	      17: dimension[current_level]:=dimension[current_level]+1;
	     otherwise
	     end;
	     current := current^.down;
	   end;
	 imids := current_level;
	 istfs := dimension[0];
	 if imids > skeleton^[8] then skeleton^[8] := imids;
	 if istfs > skeleton^[8] then skeleton^[8] := istfs;
	 for i := 1 to 12 do
	   node_data^[8+i].input_length := skeleton^[10+skeleton^[8]]
	       -ord((i<5)or(i>8))+4*ord((i mod 4)=0);
	 nmids := 81 div (node_data^[16].input_length + 1);
	 current_level := 0;
       end;
    first_item := first^.down^.down^.down^.down;
    msg1 := null;
    msg2 := null;
    promptupd := 2;
    if (name <> null) and (not (inkey in [4,5]))  then
      begin
	alter_display_space ( first );
	reduce ( 2, skeleton^[10] );
	rtype := 2;
      end
    if (not update) and (name <> null) and (not (inkey in [4,5])) then
      begin
	while first <> nil do delete ( first );
	initialize ( first );
	init;
	msg1 := messages^[19];
	msg2 := messages^[20];
	imids := 0;
	istfs := 0;
	nmids := 3;
	add2 := false;
	destroy := false;
	last_ins := 0;
	for i := 1 to 10 do dimension[i] := -1;
	dimension[0] := 0;
	dimension[-1] := 0;
	current_level := 0;
	current_sub_level := 0;
	leftnode := 1;	 lnode := 1;
	icol := 0;
	topnode := 1;	     tnode := 1;
	irow := 0;
	promptupd := 1;
      end;
  end;

  (********************************************************)

  procedure org_update;

  begin
    if msg1 = messages^[19] then
      begin
	if inkey in [4,10,43] then
	  inkey := 0
	if inkey in [7,41] then
	  begin
	    msg1 := null;
	    msg2 := null;
	  end;
      end;
  end;

  (********************************************************)

  begin


     pname := 'org.ctl';
     gtype := 'org';
     pltype := 'sld';
     cname := 'Organization';
     sldtype := 'sldorg' ;

     get_equip;
     get_parameter ( pname );

   repeat
     select;
     if not endflag then
     begin

       dsinit;
       if name <> null then load_chart ( name, first )
		 else initialize ( first );
	inkey	:= 0;

	org_initialize;

	current := first;
	current_node  := current^.etype;
	  while inkey <> 1 do
	    begin
	      inkey := 0;
	      incom ( 22, msg1, msg2, -2, rtype, 0, 0, 0, 4, 0, 1, 1,
		      1, 1, 1, 1, 1, status, buffer, length, inkey );

	      org_update;

	      perform2 ( inkey, current, current_node, first );
	      perform  ( inkey, current, current_node, first )
	    end;
    end;
    until endflag;
  end.
