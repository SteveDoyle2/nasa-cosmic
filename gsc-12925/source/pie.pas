  { $INCLUDE : 'compile.inc' }
  { $INCLUDE : 'vbpas.int' }
  { $INCLUDE : 'option.int' }
  { $INCLUDE : 'getparam.int' }
  { $INCLUDE : 'utility.int' }
  { $INCLUDE : 'sfpas.int' }
  { $INCLUDE : 'dialog.int' }
  { $INCLUDE : 'ldb.int'     }
  { $INCLUDE : 'execute.int' }
  { $INCLUDE : 'dspas.int' }
  { $INCLUDE : 'piealtd.int' }
  { $INCLUDE : 'pieexec.int' }
  { $INCLUDE : 'sldetc.int' }

  program pie ( input, output );

     USES vbpas;

     USES option;

     USES getparam;

     USES utility;

     USES sfpas;

     USES dialog;

     USES ldb;

     USES execute;

     USES dspas;

     USES piealtd;

     USES pieexec;

     USES sldetc;

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
      buf,msg1,msg2   : lstring(80);


  (********************************************************)

  procedure pie_initialize;

  var

      i : byte;
   k, m : integer;

    procedure init;

      var
	i : integer;

      begin
	 current := first^.up;
	 delete ( current );
	 current := first;
	 if form=2 then
	    begin
	       k := 9 ;
	       m := 12 ;
	    end ;
	 else
	    begin
	       k := 12 ;
	       m := 9 ;
	       while current^.etype <> 15 do current := current^.down;
	       delete ( current ) ;
	    end ;

	 while current^.etype <> k do current := current^.down;
	 delete ( current ) ;
	 while current^.etype <> m+2 do current := current^.down;

	 for i := 1 to 9 do
	     begin
	       add_node ( current, m );
	       current^.y := current^.up^.y + 16;
	       if i = 5 then current^.ytext:=node_data^[m].ytext
		 else current^.ytext:=current^.up^.ytext+1;
	       if i>4 then current^.xtext:=node_data^[m].xtext+40;
	       current := current^.down;
	       current^.y := current^.up^.y;
	       current^.ytext := current^.up^.ytext;
	       if i>4 then current^.xtext:=node_data^[m+1].xtext+40;
	       current := current^.down;
	       current^.y := current^.up^.y + 8;
	       current^.ytext := current^.up^.ytext+1;
	       if i>4 then current^.xtext:=node_data^[m+2].xtext+40;
	     end;

	 if form=2 then current := current^.down^.down ;
	 else current := current^.down;
	 for i := 1 to 2 do
	     begin
	       add_node ( current, 16 );
	       current^.y := current^.up^.y + 10;
	       current^.ytext := current^.up^.ytext+1;
	     end;
      end;

  begin
    saved := true;
    erro := false ;
    skipfield := [17];
    rtype := 2;
    color := skeleton^[1];
    if name = null then
       begin
	 form := 1 ;
	 symbol := 0 ;
	 color := 1 ;
	 init;
	 format(inkey,current,first);
       end;
    else
       begin
	  current := first^.up ;
	  with current^ do
	     begin
		form := ord ( field^[1] ) - 48	;
		symbol := ord ( field^[2] ) - 48  ;
		color := ord ( field^[3] ) - 48 ;
	     end ;
	  delete ( current ) ;
       end ;

    msg1 := null;
    msg2 := null;
    promptupd := 2;
    if (name <> null) and (not (inkey in [4,5]))  then
      begin
	alter_display_space ( first ) ;
	reduce ( 2, skeleton^[1] );
	rtype := 2;
      end;
    if (not update) and (name <> null) and (not (inkey in [4,5])) then
      begin
	while first <> nil do delete ( first );
	initialize ( first );
	init;
	msg1 := messages^[19];
	msg2 := messages^[20];
	promptupd := 1;
      end;
  end;

  (********************************************************)

  procedure pie_update;

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

     pname := 'pie.ctl';
     gtype := 'pie';
     pltype := 'sld';
     cname := 'Pie';
     sldtype := 'sldpie';

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

	pie_initialize;

	current := first;
	current_node  := current^.etype;
	  while inkey <> 1 do
	    begin
	      inkey := 0;
	      incom ( 22, msg1, msg2, -2, rtype, 0, 0, 0, 4, 0, 1, 1,
		      1, 1, 1, 1, 1, status, buffer, length, inkey );

	      pie_update;

	      perform2 ( inkey, current, current_node, first );
	      perform  ( inkey, current, current_node, first )
	    end;
    end
    until endflag;
  end.
