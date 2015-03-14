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
  { $INCLUDE : 'sldetc.int' }
  { $INCLUDE : 'nsqdraw.int' }
  { $INCLUDE : 'nsqexec.int' }
  { $INCLUDE : 'nsqetc.int' }
  { $INCLUDE : 'nsqalts.int' }

  program nsqchart ( input, output );

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

     USES nsqdraw;

     USES nsqexec;

     USES nsqetc;

     USES nsqalts;

  var
      pname	       : lstring(14);
      length	       : byte;
      inkey	       : byte;
      current_node     : byte;
      current, first   : entity;
      status	       : mode;
      buffer	       : lstring(127);
      reply	       : char;
      i, k	       : byte;
      buf,msg1,msg2    : lstring(80);


  (********************************************************)

  procedure nsq_initialize;

  begin
    saved := true;
    skipfield := [18];
    color1 := 3;
    leftnode := 1;   lnode := 1;
    icol := 0;
    topnode := 1;    tnode := 1;
    irow := 0;
    nmids := 3;
    nlows := 3;
    first_line := node_data^[2].ytext;
    last_line := 17;
    delkey := true;
     if name <> null then
       begin
	 current := first^.up;
	 with current^ do
	   begin
	     funcnum := (ord (field^[1]) - 48) * 10 + ord (field^[2]) - 48;
	     form := ord (field^[3]) - 48;
	     color := ord (field^[4]) - 48;
	     symbol := ord (field^[5]) - 48;
	   end;
	 delete ( current );
       end
      else
       begin
	 funcnum :=  3;
	 form := 2;
	 color := 1;
	 symbol := 0;
	 boxsizes;
	 elist ( first );
	 format ( inkey, current, first );
       end;
    msg1 := null;
    msg2 := null;
    promptupd := 2;
    boxsizes;
    if (name <> null) and (not (inkey in [4,5])) then
      begin
	draw2 ( first );
	reduce ( 2, skeleton^[1] );
	rtype := 2;
      end;
    if (not update) and (name <> null) and (not (inkey in [4,5])) then
      begin
	while first <> nil do delete ( first );
	initialize ( first );
	elist ( first );
	funcnum := 3;
	imids := 0;
	nmids := 3;
	nlows := 3;
	msg1 := messages^[19];
	msg2 := messages^[20];
	leftnode := 1;	 lnode := 1;
	icol := 0;
	topnode := 1;	     tnode := 1;
	irow := 0;
	promptupd := 1;
      end;
  end;


  procedure nsq_update;

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


     pname   := 'nsq.ctl';
     gtype   := 'nsq';
     pltype  := 'sld';
     cname   := 'N-Square';
     sldtype := 'sldnsq';

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

	nsq_initialize;

	current := first;
	current_node  := current^.etype;
	  while inkey <> 1 do
	    begin
	      inkey := 0;
	      incom ( 22, msg1, msg2, -2, rtype, 0, 0, 0, 4, 0, 1, 1,
		      1, 1, 1, 1, 1, status, buffer, length, inkey );

	      nsq_update;

	      perform2 (first,current,current_node,inkey );
	      perform  ( inkey, current, current_node, first ) ;
	    end;
    end;
    until endflag;
  end.
