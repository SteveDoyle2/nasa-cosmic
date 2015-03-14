  { $INCLUDE : 'compile.inc' }
  { $INCLUDE : 'vbpas.int' }
  { $INCLUDE : 'option.int' }
  { $INCLUDE : 'getparam.int' }
  { $INCLUDE : 'utility.int' }
  { $INCLUDE : 'sfpas.int' }
  { $INCLUDE : 'dialog.int' }
  { $INCLUDE : 'wbsalts.int' }
  { $INCLUDE : 'ldb.int'     }
  { $INCLUDE : 'execute.int' }
  { $INCLUDE : 'dspas.int' }
  { $INCLUDE : 'wbsaltd.int' }
  { $INCLUDE : 'wbsexec.int' }
  { $INCLUDE : 'sldetc.int' }

  program wbs ( input, output );

     USES vbpas;

     USES option;

     USES getparam;

     USES utility;

     USES sfpas;

     USES dialog;

     USES wbsalts;

     USES ldb;

     USES execute;

     USES dspas;

     USES wbsaltd;

     USES wbsexec;

     USES sldetc;

  var
      pname            : lstring(14);
      length           : byte;
      inkey            : byte;
      current_node     : byte;
      current, first   : entity;
      status           : mode;
      buffer           : lstring(127);
      reply            : char;
      i, k             : byte;
      buf,msg1,msg2    : lstring(80);


  (********************************************************)

  procedure wbs_initialize;

    procedure init;

      begin
        current := first^.down^.down;
        while current <> first do
          if current^.etype in [6,8,10] then delete ( current )
            else current := current^.down;
      end;

  begin
    skipfield := [10];
    color := 1;
    saved := true;
    rtype := 2;
    add2 := false;
    destroy := false;
    last_ins := 0;
    for i := 1 to 10 do dimension[i] := -1;
    current_level := 0;
    current_sub_level := 0;
    leftnode := 1;   lnode := 1;
    icol := 0;
    topnode := 1;    tnode := 1;
    irow := 0;
    nmids := 3;
    first_line := node_data^[6].ytext;
    last_line  := node_data^[3].ytext;
    if last_line > node_data^[4].ytext then last_line := node_data^[4].ytext;
    last_line := last_line - 2;
    last_line := 21;
    delkey := true;
    if name <> null then
      begin
        current := first^.up;
        if current^.field^[1] = ' ' then skeleton^[5] := ord(current^.field^[2])-48
                           else skeleton^[5] := 10;
        if current^.field^[3] = ' ' then skeleton^[6] := ord(current^.field^[4])-48
                           else skeleton^[6] := 10;
        for i := 3 to 4 do
         skeleton^[i+4] := ord ( current^.field^[i+2] ) - 48;
        skeleton^[19] := ord ( current^.field^[7] ) - 48;
        for i := 1 to 3 do
          begin
            if current^.field^[i*2+6] = ' ' then
              skeleton^[i+19] := ord ( current^.field^[i*2+7] ) -48
             else skeleton^[i+19] := 10;
          end;
        delete ( current );
      end;
    if name = null then
       begin
         init;
         imids := 0;
         format ( inkey, current, first );
       end
         else
       begin
         current := first^.down^.down^.down^.down^.down;
         while not (current^.etype in [1,10]) do
           begin
             case current^.etype of
               6: begin
                    current_level := current_level + 1;
                    dimension[current_level] := 0;
                  end;
               8: begin
                  dimension[current_level]:=dimension[current_level]+1;
                  end;
             otherwise
             end;
             current := current^.down;
           end;
         imids := current_level;
         if imids > skeleton^[5] then skeleton^[5] := imids;
         for i := 1 to 4 do
           node_data^[5+i].input_length := skeleton^[8+skeleton^[5]]
                                          -((i-1) div 2);
         nmids := 84 div (node_data^[6].input_length + 4);
         current_level := 0;
       end;
    msg1 := null;
    msg2 := null;
    promptupd := 2;
    if (name <> null) and (not (inkey in [4,5]))  then
      begin
        alter_display_space ( first );
        reduce ( 2, skeleton^[1] );
        rtype := 2;
      end;
    if (not update) and (name <> null) and (not (inkey in [4,5])) then
      begin
        while first <> nil do delete ( first );
        initialize ( first );
        init;
        imids := 0;
        nmids := 3;
        msg1 := messages^[19];
        msg2 := messages^[20];
        add2 := false;
        destroy := false;
        last_ins := 0;
        for i := 1 to 10 do dimension[i] := -1;
        current_level := 0;
        current_sub_level := 0;
        leftnode := 1;   lnode := 1;
        icol := 0;
        topnode := 1;        tnode := 1;
        irow := 0;
        promptupd := 1;
      end;
  end;

  (********************************************************)

  procedure wbs_update;

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


     pname := 'wbs.ctl';
     gtype := 'wbs';
     pltype := 'sld';
     cname := 'Work Breakdown Structure';
     sldtype := 'sldwbs';

     get_equip;
     get_parameter ( pname );

   repeat
     select;
     if not endflag then
     begin

       dsinit;
       if name <> null then load_chart ( name, first )
                 else initialize ( first );
        inkey   := 0;

        wbs_initialize;

        current := first;
        current_node  := current^.etype;
          while inkey <> 1 do
            begin
              inkey := 0;
              incom ( 22, msg1, msg2, -2, rtype, 0, 0, 0, 4, 0, 1, 1,
                      1, 1, 1, 1, 1, status, buffer, length, inkey );

              wbs_update;

              perform2 ( inkey, current, current_node, first );
              perform  ( inkey, current, current_node, first )
            end;
    end;
    until endflag;
  end.
