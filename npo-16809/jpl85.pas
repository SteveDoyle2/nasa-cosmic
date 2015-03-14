








(****************************************************************************)
(*                                                                          *)
(*             OPTIMAL NETWORK TOPOLOGY DESIGN  (BASIC VERSION)             *)
(*                              December  1984                              *)
(*                                                                          *)
(* This program accepts a set of communication link elements (together with *)
(* the stations they connect), and outputs candidate topologies (those that *)
(* connect all the stations) in increasing order of their total costs.      *)
(*                                                                          *)
(****************************************************************************)
 

program JPL84 (input, output, fin, fout);

label 99;

const

  N = 5;  (* This is the total number of stations in the network. *)

type

  stations = array[1..N] of integer;

  vector   = array[0..N] of integer;

  station_link = ^station;

  station =
    record
      index   : integer;
      next    : station_link
    end;

  element_link = ^element;

  element = 
    record
      index        : integer;
      weight       : real;
      connectivity : integer;
      stations     : station_link;
      next         : element_link
    end;

  list_link = ^list_data;
 
  list_data =
    record
      content_source : list_link;
      size           : integer;
      weight         : real;
      connection     : stations;
      next           : list_link;
      end_element    : element_link
    end;

  next_list_link = ^next_list_data;

  next_list_data =
    record
      size           : integer;
      weight         : real;
      connection     : stations;
      new_element    : element_link;
      source         : list_link;
      content_source : list_link
    end;

  node_link = ^tree_node;

  tree_node =
    record
      parent         : node_link;
      left_child     : node_link;
      right_child    : node_link;
      left_neighbor  : node_link;
      right_neighbor : node_link;
      content        : next_list_link
    end;


var
  fin, fout                                                : text;
  answer, answer2, answer3, connected                      : char;
  size, index, count, count1                               : integer;       
  v                                                        : vector;
  se, sce                                                  : station_link;
  element_base, e, ce                                      : element_link;
  list_base, print_base, list_end, list_buffer, pp, pq, op : list_link;
  next_list_buffer, np                                     : next_list_link;
  node_base, tree_end, parent, node_buffer, p              : node_link;


procedure INSTRUCTIONS;
  begin 
    writeln(' ');
    writeln('The number of stations has been fixed to be', N:3, '.');
    writeln('This number is determined by the value N in the const');
    writeln('statement at the beginning of this program, and can be');
    writeln('changed by editing that part of the program alone.');
    writeln(' ');
    writeln('User has to enter the total number of link elements,');
    writeln('then enter their weights in INCREASING order, together');
    writeln('with the stations that they connect.  A link element is');
    writeln('allowed to connect more than two stations.');
    writeln(' ')
  end;


(********************************************************)
(* This procedure accepts input data from the terminal. *)
(********************************************************)
procedure INPUT_DATA_TERMINAL;
  begin
    writeln(' ');
    write('How many link elements do you have ? (must be greater than 1)  ');
    readln(size);
    writeln(' ');
    new(element_base);
    element_base^.index := 0;
    element_base^.weight := 0;
    ce := element_base;
    for index := 1 to size do
      begin
        new(e);
	e^.index := index;
        writeln(' ');
	write('Enter weight of element #', index:3, '     ');
	readln(e^.weight);
        write('How many stations are connected together by this element ? ');
        readln(e^.connectivity);
        new(e^.stations);
        write('Enter identity of station #  1 : ');
        readln(e^.stations^.index);
        e^.stations^.next := nil;
        sce := e^.stations;
        for count := 2 to e^.connectivity do
          begin
            new(se);
            write('Enter identity of station #', count:3, ' : ');
            readln(se^.index);
            se^.next := nil;
            sce^.next := se;
            sce := se
          end;
	e^.next := nil;
	ce^.next := e;
	ce := e
      end
  end;


procedure INITIALIZE;
  begin

    new(list_base);
    list_base^.content_source        := nil;
    list_base^.size                  := 0;
    list_base^.weight                := 0;
    list_base^.next                  := nil;
    list_base^.end_element           := element_base;
    for count := 1 to N do
      list_base^.connection[count]   := 0;

    new(next_list_buffer);
    next_list_buffer^.size           := 1;
    next_list_buffer^.weight         := element_base^.next^.weight;
    next_list_buffer^.new_element    := element_base^.next;
    next_list_buffer^.source         := list_base;
    next_list_buffer^.content_source := nil;
    for count := 1 to N do
      next_list_buffer^.connection[count] := 0;
    se := next_list_buffer^.new_element^.stations;
    for count := 1 to next_list_buffer^.new_element^.connectivity do
      begin
        next_list_buffer^.connection[se^.index] := 1;
        se := se^.next
      end;

    new(node_base);
    node_base^.parent         := nil;
    node_base^.left_child     := nil;
    node_base^.right_child    := nil;
    node_base^.left_neighbor  := nil;
    node_base^.right_neighbor := nil;
    node_base^.content        := next_list_buffer;

    list_end := list_base;
    parent   := node_base;
    tree_end := node_base
  end;


(******************************************************)
(* This procedure adds the least-weight subset in the *)
(* tree to the end of the list of ordered subsets.    *)
(* The least-weight subset in the tree is always at   *)
(* the root of the tree.                              *)
(******************************************************)
procedure ADD_TO_LIST;
  begin
    new(list_buffer);
    list_buffer^.content_source := node_base^.content^.content_source;
    list_buffer^.size           := node_base^.content^.size;
    list_buffer^.weight         := node_base^.content^.weight;
    for count := 1 to N do
      list_buffer^.connection[count] := node_base^.content^.connection[count]; 
    list_buffer^.next           := nil;
    list_buffer^.end_element    := node_base^.content^.new_element;
    list_end^.next              := list_buffer;
    list_end                    := list_buffer
  end;


(*******************************************************)
(* This procedure updates the connectivity information *)
(* of a newly formed subset of link elements.          *)
(*******************************************************)
procedure CONNECTIVITY_UPDATE (cv:vector; Var ccon:stations);
  var ci, cj, ch, cs, flag : integer;
  begin
    flag := 0;
    ch := 1;
    for ci := 1 to N do
      if ccon[ci] > ch then ch := ccon[ci];
    ch := ch + 1;
    for ci := 1 to cv[0] do 
      begin
        if ccon[cv[ci]] = 0 then ccon[cv[ci]] := ch;
        if ccon[cv[ci]] > 1 then
          begin
            cs := ccon[cv[ci]];
            for cj := 1 to N do 
              if ccon[cj] = cs then ccon[cj] := ch
          end;
        if ccon[cv[ci]] = 1 then flag := 1
      end;
    if flag = 1 then 
      for ci := 1 to N do
        if ccon[ci] = ch then ccon[ci] := 1
  end;


(****************************************************************)
(* When the subset which has just been added to the ordered     *)
(* list is not the last child of its parent, this procedure     *)
(* makes its parent's next best child the new root of the tree. *)
(****************************************************************)
procedure ADD_TO_TREE_FROM_OLD_SOURCE;
  begin
    new(next_list_buffer);
    next_list_buffer^.size   := node_base^.content^.source^.size;
    next_list_buffer^.weight := node_base^.content^.weight -
                                node_base^.content^.new_element^.weight +
                                node_base^.content^.new_element^.next^.weight;
    next_list_buffer^.new_element    := node_base^.content^.new_element^.next;
    next_list_buffer^.source := node_base^.content^.source;
    for count := 1 to N do
      next_list_buffer^.connection[count] := 
                                   next_list_buffer^.source^.connection[count];
    se := next_list_buffer^.new_element^.stations;
    v[0] := next_list_buffer^.new_element^.connectivity;
    for count := 1 to next_list_buffer^.new_element^.connectivity do 
      begin
        v[count] := se^.index;
        se := se^.next
      end;
    CONNECTIVITY_UPDATE(v, next_list_buffer^.connection);
    next_list_buffer^.content_source := node_base^.content^.content_source;
    node_base^.content       := next_list_buffer
  end;


(**************************************************************)
(* When the subset which has just been added to the ordered   *)
(* list is the last child of its parent, this procedure       *) 
(* removes that subset from the tree and moves the subset at  *) 
(* the end of the tree up to become the new root of the tree. *)
(**************************************************************)
procedure RESTORE_ROOT;
  begin
  if node_base <> tree_end then
    begin
      node_base^.content := tree_end^.content; 
      if tree_end^.parent^.right_child = nil 
        then
          tree_end^.parent^.left_child := nil
        else
          begin
            tree_end^.parent^.right_child := nil;
            parent := tree_end^.parent
          end; 
      tree_end := tree_end^.left_neighbor; 
      tree_end^.right_neighbor := nil
    end
  end;


(*********************************************)
(* Since a new root has just been made, this *)
(* procedure reorders the tree from its root *) 
(* to preserve its ordered structure.        *)
(*********************************************)
procedure REORDER_TREE_FROM_ROOT;
  label 99;
  begin
  if node_base <> tree_end then
    begin
      p := node_base;
      while p^.right_child <> nil do
        begin
          if ((p^.content^.weight > p^.left_child^.content^.weight) or 
              (p^.content^.weight > p^.right_child^.content^.weight))
            then
              begin
                if  p^.left_child^.content^.weight >
                   p^.right_child^.content^.weight 
                  then 
                    begin
                      np := p^.content;
                      p^.content := p^.right_child^.content;
                      p^.right_child^.content := np;
                      p := p^.right_child
                    end
                  else
                    begin
                      np := p^.content;
                      p^.content := p^.left_child^.content;
                      p^.left_child^.content := np;
                      p := p^.left_child
                    end
              end
            else goto 99
        end;
      if p^.left_child = nil 
        then goto 99
        else
          begin
            if p^.content^.weight > p^.left_child^.content^.weight
              then
                begin
                  np := p^.content;
                  p^.content := p^.left_child^.content;
                  p^.left_child^.content := np
                end
              else goto 99
          end;
      99 : np := nil
    end
  end;


(*******************************************************)
(* The subset which has just been added to the ordered *)
(* list now becomes a parent.  This procedure adds its *) 
(* best child to the end of the tree, and reorders the *)
(* tree from its end to preserve its ordered structure.*)
(*******************************************************)
procedure ADD_NEW_TO_TREE_AND_REORDER;
  label 999;
  begin
    new(next_list_buffer);
    next_list_buffer^.size           := list_end^.size + 1;
    next_list_buffer^.weight         := list_end^.weight +
                                        list_end^.end_element^.next^.weight;
    next_list_buffer^.new_element    := list_end^.end_element^.next;
    next_list_buffer^.source         := list_end;
    next_list_buffer^.content_source := list_end;

    for count := 1 to N do
      next_list_buffer^.connection[count] := 
                                   next_list_buffer^.source^.connection[count];
    se := next_list_buffer^.new_element^.stations;
    v[0] := next_list_buffer^.new_element^.connectivity;
    for count := 1 to next_list_buffer^.new_element^.connectivity do 
      begin
        v[count] := se^.index;
        se := se^.next
      end;
    CONNECTIVITY_UPDATE(v, next_list_buffer^.connection);

    new(node_buffer);
    node_buffer^.parent           := parent;
    node_buffer^.left_child       := nil;
    node_buffer^.right_child      := nil;
    node_buffer^.left_neighbor    := tree_end;
    node_buffer^.right_neighbor   := nil;
    node_buffer^.content          := next_list_buffer;
    
    if parent^.left_child = nil 
      then parent^.left_child := node_buffer
      else
        begin
	  parent^.right_child := node_buffer;
	  parent := parent^.right_neighbor
	end;
    tree_end^.right_neighbor := node_buffer;
    tree_end := node_buffer;

    p := tree_end;
    while p^.parent <> nil do
      begin
        if p^.content^.weight < p^.parent^.content^.weight 
          then
            begin
              np := p^.content;
              p^.content := p^.parent^.content;
              p^.parent^.content := np;
              p := p^.parent
            end
          else goto 999
      end;
    999 : np := nil
  end;


(*************************************)
(* This procedure outputs a feasible *)
(* topology to the terminal.         *)
(*************************************)
procedure OUTPUT_SINGLE;
  begin
    writeln(' ');
    writeln('SUBSET   # ', count1:5);
    writeln('WEIGHT   =  ', list_end^.weight);
    write('ELEMENTS :');
    print_base := nil;
    pq := list_end;
    while pq <> nil do
      begin
        new(op); 
        op^.end_element := pq^.end_element;
        op^.next := print_base;
        print_base := op;
        pq := pq^.content_source
      end;
    op := print_base;
    while op <> nil do
      begin
        write(op^.end_element^.index:4);
        op := op^.next
      end;
    writeln(' ')
  end;


begin     (* MAIN PROGRAM *)
  writeln(' ');
  writeln(' ');

  repeat
    write('Do you need instructions (Y/N) ? ');
    readln(answer)
  until ((answer = 'Y') or (answer = 'y')) or 
        ((answer = 'N') or (answer = 'n'));
  if (answer = 'Y') or (answer = 'y') then
    begin
      INSTRUCTIONS;
      repeat
        write('Are you ready (Y/N) ? ');
        readln(answer)
      until (answer = 'Y') or (answer = 'y')
    end;
  
  writeln(' ');
    INPUT_DATA_TERMINAL;

                                  
  INITIALIZE;                     
  count1 := 1;                     
  while  (count1 < (2**size)) do   
    begin                         
      ADD_TO_LIST;                
      (*****************************************************)
      (* A new subset has just been generated and added to *)
      (* the ordered list.  The following few lines test   *)
      (* to see if this subset forms a connected network.  *)
      (* Additional tests can be inserted at this point.   *)
      (*****************************************************)
      connected := 'Y';
      for index := 1 to N do 
        if list_end^.connection[index] <> 1 
          then connected := 'N';
      if connected = 'Y' then 
        begin
          OUTPUT_SINGLE;
          repeat
            write('Do you want to continue (Y/N) ? ');
            readln(answer3)
          until ((answer3 = 'Y') or (answer3 = 'y')) or 
                ((answer3 = 'N') or (answer3 = 'n'));
          if (answer3 = 'N') or (answer3 = 'n') then goto 99
        end;
            
      count1 := count1 + 1; 
      if node_base^.content^.new_element^.next <> nil 
        then ADD_TO_TREE_FROM_OLD_SOURCE
        else RESTORE_ROOT;
      REORDER_TREE_FROM_ROOT;
      if list_end^.end_element^.next <> nil 
	then ADD_NEW_TO_TREE_AND_REORDER
    end;

  99: 
  writeln(' ');
  writeln('END OF EXECUTION.');   
  writeln(' ')
end.

$ 