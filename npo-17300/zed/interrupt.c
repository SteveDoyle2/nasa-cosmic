#include "glob"
#include <iodef>
#include <ssdef>
#include <descrip>

#define LIB$M_CLI_CTRLY 0x2000000

init_interrupts()
{
  $DESCRIPTOR(tt_desc, "TT");
  int i;
  i = sys$assign (&tt_desc, &tt_chan, 0, 0);
  if (i != SS$_NORMAL) stop (i);
  set_ctrl_c_ast();
  interrupt = FALSE;
}

ctrl_c_ast()
{
  interrupt = TRUE;
}

do_interrupt()
{
  set_ctrl_c_ast();
  interrupt = FALSE;
}

set_ctrl_c_ast()
{
  int i;
  i = sys$qiow (0, tt_chan, IO$_SETMODE | IO$M_CTRLCAST, 0, 0, 0,
          ctrl_c_ast, 0, 3, 0, 0, 0);
  if (i != SS$_NORMAL) stop (i);
}
