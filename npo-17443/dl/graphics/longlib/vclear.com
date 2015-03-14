$ ! clear screen and reset graphics card on vt220 terminal equipped with selnar sg220
$ ! *** LAST REVISED ON 13-DEC-1985 10:52:13.06
$ ! *** SOURCE FILE: [LONG.GRAPHICS.NEW]VCLEAR.COM
$ ws := write sys$output
$ ws "[5i1"				! select graphics card
$ !ws "OO ` @OR ` @"			! reset origin
$ !ws "OX ` @OY ` @OW `` @@"	! reset gains, reset write mode
$ ws ""				! clear graphics screen
$ ws "2[4i"				! select vt220
$ ws "[2J"					! clear text screen
