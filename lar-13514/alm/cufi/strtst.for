	program strtst	! test of strlead.for
	implicit none	
	integer*4	iret,strip_leading
	character*20	doda,doda_new
	byte	doda_b(20)
	equivalence (doda_b(1),doda_new)
10	read(5,1)doda
1	format(1a20)
	read(5,1)doda_new
	iret = strip_leading(doda_new,doda)
	write(5,2)doda_new,doda_b,iret
2	format(' #',1a20,20z3,i)
	go to 10
	end
