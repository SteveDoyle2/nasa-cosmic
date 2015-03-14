




C       *****************************************************************
C       * samplef.f			version 1.0 sun/unix, 5-8-86
C       *****************************************************************
C       * Contains sample code for external routines defined in FORTRAN.
C       *   The file "starlink.c" is initialized to link these two
C       *   routines into the STAR environment as external functions
C       *   under the class "fortran_function".
C       *****************************************************************

        integer function powers()
C         ************************************************************
C         * See Example 35, STAR Tutorial Guide.
C         ************************************************************
        integer beglissca,endlissca,inslisathea
        integer insnexlisele,maknum,maklis,getnexlisele
        double precision getnum
        external beglissca,endlissca,inslisathea,getnum
        external insnexlisele,maknum,maklis,getnexlisele
        lis = maklis()
        lis = inslisathea(lis,maknum(1.0))
        lis = beglissca(lis,1)
        do 10 i = 1,10
        x = getnum(getnexlisele(lis,1))
10      lis = insnexlisele(lis,1,maknum(x * 2))
        lis = endlissca(lis,1)
        powers = lis
        return
        end

        integer function polynomial(lis1,num1)
C         ************************************************************
C         * Takes a LIST of coefficients for a polynomial expression
C         *   in one variable, plus a NUMBER to be substituted for
C         *   the variable.  Returns a NUMBER corresponding to the
C         *   calculated result of the substitution operation.
C         ************************************************************
        integer maknum,beglissca,endlissca,getnexlisele
        double precision getnum
        external maknum,beglissca,endlissca,getnexlisele,getnum
        sum = 0.0
        x = getnum(num1)
        lis1 = beglissca(lis1,1)
10      i = getnexlisele(lis1,1)
        if(i .eq. 0) go to 20
        sum = sum * x + getnum(i)
        go to 10
20      polynomial = maknum(sum)
        return
        end
