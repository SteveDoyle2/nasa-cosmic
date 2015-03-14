




C       *****************************************************************
C       * samplef.for			version 1.0 vax/vms, 5-8-86
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
        integer beglissca_,endlissca_,inslisathea_
        integer insnexlisele_,maknum_,maklis_,getnexlisele_
        double precision getnum_
        external beglissca_,endlissca_,inslisathea_,getnum_
        external insnexlisele_,maknum_,maklis_,getnexlisele_
        lis = maklis_()
        lis = inslisathea_(lis,maknum_(1.0))
        lis = beglissca_(lis,1)
        do 10 i = 1,10
        x = getnum_(getnexlisele_(lis,1))
10      lis = insnexlisele_(lis,1,maknum_(x * 2))
        lis = endlissca_(lis,1)
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
        integer maknum_,beglissca_,endlissca_,getnexlisele_
        double precision getnum_
        external maknum_,beglissca_,endlissca_,getnexlisele_,getnum_
        sum = 0.0
        x = getnum_(num1)
        lis1 = beglissca_(lis1,1)
10      i = getnexlisele_(lis1,1)
        if(i .eq. 0) go to 20
        sum = sum * x + getnum_(i)
        go to 10
20      polynomial = maknum_(sum)
        return
        end
