      program main
      common /inp/rx0, rxMax, rxStep, ry0, ryMax, ryStep
      common /tabl/n,m
      common /const/pi, eps
      common /other/ry, fnull
      logical validate
      open (1, FILE='inperiod2.txt')
      read (1, *) rx0, rxMax, rxStep, ry0, ryMax, ryStep
      close (1)
      pi=3.141592652
      eps=1.1920929e-07
      fxnull=10.**(aint(log10(abs(rxStep))-3.))
      fynull=10.**(aint(log10(abs(ryStep))-3.))
      if(rxStep.lt.ryStep)then
        fnull=fxnull
      else
        fnull=fynull
      end if
      if (validate()) goto 13
      call size
      call table
      goto 14
13    pause
14    end

      function equal(x, xnext)
      character*11 a, b
      write(a, 20) x
      write(b, 20) xnext
      if(a.eq.b)then 
        equal=1 
      else 
        equal=0 
      endif 
20    format(E11.4)
      end
      

      logical function validate()
      common /inp/rx0, rxMax, rxStep, ry0, ryMax, ryStep
      common /const/pi, eps
      common /other/ry, fnull
      if((rx0.GT.rxMax).OR.(ry0.GT.ryMax))Then
        print*,'Min>Max'
        goto 11
      else if((rxStep.LT.0).OR.(ryStep.LT.0))Then
        print*,'Step<0'
        goto 11
      endif
!      rxcol=max(abs(rxMax),abs(rx0))
!      rxStepMin=rStep(rxcol)
!      if (rxStep.LT.rxStepMin)rxStep = rxStepMin
!      rycol=max(abs(ryMax),abs(ry0))
!      ryStepMin=rStep(rycol)
!      if (ryStep.LT.ryStepMin)ryStep = ryStepMin
      
!      else if(((ryMax-ry0).GE.eps).AND.(ryStep.LE.(max(abs(ryMax),
!     @abs(ry0))*ceps)))Then
!        print*,'Invisible step y'
!         goto 11
!      else if(((rxMax-rx0).GE.eps).AND.(rxStep.LE.max(abs(rxMax),
!     @abs(rx0))*ceps))Then
!        print*,'Invisible step x'
!         goto 11
      validate = .FALSE.
      goto 12
11    validate = .TRUE.
12    end
      
      subroutine table
      common /inp/rx0, rxMax, rxStep, ry0, ryMax, ryStep
      common /tabl/n,m
      common /const/pi, eps
      common /other/ry, fnull
      open(2, FILE='outperiod2.txt')
1     format (5X, 'y\x', 5X, ' || '\)
2     format (E13.4,' | '\)
3     format (16('–')\)
4     format (16('=')\)
21    format (E13.4, ' || ' \)

      write(2,1)
      do i=0,m,1
        x=rxStep*i+rx0
        if((i.ne.0).AND.(equal(x, xprev).eq.1))then
            x=rxStep*i+rx0
            goto 30
        end if
        if (abs(x) .LT. fnull) x=0
        write (2,2) x
        xprev=x
30      continue
      end do
      if (abs(x-rxMax).GE.fnull) then
        write (2,2) rxMax
      endif
      write(2,*)
      do i=0, m+1, 1
        write(2,4)
      enddo
      write (2,*)
      do i=0, n, 1
        ri=ryStep*i+ry0
        if((i.ne.0).AND.(equal(ri, riprev).eq.1))then
            ri=ryStep*i+ry0
            goto 31
        end if
        if (abs(ri) .lt. fnull) ri=0
        write(2,21) ri
        ry=ri
        call stroka
        riprev=ri
31      continue
      end do
      if (abs(ry-ryMax).GE.fnull) then
        write(2,21)ryMax
        ry=ryMax
        call stroka
      endif
      close (2)
      end
      
      subroutine size 
      common /inp/rx0, rxMax, rxStep, ry0, ryMax, ryStep
      common /tabl/n,m
      a=(rxMax-rx0)/rxStep
      m=int(a)
      b=(ryMax-ry0)/ryStep
      n=int(b)
      end

      subroutine stroka
      common /inp/rx0, rxMax, rxStep, ry0, ryMax, ryStep
      common /tabl/n,m
      common /other/ry, fnull
      common /const/pi, eps
2     format (E13.4,' | '\)
3     format (16('–')\)
4     format (a13(' | ')\)

      do j=0, m, 1
        rj=rxStep*j+rx0
        if((j.ne.0).AND.(equal(rj, rjprev).eq.1))then
            rj=rxStep*j+rx0
            goto 32
        end if
        if((abs(abs(rj+ry)*pi/180.0-pi/2)).lE.eps)then
            write(2,4) 'inf'
        else if((abs(rj+ry)*pi/180.0).LE.fnull)then
            write(2,4) '0'
        else
            write (2,2) TAN((ry+rj)*pi/180.0)
        endif
        rx=rj
        rjprev=rj
32      continue
       end do
  
       if (abs(rx-rxMax).GE.fnull) then
        rCos=COS((ry+rj)*pi/180.0)
        rSin=SIN((ry+rj)*pi/180.0)
        if((abs(abs(rxMax+ry)*pi/180.0-pi/2)).lE.eps)then
            write(2,4) 'inf'
        else if((abs(rxMax+ry)*pi/180.0).LE.fnull)then
            write(2,4) '0'
        else
            write (2,2) TAN((ry+rxMax)*pi/180.0)
        endif
       endif
       write(2,*)
       do l=0, m+1,1
           write (2,3)
       end do
       write(2,*)
       end
