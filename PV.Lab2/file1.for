      program main
      common /inp/rx0, rxMax, rxStep, ry0, ryMax, ryStep
      logical checkERR
      read *, rx0, rxMax, rxStep, ry0, ryMax, ryStep
      if (checkERR()) goto 13
      call countTan
      goto 14
13    print *, 'Invalid input'
      pause
14    end
      
      logical function checkERR()
      common /inp/rx0, rxMax, rxStep, ry0, ryMax, ryStep
      if ((rx0 + rxStep) .GE. rxMax) goto 12
      if ((ry0 + ryStep) .GE. ryMax) goto 12
      if (rxStep .LE. 0) goto 12
      if (ryStep .LE. 0) goto 12
      checkERR = .FALSE.
      goto 11
12    checkERR = .TRUE.
11    end
      
      subroutine countTan
      common /inp/rx0, rxMax, rxStep, ry0, ryMax, ryStep
      do ri=rx0, (rxMax + rxStep), rxStep
        if (ri .GT. rxMax) then
        rt = rxMax
        else
        rt = ri
        end if
        do rj=ry0, ryMax, ryStep
            rTan = TAN(rt+rj)
            write (*,1) rTan
1           format (f9.4,' | ',$)
        end do
        if (rj .GT. ryMax) then
            rTan = TAN(rt+ryMax)
            write (*,1) rTan
        end if
        write (*,*) ' '
      end do
      end
