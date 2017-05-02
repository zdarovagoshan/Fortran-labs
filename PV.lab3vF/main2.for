      program main
      common /param/iN, iM
      common /id/idInfPrf, idMtr, idV, idVR
      dimension m(1000000)
      call read_param(m)
      idInfPrf = 1
      idMtr = iN+2
      idV = idMtr+m(iN+1)-1
      idVR = idV+iM
      call read_data(m(idInfPrf), m(idMtr),m(idV))
      call multi(m(idInfPrf),m(idMtr),m(idV),m(idVR))
      call write_result(m(idVR))
      pause 'End'
      end
      
!--------------------------------------------------------------------
      
      subroutine read_param(m)
      common /param/iN, iM
      dimension m(*)
      open (10, file = 'prm.txt')
      read (10,*) iN, iM
      close(10)
      if (iN .ne. iM) stop 'Invalid input'
      
      open(20, file = 'inf_prf.txt')
      do i=1,iN+1
        read (20,*) m(i)
      enddo
      close(20)
      end
      
!--------------------------------------------------------------------
      
      subroutine read_data(inf, mtr, v)
      common /param/iN, iM
      common /id/idInfPrf, idMtr, idV, idVR
      dimension inf(iN+1), mtr(idV-idMtr), v(iM)
      
      open(21, file = 'mtr.txt')
      do i=1, inf(iN+1)-1
        read (21,*) mtr(i)
      enddo
      close(21)
      
      open(22, file = 'vector.txt')
      do i=1, iM
        read (22,*) v(i)
      enddo
      close(22)
      end
      
!-------------------------------------------------------------------

      subroutine write_result(vr)
      common /param/iN, iM
      common /id/idInfPrf, idMtr, idV, idVR
      dimension vr(iN)
      
      open (30, file = 'result.txt')
      do i=1, iN
        write (30,*) vr(i), ' '
      enddo
      close(30)
      end
      
!-------------------------------------------------------------------

      subroutine multi(inf, mtr, v, vr)
      common /param/iN, iM
      common /id/idInfPrf, idMtr, idV, idVR
      dimension inf(iN+1), mtr(idV-idMtr), v(iM), vr(iN)
      
      do i=1,iN
        vr(i) = 0
      enddo
      iStr = 0
      iStl = 1
      do i=1, inf(iN+1)-1
101     if (i .eq. inf(iStl+1)) then
            iStl = iStl+1
            iStr = 0
            goto 101
        endif
        vr(iStl+iStr) = vr(iStl+iStr)+mtr(i)*v(iStl)
        if (iStr .ne. 0) vr(iStl) = vr(iStl)+mtr(i)*v(iStl+iStr)
        iStr = iStr+1
      enddo
      end
