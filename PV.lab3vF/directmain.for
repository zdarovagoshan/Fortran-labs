      program main
      common /param/iN, iM
      common /id/idInfPrf, idMtr, idV, idVR
      dimension m(2147483647)
      call read_param(m(1))
      idInfPrf = 1
      idMtr = iN+2
      idV = idMtr+m(iN+1)-1
      idVR = idV+iM
      irmcount=idVR+iM
      if (irmcount.gt.2147483647) go to 300
      call read_data(m(idInfPrf), m(idMtr),m(idV))
      call multi(m(idInfPrf),m(idMtr),m(idV),m(idVR))
      call write_result(m(idVR))
      goto 301
300   print *,'Not enough memory'
301   pause 'End'
      end
      
!--------------------------------------------------------------------
      
      subroutine read_param(inf)
      common /param/iN, iM
      dimension inf(*)
      open (10, file = 'prm.txt')
      read (10,*) iN, iM
      close(10)
      if ((iN.ne.iM).OR.(iN.LE.0).OR.(iM.LE.0))stop'Invalid input'
      
      open(20, file = 'inf_prf.bin', ACCESS='DIRECT', recl=4)
      do i=1,iN+1
        read (20,rec=i) inf(i)
      enddo
      close(20)
      end
      
!--------------------------------------------------------------------
      
      subroutine read_data(inf, rmtr, rv)
      common /param/iN, iM
      common /id/idInfPrf, idMtr, idV, idVR
      dimension inf(iN+1), rmtr(idV-idMtr), rv(iM)
      
      open(21, file = 'mtr.bin', ACCESS='DIRECT', recl=4)
      do i=1, inf(iN+1)-1
        read (21,rec=i) rmtr(i)
      enddo
      close(21)
      
      open(22, file = 'vector.bin', ACCESS='DIRECT', recl=4)
      do i=1, iM
        read (22,rec=i) rv(i)
      enddo
      close(22)
      end
      
!-------------------------------------------------------------------

      subroutine write_result(rvr)
      common /param/iN, iM
      common /id/idInfPrf, idMtr, idV, idVR
      dimension rvr(iN)
      
      open (30, file = 'result.txt')
      do i=1, iN
        write (30,*) rvr(i), ' '
      enddo
      close(30)
      end
      
!-------------------------------------------------------------------

      subroutine multi(inf, rmtr, rv, rvr)
      common /param/iN, iM
      common /id/idInfPrf, idMtr, idV, idVR
      dimension inf(iN+1), rmtr(idV-idMtr), rv(iM), rvr(iN)
      do i=1,iN
        rvr(i) = 0
      enddo
      do iStl=1, iN
        ib = inf(iStl)
        if(ib .ne. inf(iStl+1)) rvr(iStl) = rvr(iStl)+
     +  rmtr(ib)*rv(iStl)
        do j=inf(iStl)+1, inf(iStl+1)-1
            ia = iStl+j-inf(iStl)
            rvr(ia) = rvr(ia)+rmtr(j)*rv(iStl)
            rvr(iStl) = rvr(iStl)+rmtr(j)*rv(ia)
        enddo
      enddo
      end
