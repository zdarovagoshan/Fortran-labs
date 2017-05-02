      program main
      common /param/iN, iM
      common /id/idInfPrf, idMtr, idV, idVR
      dimension m(1000000)
      call read_param(m(1))
      idInfPrf = 1
      idMtr = iN+2
      idV = idMtr+m(iN+1)-1
      idVR = idV+iM
      irmcount=idVR+iM
      if (irmcount.gt.1000000) go to 300
4     print*,'1-bin to txt'
      print*,'2-txt to bin'
      print*,'3-exit'
      read*,iflag
      if(iflag.eq.1)goto 1
      if(iflag.eq.2)goto 2
      if(iflag.eq.3)goto 301
      goto 4

1     call txtConvert(m(idInfPrf), m(idMtr),m(idV))
      goto 301
2     call binConvert(m(idInfPrf), m(idMtr),m(idV))
      goto 301
300   print *,'Not enough memory'
301   pause 'End'
      end
      
!--------------------------------------------------------------------

      subroutine binConvert(inf, rmtr, rv)
      common/param/iN, iM
      common /id/idInfPrf, idMtr, idV, idVR
      dimension inf(iN+1), rmtr(idV-idMtr), rv(iM)
      call textdata(inf, rmtr, rv)
      open(31,file='inf_prf.bin', ACCESS='DIRECT', recl=4)
      open(32,file='mtr.bin', ACCESS='DIRECT', recl=4)
      open(33,file='vector.bin', ACCESS='DIRECT', recl=4)
      do i=1, iN
        write(31,rec=i) inf(i)
        write(33,rec=i) rv(i)
      enddo
      write (31,rec=i) inf(i) 
      do i=1, inf(iN+1)-1
        write (32,rec=i) rmtr(i)
      enddo
      close(31)
      close(32)
      close(33)
      end

!--------------------------------------------------------------------

      subroutine txtConvert(inf, rmtr, rv)
      common/param/iN, iM
      common /id/idInfPrf, idMtr, idV, idVR
      dimension inf(iN+1), rmtr(idV-idMtr), rv(iM)
      call bindata(inf, rmtr, rv)
      open(31,file='inf_prf.txt')
      open(32,file='mtr.txt')
      open(33,file='vector.txt')
      do i=1, iN
        write(31,*) inf(i)
        write(33,*) rv(i)
      enddo
      write (31,*) inf(i) 
      do i=1, inf(iN+1)-1
        write (32,*) rmtr(i)
      enddo
      close(31)
      close(32)
      close(33)
!50    format(E15.7)
      end

!--------------------------------------------------------------------

      subroutine textdata(inf, rmtr, rv)
      common/param/iN, iM
      common /id/idInfPrf, idMtr, idV, idVR
      dimension inf(iN+1), rmtr(idV-idMtr), rv(iM)
      open(21,file='inf_prf.txt')
      open(22,file='mtr.txt')
      open(23,file='vector.txt')
      do i=1,iN
        read(21,*,err=302,end=302) inf(i)
        read (23,*,err=302,end=302) rv(i)
      enddo
      read(21,*,err=302,end=302) inf(i)
      do i=1, inf(iN+1)-1
        read (22,*,err=302,end=302) rmtr(i)
      enddo
      close(21)
      close(22)
      close(23)
      goto 303
302   print*, 'Error reading file'
      pause
      stop
303   continue
      end

!--------------------------------------------------------------------

      subroutine bindata(inf, rmtr, rv)
      common/param/iN, iM
      common /id/idInfPrf, idMtr, idV, idVR
      dimension inf(iN+1), rmtr(idV-idMtr), rv(iM)
      open(21,file='inf_prf.bin',ACCESS='DIRECT',recl=4)
      open(22,file='mtr.bin',ACCESS='DIRECT',recl=4)
      open(23,file='vector.bin',ACCESS='DIRECT',recl=4)
      do i=1,iN
        read(21,rec=i,err=202,end=202) inf(i)
        read (23,rec=i,err=202,end=202) rv(i)
      enddo
      read(21,rec=i,err=202,end=202) inf(i)
      do i=1, inf(iN+1)-1
        read (22,rec=i,err=202,end=202) rmtr(i)
      enddo
      close(21)
      close(22)
      close(23)
      goto 203
202   print*, 'Error reading file'
      pause
      stop
203   continue
      end

!--------------------------------------------------------------------
      
      subroutine read_param(inf)
      common /param/iN, iM
      dimension inf(*)
      open (10, file = 'prm.txt')
      read (10,*) iN, iM
      close(10)
      if ((iN.ne.iM).OR.(iN.LE.0).OR.(iM.LE.0))stop'Invalid input'
      
      open(11, file = 'inf_prf.txt')
      do i=1,iN+1
        read (11,*) inf(i)
      enddo
      close(11)
      end
