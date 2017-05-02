      program main
      common/param/iN, iM
      common/id/idInfPrf, idMtr, idV, idVR
      dimension m(1000000)
      call read_param(m(1))
      idInfPrf = 1
      idMtr = iN+2
      idV = idMtr+m(iN+1)-1
      idVR = idV+iM
      irmcount=idVR+iM
      if (irmcount.gt.1000000) go to 300
      call read_data(m(idInfPrf), m(idMtr),m(idV))
      call output_matrix(m(idInfPrf), m(idMtr), m(idV))
300   print *,'Not enough memory'
      end

!--------------------------------------------------------------------
      
      subroutine read_param(inf)
      common /param/iN, iM
      dimension inf(iN+1)
      open (10, file = 'prm.txt')
      read (10,*) iN, iM
      close(10)
      if ((iN.ne.iM).OR.(iN.LE.0).OR.(iM.LE.0))stop'Invalid input'
      
      open(20, file = 'inf_prf.txt')
      do i=1,iN+1
        read (20,*) inf(i)
      enddo
      close(20)
      end
      
!--------------------------------------------------------------------
      
      subroutine read_data(inf, rmtr, rv)
      common /param/iN, iM
      common /id/idInfPrf, idMtr, idV, idVR
      dimension inf(iN+1), rmtr(idV-idMtr), rv(iM)
      
      open(21, file = 'mtr.txt')
      do i=1, inf(iN+1)-1
        read (21,*) rmtr(i)
      enddo
      close(21)
      
      open(22, file = 'vector.txt')
      do i=1, iM
        read (22,*) rv(i)
      enddo
      close(22)
      end

!--------------------------------------------------------------------

      subroutine output_matrix(inf, rmtr, a)
      common /param/iN, iM
      common /id/idInfPrf, idMtr, idV, idVR
      dimension inf(iN+1), rmtr(idV-idMtr)
      dimension a(iN, iN)
51    format(' ',E15.7\)
52    format(' ',E15.7)
      open(60,file='matrix.txt')
      do i=1, iN
        do j=1, iN
            a(i,j)=0
        enddo
      enddo
      
      do i=1, iN
        if (inf(i) .ne. inf(i+1)) a(i,i)=rmtr(inf(i))
      enddo
      do i=1,iN
        do jadr=inf(i)+1,inf(i+1)-1
            j=i+(jadr - inf(i))
            a(i,j)=rmtr(jadr)
            a(j,i)=rmtr(jadr)
        enddo
      enddo
      
      do i=1,iN
        do j=1,iN-1
            write(60,51) a(i,j)
        enddo
        write(60,52) a(i,iN)
      enddo
      close(60)
      end
