      program main
      print *,'Enter size'
      read *,n
      call generator(n)
      goto 301
301   pause 'End'
      end
      
!--------------------------------------------------------------------

      subroutine generator(n)
      open (10, file = 'prm.txt')
      open (11, file = 'inf_prf.bin', ACCESS='DIRECT', recl=4)
      open (12, file = 'mtr.bin', ACCESS='DIRECT', recl=4)
      open (13, file = 'vector.txt')
      write (10,*) n
      write (10,*) n
      write (11,rec=1) 1
      ih=1
      do i=1,n
        ih=ih+n+1-i
        write (11,rec=i+1) ih
        
        j=n-i
        k=1
        write (12,rec=k) 2
        k=k+1
        do m=2,j+1
            write (12,rec=k) 1
            k=k+1
        enddo
        
        write (13,*) 1
      enddo
      end

