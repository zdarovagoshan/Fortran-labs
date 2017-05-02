      program main
1     print*,'Select method:'
      print*,'1) The method of Newton-Cotes-3'
      print*,'2) The method of Gauss-2'
      print*,'3) Exit'
      read*,m
      open(10,file='Newton-Cotes-3.txt')
      open(11,file='Gauss-2.txt')
      if(m.eq.3) goto 10
      if(m.lt.1.or.m.gt.3) goto 1
      if(m.eq.1)then
!Method of Newton-Cotes 3
        print*,'Method of Newton-Cotes-3'
!        print*,'Enter quantity of segments:'
!        read*,N  !количество сегментов разбиения отрезка
!        print*, 'Enter begin and end:'
!        read*, a,b
2       print*, 'Select:'
        print*, '1) Polynomial'
        print*, '2) Oscillating'
        read*,m1
        if((m1.lt.1).or.(m1.gt.2)) goto 2
        if(m1.eq.1)then
            print*, 'Enter degree of polynomial:'
            read*,k
            print*, 'Enter numbers of degrees of 2:'
            read*,d
            do i=0, d
                N=2**i
                t=rncpoly(1,3,N,k)
                write(10,301) k
        endif
        if(m1.eq.2)then
            t=rncpoly_os(a,b,N)
        endif
        write(10,300) N,a,b,t
      endif
      if(m.eq.2)then
!Method of Gauss-2
        print*,'Method of Gauss-2'
        print*,'Enter quantity of segments:'
        read*,N
        print*,'Enter begin and and:'
        read*, a,b
3       print*, 'Select:'
        print*, '1) Polynomial'
        print*, '2) Oscillating'
        read*,m1
        if(m1.lt.1.or.m1.gt.2) goto 3
        if(m1.eq.1)then
            print*, 'Enter degree of polynomial:'
            read*,k
            t=gauss2(a,b,N,k)
            write(11,301) k
        endif
        if(m1.eq.2)then
            t=gauss2_os(a,b,N)
        endif
        write(11,300) N,a,b,t
       endif
       
      pause 'Completed.'
      goto 1
300   format('Quantity of segments = 'I8/'a = 'F23.9/'b = 'F23.9/
     ,'Integral = 'F23.9)
301   format('Degree of polynomial = 'I8)
10    close(10)
      close(11)
      end
      
      
!--------------------------------------------------------------------

      real function rncpoly(a,b,N,k)
      dimension h(50)
      x(1)=a
      x(2)=
      x(3)=
      x(4)=b
      h(1)=a
      h(2)=b
      do i=2, 4
        h(i)=(x(i)-x(i-1))/N
      enddo
      s1=0
      s2=0
      w=0
      do i=1,N-1
        w=w+h
        s1=s1+fun(w,k)
      enddo
      do i=0,N-1
        s2=s2+fun((a+(i)*h+h/2.0),k)
      enddo
      rncpoly=h*(fun(a,k)+fun(b,k)+4.0*s2+2.0*s1)/6.0
      return
      end
      
!--------------------------------------------------------------------

      real function rncpoly_os(a,b,N)
      h=(b-a)/N
      s1=0
      s2=0
      do i=1,N-1
        s1=s1+osfun((a+(i)*h))
      enddo
      do i=0,N-1
        s2=s2+osfun((a+(i)*h+h/2.0))
      enddo
      rncpoly_os=h*(osfun(a)+osfun(b)+4.0*s2+2.0*s1)/6.0
      return
      end
      
!--------------------------------------------------------------------      
      real function fun(x,k)
      fun=(k+1.0)*x**k
      return 
      end
       
!--------------------------------------------------------------------       
     
      real function osfun(x)
      osfun=-10.*sin(10.*x) 
      return 
      end
      
      
!--------------------------------------------------------------------      
      
      real function gauss2(a,b,N,k)
      dimension x(2)
      x(1)=-0.5773502691
      x(2)=0.5773502691
      q=1
      s2=0
      h=(b-a)/N
      do i=1,2
        s1=0
        do j=1,N
            s1=s1+h*fun((a+h*(j-1)+a+h*(j))/2.0+x(i)*h/2.0,k)
        enddo
        s2=s2+q*s1
      enddo
      Gauss2=s2/2.0
      return
      end
      
!--------------------------------------------------------------------      
      
      real function gauss2_os(a,b,N)
      dimension x(2)
      x(1)=-0.5773502691
      x(2)=0.5773502691
      q=1
      h=(b-a)/N
      do i=1,2
        s1=0
        do j=1,N
            s1=s1+h*osfun((a+h*(j-1)+a+h*(j))/2.0+x(i)*h/2.0)
        enddo
        s2=s2+q*s1
      enddo    
      Gauss2_os=s2/2.0
      return
      end

