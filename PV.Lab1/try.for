      program main
      common /ext/ t(3,2)
      common /int/ a, b, c, alpha, cos_min, S, pi
      CHARACTER*42 ERROR_massage
      ERROR_massage = 'ERROR: Coordinates are not set correctly'
      pi = 3.14159265
6     call menu
      read *, Ipoint_button
      goto (1,2,3,4,5) Ipoint_button
1       call coordinates
        goto 6
2       call calculate_square 
        print *, S
        goto 6
3       call angle_min
        print *, alpha
        goto 6
4       call calculate_cos_min
        goto 6
5     end  

      subroutine menu
      print *, '1 - Write coordinates'
      print *, '2 - Print square'
      print *, '3 - Print smallest angle'
      print *, '4 - Print cos of a smallest angle'
      print *, '5 - Exit'
      end
      
      subroutine coordinates
      common /ext/ t(3,2)
      common /int/ a, b, c, alpha, cos_min, S, pi
      i = 1
      do i=1,3
        read *, t(i,1), t(i,2)
      end do
      call calculate_sides
      end
      
      subroutine calculate_sides
      common /ext/ t(3,2)
      common /int/ a, b, c, alpha, cos_min, S, pi
      a=sqrt((t(2,1)-t(1,1))**2+(t(2,2)-t(1,2))**2)
      b=sqrt((t(3,1)-t(2,1))**2+(t(3,2)-t(2,2))**2)
      c=sqrt((t(1,1)-t(3,1))**2+(t(1,2)-t(3,2))**2)
      print *, 'Sides lenght(a,b,c):'
      print *,a,b,c
      end
      
      subroutine calculate_square
      common /int/ a, b, c, alpha, cos_min, S, pi
      P = (a+b+c)/2
      SS=p*(p-a)*(p-b)*(p-c)
      S = sqrt(SS)
      end

      subroutine angle_min
      common /int/ a, b, c, alpha, cos_min, S, pi
      if (a .LE. b .AND. a .LE. c) then
      alpha = (b**2 + c**2 - a**2)/(2*b*c)
      else 
        if (b .LE. c) then
        alpha = (a**2 + c**2 - b**2)/(2*a*c)
        else 
            alpha = (a**2 + b**2 - c**2)/(2*a*b)
        endif
      endif
      alpha = acos(alpha) * 180 / pi
      end
      
      subroutine calculate_cos_min
      common /int/ a, b, c, alpha, cos_min, S, pi
      call angle_min
      cos_min = cos(alpha * pi / 180)
      print *, cos_min
      end
      





