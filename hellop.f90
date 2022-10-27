!Quadratic eqeationprogram quadratic
program quadratic
    implicit none
    real a,b,c,d,p,rootd,x1,x2,realroot,imagroot
  
    write(*,*) "Give a,b,c"
    read (*,*) a,b,c
    write(*,*) "a= ",a,"b= ",b,"c= ",c
  
    d=(b*b)-(4*a*c)
    write (*,*) "The discriminant is", d
  
    write (*,*) "Finding the solutions to the quadratic equation:"
    write (*,*) "(",a,")x^2+ (",b,")x + (",c,")"
  
    !For complex roots
    If (d<0) then
          rootd=sqrt(-d)
          write (*,*) "The discriminant is less than zero. Roots are complex"
          x1=(-b)/(2*a) + (-rootd)/(2*a)
          x2=(-b)/(2*a) - (-rootd)/(2*a)
          realroot=(x1+x2)/2
          imagroot=(x1-x2)/2
        write (*,*) "Root1= ",realroot," + ",imagroot,"i"
        write (*,*) "Root2= ",realroot," - ",imagroot,"i"
        write(*,*) "Real part of root= ", abs(realroot)," Imaginary part of root= ", abs(imagroot)
        write (*,*) "The modulus is", sqrt(realroot**2+imagroot**2)
          else
        
        !For real roots
          p=(-b)
        rootd=sqrt(d)
        write (*,*) "The discriminant is greater than or equal to zero. The roots are real."
          if (p>0) then
            x1=(-b)/(2*a)+rootd/(2*a)
            else
            x1=(-b)/(2*a)-rootd/(2*a)
          end if
          x2=c/(a*x1)
      
          write(*,*) "Root1= ",x1,"Root2= ",x2
        write (*,*) "The sum of the roots is ",x1+x2," and (-b/a) is ",-b/a
           write (*,*) "The product of the roots is ",x1*x2," and (c/a) is ",c/a
    end if
 end program quadratic