!A program to calculate the weighted rank for a set of data points
      subroutine wtdr(x,w,n,d,answer)
      integer n,d,i,j,k,l,p
      real*8 w(n),temp(d),final(d),answer(n,d),x(n,d),s,dist,z
      z = 0
      !compute the sum of the taus
      do p=1,n
        z = z+w(p)
      end do
      
      !compute the weighted rank function for the data
      do i=1,n
        do k=1,d
        final(k)=0
        end do
        do j=1,n
          s = 0
          temp = x(i,:) - x(j,:)
          !compute the euclidean distance
          do l = 1,d
            s = s + (temp(l)**2)
          end do
          dist = sqrt(s)
          !ensure that there is no division by zero
          if(dist .NE. 0) then
             temp = w(j)*(temp/dist)
          end if
          !normalize the values
          temp = temp/z
          final = final + temp
        end do
        answer(i,:) = final
      end do
      return
      end subroutine wtdr


      
