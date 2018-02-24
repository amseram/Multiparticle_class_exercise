      Program main
          implicit double precision (a-h,o-z)
          integer          n_level, n_k_val
          double precision omega
          open (101, file="param", status="unknown")
          read (101, *) n_level, n_k_val, omega
          close(101)
          n_level = 2 * n_level + 1
          n_k_val = 2 * n_k_val + 1
          call calc(n_level, n_k_val, omega)
      End program main

      subroutine calc(n_level, n_k_val, omega)
          implicit double precision (a-h,o-z)
          integer          i, j, k, kr
          integer          n_level, n_k_val
          integer          ierr
          character*16     formc
          double precision omega, val_ks(n_k_val), por
          double precision h(n_k_val, n_level,n_level)
          double precision evs(n_k_val, n_level)
          double precision wks(n_k_val, n_level)
          write (formc, "(A1,I3,A6)") "(", n_k_val ,"F16.5)"
          h    = 0.d0
          evs  = 0.d0
          wks  = 0.d0
          por  = 1.0 * 3.1416  / n_k_val 
          do kr = -(n_k_val - 1)/2, (n_k_val - 1)/2
            k = kr + (n_k_val - 1)/2 + 1
            do j = (1 - n_level)/2, (n_level - 1)/2
              i = j + (n_level - 1)/2 + 1
              h(k, i, i) = 0.5 * ((kr * por + j)**2)
              if ( j .lt. (n_level - 1)/2 ) then
                h(k, i, i + 1) = omega * 0.5
              end if
              if ( j .gt. (1 - n_level)/2 ) then
                h(k, i, i - 1) = omega * 0.5
              end if
            end do
            val_ks(k) = kr * por 
          end do

          do k = 1, n_k_val
            ierr = 0
            call tred2e(n_level,
     &                  n_level,
     &                  h(k,:,:),
     &                  evs(k,:),
     &                  wks(k,:),
     &                  h(k,:,:))
            call tql2e (n_level,
     &                  n_level,
     &                  evs(k,:),
     &                  wks(k,:),
     &                  h(k,:,:),
     &                  ierr)
          end do

          write (6,trim(formc)) (val_ks(i),i = 1, n_k_val)
          do i = 1, n_level
            write (6, trim(formc)) (evs(k,i), k = 1, n_k_val)
          end do
          return
      End Subroutine calc
