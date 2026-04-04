         parameter(m=1024,n=512)
         real x(m,n),y(m,n),cori(m,n),dep(m,n),dep_swan(m,n)
         real h(m,n),breakwater(m,n),dep_brk(m,n)
         integer iobs(m,n)

         depmax=10.
         depmin=0.002
         slope=1./30.0
         dx=2.0
         dy=2.0
         y_shift=250.0
         breakwater_width = 30.0
         breakwater_length= 400.0
         y_break=-150.0
         x_break=0.0
        

         do j=1,n
         do i=1,m
          x(i,j)=(i-(m+1.0)/2.0)*dx
          y(i,j)=(j-(n+1.0)/2.0)*dy-y_shift
         enddo
         enddo

         width_surf=100.0
         width_inlet=300.0
         ramp_channel=150.0
         rlength_inlet = 100.0
         dep_inlet=5.0
         dep_basin=5.0
         flat=10.0

!   shoal
         a_in=150.0
         a_out=200.0
         b_in=80.0
         b_out=150.0
         height=9.0
         shoal_sitting = 10.0
         pi=3.1415926

         do j=1,n
         do i=1,m
!         dep(i,j)=hconst(i,j)    
         r=sqrt(x(i,j)*x(i,j)+y(i,j)*y(i,j))
         phi=atan2(y(i,j),x(i,j))
         r_out=a_out*a_out*b_out*b_out/
     &         (a_out*a_out*sin(phi)*sin(phi)
     &         +b_out*b_out*cos(phi)*cos(phi))
         r_out=sqrt(r_out)
         r_in=a_in*a_in*b_in*b_in/
     &         (a_in*a_in*sin(phi)*sin(phi)
     &         +b_in*b_in*cos(phi)*cos(phi))
         r_in=sqrt(r_in)

         dis=r_out-r
         rate=r_out-r_in
         h(i,j)=shoal_sitting-height*sin(pi/rate*dis)
!         print*,i,j,r,r_in,r_out,dis,h(i,j)
         if(r.gt.r_out.or.r.lt.r_in) h(i,j)=shoal_sitting
         if(y(i,j).gt.0.)h(i,j)=shoal_sitting
        enddo
        enddo

!        open(1,file='tmp.txt')
!         do j=1,n
!           write(1,100)(h(i,j),i=1,m)
!         enddo
!        close(1)
        

         do j=1,n
         do i=1,m
          dep(i,j)=flat
! slope
          if(y(i,j).gt.-width_surf)then
            dep(i,j)=flat-flat*(y(i,j)+width_surf)/width_surf
          endif
! channel
          if(abs(x(i,j)).le.ramp_channel)then
          if(dep(i,j).lt.dep_inlet*cos(pi/(2.0*ramp_channel)*x(i,j)))
     &       dep(i,j)=dep_inlet*cos(pi/(2.0*ramp_channel)*x(i,j))
          endif
! basin
          if(y(i,j).ge.rlength_inlet)then
            dep(i,j)=dep_basin
          endif

         enddo
         enddo


! combine

        do j=1,n
         do i=1,m
           dep(i,j)=min(dep(i,j),h(i,j))
         enddo
        enddo

! obstacles

        do j=1,n
         do i=1,m
           breakwater(i,j)=0.0
           iobs(i,j)=1
           dep_brk(i,j)=dep(i,j)
         enddo
        enddo        

        do i=412,612
         do j=300,301
           iobs(i,j)=0
           breakwater(i,j)=breakwater_width
         enddo
        enddo

         do j=1,n
         do i=1,m
           do ii=412,612
           do jj=300,301
            r=sqrt((x(i,j)-x(ii,jj))**2+(y(i,j)-y(ii,jj))**2)
            if (r.le.breakwater_width)then
              d=depmax-depmax*(breakwater_width-r)/breakwater_width
              if(dep_brk(i,j).gt.d)dep_brk(i,j)=d
            endif
           enddo
           enddo
         enddo
         enddo


        open(1,file='x_shoal_inlet.txt')
         do i=1,m
           write(1,100)(x(i,j),j=1,n)
         enddo
        close(1)
        open(1,file='y_shoal_inlet.txt')
         do i=1,m
           write(1,100)(y(i,j),j=1,n)
         enddo
        close(1)
    

        open(1,file='dep_shoal_inlet.txt')
         do i=1,m
           write(1,100)(dep(i,j),j=1,n)
         enddo
        close(1)

       open(1,file='dep_shoal_inlet_brk.txt')
         do i=1,m
           write(1,100)(dep_brk(i,j),j=1,n)
         enddo
        close(1)


        open(1,file='obs_shoal_inlet.txt')
         do i=1,m
           write(1,111)(iobs(i,j),j=1,n)
         enddo
        close(1)

        open(1,file='brk_shoal_inlet.txt')
         do i=1,m
           write(1,100)(breakwater(i,j),j=1,n)
         enddo
        close(1)


100     format(3000f12.3)
111     format(3000I3)

        end
