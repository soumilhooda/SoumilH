!------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------
Implicit None
Character(len=100)     ::   infile
Integer                ::   stat1,stat2,jj,kk,ll,nx,ny, inx,inbox,in,inn,jn,jnn,dummy(1)
Real                   ::   alat,alon,alatmin,alatmax,alonmin,alonmax,gridreso,ixx,iyy,val,alon1,alat1
Real,Allocatable       ::   blat(:),blon(:),tmp(:)
real,Allocatable       ::   asum(:,:),avg(:,:),point(:,:)
Parameter(alatmin=15,alatmax=20,alonmin=77,alonmax=82,gridreso=0.005) 
!------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------
nx=(alonmax-alonmin)/gridreso+1
ny=(alatmax-alatmin)/gridreso+1
write(*,*)nx,ny
allocate(blat(ny),blon(nx),tmp(24))
allocate(asum(nx,ny))
allocate(avg(nx,ny))
allocate(point(nx,ny))

do jj=1,nx
  blon(jj)=alonmin+(jj-1)*gridreso
enddo

do jj=1,ny
   blat(jj)=alatmin+(jj-1)*gridreso
enddo

asum=0
point=00
open(1,file='list',iostat=stat1)
if(stat1 == 0 )then
do
 read(1,*,iostat=stat1)infile
 if(stat1 /= 0)exit
 open(2,file=trim(infile),iostat=stat2)
 write(*,*)infile
!---------------------------------------------------------------------
 open(100,file='road.grd',access='direct',recl=1*nx*ny)
!---------------------------------------------------------------------
   if(stat2 == 0)then
   do
       read(2,*,iostat=stat2)alon,alat,alon1,alat1,val
!------------------------------------------------
!------------------------------------------------
       if(stat2 /= 0)exit
          ixx=(alon-alonmin)/gridreso+1.0                            !!! X Postion of grid point
          iyy=(alat-alatmin)/gridreso+1.0                            !!! Y position of grid point

          in=floor(ixx)
          inn=ceiling(ixx)
          jn=floor(iyy)
          jnn=ceiling(iyy)
          inx = inbox(alon, alat, blon(in),blon(inn),blat(jn),blat(jnn))
!         write(*,*)alat,alon,val,ixx,iyy,inx,in,inn,jn,jnn
!         if(inx.eq.1 .and. val >=1 .and. val < 25.0) then
          if(inx.eq.1) then
                   asum(in,jn)=asum(in,jn)+val
		   point(in,jn)=point(in,jn)+1
                endif 
     enddo !!! End of File 
    endif  !!! End of File
enddo        !! number of Flies
endif        !! Number of Files
write (*,*)'reading done'
!---------------------------------------------------------------------
!---------------------------------------------------------------------
do jj=1,nx
  do kk=1,ny
         if(point(jj,kk) >= 1)then
    write(777,'(I5,2x,I5,f8.2)')jj,kk,point(jj,kk)
	      avg(jj,kk)=asum(jj,kk)/point(jj,kk)
	  else
	      avg(jj,kk)=-999.0
	  endif
  enddo
enddo
           write(100,rec=1)avg
!--------------------------------------------------------------------
stop
end
!-----------------------------------------------------------------------
!   Inbox Function
!-----------------------------------------------------------------------
integer function inbox(x,y,xl,xr,yb,yt)
implicit none
real x, y, xl, xr, yb, yt
if(x.gt.xl.and.x.lt.xr.and.y.gt.yb.and.y.lt.yt) then
   inbox=1
else
  inbox=0
endif
return
end function inbox
!-----------------------------------------------------------------------
