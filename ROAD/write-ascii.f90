integer, parameter :: nx=1001,ny=1001,nt=8
real, parameter    :: reso=0.005,slat=15.0025,slon=77.0025
real(kind=4)  :: var11(nx,ny),var22(nx,ny),var33(nx,ny),road(nx,ny)
real(kind=4)  :: var44(nx,ny,nt),var55(nx,ny,nt),var66(nx,ny,nt)
real(kind=4)  :: alat(ny),alon(nx)
open(11,file='light2013-2020.grd',access='direct',recl=nx*ny)
open(22,file='ndvi2013-2020.grd',access='direct',recl=nx*ny)
open(33,file='lulc2013-2020.grd',access='direct',recl=nx*ny)
open(10,file='road.grd'         ,access='direct',recl=nx*ny)
open(991,file='light2013-2020.ascii')
open(992,file='ndvi2013-2020.ascii')
open(993,file='lulc2013-2020.ascii')
open(994,file='road.ascii')
  read(10,rec=1)road
  where (road <0)
     road=00
  endwhere
do itime=1,nt
  read(11,rec=itime)var11
  read(22,rec=itime)var22
  read(33,rec=itime)var33
  where(var11 <=0)
   var22=-999.0
   var33=-999.0
 endwhere
  var44(:,:,itime)=var11(:,:)
  var55(:,:,itime)=var22(:,:)
  var66(:,:,itime)=var33(:,:)
enddo

do jj=1,nx
  alon(jj)=slon+(jj-1)*reso
enddo
do jj=1,ny
  alat(jj)=slat+(jj-1)*reso
enddo

do jj=1,nx
    do kk=1,ny
        if(all(var44(jj,kk,1:8)>0))write(991,'(10(f12.5,2x))')alat(kk),alon(jj),(var44(jj,kk,itime),itime=1,nt)
        if(all(var44(jj,kk,1:8)>0))write(992,'(10(f12.5,2x))')alat(kk),alon(jj),(var55(jj,kk,itime),itime=1,nt)
        if(all(var44(jj,kk,1:8)>0))write(993,'(10(f12.5,2x))')alat(kk),alon(jj),(var66(jj,kk,itime),itime=1,nt)
        if(all(var44(jj,kk,1:8)>0))write(994,'(3(f12.5,2x))')alat(kk),alon(jj),road(jj,kk)
    enddo
enddo
stop
end
