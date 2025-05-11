!module differencemethod!!
! xiangjun shi  20190416 !
!!!!!!!!!!!!!!!!!!!!!!!!!!
   module differencemethod
     implicit none
     private
     save
	 integer, parameter :: nx=50     ! X space resolotion	空间网格数(可修改)	   
	 integer, parameter :: nt=6      ! total time step number  总时间积分数(可修改)
	 real, parameter :: dx=20.0 	 ! dx  空间格距长度(可修改)
	 real, parameter :: dt=10		 ! dtime  时间步长(可修改)
	 real, parameter :: C=5.0        ! speed  相速度(可修改)  
	 real, parameter :: A2=0.5		 ! now timepoint
	 real, parameter :: B2=0.5 		 ! **next	timepoint  A1+B1=1.0
	 real, parameter :: Pi=3.1415926
	 public :: FXzhong,mean1,mean2
	 public :: Pi,nx,dt,nt,C,dx,A2,B2

     contains
 
	 subroutine FXqian(Xin,FXout)   !!!!空间前差子程序
	   implicit none 
	   real, intent(in)  :: Xin(nx)
	   real, intent(out) :: FXout(nx)
	   real Xtemp(0:nx+1)
	   integer i
	   Xtemp(1:nx)=Xin(1:nx)
       Xtemp(0)   =Xin(nx)
       Xtemp(nx+1)=Xin(1)
       do i=1,nx
	      FXout(i)=-C*(Xtemp(i+1)-Xtemp(i))/dx
       enddo
	 endsubroutine FXqian

	 subroutine FXzhong(Xin,FXout)   !!!!空间中央差子程序
	   implicit none 
	   real, intent(in)  :: Xin(nx)
	   real, intent(out) :: FXout(nx)
	   real Xtemp(0:nx+1)
	   integer i
	   Xtemp(1:nx)=Xin(1:nx)
       Xtemp(0)   =Xin(nx)
       Xtemp(nx+1)=Xin(1)
       do i=1,nx
          FXout(i)=-C*(Xtemp(i+1)-Xtemp(i-1))/(2*dx)
       enddo
	 endsubroutine FXzhong

	 subroutine FXhou(Xin,FXout)   !!!!空间后差子程序
	   implicit none 
	   real, intent(in)  :: Xin(nx)
	   real, intent(out) :: FXout(nx)
	   real Xtemp(0:nx+1)
	   integer i
	   Xtemp(1:nx)=Xin(1:nx)
       Xtemp(0)   =Xin(nx)
       Xtemp(nx+1)=Xin(1)
       do i=1,nx
          FXout(i)=-C*(Xtemp(i)-Xtemp(i-1))/dx
       enddo
	 endsubroutine FXhou



	 subroutine mean1(Xin,mean)     !!!!数据求平均值的子程序
	   implicit none 
	   real, intent(in)  :: Xin(nx)
	   real, intent(out) :: mean
	   real sum
	   integer i
       sum=0.
       do i=1,nx
          sum=sum+Xin(i)
       enddo
	   mean=sum/nx
	 endsubroutine mean1

	 subroutine mean2(Xin,mean)     !!!!数据的平方求平均值的子程序
	   implicit none 
	   real, intent(in)  :: Xin(nx)
	   real, intent(out) :: mean
	   real sum
	   integer i
       sum=0.
       do i=1,nx
          sum=sum+Xin(i)**2
       enddo
	   mean=(sum/nx)**0.5
	 endsubroutine mean2



    end module differencemethod


!!!!!! main program  !!!!!
! xiangjun shi  20190416 !
!!!!!!!!!!!!!!!!!!!!!!!!!!
    program main     !!!!主程序
	   use differencemethod,only:nx,dt,nt,C,dx,A2,B2,Pi, &
	                             FXzhong,mean1,mean2    !!!!!调用上面differencemethod模块中的部分子程序和数组
       implicit none
       real Xinit(1:nx)     !!!!定义输入数组
	   real Xnow(1:nx),Xnext(1:nx),Xnexttemp(1:nx),Xprevious(1:nx)
	   real Xnowdt(1:nx),Xnexttempdt(1:nx)
       real Xoutput(1:nx,0:5)     !!!!定义输出数组(共6列，第一列为初始值，第二列为解析解，第三-六列为四个时间积分时刻的数值解)
	   real Xmean,Xmean1(0:5),Xmean2(0:5)
       ! output 6 timepoint 《0-Initial Value,1-Analytic Solutions,2-5 Numerical Solution》
       integer i,ip,t,toutput,Nishift
       
       !!!设定初始值(可修改)
       !Xinit(1:nx)=0
       !Xinit(200:220)=1
       do i=1,nx
		  Xinit(i)=0.5+sin(2*2*Pi*i/50)
		  !Xinit(i)=0.5+sin(1*2*Pi*i/50) 
		  !Xinit(i)=0.5+cos(2*Pi*i/3.0)
	   enddo 
       !!!!!!!!!!

       !!!给输出数组的第一列赋值初始值
       Xnow(1:nx)=Xinit(1:nx)
       Xoutput(1:nx,0)=Xinit(1:nx)        !0-Initial Value
	   !!!!!!!!!

	   !!!给输出数组的第二列赋值解析解
	   Nishift=int(C*nt*dt/dx)
	   Nishift=mod(Nishift,nx)
	   write(*,*) C*nt*dt/dx
	   do i=1,nx
		  ip=i-Nishift
		  ip=mod(ip+nx-1,nx)+1
		  !write(*,*) i,ip
		  Xoutput(i,1)=Xinit(ip)	       !1-Analytic Solutions
	   enddo
	   !!!!!!!!!

	   !!!采用空间中央差、时间中央差格式求取数值解，并给输出数组的第三-六列赋值数值解
	   ! first timestep  !!!!
	   call FXzhong(Xnow,Xnowdt)   !!!空间中央差格式
	   Xnexttemp=Xnow+Xnowdt*dt   
	   call FXzhong(Xnexttemp,Xnexttempdt)
	   Xnext=Xnow+Xnowdt*dt*A2+Xnexttempdt*dt*B2
	   Xprevious=Xnow
	   Xnow=Xnext


       do t=2,nt    !!!!第二步开始为中央差格式
          call 	FXzhong(Xnow,Xnowdt)   !!!空间中央差格式
		  Xnext=Xprevious+Xnowdt*dt*2  !!!时间中央差格式
	      Xprevious=Xnow
	      Xnow=Xnext
		  do i=1,nx
             if (Xnow(i).gt.1000)  Xnow(i)=1000  !!!输出数据设置上限1000
			 if (Xnow(i).lt.-1000) Xnow(i)=-1000  !!!输出数据设置下限-1000
          enddo
          if (mod(t,(nt+3)/4).eq.0) then    !!!每隔总时间积分数的1/4输出一次数值，记录在xnow第三-六列
             toutput=int(t/((nt+3)/4))
             !write(*,*) toutput
             Xoutput(1:nx,toutput+1)=Xnow(1:nx)
          endif  
		  if (t.eq.nt)  Xoutput(1:nx,5)=Xnow(1:nx)
       enddo
	   !!!!!!!!!!!

	   !!!将结果输出至文件result.txt,用于后续绘图
       open(22,file='result.txt',form='formatted')
	   write(22, '(a4,f4.1,a4,f4.1,a4,f5.2,a4,f4.1,a4,f4.1a4,I4)') &
	        	"dx=",dx,"C=",C,"dt=",dt,"A2=",A2,"B2=",B2,"nt=",nt
       do i=1,nx
          write(22,'(f8.2,2x,6f8.2)') i*dx,Xoutput(i,0:5)
       enddo
       close(22)
	   !!!!!!!!!!

	   !!!求取初始值、解析解、不同时刻数值解的平均值和平方的平均值，并输出
	   do t=0,5
          call mean1(Xoutput(1:nx,t),Xmean)
		  Xmean1(t)=Xmean
		  call mean2(Xoutput(1:nx,t),Xmean)
		  Xmean2(t)=Xmean
	   enddo
	   write(*,'(a8,4x,6f8.4)')	"Xmean1:", Xmean1(0:5)
	   write(*,'(a8,4x,6f8.4)')	"Xmean2:", Xmean2(0:5)
	   !!!!!!

	   !for Excel Plot
	   open(22,file='Excel.txt',form='formatted')
       do i=1,nx
          write(22,'(f8.2,2x,6f8.2)') i*dx,Xoutput(i,0),Xoutput(i,1),Xoutput(i,5)
       enddo
       close(22)


     end program main

