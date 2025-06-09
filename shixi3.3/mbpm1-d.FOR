c      正压原始方程模式
c      1985 10 29 by  Shen tongli
c      m=20 为x方向格点数，n=16 为y方向格点数，d为网格距，rm为放大系数
c      f为地转参数，w为工作数组，cla，clo分别为区域中心纬度和经度
c      dt为时间步长，s为平滑系数
c      ua，ub，uc分别为n-1，n，n+1时间层的x方向风速
c      va，vb，vc分别为n-1，n，n+1时间层的y方向风速
c      za，zb，zc分别为n-1，n，n+1时间层的位势高度
c      na用于控制12小时的预报；nb用于记录时间积分步数；nt2=72用于判别
c      是否积分12小时，是否该做内点平滑；nt4=6用于判定是否该做边界平滑；
c      nt5用于判定是否该做时间平滑。 
c      zo是为了减小重力惯性外波的波速，增加差分格式的稳定性而引入的位势高度。
      program shen2
	parameter(m=20,n=16,d=300000.0,cla=51.0,clo=118.0,dt=600.0)
      dimension ua(m,n),va(m,n),za(m,n),ub(m,n),vb(m,n),zb(m,n),
     *		uc(m,n),vc(m,n),zc(m,n),rm(m,n),f(m,n),w(m,n)
      zo=2500.0
      s=0.5
      nt2=72
      nt4=6
      nt5=36
      c1=dt/2.0
      c2=dt*2.0
c  为便于Grads做图而建立的位势高度场数据文件h.grd（包括初始场和预报场）    
      open(10,file='h.grd',form='unformatted')         

c  计算放大系数和地转参数,并写入数据文件中
      call cmf(rm,f,d,cla,m,n)
      open(1,file='rm.dat',status='replace')
       write(1,101) rm
      close(1)
 101  format(20f10.5)
      open(1,file='f.dat',status='replace')
       write(1,103) f
	close(1)
 103  format(20e15.5)

c  读入初始资料场 
      open(2,file='za.dat',status='old')
  	 read(2,102) za 
      close(2)
 102  format(20f6.0)

c  为便于做图,将初始数据资料写入二进制数据文件h.grd中
 	write(10) ((za(i,j),i=1,m),j=1,n)     

ccccccccccccccccccccccc加入地转风子程序后此处需要修改cccccccccccccccccccccccccc
c  计算地转风初值   
      call cgw(ua,va,za,rm,f,d,m,n)
      open(4,file='ua.dat',status='replace')
      write(4,104) ua
      close(4)
      open(5,file='va.dat',status='replace')
      write(5,104) va
      close(5)
 104  format(20f10.5) 
c 加入地转风子程序后以下注释，以上启用
c      open(4,file='ua.dat',status='old')
c     read(4,104) ua
c      close(4)
c      open(5,file='va.dat',status='old')
c      read(5,104) va
c      close(5)
c 104  format(20f10.5)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c   边值传送子程序
      call tbv(ub,vb,zb,ua,va,za,m,n)
      call tbv(uc,vc,zc,ua,va,za,m,n)

c   开始预报  
      do 10 na=1,2
       nb=0
c   欧拉后差积分1小时
      do 20 nn=1,6
       call ti(ua,va,za,ua,va,za,ub,vb,zb,rm,f,d,dt,zo,m,n)
       call ti(ua,va,za,ub,vb,zb,ua,va,za,rm,f,d,dt,zo,m,n)
       nb=nb+1
  20  continue

c   边界平滑子程序
      call ssbp(za,w,s,m,n)
      call ssbp(ua,w,s,m,n)
      call ssbp(va,w,s,m,n)

c   前差积分半步
      call ti(ua,va,za,ua,va,za,ub,vb,zb,rm,f,d,c1,zo,m,n)
c   中央差积分半步
      call ti(ua,va,za,ub,vb,zb,uc,vc,zc,rm,f,d,dt,zo,m,n)
      nb=nb+1
c  数组传送子程序
      call ta(ub,vb,zb,uc,vc,zc,m,n)
c  中央差积分一步,共积分11小时
      do 30 nn=1,66
       call ti(ua,va,za,ub,vb,zb,uc,vc,zc,rm,f,d,c2,zo,m,n)
       nb=nb+1
c  打印积分步数,na大循环步,nb小循环步
       call pv(na,nb)
c  判断是否积分12小时
       if(nb.eq.nt2) go to 80
c  判断是否做边界平滑
       if(nb/nt4*nt4.eq.nb) go to 40
       go to 50
  40   call ssbp(zc,w,s,m,n)
       call ssbp(uc,w,s,m,n)
       call ssbp(vc,w,s,m,n)

c  判断是否做时间平滑
  50   if(nb.eq.nt5) go to 60
       if(nb.eq.nt5+1) go to 60
       go to 70
c  时间平滑子程序
  60   call ts(ua,ub,uc,va,vb,vc,za,zb,zc,s,m,n)

c  数组传送,为下一轮积分做准备
  70   call ta(ua,va,za,ub,vb,zb,m,n)
       call ta(ub,vb,zb,uc,vc,zc,m,n)
  30  continue

c  区域内点平滑
  80  call ssip(zc,w,s,m,n,2)
      call ssip(uc,w,s,m,n,2)
      call ssip(vc,w,s,m,n,2)

c  打印积分步数
      call pv(na,nb)

c  数组传送,为后12小时的积分做准备
  10  call ta(ua,va,za,uc,vc,zc,m,n)

c  存放预报结果
      open(6,file='zc.dat',status='replace')
      write(6,102) zc
      close(6)
	write(10) ((zc(i,j),i=1,m),j=1,n)    
	open(7,file='uc.dat',status='replace')
      write(7,104) uc
      close(7)
      open(8,file='vc.dat',status='replace')
      write(8,104) vc
      close(8)
      stop
      end

c     computing map factors and coriolis parameter
c     rk为圆锥常数,rlq为兰勃特投影映像平面上赤道到北极点的距离,a为地球半径
c     sita为标准余纬,psx为区域中心余纬,r为模式中心到北极的距离
      subroutine cmf(rm,f,d,cla,m,n)
      dimension rm(m,n),f(m,n)
      rk=0.7156
      rlq=11423370.0
      a=6371000.0
      conv=57.29578
      w1=2.0/rk
      sita=30.0/conv
      psx=(90.0-cla)/conv

c  计算模式中心到北极的距离r 
      cel0=a*sin(sita)/rk
      cel=(tan(psx/2.0))/(tan(sita/2.0))
      r=cel0*cel**rk

c  确定网格坐标原点在地图坐标系中的位置
      xi0=-(m-1)/2.0
      yj0=r/d+(n-1)/2.0

c  求各格点至北极点的距离rl,(xj,yi)为模式各格点在地图坐标系中的位置  
      do 10 i=1,m
      do 10 j=1,n
      xi=xi0+(i-1)
      yj=yj0-(j-1)
      rl=sqrt(xi**2+yj**2)*d

c  求放大系数rm和柯氏参数f
      w2=(rl/rlq)**w1
      sinl=(1.0-w2)/(1.0+w2)
      rm(i,j)=rk*rl/(a*sqrt(1.0-sinl**2))
  10  f(i,j)=1.4584e-4*sinl
      return
      end

c     computing geostrophic winds
c   请同学编写地转风初值的子程序！！！应用书中（4.134）式，要求:能用中央差的地方一定要用中央差!
      subroutine cgw(ua,va,za,rm,f,d,m,n)

      dimension ua(m,n),va(m,n),za(m,n),rm(m,n),f(m,n)
      
c     定义重力加速度常数
      g = 9.8
c     定义网格边界
      m1 = m-1
      n1 = n-1
      
c     内部点计算 - 使用中央差分
      do 10 i=2,m1
      do 10 j=2,n1
c       u分量 = -放大系数*重力/科氏力*位势高度y方向导数
        ua(i,j) = -rm(i,j)*g*(za(i,j+1)-za(i,j-1))/(2.0*d*f(i,j))
c       v分量 = 放大系数*重力/科氏力*位势高度x方向导数
        va(i,j) = rm(i,j)*g*(za(i+1,j)-za(i-1,j))/(2.0*d*f(i,j))
10    continue
      
c     左右边界点计算 (i=1和i=m)
      do 20 j=2,n1
c       左边界：u用中央差，v用前向差
        ua(1,j) = -rm(1,j)*g*(za(1,j+1)-za(1,j-1))/(2.0*d*f(1,j))
        va(1,j) = rm(1,j)*g*(za(2,j)-za(1,j))/(d*f(1,j))
        
c       右边界：u用中央差，v用后向差
        ua(m,j) = -rm(m,j)*g*(za(m,j+1)-za(m,j-1))/(2.0*d*f(m,j))
        va(m,j) = rm(m,j)*g*(za(m,j)-za(m-1,j))/(d*f(m,j))
20    continue
      
c     上下边界点计算 (j=1和j=n)
      do 30 i=2,m1
c       下边界：u用前向差，v用中央差
        ua(i,1) = -rm(i,1)*g*(za(i,2)-za(i,1))/(d*f(i,1))
        va(i,1) = rm(i,1)*g*(za(i+1,1)-za(i-1,1))/(2.0*d*f(i,1))
        
c       上边界：u用后向差，v用中央差
        ua(i,n) = -rm(i,n)*g*(za(i,n)-za(i,n-1))/(d*f(i,n))
        va(i,n) = rm(i,n)*g*(za(i+1,n)-za(i-1,n))/(2.0*d*f(i,n))
30    continue
      
c     四个角点计算 - 使用单侧差分
c     左下角
      ua(1,1) = -rm(1,1)*g*(za(1,2)-za(1,1))/(d*f(1,1))
      va(1,1) = rm(1,1)*g*(za(2,1)-za(1,1))/(d*f(1,1))
      
c     右下角
      ua(m,1) = -rm(m,1)*g*(za(m,2)-za(m,1))/(d*f(m,1))
      va(m,1) = rm(m,1)*g*(za(m,1)-za(m-1,1))/(d*f(m,1))
      
c     左上角
      ua(1,n) = -rm(1,n)*g*(za(1,n)-za(1,n-1))/(d*f(1,n))
      va(1,n) = rm(1,n)*g*(za(2,n)-za(1,n))/(d*f(1,n))
      
c     右上角
      ua(m,n) = -rm(m,n)*g*(za(m,n)-za(m,n-1))/(d*f(m,n))
      va(m,n) = rm(m,n)*g*(za(m,n)-za(m-1,n))/(d*f(m,n))
      
      return
      end

c     time integrations
      subroutine ti(ua,va,za,ub,vb,zb,uc,vc,zc,rm,f,d,dt,zo,m,n)
      dimension ua(m,n),va(m,n),za(m,n),ub(m,n),vb(m,n),zb(m,n),
     *		uc(m,n),vc(m,n),zc(m,n),rm(m,n),f(m,n)
       c=0.25/d
       m1=m-1
       n1=n-1
      do 10 i=2,m1
      do 10 j=2,n1
      e=-c*rm(i,j)*((ub(i+1,j)+ub(i,j))*(ub(i+1,j)-ub(i,j))
     *		   +(ub(i,j)+ub(i-1,j))*(ub(i,j)-ub(i-1,j))
     *		   +(vb(I,j-1)+vb(i,j))*(ub(i,j)-ub(i,j-1))
     *		   +(vb(I,j)+vb(i,j+1))*(ub(i,j+1)-ub(i,j))
     *		   +19.6*(zb(i+1,j)-zb(i-1,j)))+f(i,j)*vb(i,j)
      uc(i,j)=ua(i,j)+e*dt
      g=-c*rm(i,j)*((ub(I+1,j)+ub(i,j))*(vb(i+1,j)-vb(i,j))
     *		   +(ub(I,j)+ub(i-1,j))*(vb(i,j)-vb(i-1,j))
     *		   +(vb(I,j-1)+vb(i,j))*(vb(i,j)-vb(i,j-1))
     *		   +(vb(I,j)+vb(i,j+1))*(vb(i,j+1)-vb(i,j))
     *		   +19.6*(zb(i,j+1)-zb(i,j-1)))-f(i,j)*ub(i,j)
  10  vc(i,j)=va(i,j)+g*dt
      do 20 i=2,m1
      do 20 j=2,n1
      h=-c*rm(i,j)*((ub(I+1,j)+ub(i,j))*(zb(i+1,j)-zb(i,j))
     *		   +(ub(I,j)+ub(i-1,j))*(zb(i,j)-zb(i-1,j))
     *		   +(vb(I,j-1)+vb(i,j))*(zb(i,j)-zb(i,j-1))
     *		   +(vb(I,j)+vb(i,j+1))*(zb(i,j+1)-zb(i,j))
     *	+2.0*(zb(i,j)-zo)*(ub(i+1,j)-ub(i-1,j)+vb(i,j+1)-vb(i,j-1)))
  20  zc(i,j)=za(i,j)+h*dt
      return
      end

c     time smoothimg
      subroutine ts(ua,ub,uc,va,vb,vc,za,zb,zc,s,m,n)
      dimension ua(m,n),va(m,n),za(m,n),
     *		ub(m,n),vb(m,n),zb(m,n),
     *		uc(m,n),vc(m,n),zc(m,n)
      m1=m-1
      n1=n-1
      do 10 i=2,m1
      do 10 j=2,n1
      ub(i,j)=ub(i,j)+s*(ua(i,j)+uc(i,j)-2.0*ub(i,j))/2.0
      vb(i,j)=vb(i,j)+s*(va(i,j)+vc(i,j)-2.0*vb(i,j))/2.0
  10  zb(i,j)=zb(i,j)+s*(za(i,j)+zc(i,j)-2.0*zb(i,j))/2.0
      return
      end

c     space smoothing for internal points 区域内5点平滑(正逆平滑)
c   请同学编写区域内5点平滑(正逆平滑)的子程序！！！应用书中（4.126）式
c   注：此程序必须设计成开关形式，保证既可选做正逆平滑，又可选做正平滑   l=1为只执行正平滑，l=2为执行正逆平滑.    
      subroutine ssip(a,w,s,m,n, l)
c     space smoothing for internal points 区域内5点平滑(正逆平滑)

      dimension a(m,n),w(m,n)
      m1=m-1
      n1=n-1
c     执行正平滑
      do 10 i=2,m1
      do 10 j=2,n1
      w(i,j)=a(i,j)*(1.0-s)+s/4.0*(a(i-1,j)+a(i+1,j)+a(i,j-1)+a(i,j+1))
  10  continue

c     将正平滑结果传回原数组
      do 20 i=2,m1
      do 20 j=2,n1
      a(i,j)=w(i,j)
  20  continue
      
c     如果l=2，执行逆平滑
      if(l.eq.2) then
c       执行逆平滑
        do 30 i=2,m1
        do 30 j=2,n1
        temp = s/4.0*(a(i-1,j)+a(i+1,j)+a(i,j-1)+a(i,j+1))
        w(i,j) = a(i,j)*(1.0+s) - temp
  30    continue
        
c       将逆平滑结果传回原数组
        do 40 i=2,m1
        do 40 j=2,n1
        a(i,j)=w(i,j)
  40    continue
      endif
      return
      end


c     space smoothing for boundary points 边界九点平滑
      subroutine ssbp(a,w,s,m,n)
      dimension a(m,n),w(m,n)
      m1=m-1
      m3=m-3
      n1=n-1
      n2=n-2
      n3=n-3
      do 10 i=2,m1
      do 10 j=2,n1,n3
  10  w(i,j)=a(i,j)+0.5*s*(1.0-s)*
     *	     (a(i-1,j)+a(i+1,j)+a(i,j-1)+a(i,j+1)-4.0*a(i,j))+0.25*s*s*
     *	     (a(i-1,j-1)+a(i-1,j+1)+a(i+1,j-1)+a(i+1,j+1)-4.0*a(i,j))
      do 20 i=2,m1,m3
      do 20 j=3,n2
  20  w(i,j)=a(i,j)+0.5*s*(1.0-s)*
     *	     (a(i-1,j)+a(i+1,j)+a(i,j-1)+a(i,j+1)-4.0*a(i,j))+0.25*s*s*
     *	     (a(i-1,j-1)+a(i-1,j+1)+a(i+1,j-1)+a(i+1,j+1)-4.0*a(i,j))
      do 30 i=2,m1
      do 30 j=2,n1,n3
  30  a(i,j)=w(i,j)
      do 40 i=2,m1,m3
      do 40 j=3,n2
  40  a(i,j)=w(i,j)
      return
      end


c     transmiting arrays  数组传送
      subroutine ta(ua,va,za,ub,vb,zb,m,n)
      dimension ua(m,n),va(m,n),za(m,n),ub(m,n),vb(m,n),zb(m,n)
      do 10 i=1,m
      do 10 j=1,n
      ua(i,j)=ub(i,j)
      va(i,j)=vb(i,j)
  10  za(i,j)=zb(i,j)
      return
      end

c     transmiting boundary valaus  赋固定边界值
      subroutine tbv(ua,va,za,ub,vb,zb,m,n)
      dimension ua(m,n),va(m,n),za(m,n),ub(m,n),vb(m,n),zb(m,n)
      m1=m-1
      n1=n-1
      do 10 i=1,m
      do 10 j=1,n,n1
      ua(i,j)=ub(i,j)
      va(i,j)=vb(i,j)
  10  za(i,j)=zb(i,j)
      do 20 i=1,m,m1
      do 20 j=1,n
      ua(i,j)=ub(i,j)
      va(i,j)=vb(i,j)
  20  za(i,j)=zb(i,j)
      return
      end

c     printing variables  打印积分步数
      subroutine pv(na,nb)
      write(*,100)na,nb
 100  format(//////5x,3hna=,i3,5x,3hnb=,i2/)
      return
      end 
