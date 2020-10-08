module global
integer i

integer,parameter:: nb=53,nh=21,nbp=40 ! original nb is 58

real(8) gridh(nh)
real(8),parameter::lambdab=0.6d0,tau_h=0.05d0,kappa_h=0.035d0,premium=0.03d0
! the h_supply is calculated from berueu of statistics, china. the original value is 1.8.
real(8),parameter:: lambdab_old=0.7d0,rb=0.0325d0,rm=0.05d0,beta=0.975d0
real(8),parameter:: delta_h=0.0375d0,g=0d0,h_constant=75d0,delta=0.1d0,alpha=0.9d0
real(8),parameter:: phi=0.8d0 ,psi=8d0,gama=0.8d0,bconstant=120d0,sigma=2d0,land=0.15d0,eta=0.5d0
!real(8),parameter:: bconstant=2.85d0 i used to set 2.85
! the annual rent/ price in china is about 3%

! kf can mean contract tax
integer, parameter ::agemin=21,agemax=90,retire=60

integer,parameter:: ne=5
real(8),parameter::rho=0.95d0,sig=sqrt(0.03d0)  ! orginal taken from ...
!real(8),parameter::rho=0.9d0,sig=sqrt(0.07d0) ! makes the wealth distribution match the data better...

real(8),parameter:: small=-1000000000d0
integer,parameter:: small_int=-100000000

real(8), parameter::period(21:60)=(/1d0&
,1.037625364d0&
,1.074072191d0&
,1.109340482d0&
,1.143430235d0&
,1.176341452d0&
,1.208074132d0&
,1.238628276d0&
,1.268003882d0&
,1.296200952d0&
,1.323219485d0&
,1.349059481d0&
,1.373720941d0&
,1.397203864d0&
,1.41950825d0&
,1.440634099d0&
,1.460581411d0&
,1.479350187d0&
,1.496940426d0&
,1.513352128d0&
,1.528585294d0&
,1.542639922d0&
,1.555516014d0&
,1.567213569d0&
,1.577732588d0&
,1.587073069d0&
,1.595235014d0&
,1.602218422d0&
,1.608023293d0&
,1.612649628d0&
,1.616097426d0&
,1.618366687d0&
,1.619457411d0&
,1.619369598d0&
,1.618103249d0&
,1.615658363d0&
,1.61203494d0&
,1.607232981d0&
,1.601252484d0&
,1.594093451d0/)


real(8) gride(ne),markov(ne,ne),gridb(nb),output
  ! these may change!!!!!!!!!!
real(8) bfmin,bfmax,bmax,bmin,p,d,replace,qb,w,m_y(agemin:agemax,ne)

real(8) ownership,wealth_gini,networthratio,r,base

real(8) v_initial(ne,nh,nb,agemin:agemax),v_new(ne,nh,nb,agemin:agemax),v_old(ne,nh,nb,agemin:agemax)
real(8) welfare_gain(ne,nh,nb,agemin:agemax),c_old(ne,nh,nb,agemin:agemax),s_old(ne,nh,nb,agemin:agemax)

real(8) h_supply, construction_lasttime,wealth_lasttime,qm

integer pu_old(ne,nh,nb,agemin:agemax)

real(8) sell_a,hold_a, renthouse_a, purchase_a,bequest_a,initialwealth,a_a,initialwealth_new,construction,construction_new

real(8) h2140(nh),h6190(nh),h4160(nh)

real(8) maxhouse(agemin:agemax)
real(8) survival(agemin:agemax+1), death(agemin:agemax+1)
!define all the policy matrixs

real(8) life_measure(agemin:agemax),life_income1(14),life_measure_old(agemin:agemax)
real(8) life_income(agemin:agemax),life_income2(14),life_income3(14)

real(8) v(ne,nh,nb,agemin:agemax),maxincome
real(8) c(ne,nh,nb,agemin:agemax),s(ne,nh,nb,agemin:agemax),age5(14),life_cons5(14)
real(8) life_house5(14),life_asset5(14),life_ownership5(14)
integer pu(ne,nh,nb,agemin:agemax)
real(8) v_adjust(ne,nh,nb,agemin:agemax)
real(8) popgrowth,back(agemin:agemax)

integer,allocatable::house(:,:),income(:, :)
real(8),allocatable::a(:,:),cons(:,:),hold(:,:),purchase(:,:)

integer zeropoint

real(8) n_h,networth,consexp,houseexp,totalincome,bound,housebound,n_f,totallabor,totalexp
real(8) wealth75,wealth50,wealth60,bequestratio,employmentratio,houseexpratio,cons_a

real(8) life_cons(agemin:agemax),life_house(agemin:agemax),maxsave(agemin:agemax)
real(8) life_asset(agemin:agemax),life_ownership(agemin:agemax),m(ne,nh,nb,agemin:agemax)
real(8) m_initial(ne,nh,nb,agemin:agemax)

real(8) life_cons1(14),life_house1(14)
real(8) life_asset1(14),life_ownership1(14)

real(8) life_cons2(14),life_house2(14)
real(8) life_asset2(14),life_ownership2(14)

real(8) life_cons3(14),life_house3(14)
real(8) life_asset3(14),life_ownership3(14)

real(8) s_1(ne,nh,nb,agemin:agemax),c_1(ne,nh,nb,agemin:agemax)
real(8) s_2(ne,nh,nb,agemin:agemax),c_2(ne,nh,nb,agemin:agemax)
real(8) s_3(ne,nh,nb,agemin:agemax),c_3(ne,nh,nb,agemin:agemax)
integer pu_1(ne,nh,nb,agemin:agemax),pu_2(ne,nh,nb,agemin:agemax),pu_3(ne,nh,nb,agemin:agemax)
real(8) m1(ne,nh,nb,agemin:agemax),m2(ne,nh,nb,agemin:agemax),m3(ne,nh,nb,agemin:agemax)

real(8) unconstrained, saver,constrained(agemin:agemax),pension,housetax

real(8) v1(ne,nh,nb,agemin:agemax),v2(ne,nh,nb,agemin:agemax),v3(ne,nh,nb,agemin:agemax)

real(8) trans1,trans2,trans3,construction1,construction2,construction3,p1,p2,p3,r1,r2,r3,w1,w2,w3

real(8) pu1,pu2,pu3,kl1,kl2,kl3,totalmeasure1,totalmeasure2,totalmeasure3

real(8) ownership1,ownership2,ownership3

real(8) h2_25(nh),h2_45(nh),h2_65(nh)
real(8) gridb_old(nb),gridb_new(nb),shouru_old(agemin:agemax,ne),shouru_new(agemin:agemax,ne),labor(agemin:retire,ne)

real(8) bequestpositive,totalmeasure

real(8) klratio_lasttime,klratio_new,klratio,piratio1,piratio2,piratio3

real(8) timestart,timeend1,timeend2,retireincome,timeend,age(agemin:agemax),error
real(8) bfnext,bpmax,bpmin,resource,temp,temp2,temp3,shouru(agemin:agemax,ne)
real(8) tau_ss
real(8), parameter:: growth=0.08d0
!real(8) gridb_neg(nb*0.6d0)


real(8) maxsaving(agemin:agemax)
integer max_index(agemin:agemax)

real(8) maxhousing(agemin:agemax)
integer max_index_h(agemin:agemax)

contains

subroutine gridb_gen
use toolbox
integer b,j,e
real(8) gridb_positive(28),gridb_negative(nb-28+1)
bmax=360d0 ! i dont know whether this is correct or not. from the life cycle plot plot, I find that it is around 20.
bmin=-85d0 ! the numerical shows that for each period , no one will be less than -65. this is subject to the parameter, ofcourse.
! how to set the growth of the distance?? need some adjustment?
call grid_Cons_Grow(gridb_positive,0d0,bmax,growth) ! growing distance
call grid_cons_grow(gridb_negative,0d0,-bmin,growth)
gridb_negative=-gridb_negative
call sort(gridb_negative)
gridb(1:nb-28)=gridb_negative(1:nb-28)
gridb(nb-28+1:nb)=gridb_positive(1:28)
zeropoint=nb-28+1

open(1, file='gridb.xls', status='replace') 
do b=1,nb
 write(1,*) gridb(b)
end do
close(1) 

end

subroutine tauchen(markov,s)
use toolbox
real(8) markov(ne,ne),s(ne),step
integer i,j,k
s(1)    = -3d0*sqrt(sig**2/(1-rho**2));
s(Ne)    =  +3d0*sqrt(sig**2/(1-rho**2));
step    = (s(Ne)-s(1))/(Ne-1);
do i=2,(Ne-1)
   s(i) = s(i-1) + step; 
end do
do j = 1,Ne
    do k = 1,Ne
        if (k == 1) then
            markov(j,k) = normalCDF((s(1) - rho*s(j) + step/2) / sig);
        else if (k == Ne) then
            markov(j,k) = 1 - normalCDF((s(Ne)  - rho*s(j) - step/2) / sig);
        else
            markov(j,k) = normalCDF((s(k)  - rho*s(j) + step/2) / sig) - normalCDF((s(k) - rho*s(j) - step/2) / sig)
        end if
    end do
end do

end

subroutine bilinear(y,ay,ny,x,ax,nx,xl,xr,yu,yd,failu,fairu,faild,faird,outbound)
real(8),intent(in):: y,ay(ny),x,ax(nx)
integer,intent(in):: ny,nx
real(8),intent(out)::failu,fairu,faild,faird
integer,intent(out):: xl,xr,yu,yd
integer i,outbound
outbound=0
xl=1
xr=1
yu=1
yd=1
if (y.le.ay(1)) then

yu=2
yd=1

elseif (y.ge.ay(ny)) then
yu=ny
yd=ny-1
else

do i=2,ny
if (ay(i).ge.y) then
yu=i
yd=i-1
exit
end if
end do
end if

if (x.le.ax(1)) then

xr=2
xl=1

elseif (x.ge.ax(nx)) then
xr=nx
xl=nx-1
else
do i=2,nx
if (ax(i).ge.x) then
xl=i-1
xr=i
exit
end if
end do
end if

failu=    ((ax(xr)-x)*(ay(yd)-y) )   /(   (ax(xr)-ax(xl))*(ay(yd)-ay(yu))    )
faild=     ( (ax(xr)-x)*(y-ay(yu)) )    /(    (ax(xr)-ax(xl))*(ay(yd)-ay(yu))    )
fairu=    ((x-ax(xl))*(ay(yd)-y) )  /(   (ax(xr)-ax(xl))*(ay(yd)-ay(yu))   )
faird=   ( (x-ax(xl))*(y-ay(yu)))    /(    (ax(xr)-ax(xl))*(ay(yd)-ay(yu))    )

end



function judge(x) 
logical x
integer judge
if (x .eqv..true.) then
judge=1
else
judge=0
end if
end

subroutine age_gen
integer j
age(agemin)=21d0
do j=agemin+1,agemax
age(j)=age(j-1)+1d0
end do
end 

subroutine linear(x,a,il,ir,fai)
integer i
real(8),intent(in),dimension(:):: a
real(8),intent(in):: x
real(8),intent(out)::fai
integer,intent(out)::il,ir
integer na
fai=0d0
il=1
ir=1
na=size(a)
if (x.le.a(1)) then
il=1
ir=2
fai=(x-a(1))/(a(2)-a(1))
end if
if (x.ge.a(na)) then
il=na-1
ir=na
fai=(x-a(na-1))/(a(na)-a(na-1))
end if
do i=2,na
if (x.le.a(i)) then
il=i-1
ir=i
fai=(x-a(il))/(a(ir)-a(il))
exit
end if
end do
fai=1d0-fai

end

function hdist(agestart,ageend)
integer h,agestart,ageend,j
real(8) hdist(nh)
hdist=0d0
do j=agestart,ageend
     do h=1,nh
     hdist(h)=hdist(h)+sum(m(:,h,:,j))
     end do
end do

hdist=hdist/sum(m(:,:,:,agestart:ageend))
end

function hshare(hdist,percentile)
real(8) hdist(nh),s,hshare,percentile
integer i,h
s=0d0
hshare=0d0
do h=1,nh
s=s+hdist(h)
if (s .ge. percentile) then
hshare=gridh(h)
exit
end if
end do
end 

subroutine gridh_gen
use toolbox
integer h
gridh(1)=0d0
call Grid_Cons_Grow(gridh(2:nh),6.85d0,130d0,0.17d0)! the original one is equal distance.
!call Grid_Cons_Grow(gridh,0d0,140d0,0.15d0)
! currently, for medium aged people, most can afford the largest house.
! this is not realistic.
!print*,'housing grid'

open(1, file='gridh.xls', status='replace') 
  do h=1,nh
 write(1,*) gridh(h)
end do
close(1) 
end 


subroutine array_spline(x_orig, y_orig,n,x_eval,y_eval,n_eval)
integer n,n_eval,i
real(8) x_orig(n),y_orig(n),b(n),c(n),d(n)
real(8) x_eval(n_eval),y_eval(n_eval)

call spline (x_orig, y_orig, b, c, d,n) 
do i=1, n_eval
 
  y_eval (i)= ispline(x_eval(i), x_orig, y_orig, b, c, d, n)
  
end do
end 
!
 

subroutine spline (x, y, b, c, d, n)
!======================================================================
!  Calculate the coefficients b(i), c(i), and d(i), i=1,2,...,n
!  for cubic spline interpolation
!  s(x) = y(i) + b(i)*(x-x(i)) + c(i)*(x-x(i))**2 + d(i)*(x-x(i))**3
!  for  x(i) <= x <= x(i+1)
!  Alex G: January 2010
!----------------------------------------------------------------------
!  input..
!  x = the arrays of data abscissas (in strictly increasing order)
!  y = the arrays of data ordinates
!  n = size of the arrays xi() and yi() (n>=2)
!  output..
!  b, c, d  = arrays of spline coefficients
!  comments ...
!  spline.f90 program is based on fortran version of program spline.f
!  the accompanying function fspline can be used for interpolation
!======================================================================
implicit none
integer n
double precision x(n), y(n), b(n), c(n), d(n)
integer i, j, gap
double precision h

gap = n-1
! check input
if ( n < 2 ) return
if ( n < 3 ) then
  b(1) = (y(2)-y(1))/(x(2)-x(1))   ! linear interpolation
  c(1) = 0.
  d(1) = 0.
  b(2) = b(1)
  c(2) = 0.
  d(2) = 0.
  return
end if
!
! step 1: preparation
!
d(1) = x(2) - x(1)
c(2) = (y(2) - y(1))/d(1)
do i = 2, gap
  d(i) = x(i+1) - x(i)
  b(i) = 2.0*(d(i-1) + d(i))
  c(i+1) = (y(i+1) - y(i))/d(i)
  c(i) = c(i+1) - c(i)
end do
!
! step 2: end conditions 
!
b(1) = -d(1)
b(n) = -d(n-1)
c(1) = 0.0
c(n) = 0.0
if(n /= 3) then
  c(1) = c(3)/(x(4)-x(2)) - c(2)/(x(3)-x(1))
  c(n) = c(n-1)/(x(n)-x(n-2)) - c(n-2)/(x(n-1)-x(n-3))
  c(1) = c(1)*d(1)**2/(x(4)-x(1))
  c(n) = -c(n)*d(n-1)**2/(x(n)-x(n-3))
end if
!
! step 3: forward elimination 
!
do i = 2, n
  h = d(i-1)/b(i-1)
  b(i) = b(i) - h*d(i-1)
  c(i) = c(i) - h*c(i-1)
end do
!
! step 4: back substitution
!
c(n) = c(n)/b(n)
do j = 1, gap
  i = n-j
  c(i) = (c(i) - d(i)*c(i+1))/b(i)
end do
!
! step 5: compute spline coefficients
!
b(n) = (y(n) - y(gap))/d(gap) + d(gap)*(c(gap) + 2.0*c(n))
do i = 1, gap
  b(i) = (y(i+1) - y(i))/d(i) - d(i)*(c(i+1) + 2.0*c(i))
  d(i) = (c(i+1) - c(i))/d(i)
  c(i) = 3.*c(i)
end do
c(n) = 3.0*c(n)
d(n) = d(n-1)
end subroutine spline

  function ispline(u, x, y, b, c, d, n)
!======================================================================
! function ispline evaluates the cubic spline interpolation at point z
! ispline = y(i)+b(i)*(u-x(i))+c(i)*(u-x(i))**2+d(i)*(u-x(i))**3
! where  x(i) <= u <= x(i+1)
!----------------------------------------------------------------------
! input..
! u       = the abscissa at which the spline is to be evaluated
! x, y    = the arrays of given data points
! b, c, d = arrays of spline coefficients computed by spline
! n       = the number of data points
! output:
! ispline = interpolated value at point u
!=======================================================================
implicit none
double precision ispline
integer n
double precision  u, x(n), y(n), b(n), c(n), d(n)
integer i, j, k
double precision dx

! if u is ouside the x() interval take a boundary value (left or right)
if(u <= x(1)) then
  ispline = y(1)
  return
end if
if(u >= x(n)) then
  ispline = y(n)
  return
end if

!*
!  binary search for for i, such that x(i) <= u <= x(i+1)
!*
i = 1
j = n+1
do while (j > i+1)
  k = (i+j)/2
  if(u < x(k)) then
    j=k
    else
    i=k
   end if
end do
!*
!  evaluate spline interpolation
!*
dx = u - x(i)
ispline = y(i) + dx*(b(i) + dx*(c(i) + dx*d(i)))
end function ispline


function u(c,h)
real(8) c,h,rent,u
    !u=(c**(1d0-phi))*((h+h_constant)**phi)
    u=(1d0-phi)*c**(1d0-gama)+phi*(h+h_constant)**(1d0-gama)
    u=u**(1d0/(1d0-gama))
    u=u**(1d0-sigma)-1d0   
    u=u/(1-sigma)
    return 
end

function bequestutility(b)
real(8) bequestutility,b
   
    bequestutility=(b+bconstant)**((1-sigma))-1d0  
    
    bequestutility=bequestutility*psi/(1-sigma)
    return 
end


subroutine survival_gen_initial
integer j
survival(agemin:40)=1d0
survival(41)=1d0
survival(50)=0.992d0
do j=42,50
  survival(j)=survival(j-1)+(survival(50)-survival(41))/(10-1)
end do
survival(51)=0.9915d0
survival(60)=0.988d0
do j=52,60
  survival(j)=survival(j-1)+(survival(60)-survival(51))/(10-1)
end do
survival(61)=0.9879d0
survival(70)=0.965d0
do j=62,70
  survival(j)=survival(j-1)+(survival(70)-survival(61))/(10-1)
end do
survival(71)=0.9649d0
survival(80)=0.925d0
do j=71,80
  survival(j)=survival(j-1)+(survival(80)-survival(71))/(10-1)
end do
survival(81)=0.924d0
survival(agemax)=0.82d0
do j=81,agemax
  survival(j)=survival(j-1)+(survival(agemax)-survival(81))/(10-1)
end do
survival(agemax+1)=0d0
death=1d0-survival

open(1, file='death.xls', status='replace')  
   do j=agemin,agemax+1
      write(1,*) death(j)
    end do
close(1)

end

subroutine survival_gen_new
integer j
survival(agemin:40)=1d0
survival(41)=1d0
survival(50)=0.9985d0
do j=42,50
  survival(j)=survival(j-1)+(survival(50)-survival(41))/(10-1)
end do
survival(51)=0.9975d0
survival(60)=0.995d0
do j=52,60
  survival(j)=survival(j-1)+(survival(60)-survival(51))/(10-1)
end do
survival(61)=0.9945d0
survival(70)=0.985d0
do j=62,70
  survival(j)=survival(j-1)+(survival(70)-survival(61))/(10-1)
end do
survival(71)=0.9845d0
survival(80)=0.948d0
do j=71,80
  survival(j)=survival(j-1)+(survival(80)-survival(71))/(10-1)
end do
survival(81)=0.9475d0
survival(agemax)=0.88d0
do j=81,agemax
  survival(j)=survival(j-1)+(survival(agemax)-survival(81))/(10-1)
end do
survival(agemax+1)=0d0
death=1d0-survival

open(1, file='death_new.xls', status='replace')  
   do j=agemin,agemax+1
      write(1,*) death(j)
    end do
close(1)

end

end module
