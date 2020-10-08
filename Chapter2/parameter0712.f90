module global

use toolbox
integer,parameter:: nb=52,nh=8,nbp=40,nrent=4,nbf=25,nm=20
integer,parameter:: people=15000,retire=60

real(8) gridh(nh),gridrent(nrent)
!real(8),parameter:: rategap=0.021d0,mult=24d0,eta=0.85d0,participate=0.3d0
real(8),parameter:: rategap=0.021d0,mult=24d0,eta= 0.85d0,participate=0.3d0,growth=0.08d0
! set the rategap to 0.021
real(8),parameter:: tau_ss=0.08d0,replace=0.6d0,wage=1d0
real(8),parameter:: lambdab=0.6d0,tau_h=0d0,kappa_h=0.035d0 ! kappa_h=0.03?
!real(8),parameter:: kf=0.017d0,delta_h=0.015d0,beta=0.93d0,omega_bar=1.01d0,land=0.02d0,alpha=0.55d0,theta=0.1d0
!!ok real(8),parameter:: kf=0.017d0,delta_h=0.015d0,beta=0.95d0,omega_bar=1.01d0,land=0.02d0,alpha=0.55d0,theta=0.1d0
real(8),parameter:: kf=0.017d0,delta_h=0.015d0,beta=0.9995d0,omega_bar=1.05d0,land=0.02d0,alpha=0.55d0,theta=0.15d0
! original value is beta=0.9995
!real(8),parameter:: phi=0.6d0 ,psi=50d0,gama=0.8d0,bconstant=10d0,sigma=2d0,fi=0.975d0
real(8),parameter:: phi=0.54d0 ,psi=30d0,gama=0.8d0,bconstant=10d0,sigma=2d0,fi=0.005d0
real(8),parameter:: premium=0.03d0
!!ok real(8),parameter:: phi=0.46d0 ,psi=100d0,gama=0.8d0,bconstant=10d0,sigma=2d0,fi=0.975d0
! the annual rent/ price in china is about 3%
integer, parameter ::agemin=21,agemax=75
integer,parameter:: ne=5
real(8),parameter::rho=0.83d0,sig=sqrt(0.055d0)
real(8),parameter:: small=-100000000d0
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

real(8) gride(ne),markov(ne,ne),gridb(nb),gridbf(nbf)
real(8) subsidy, hpf_pay
real(8) hw(people,agemin:agemax) ! the human wealth of people at any age
!define all the policy matrixs
real(8) trans_relay, construction_relay, rh_relay,hdist(nh),bfdist(nbf)
real(8) maxsaving(agemin:agemax),maxhpf(agemin:agemax)
real(8) re_hpf(ne,nb,nbf,agemin:agemax)
integer pu_hpf(ne,nb,nbf,agemin:agemax)
real(8) app_hpf(ne,nh,nb,nbf,agemin:agemax)
real(8) apo_hpf(ne,nb,nbf,agemin:agemax)
real(8) apr_hpf(ne,nb,nbf,agemin:agemax)
real(8) cp_hpf(ne,nh,nb,nbf,agemin:agemax)
real(8) cr_hpf(ne,nb,nbf,agemin:agemax)
real(8) co_hpf(ne,nb,nbf,agemin:agemax)
integer pay_hpf(ne,nh,nb,nbf,agemin:agemax)
integer buy_hpf(ne,nb,nbf,agemin:agemax)
integer keep_hpf(ne,nh,nb,nbf,agemin:agemax)
real(8) bfnext_r(ne,nb,nbf,agemin:agemax)
real(8) bfnext_o(ne,nb,nbf,agemin:agemax)
real(8) bfnext_p(ne,nh,nb,nbf,agemin:agemax)
real(8) hshare20,hshare40,hshare60,hshare80,hshare100
real(8) life_ownership(agemin:agemax),life_house(agemin:agemax)
real(8) life_ownership_hpf(agemin:agemax),life_house_hpf(agemin:agemax)
real(8) life_asset(agemin:agemax),life_asset_hpf(agemin:agemax)
real(8) life_bf(agemin:agemax),life_cons(agemin:agemax),life_cons_hpf(agemin:agemax)
real(8) m_y(agemin:60,ne)
integer inc(people,agemin:agemax),hou(people,agemin:agemax)
real(8) bequest_a,asset_a,hold_a,bf_a,renthouse_a,trans,totallabor,ownership
! define all the value matrixs
real(8) vr_hpf(ne,nb,nbf,agemin:agemax),vo_hpf(ne,nb,nbf,agemin:agemax),vn_hpf(ne,nb,nbf,agemin:agemax)
real(8) vp_hpf(ne,nh,nb,nbf,agemin:agemax),vs_hpf(ne,nh,nb,nbf,agemin:agemax),vh_hpf(ne,nh,nb,nbf,agemin:agemax)

real(8) p,d,rh,rb,rm,renterincome
real(8),allocatable::m_hpf(:,:,:,:,:),m(:,:,:,:)

real(8) cs_hpf(ne,nh,nb,nbf,agemin:agemax),aps_hpf(ne,nh,nb,nbf,agemin:agemax),hn_hpf(ne,nb,nbf,agemin:agemax)
real(8) hs_hpf(ne,nh,nb,nbf,agemin:agemax),cn_hpf(ne,nb,nbf,agemin:agemax)
real(8)  apn_hpf(ne,nb,nbf,agemin:agemax),hpfuse(ne,nh,nb,nbf,agemin:agemax)
real(8) hshare20_hpf,hshare20_hpf_m,hshare20_nohpf,hshare20_nohpf_m
real(8) hshare40_hpf,hshare40_hpf_m,hshare40_nohpf,hshare40_nohpf_m
real(8) hshare60_hpf,hshare60_hpf_m,hshare60_nohpf,hshare60_nohpf_m
real(8) hshare80_hpf,hshare80_hpf_m,hshare80_nohpf,hshare80_nohpf_m
real(8) hshare100_hpf,hshare100_hpf_m,hshare100_nohpf,hshare100_nohpf_m
real(8) bmin,bmax,bfmin,bfmax,age5(11),maxasset(agemin:agemax),maxasset_hpf(agemin:agemax)
real(8) life_ownership5(11)
real(8) maxrent(agemin:agemax),maxrent_hpf(agemin:agemax)

real(8) networth,totalexp,houseexp,wealth50,wealth75,wealth60
real(8) bequestratio,networthratio,n_h,totalincome,ownership35

real(8) shouru(agemin:agemax,ne),totalloan
integer zero

integer hou_hpf(people,agemin:agemax),inc_hpf(people, agemin:agemax)
integer ass(people,agemin:agemax),hpf(people, agemin:agemax)
real(8) ass_new(people,agemin:agemax)

integer max_index_hpf(agemin:agemax),max_index(agemin:agemax)

real(8) hou_sorted(people,agemin:agemax), ass_sorted(people, agemin:agemax)
real(8) hou_low(people),hou_medium(people),hou_high(people)
real(8) ass_low(people),ass_medium(people),ass_high(people)

real(8) rentpeople(agemin:agemax),rentpeople_hpf(agemin:agemax),hpfloan,hpfbalance

integer ind_h(people),ind_a(people)

real(8) bequestpositive,age(agemin:agemax),maxhouse(agemin:agemax),maxhouse_hpf(agemin:agemax)

real(8) life_asset5(11),life_cons5(11),life_house5(11),hpfconstrained(agemin:retire),hpfunconstrained(agemin:retire)

real(8) life_seller(agemin:agemax),life_buyer(agemin:agemax),life_renter(agemin:agemax),life_rent(agemin:agemax)
real(8) purchase_a,sell_a,life_sell(agemin:agemax)

real(8) life_seller_hpf(agemin:agemax),life_buyer_hpf(agemin:agemax),life_renter_hpf(agemin:agemax),life_rent_hpf(agemin:agemax)


real(8) re(ne,nb,agemin:agemax)
integer pu(ne,nb,agemin:agemax)
real(8) app(ne,nh,nb,agemin:agemax)
real(8) apo(ne,nb,agemin:agemax)
real(8) apr(ne,nb,agemin:agemax)
real(8) cp(ne,nh,nb,agemin:agemax)
real(8) cr(ne,nb,agemin:agemax)
real(8) co(ne,nb,agemin:agemax)
integer buy(ne,nb,agemin:agemax)
integer keep(ne,nh,nb,agemin:agemax)
real(8) vr(ne,nb,agemin:agemax)
real(8) vo(ne,nb,agemin:agemax)
real(8) vn(ne,nb,agemin:agemax)
real(8) vp(ne,nh,nb,agemin:agemax)
real(8) vs(ne,nh,nb,agemin:agemax)
real(8) vh(ne,nh,nb,agemin:agemax)

real(8) app_hpf_original(ne,nh,nb,nbf,agemin:agemax)
real(8) bfnext_p_original(ne,nh,nb,nbf,agemin:agemax)
real(8) apr_hpf_original(ne,nb,nbf,agemin:agemax),apo_hpf_original(ne,nb,nbf,agemin:agemax)
integer pu_hpf_original(ne,nb,nbf,agemin:agemax),buy_hpf_original(ne,nb,nbf,agemin:agemax)
integer keep_hpf_original(ne,nh,nb,nbf,agemin:agemax)
real(8) app_original(ne,nh,nb,agemin:agemax),apr_original(ne,nb,agemin:agemax)
real(8) apo_original(ne,nb,agemin:agemax)
integer keep_original(ne,nh,nb,agemin:agemax)
real(8) bfnext_r_original(ne,nb,nbf,agemin:agemax)
real(8) bfnext_o_original(ne,nb,nbf,agemin:agemax)
integer pu_original(ne,nb,agemin:agemax)
integer buy_original(ne,nb,agemin:agemax)

real(8) m_original(ne,nh,nb,agemin:agemax),m_original_hpf(ne,nh,nb,nbf,agemin:agemax)


real(8) ownershipbot20_benchmark(agemin:agemax),ownershipmid20_benchmark(agemin:agemax)
real(8) ownershiptop20_benchmark(agemin:agemax)
real(8) ownershipbot20_nohpf(agemin:agemax),ownershipmid20_nohpf(agemin:agemax)
real(8) ownershiptop20_nohpf(agemin:agemax)
real(8) ownershipbot20_hpf(agemin:agemax),ownershipmid20_hpf(agemin:agemax)
real(8) ownershiptop20_hpf(agemin:agemax)
real(8) housebot20_benchmark(agemin:agemax),housemid20_benchmark(agemin:agemax)
real(8) housetop20_benchmark(agemin:agemax)
real(8) housebot20_nohpf(agemin:agemax),housemid20_nohpf(agemin:agemax)
real(8) housetop20_nohpf(agemin:agemax)
real(8) housebot20_hpf(agemin:agemax),housemid20_hpf(agemin:agemax)
real(8) housetop20_hpf(agemin:agemax)
real(8) wea(people,agemin:agemax)
real(8) wealthbot20_benchmark,wealthmid20_benchmark,wealthtop20_benchmark
real(8) wealthbot20_hpf,wealthmid20_hpf,wealthtop20_hpf
real(8) wealthbot20_nohpf,wealthmid20_nohpf,wealthtop20_nohpf
real(8) housebotmid20_benchmark(agemin:agemax),housemidtop20_benchmark(agemin:agemax)
real(8) housebotmid20_nohpf(agemin:agemax),housemidtop20_nohpf(agemin:agemax)
real(8) housebotmid20_hpf(agemin:agemax),housemidtop20_hpf(agemin:agemax)




contains
subroutine tauchen(markov,s)
use toolbox
real(8) markov(ne,ne),s(ne),step
integer i,j,k,e,ep
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


open(1,file='markov.xls',status='replace')
do e=1,ne
 do ep=1,ne
   write(1,*) markov(e,ep)
 end do
 end do
close(1)

end

subroutine housegrid_gen
use toolbox
integer h,rent

!real(8),parameter:: gridh(nh)=(/0d0,1.55d0,1.92d0,2.48d0,3.15d0,4.03d0,5.15d0/)
!gridh =(/0d0,2.98d0,3.98d0,5.58d0,6.15d0,8.03d0,10.15d0/)
!gridh =(/0d0,2.58d0,3.1d0,4.4d0,6.00d0,8.1d0,10.00d0,12.75d0/) !2.28 this is the original one
!gridh =(/0d0,2.5d0,3.0d0,4.0d0,5.50d0,7.5d0,10.00d0,13.00d0,16.5d0/) !2.28
gridh(1)=0d0
call grid_Cons_Grow(gridh(2:nh),2.6d0,16.5d0,0.1d0)
print*, gridh
!gridh =(/0d0,2.5d0,3.1d0,4.1d0,5.60d0,7.6d0,10.10d0,13.1d0,16.6d0/)
gridrent=(/0.7d0,1.45d0,2.05d0,3.05d0/)


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

function judge(x)
logical x
integer judge
if (x .eqv..true.) then
judge=1
else
judge=0
end if
end

recursive subroutine QsortC(A,ind)
    real(8), intent(in out), dimension(:) :: A
    integer, intent(in out), dimension(:) :: ind
    integer :: iq
    if(size(A) > 1) then
      call Partition(A, iq,ind)
      call QsortC(A(:iq-1),ind(:iq-1))
      call QsortC(A(iq:),ind(iq:))
    endif
end subroutine QsortC

subroutine Partition(A, marker,ind)
    real(8), intent(in out), dimension(:) :: A
    integer, intent(in out), dimension(:) :: ind
    integer, intent(out) :: marker
    integer :: i, j
    real(8) :: temp
    real(8) :: x      ! pivot
    integer temp_int
    x = A(1)
    i= 0
    j= size(A) + 1
    do
      j = j-1
      do
        if (A(j) <= x) exit
        j = j-1
      end do
      i = i+1
      do
        if (A(i) >= x) exit
        i = i+1
      end do
      if (i < j) then
        ! exchange A(i) and A(j)
        temp = A(i)
        A(i) = A(j)
        A(j) = temp
        temp_int=ind(i)
        ind(i)=ind(j)
        ind(j)=temp_int

      elseif (i == j) then
        marker = i+1
        return
      else
        marker = i
        return
      endif
    end do
end subroutine Partition

subroutine bilinear(y,ay,ny,x,ax,nx,xl,xr,yu,yd,failu,fairu,faild,faird)
integer,intent(in):: ny,nx
real(8),intent(in):: ay(ny),ax(nx)
real(8),intent(in):: y,x
real(8),intent(out)::failu,fairu,faild,faird
integer,intent(out):: xl,xr,yu,yd
integer i,sign1,sign2

xl=1
xr=1
yu=1
yd=1
failu=0d0
fairu=0d0
faild=0d0
faird=0d0
if (ax(1).ge. x) then
  xl=1
  xr=2
  if (ay(1).ge.y) then
  fairu=0d0
  faird=0d0
  failu=0d0
  faild=1d0

  yd=1
  yu=2
  elseif (ay(ny).le.y) then
  fairu=0d0
  faird=0d0
  failu=1d0
  faild=0d0

  yd=ny-1
  yu=ny
  else
    do i=2,ny
      if (ay(i)>y) then
         yu=i
         yd=i-1
         failu=    (ay(yd)-y )   /(   (ay(yd)-ay(yu))    )
         faild=     (y-ay(yu) )/(   (ay(yd)-ay(yu))    )
         fairu=  0d0
         faird=   0d0
         exit
      end if
    end do
   end if


elseif (ax(nx).le. x) then
  xl=nx-1
  xr=nx
  if (ay(1).ge.y) then
  yu=2
  yd=1
  fairu=0d0
  faird=1d0
  failu=0d0
  faild=0d0
  else if (ay(ny).le.y) then
  yu=ny
  yd=ny-1
  fairu=1d0
  faird=0d0
  failu=0d0
  faild=0d0
  else
  do i=2,ny
  if (ay(i)>y) then
  yu=i
  yd=i-1
  failu=   0d0
  faild=    0d0
  fairu=   (ay(yd)-y )   /(   (ay(yd)-ay(yu))    )
  faird=    (y-ay(yu) )    /(   (ay(yd)-ay(yu))    )
  exit
  end if
  end do
  end if

else
  do i=2,nx
    if (ax(i)>x) then
      xl=i-1
      xr=i
      exit
    end if
  end do
  if (ay(1).ge.y) then
  yu=2
  yd=1
  failu=    0d0
  faild=     (ax(xr)-x) /(    (ax(xr)-ax(xl))   )
  fairu=   0d0
  faird=    (x-ax(xl)) /(  (ax(xr)-ax(xl))  )
  elseif (ay(ny).le.y) then
  yu=ny
  yd=ny-1
  failu=    (ax(xr)-x) /(    (ax(xr)-ax(xl))   )
  faild=     0d0
  fairu=   (x-ax(xl)) /(  (ax(xr)-ax(xl))  )
  faird=    0d0
  else
  do i=2,ny
    if (ay(i)>y) then
    yu=i
    yd=i-1
   exit
   end if
  end do
  failu=    ((ax(xr)-x)*(ay(yd)-y) )   /(   (ax(xr)-ax(xl))*(ay(yd)-ay(yu))    )
  faild=     ( (ax(xr)-x)*(y-ay(yu)) )    /(    (ax(xr)-ax(xl))*(ay(yd)-ay(yu))    )
  fairu=    ((x-ax(xl))*(ay(yd)-y) )  /(   (ax(xr)-ax(xl))*(ay(yd)-ay(yu))   )
  faird=   ( (x-ax(xl))*(y-ay(yu)))    /(    (ax(xr)-ax(xl))*(ay(yd)-ay(yu))    )
end if
end if
end

subroutine judge2(x,fai_lu,fai_ru,fai_ld,fai_rd,il,ir,iu,id,cordlr,cordud)
real(8),intent(in):: x,fai_lu,fai_ru,fai_ld,fai_rd
integer,intent(in):: il,ir,iu,id
integer,intent(out):: cordlr,cordud

 if((x .ge.fai_lu) .and. (x<fai_lu+fai_ru)) then
cordlr=ir
cordud=iu
else if ((x.ge.(fai_lu+fai_ru)) .and. (x<(fai_lu+fai_ru+fai_ld))) then
cordlr=il
cordud=id
else if (  (x.ge.(fai_lu+fai_ru+fai_ld))  .and. (x.le.(fai_lu+fai_ru+fai_ld+fai_rd))  )then
cordlr=ir
cordud=id
else
cordlr=il
cordud=iu
end if
end




subroutine gridb_gen
integer b
bmax=300d0
bmin=-7.3d0*lambdab*gridh(nh)
print*,'bmin is',bmin
call grid_Cons_Grow(gridb,bmin,bmax,growth) ! growing distance
!print*,gridb
do b=1,nb
if (gridb(b)>0d0) then !? check gridb(b)<0d0
gridb(b)=0d0
zero=b
print*, 'zero is',zero
exit
end if
end do

open(1, file='gridb.xls', status='replace')
      do b=1,nb
      write(1,*) gridb(b)
           end do
         
close(1)
end

subroutine shouru_gen
! notice: this subroutine is used to generate the labor income before tax implementationa or pension calculation.
integer j,e
do j=agemin,agemax
 do  e=1,ne
   if (j.le.retire) then
   shouru(j,e)=exp(gride(e)+period(j))*wage
   else
   shouru(j,e)=exp(gride(e)+period(retire))*wage
   end if
 end do
end do


open(1, file='shouru.xls', status='replace')
 do j=agemin,agemax
      do e=1,ne
      write(1,*) shouru(j,e)
           end do
           end do
close(1)

end

subroutine gridbf_gen
integer bf
bfmin=0d0
bfmax=theta*sum(shouru(agemin:retire,5))*0.8d0
!print*, 'bfmax is',bfmax
call grid_Cons_Grow(gridbf,bfmin,bfmax,0.1d0)
open(1, file='gridbf.xls', status='replace')
 
      do bf=1,nbf
      write(1,*) gridbf(bf)
           end do
close(1)
end

function cumsum(array)
integer n,i
real(8),dimension(:)::array
real(8),allocatable:: cumsum(:)
real(8) s
n=size(array)
allocate(cumsum(n))
s=0d0
do i=1,n
s=s+array(i)
cumsum(i)=s
end do

end
end module
