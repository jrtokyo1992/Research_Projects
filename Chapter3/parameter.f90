module global
use toolbox
integer i
integer,parameter:: nb=53,nh=8,nbp=40 ! original nb is 58
real(8) gridh(nh)
real(8),parameter::lambdab=0.6d0,tau_h=0.05d0,kappa_b=0.05d0,premium=0.03d0,lambdab_old = 0d0, kappa_s = 0.25d0
real(8),parameter:: delta_h=0.0375d0,g=0.05d0,h_constant=5d0,delta=0.1d0,alpha=0.35d0
real(8),parameter:: phi=0.33d0,sigma=2d0,land=20d0,eta=0.5d0 , beta_original = 0.96d0, psi = 20d0, b_constant = 40d0
integer,parameter ::agemin=21,agemax=90,retire=60
integer,parameter:: ne=5
real(8),parameter:: rho=0.84d0,sig=sqrt(0.055d0)  
real(8),parameter:: small=-1000000000d0, growth = 0.08d0
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


real(8) gride(ne),markov(ne,ne),gridb(nb),output,beta
real(8) life_purchase(agemin:agemax),life_sell(agemin:agemax)
real(8) bmax,bmin,p,d,replace,qb,w,m_y(agemin:agemax,ne)
real(8) popgrowth_old, survival_old(agemin:agemax+1)
real(8) re, pop_pre_retire, pop_after_retire,scaling, omega
real(8) ownership,networthratio,r,base, total_pension
real(8) be(ne,nh,nb,agemin:agemax), popgrowth

real(8) qm
integer pu_old(ne,nh,nb,agemin:agemax)
real(8) sell_a,hold_a, renthouse_a, purchase_a,bequest_a,trans,a_a,trans_new,construction,construction_new

real(8) maxhouse(agemin:agemax)
real(8) survival(agemin:agemax+1), death(agemin:agemax+1)
real(8) death_old(agemin:agemax+1), g_e_ratio
real(8) survival_new(agemin:agemax+1), death_new(agemin:agemax+1),popgrowth_new

!define all the policy matrixs

real(8) life_measure(agemin:agemax),life_income_avg(14),life_measure_old(agemin:agemax)
real(8) life_income(agemin:agemax),life_income2(14),life_income3(14)
real(8) v(ne,nh,nb,agemin:agemax),maxincome
real(8) c(ne,nh,nb,agemin:agemax),s(ne,nh,nb,agemin:agemax),age5(14),life_cons5(14)
real(8) life_house5(14),life_asset5(14),life_ownership5(14)
integer pu(ne,nh,nb,agemin:agemax)
!real(8) back(agemin:agemax)
real(8) l_p,g_e,house_trans_revenue,gama

integer zeropoint

real(8) n_h,networth,consexp,houseexp,totalincome,bound,housebound,totallabor,totalexp
real(8) bequestratio,cons_a, p_trend
! define some life cycle series
real(8) life_cons(agemin:agemax),life_house(agemin:agemax),maxsave(agemin:agemax)
real(8) life_asset(agemin:agemax),life_ownership(agemin:agemax),m(ne,nh,nb,agemin:agemax)

real(8) unconstrained, saver,constrained(agemin:agemax),pension,housetax,h_trend
real(8) totalmeasure1,klratio1
real(8) gridb_old(nb),gridb_new(nb),labor(agemin:retire,ne)
real(8) bequestpositive,totalmeasure
real(8) klratio_lasttime,klratio_new,klratio,piratio1,piratio2,piratio3
real(8) age(agemin:agemax),error
real(8) bpmax,bpmin,resource,temp,temp2,temp3,shouru(agemin:agemax,ne)
real(8) tau_ss
real(8) maxsaving(agemin:agemax)
integer max_index(agemin:agemax)
real(8) maxhousing(agemin:agemax)
integer max_index_h(agemin:agemax)

real(8) life_cost(agemin:agemax),life_resource(agemin:agemax)
real(8) life_cost1(agemin:agemax)
real(8) p_init,r_init,w_init,ownership_init,&
        hold_a_init, construction_init,klratio_init, l_p_init,trans_init, totalmeasure_init,&
        tau_ss_init, replace_init,omega_init, g_e_ratio_init
contains

! generate the grid for liquid asset.
subroutine gridb_gen
use toolbox
integer b,j,e
real(8) gridb_positive(28),gridb_negative(nb-28+1)
bmax = 220d0 ! i dont know whether this is correct or not. from the life cycle plot plot, I find that it is around 20.
bmin = -20d0 ! the numerical shows that for each period , no one will be less than -65. this is subject to the parameter, ofcourse.
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

subroutine gridh_gen
use toolbox
integer h
gridh(1)=0d0

!call Grid_Cons_Grow(gridh(2:nh),3.85d0,50d0,0.15d0)! the original one is equal distance.

call Grid_Cons_Grow(gridh(2:nh),4.5d0,55d0,0.05d0)! the original one is equal distance.

open(1, file='gridh.xls', status='replace') 
  do h=1,nh
 write(1,*) gridh(h)
end do
close(1) 
end 


function u(c,h)
real(8) c,h,rent,u
    u = (c**(1d0-phi))*((h+h_constant)**phi)
    u = u**(1d0-sigma)-1d0   
    u = u/(1-sigma)
    return 
end


subroutine survival_gen_initial
integer j
survival(21:45) = 0.997d0
survival(46:50) = 0.995d0
survival(51:55) = 0.985d0
survival(56:60) = 0.98d0
survival(61:65) = 0.975d0
survival(66:70) = 0.97d0
survival(71:75) = 0.948d0
survival(76:80) = 0.902d0
survival(81:85) = 0.87d0
survival(85:90) = 0.83d0
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
survival_new(21:45) = 1d0
survival_new(46:50) = 0.997d0
survival_new(51:55) = 0.997d0
survival_new(56:60) = 0.997d0
survival_new(61:65) = 0.994d0
survival_new(66:70) = 0.992d0
survival_new(71:75) = 0.983d0
survival_new(76:80) = 0.977d0
survival_new(81:85) = 0.93d0
survival_new(85:90) = 0.902d0
survival_new(agemax+1)= 0d0
death_new=1d0-survival_new

open(1, file='death_new.xls', status='replace')  
   do j = agemin,agemax+1
      write(1,*) death_new(j)
    end do
close(1)

end

function bq_motive(bequest)
real(8) bq_motive, bequest
bq_motive = (bequest + b_constant) ** ((1-sigma)*(1-alpha+alpha*phi))
bq_motive = psi* (bq_motive-1d0 )/(1d0-sigma)

end

subroutine opt_after(j)
real(8) cons,bequest
integer ir,il,b,h,e,hp,j,ep,bp,times
real(8) fai,lasttmp,resource,tmp2,tmp3,tmp,gridbp(nbp)

!!$omp parallel do private(h,e,bpmin,hp,bpmax,tmp,tmp2,tmp3,il,ir,fai,gridbp,lasttmp,bp,ep,resource,cons)

do b = 1,max_index(j)
    do h = 1,max_index_h(j)
    if (gridb(b)<-p*gridh(h)*lambdab_old*(1d0-delta_h)) cycle
        do e = 1,ne       
            ! start optimization
            do hp=1,nh
                lasttmp=small
               ! times=0
                resource=gridb(b)+trans&
                +pe(e) * w*replace+p*gridh(h)-p*gridh(hp)*h_trend&
                -cost(gridh(h),gridh(hp))
                bpmax = resource*(judge(resource>0d0)*(1d0/qb)+&
                judge(resource.le.0d0)*(1d0/qm))/(1d0+g)
                bpmax = bpmax-1d-10
                bpmin=-p*gridh(hp)*lambdab_old*(1d0-delta_h)
                if (bpmin.ge.bpmax) exit
                call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
                do bp=1,nbp
                    cons = resource-gridbp(bp)*(1d0+g)*(qb*judge(gridbp(bp)>0d0)&
                    +qm*judge(gridbp(bp) .le. 0d0))
                    bequest = gridbp(bp) + p * (1d0 -delta_h - kappa_s) * gridh(hp)
                    call linear(gridbp(bp), gridb, il, ir, fai)
                    tmp3 = fai*v(e,hp,il,j+1)+(1d0-fai)*v(e,hp,ir,j+1)  ! the vh is equal to vh in the latex  
                    tmp = u(cons,gridh(h))+beta*(1d0-death(j+1))*tmp3 +&
                     beta*death(j+1)*bq_motive(bequest)
                    if (tmp>v(e,h,b,j)) then
                        v(e,h,b,j)=tmp
                        s(e,h,b,j)=gridbp(bp)
                        pu(e,h,b,j)=hp
                        c(e,h,b,j)=cons
                        be(e,h,b,j) = bequest
                    end if
                    if (tmp<lasttmp) exit
                    lasttmp=tmp
                end do
            enddo
        enddo
    enddo
end do
  ! !$omp  end  parallel do   
end 

subroutine opt_before(j)
real(8) cons,bequest
integer ir,il,b,h,e,hp,j,ep,bp
real(8) fai,lasttmp,resource,tmp2,tmp3,tmp,gridbp(nbp)

!$omp parallel do private(h,e,bpmin,hp,bpmax,tmp,tmp2,tmp3,il,ir,fai,gridbp,lasttmp,bp,ep,resource,cons, bequest)
do b = 1,max_index(j)
    do h = 1,max_index_h(j)
        if (gridb(b)<-p*gridh(h)*lambdab*(1d0-delta_h)) cycle
        do e = 1,ne
             ! start optimization
            do hp=1,nh
                lasttmp = small
                resource = gridb(b)+trans+labor(j,e)*(1d0-tau_ss)*w+&
                p*gridh(h)-p*gridh(hp)*h_trend-cost(gridh(h),gridh(hp))
                bpmax = resource*(judge(resource>0d0)*(1d0/qb)+judge(resource.le.0d0)*(1d0/qm))/(1d0+g)
                bpmax = bpmax-1d-10
                bpmin = -p*gridh(hp)*lambdab*(1d0-delta_h)
                if (bpmin.ge.bpmax) exit
                call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
                do bp = 1,nbp
                    cons = resource-gridbp(bp)*(1d0+g)*(qb*judge(gridbp(bp)>0d0)+qm*judge(gridbp(bp) .le. 0d0))
                    tmp3 = 0d0
                    call linear(gridbp(bp), gridb, il, ir, fai)
                    do ep = 1,ne
                        tmp2 = fai*v(ep,hp,il,j+1)+(1d0-fai)*v(ep,hp,ir,j+1)  ! the vh is equal to vh in the latex
                        tmp3 = tmp3 + markov(e,ep) * tmp2
                    end do
                    bequest = gridbp(bp) + p * (1d0 -delta_h - kappa_s) * gridh(hp)
                    tmp = u(cons,gridh(h))+beta*(1d0-death(j+1))*tmp3 + beta* death(j+1)*bq_motive(bequest)
                    if (tmp>v(e,h,b,j)) then
                        v(e,h,b,j) = tmp
                        s(e,h,b,j) = gridbp(bp)
                        pu(e,h,b,j) = hp
                        c(e,h,b,j) = cons
                        be(e,h,b,j) = bequest
                    end if
                    if (tmp<lasttmp) exit
                    lasttmp = tmp
                end do
            enddo
        end do
    enddo
enddo
 !$omp  end  parallel do        
end 


subroutine opt_last
real(8) cons,income,gridbp(nbp), bequest, tmp
integer ir,il
real(8) fai
integer b,h,e,bp,ep,hp

!$omp parallel do private(h,e,bpmax,cons,tmp,gridbp,bp,hp, resource, bequest)

do b = 1,nb
    do h=1,nh
        if (gridb(b)<-p*gridh(h)*lambdab_old*(1d0-delta_h)) cycle
        do e=1,ne
          do hp = 1, nh
          resource=gridb(b)&
          +pe(e) * w*replace+p*gridh(h)+ trans-cost(gridh(h),gridh(hp))- p* gridh(hp)
          bpmax = resource*(judge(resource>0d0)*(1d0/qb)+&
          judge(resource.le.0d0)*(1d0/qm))/(1d0+g)
          bpmax = bpmax-1d-10
          bpmin= 0d0
          if (bpmin.ge.bpmax) exit
          call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
          do bp =1, nbp
             cons = resource-gridbp(bp)*(1d0+g)*(qb*judge(gridbp(bp)>0d0)&
             +qm*judge(gridbp(bp) .le. 0d0))
             bequest = gridbp(bp) + p* (1d0- delta_h - kappa_s) * gridh(hp)
             if (cons.le.0d0) exit
             tmp = u(cons, gridh(h)) + beta * bq_motive(bequest)
             if (tmp > v(e,h,b,agemax)) then
                v(e,h,b,agemax) = tmp
                s(e,h,b,agemax) = gridbp(bp)
                c(e,h,b,agemax) = cons
                pu(e,h,b,agemax) = hp
                be(e,h,b,agemax) = bequest
             end if
            end do
          end do
        enddo
    enddo
end do      
   !$omp  end  parallel do   
end


! compute the policy function for people at retirement age, age 60
subroutine opt_retire
real(8) cons,bequest
integer ir,il,b,h,e,hp,ep,bp
real(8) fai,lasttmp,resource,tmp2,tmp3,tmp,gridbp(nbp)

!$omp parallel do private(h,e,bpmin,hp,bpmax,tmp,tmp2,tmp3,il,ir,fai,gridbp,lasttmp,bp,ep,resource,cons, bequest)
do b=1,max_index(retire)
    do h=1,max_index_h(retire)
        if (gridb(b)<-p*gridh(h)*lambdab*(1d0-delta_h)) cycle
        do e=1,ne
            do hp=1,nh
                lasttmp = small
                resource = gridb(b)+trans&
                +labor(retire,e)*(1d0-tau_ss)*w+&
                p*gridh(h)-p*gridh(hp)*h_trend-cost(gridh(h),gridh(hp))
                
                bpmax=resource*(judge(resource>0d0)*(1d0/qb)+judge(resource.le.0d0)*(1d0/qm))/(1d0+g)
                bpmax=bpmax-1d-10
                bpmin=-p*gridh(hp)*lambdab_old*(1d0-delta_h)
                if (bpmin.ge.bpmax) exit
                call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
                do bp=1,nbp
                    cons=resource-gridbp(bp)*(1d0+g)*(qb*judge(gridbp(bp)>0d0)+qm*judge(gridbp(bp) .le. 0d0))
                    tmp3=0d0
                    call linear(gridbp(bp), gridb, il, ir, fai)
                    bequest = gridbp(bp) + p * (1d0 -delta_h - kappa_s) * gridh(hp)
                    tmp3=fai*v(e,hp,il,retire+1)+(1d0-fai)*v(e,hp,ir,retire+1) ! the vh is equal to vh in the latex  
                    tmp=u(cons,gridh(h))+beta*(1d0-death(retire+1))*tmp3 + beta*death(retire+1)* bq_motive(bequest) 
                    if (tmp>v(e,h,b,retire)) then
                        v(e,h,b,retire)=tmp
                        s(e,h,b,retire)=gridbp(bp)
                        pu(e,h,b,retire)=hp
                        c(e,h,b,retire)=cons
                        be(e,h,b,retire) = bequest
                    end if
                    if (tmp<lasttmp) exit
                    lasttmp=tmp
                end do
            enddo
        enddo
    enddo
enddo
  !$omp  end  parallel do         
end 

! Housing transaction cost
function cost(h1,h2)
real(8) h1,h2,cost

cost = p*delta_h*h1
if ( h2 * h_trend >h1 ) then
    cost = cost + kappa_b * (h2*h_trend - h1)*p
else
    cost = cost + kappa_s * (h1 - h2* h_trend)  * (p - p / p_trend)
end if
end 


subroutine simulation
integer ir,il,b,h,e,ep,j
real(8) fai

! initialize the distribution matrix and aggregate variable you want to calculate 
m=0d0
a_a=0d0
bequest_a=0d0
sell_a=0d0
purchase_a=0d0
hold_a=0d0
life_purchase=0d0
life_sell=0d0
house_trans_revenue =0d0

! pre allocate the matrixs before you use this subroutine!
m(1,1,zeropoint,agemin) = 0.22d0
m(2,1,zeropoint,agemin) = 0.22d0
m(3,1,zeropoint,agemin) = 0.14d0
m(4,1,zeropoint,agemin) = 0.17d0
m(5,1,zeropoint,agemin) = 0.25d0

do j=agemin,agemax
   do b=1,nb
      do h=1,nh
        if (gridb(b)<-lambdab*p*gridh(h)*(1d0-delta_h)) cycle
         do e=1,ne
           if (m(e,h,b,j)==0d0) cycle
           if (pu(e,h,b,j)<0) cycle
          
           ! the calculation of sell_a is confusing.
           ! may be better to use house demand to plot house life cycle?
           !sell_a=sell_a+m(e,h,b,j)*gridh(h)+m(e,h,b,j)*gridh(pu(e,h,b,j))*death(j+1)/(1d0+popgrowth)
           ! at the beginning of the next period, all the existing people becomes samller due to popgrowth. therefore, the aggregate variable for these people declines too.
           hold_a=hold_a+m(e,h,b,j)*gridh(h)/(1d0-death(j))
           if (j==agemax) hold_a = hold_a + m(e,h,b,j)*gridh(pu(e,h,b,j)) !? This is correct?
           purchase_a=purchase_a+m(e,h,b,j)*gridh(pu(e,h,b,j))
           house_trans_revenue = house_trans_revenue+ &
           m(e,h,b,j)* (cost (gridh(h), gridh(pu(e,h,b,j)))- gridh(h)*p*delta_h )&
           +m(e,h,b,j)*death(j+1) * ( cost (gridh(pu(e,h,b,j)),0d0) - delta_h * p * gridh(pu(e,h,b,j)) )/(1d0+popgrowth)
           a_a=a_a+m(e,h,b,j)*(s(e,h,b,j)* (qb*judge(s(e,h,b,j)>0d0)+qm*judge(s(e,h,b,j)<=0d0)) )/(1d0+popgrowth)
           bequest_a=bequest_a+death(j+1)*m(e,h,b,j)* be(e,h,b,j)/(1d0+popgrowth)
           life_purchase(j)=life_purchase(j)+m(e,h,b,j)*gridh(pu(e,h,b,j))
           if (j<agemax)life_sell(j+1)=life_sell(j+1)+m(e,h,b,j)*gridh(pu(e,h,b,j))*(1d0-delta_h)/(1d0+popgrowth)
           if (j<agemax) then
             ! housetax=housetax+p*m(e,h,b,j)*abs(gridh(h)-gridh(pu(e,h,b,j)))*(tax*judge(h>pu(e,h,b,j))+kf*judge(h<pu(e,h,b,j)) )
              call linear(s(e,h,b,j),gridb,il,ir,fai)
              if (fai>1d0) fai=1d0
              if (fai<0d0) fai=0d0  ! be careful
              if (j<retire) then
                  do ep=1,ne
                     m(ep,pu(e,h,b,j),il,j+1) =m(ep,pu(e,h,b,j),il,j+1)+m(e,h,b,j)*fai*markov(e,ep)
                     m(ep,pu(e,h,b,j),ir,j+1)=m(ep,pu(e,h,b,j),ir,j+1)+m(e,h,b,j)*(1d0-fai)*markov(e,ep)
                  end do
               else 
                  
                  m(e,pu(e,h,b,j),il,j+1) = m(e,pu(e,h,b,j),il,j+1)+m(e,h,b,j)*fai
                  m(e,pu(e,h,b,j),ir,j+1) = m(e,pu(e,h,b,j),ir,j+1)+m(e,h,b,j)*(1d0-fai)
               end if
           
           end if
          
            end do
            end do
            
            end do
           if(j<agemax)  then
           m(:,:,:,j+1)=m(:,:,:,j+1)*(1d0-death(j+1))/(1d0+popgrowth)
           
           end if
          ! print*, sum(m(:,:,:,j))
            end do
!!$omp end parallel do
! now we finished the distribution. each cohort there are population of measure 1.
! we want the aggregate house.
bequestpositive=bequestpositive/sum(m_y(agemax,:))
ownership=ownership/totalmeasure
saver=1d0-ownership 
!unconstrained=1d0-saver-constrained
bequest_a=bequest_a/totalmeasure

end 

! Backward Induction
subroutine policy_function
integer j
v=small
c=small
s=small
pu=small_int

call opt_last

do j=agemax-1,agemin,(-1)
!print*,'age', j

 
if (j>retire) then
call opt_after(j)

elseif (j==retire) then
call opt_retire

else
call opt_before(j)

end if


end do

end

subroutine maxsaving_gen
integer j,k,il,ir
real(8) temp,fai
maxsaving(agemin) = 0d0
max_index(agemin) = zeropoint
do j = agemin+1,retire
    temp = (maxsaving(j-1)+labor(j-1,5)*(1d0-tau_ss)*w + trans)*(1d0+r)/(1d0+g)
    call linear(temp,gridb,il,ir,fai)
    maxsaving(j) = gridb(ir)
    max_index(j) = ir
end do

do j=retire+1,agemax
   temp=(maxsaving(j-1)+pe(5)*replace*w +trans)*(1d0+r)/(1d0+g)
   call linear(temp,gridb,il,ir,fai)
   maxsaving(j)=gridb(ir)
   max_index(j)=ir
end do
end


subroutine maxhousing_gen
integer j,k
maxhousing(agemin)=0d0
max_index_h(agemin)=1
do j = agemin+1,retire
    maxhousing(j)=(maxsaving(j-1)+labor(j-1,5)*(1d0-tau_ss)*w + trans)/p
    if (maxhousing(j)<gridh(max_index_h(j-1))) then
        max_index_h(j)=max_index_h(j-1)
    else
        max_index_h(j)=nh ! initialization
        do k=max_index_h(j-1),nh
            if (gridh(k)>maxhousing(j) )  then
                max_index_h(j)=k
                exit
            end if
        end do
   end if
  
end do

do j=retire+1,agemax
   maxhousing(j)=(maxsaving(j-1)+pe(5)*w*replace + trans)/p
   if (maxhousing(j)<gridh(max_index_h(j-1))) then
      max_index_h(j)=max_index_h(j-1)
   else
      max_index_h(j)=nh ! initialization
      do k=max_index_h(j-1),nh
         if (gridh(k)>maxhousing(j) )  then
             max_index_h(j)=k
             exit
         end if
      end do
   end if
end do
end

subroutine labor_gen
! get the base for pension and the base for tax revenue. 
integer j,e,ep
real(8) pe_vct(ne)
m_y=0d0
m_y(agemin,1) = 0.22d0
m_y(agemin,2) = 0.22d0
m_y(agemin,3) = 0.14d0
m_y(agemin,4) = 0.17d0
m_y(agemin,5) = 0.25d0  ! this is from chips data.
!total_pension = 0d0
base=0d0
do j = agemin,agemax-1
    if (j<retire) then
      labor(j,:) = exp(gride)*exp(period(j))
      m_y(j+1,:) = matmul(m_y(j,:), markov)
    elseif (j==retire) then
      labor(j,:) = exp(gride)*exp(period(j))
      m_y(j+1,:) = m_y(j,:)
    else
      m_y(j+1,:) = m_y(j,:)
      !base = base+m_y(j,e)*exp(period(retire)+gride(e)) 
    end if
    m_y(j+1,:) = m_y(j+1,:)*(1d0-death(j+1))/(1d0+popgrowth)  
end do
totalmeasure = sum(m_y) 
totallabor = sum (m_y(agemin:retire, :)* labor(agemin:retire, :) )
pop_pre_retire = sum( m_y(agemin: retire, :))
pop_after_retire = totalmeasure - pop_pre_retire 
do e = 1, ne
    pe_vct(e) = pe(e)
end do
base = sum (matmul (m_y(retire+1: agemax, :),pe_vct))
end

! The function for computing individual pension benefit.
function pe(e)
real(8) pe
integer e
pe = (omega * totallabor/pop_pre_retire + &
(1d0- omega) *  labor(retire,e))
end

end module
