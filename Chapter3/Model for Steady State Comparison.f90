
! In this code I compute two new equilibria. 
! new equilibrium 1: pension income declines while tax rate fixed
! new equilibrium 2: pension income fixed while tax rate raises

include 'parameter.f90'

program main
use toolbox
use global
integer b,e,ep





call CPU_TIME(timestart)

! generate the grid and transition matrix for income.
call tauchen(markov,gride)
open(1, file='markov.xls', status='replace')  
   do e=1,ne
     do ep=1,ne
      write(1,*) markov(e,ep)
    end do
    end do
close(1)

! generate age vector. this is for drawing graph.
call age_gen

! generate housing grid
call gridh_gen

call gridb_gen

call sscompare

open(1, file='gridb.xls', status='replace') 
do b=1,nb
 write(1,*) gridb(b)
end do
close(1) 

contains


subroutine sscompare
integer j,b,h,e
real(8) popgrowth_old, survival_old(agemin:agemax+1)
age5=((/(i,i=1,14,1)/)-1d0)*5d0+21d0
replace=0.7d0
popgrowth=0.025d0
call survival_gen_initial
call labor_gen
tau_ss=base*replace/totallabor
print*, 'tau_ss is',tau_ss

open(1, file='labor.xls', status='replace')  
   do j=agemin,retire
     do e=1,ne
      write(1,*) labor(j,e)
    end do
    end do
close(1) 

call steadystate
call summarize
v1=v


p1=p
r1=r
ownership1=ownership
w1=w
trans1=initialwealth
totalmeasure1=totalmeasure
construction1=construction
kl1=klratio
!piratio1=p1*sum(m)/totalincome
!print*,' price, measure and total income', p1,sum(m),totalincome
piratio1=p1*purchase_a*totalmeasure1/totalincome
print*,' price, purchase and total income', p1,purchase_a*totalmeasure1,totalincome
print*, 'the price income ratio is',piratio1

do j=1,14
life_house1(j)=dot_product(life_house(agemin+(j-1)*5 :agemin-1+j*5),&
life_measure(agemin+(j-1)*5 :agemin-1+j*5))/sum(life_measure(agemin+(j-1)*5   :agemin-1+j*5))

life_cons1(j)=dot_product(life_cons(agemin+(j-1)*5   :agemin-1+j*5),&
life_measure(agemin+(j-1)*5   :agemin-1+j*5))/sum(life_measure(agemin+(j-1)*5   :agemin-1+j*5))

life_asset1(j)=dot_product(life_asset(agemin+(j-1)*5   :agemin-1+j*5),&
life_measure(agemin+(j-1)*5   :agemin-1+j*5))/sum(life_measure(agemin+(j-1)*5   :agemin-1+j*5))

life_income1(j)=dot_product(life_income(agemin+(j-1)*5   :agemin-1+j*5),&
life_measure(agemin+(j-1)*5   :agemin-1+j*5))/sum(life_measure(agemin+(j-1)*5   :agemin-1+j*5))


end do

life_measure_old=life_measure

open(1, file='lifecycle1.xls', status='replace') 
do j=1,14
 write(1,*) life_cons1(j),life_asset1(j),life_house1(j),life_income1(j)
end do
close(1) 

open(1, file='measure1.xls', status='replace') 
do j=agemin,agemax
  do b=1,nb
      do h=1,nh
         do e=1,ne
 write(1,*) m(e,h,b,j)
end do
end do
end do
end do
close(1) 

open(1, file='policy1.xls', status='replace') 
do j=agemin,agemax
  do b=1,nb
      do h=1,nh
         do e=1,ne
 write(1,*) c(e,h,b,j),s(e,h,b,j),pu(e,h,b,j)
end do
end do
end do
end do
close(1) 


open(1, file='v1.xls', status='replace') 
do j=agemin,agemax
  do b=1,nb
      do h=1,nh
         do e=1,ne
       write(1,*) v1(e,h,b,j)
end do
end do
end do
end do
close(1) 


open(1, file='gridh.xls', status='replace') 
  do h=1,nh
 write(1,*) gridh(h)
end do
close(1) 





!-------------------------------------------------------
print*,'we next calculate the new equilibrium: change replacement ratio'
! now calculate the new one: the tax rate is fixed, but replace ratio changes.

survival_old=survival
popgrowth_old=popgrowth
popgrowth=0.01d0 ! new population growth

call survival_gen_new ! generate a new one, get the new survival and death
! we next calculate a back(j). this is to control for the demographic composition effect when calculating wealth distribution.
back(agemin)=1d0
do j=agemin+1,agemax
 back(j)=(1d0+popgrowth_old)/(survival_old(j))
 back(j)=back(j)*survival(j)/(1d0+popgrowth)
end do

open(1, file='back.xls',status='replace')
do j=agemin,agemax
write(1,*) back(j)
end do
close(1)


call labor_gen ! 
replace=tau_ss*totallabor/base ! here the tau_ss is the ratio in the initial equilibrium.
print*,'the new replacement ratio is', replace
call steadystate
call summarize


v2=v  ! we need this to calculate cev.
p2=p
r2=r
ownership2=ownership
w2=w
totalmeasure2=totalmeasure
pu2=purchase_a*totalmeasure2
construction2=construction
kl2=klratio
trans2=initialwealth
!piratio2=p2*sum(m)/totalincome
!print*,' price, measure and total income', p2,sum(m),totalincome
piratio2=p2*purchase_a*totalmeasure2/totalincome
print*,' price, purchase and total income', p2,purchase_a*totalmeasure2,totalincome
print*, 'the price income ratio is',piratio2

life_measure=life_measure_old

do j=1,14
life_house2(j)=dot_product(life_house(agemin+(j-1)*5   :agemin-1+j*5),&
life_measure(agemin+(j-1)*5   :agemin-1+j*5))/sum(life_measure(agemin+(j-1)*5   :agemin-1+j*5))

life_cons2(j)=dot_product(life_cons(agemin+(j-1)*5   :agemin-1+j*5),&
life_measure(agemin+(j-1)*5   :agemin-1+j*5))/sum(life_measure(agemin+(j-1)*5   :agemin-1+j*5))

life_asset2(j)=dot_product(life_asset(agemin+(j-1)*5   :agemin-1+j*5),&
life_measure(agemin+(j-1)*5   :agemin-1+j*5))/sum(life_measure(agemin+(j-1)*5   :agemin-1+j*5))

life_income2(j)=dot_product(life_income(agemin+(j-1)*5   :agemin-1+j*5),&
life_measure(agemin+(j-1)*5   :agemin-1+j*5))/sum(life_measure(agemin+(j-1)*5   :agemin-1+j*5))
end do

open(1, file='lifecycle2.xls', status='replace') 
do j=1,14
 write(1,*) life_cons2(j),life_asset2(j),life_house2(j),life_income2(j)
end do
close(1) 

open(1, file='measure2.xls', status='replace') 
do j=agemin,agemax
  do b=1,nb
      do h=1,nh
         do e=1,ne
 write(1,*) m(e,h,b,j)
end do
end do
end do
end do
close(1)

open(1, file='v2.xls', status='replace') 
do j=agemin,agemax
  do b=1,nb
      do h=1,nh
         do e=1,ne
 write(1,*) v2(e,h,b,j)
end do
end do
end do
end do
close(1)

h2_25=hdist(21,40)
h2_45=hdist(41,60)
h2_65=hdist(61,80)

open(1, file='hdist2.xls',status='replace')
do h=1,nh
write(1,*) h2_25(h),h2_45(h),h2_65(h)
end do
close(1)


open(1, file='policy2.xls', status='replace')  
do j=agemin,agemax
  do b=1,nb
    do h=1,nh
      do e=1,ne
      write(1,*) c(e,h,b,j),s(e,h,b,j),pu(e,h,b,j)
    end do
    end do
    end do
    end do
close(1)

!---------------------------------------------------------------
print*,'we next calculate the new equilibrium: change tax rate'
! we next calculate the new one: the replacement is fixed but the tax rate change.
replace=0.7d0
tau_ss=base*replace/totallabor ! here the tau_ss is the ratio in the initial equilibrium.
! we don't need to call labor_gen. 
print*,'the new tax rate is', tau_ss
call steadystate
call summarize


v3=v
p3=p
r3=r
ownership3=ownership
w3=w
construction3=construction
totalmeasure3=totalmeasure
pu3=purchase_a*totalmeasure3
kl3=klratio
trans3=initialwealth
!piratio3=p3*sum(m)/totalincome
!print*,' price, measure and total income', p3,sum(m),totalincome
piratio3=p3*purchase_a*totalmeasure3/totalincome
print*,' price, purchase and total income', p3,purchase_a*totalmeasure3,totalincome
print*, 'the price income ratio is',piratio3

life_measure=life_measure_old

do j=1,14
life_house3(j)=dot_product(life_house(agemin+(j-1)*5   :agemin-1+j*5),&
life_measure(agemin+(j-1)*5   :agemin-1+j*5))/sum(life_measure(agemin+(j-1)*5   :agemin-1+j*5))

life_cons3(j)=dot_product(life_cons(agemin+(j-1)*5   :agemin-1+j*5),&
life_measure(agemin+(j-1)*5   :agemin-1+j*5))/sum(life_measure(agemin+(j-1)*5   :agemin-1+j*5))

life_asset3(j)=dot_product(life_asset(agemin+(j-1)*5   :agemin-1+j*5),&
life_measure(agemin+(j-1)*5   :agemin-1+j*5))/sum(life_measure(agemin+(j-1)*5   :agemin-1+j*5))

life_income3(j)=dot_product(life_income(agemin+(j-1)*5   :agemin-1+j*5),&
life_measure(agemin+(j-1)*5   :agemin-1+j*5))/sum(life_measure(agemin+(j-1)*5   :agemin-1+j*5))
end do

open(1, file='lifecycle3.xls', status='replace') 
do j=1,14
 write(1,*) life_cons3(j),life_asset3(j),life_house3(j),life_income3(j)
end do
close(1) 

open(1, file='measure3.xls', status='replace') 
do j=agemin,agemax
  do b=1,nb
      do h=1,nh
         do e=1,ne
 write(1,*) m(e,h,b,j)
end do
end do
end do
end do
close(1)

open(1, file='v3.xls', status='replace') 
do j=agemin,agemax
  do b=1,nb
      do h=1,nh
         do e=1,ne
 write(1,*) v3(e,h,b,j)
end do
end do
end do
end do
close(1)

open(1, file='policy3.xls', status='replace')  
do j=agemin,agemax
  do b=1,nb
    do h=1,nh
      do e=1,ne
      write(1,*) c(e,h,b,j),s(e,h,b,j),pu(e,h,b,j)
    end do
    end do
    end do
    end do
close(1)


open(1, file='price.xls', status='replace') 
write(1,*) p1,r1,w1,ownership1,kl1,construction1,trans1,totalmeasure1,piratio1
write(1,*) p2,r2,w2,ownership2,kl2,construction2,trans2,totalmeasure2,pu2,piratio2
write(1,*) p3,r3,w3,ownership3,kl3,construction3,trans3,totalmeasure3,pu3,piratio3
close(1)


end 





subroutine labor_gen
! generate something related to labor
integer j,e,ep

m_y=0d0
m_y(agemin,1)=0.22d0
m_y(agemin,2)=0.22d0
m_y(agemin,3)=0.14d0
m_y(agemin,4)=0.17d0
m_y(agemin,5)=0.25d0
totallabor=0d0
base=0d0
do j=agemin,agemax
   !print*,j,sum(m_y(j,:))
    do e=1,ne
       
       if (j<retire) then
          labor(j,e)=exp(gride(e)+period(j))
          totallabor=totallabor+m_y(j,e)*labor(j,e)
         
          do ep=1,ne
             m_y(j+1,ep)=m_y(j+1,ep)+m_y(j,e)*markov(e,ep)
          end do
       elseif (j==retire) then
       labor(j,e)=exp(gride(e)+period(j))
       totallabor=totallabor+m_y(j,e)*labor(j,e)
       m_y(j+1,e)=m_y(j,e)
       else
        if (j<agemax)  m_y(j+1,e)=m_y(j,e)
      base=base+m_y(j,e)*exp(period(retire)+gride(e)) 
      
       end if
       !print*,labor(j,e)
    end do
    if (j<agemax)  m_y(j+1,:)=m_y(j+1,:)*(1d0-death(j+1))/(1d0+popgrowth)  
end do

totalmeasure=sum(m_y) 
print*,'total measure is', totalmeasure
print*, 'total labor and base',totallabor, base
end


subroutine steadystate
 ! find the initial wealth and transfer 
 ! one lesson: since you have to update three values at the same time, it will be better to slow down the update speed.
 ! otherwise it may be hard to find an equilibrium.
real(8) adjust,klmin,klmax
integer iter
adjust=0.75d0

construction_lasttime=1.2d0
wealth_lasttime=1d0
klratio_lasttime=9d0

initialwealth=wealth_lasttime
construction=construction_lasttime
klratio=klratio_lasttime
iter=1
klmin=0d0
klmax=100d0
do
r=eta*klratio**(eta-1d0)-delta
qb=1d0/(1d0+r)
qm=1d0/(1d0+r+premium)
w=(1d0-eta)*klratio**eta
p=(construction/land)**((1d0-alpha)/alpha)/alpha  ! use final goods and land to construct house.
!tau_ss=(base*replace-(p*construction-n_h)*totalmeasure)/totallabor

print*,'xxxxxxxxxxxxxxxx'
print*, 'inner loop',iter
print*,' construction ,initialwealth,klratio'
print*,construction,initialwealth,klratio
print*,'housing price, wage and interest rate is'
print*,p,w,r



!print*,'bmax and bmin',bmax,bmin
call maxsaving_gen
call maxhousing_gen


call policy_function

call simulation

initialwealth_new=bequest_a
construction_new=purchase_a-(1d0-delta_h)*sell_a
klratio_new=max(a_a/(totallabor),0d0)
!print*,'demand and sell',purchase_a,sell_a
print*,'new construction ,initial wealth and klratio is'
print*,construction_new,initialwealth_new,klratio_new

error=abs(construction-construction_new)/construction+abs(initialwealth_new-initialwealth)/initialwealth&
+abs(klratio-klratio_new)/klratio


print*, 'error is',error
if (error<0.05d0 .or. iter>20) then
construction_lasttime=construction_new
wealth_lasttime=initialwealth_new
klratio_lasttime=klratio_new
exit
end if

construction=adjust*construction+(1d0-adjust)*construction_new
klratio=adjust*klratio+(1d0-adjust)*klratio_new
initialwealth=adjust*initialwealth+(1d0-adjust)*initialwealth_new
iter=iter+1

end do

end 


subroutine simulation
integer ir,il,b,h,e,ep,j
real(8) fai


m=0d0
a_a=0d0
bequest_a=0d0
sell_a=0d0
purchase_a=0d0
! please pre allocate the matrixs before you use this subroutine!
m(1,1,zeropoint,agemin)=0.22d0
m(2,1,zeropoint,agemin)=0.22d0
m(3,1,zeropoint,agemin)=0.14d0
m(4,1,zeropoint,agemin)=0.17d0
m(5,1,zeropoint,agemin)=0.25d0
!print*,'simulation'
do j=agemin,agemax
  ! print*, 'age ',j, sum(m(:,:,:,j))
   do b=1,nb
      do h=1,nh
        if (gridb(b)<-lambdab*p*gridh(h)*(1d0-delta_h)) cycle
         do e=1,ne
        
           if (m(e,h,b,j)==0d0) cycle
           if (pu(e,h,b,j)<0) cycle
          ! print*,e,h,b,j
           sell_a=sell_a+m(e,h,b,j)*gridh(h)+m(e,h,b,j)*gridh(pu(e,h,b,j))*death(j+1)
        
           hold_a=hold_a+m(e,h,b,j)*gridh(h)
           purchase_a=purchase_a+m(e,h,b,j)*gridh(pu(e,h,b,j))
          a_a=a_a+m(e,h,b,j)*(s(e,h,b,j)* (qb*judge(s(e,h,b,j)>0d0)+qm*judge(s(e,h,b,j)<=0d0))  )/(1d0+popgrowth)
            bequest_a=bequest_a+death(j+1)*m(e,h,b,j)*(s(e,h,b,j)+p*(1d0-kappa_h-delta_h)*gridh(pu(e,h,b,j)))/(1d0+popgrowth)
           if (j<agemax) then
             ! housetax=housetax+p*m(e,h,b,j)*abs(gridh(h)-gridh(pu(e,h,b,j)))*(tax*judge(h>pu(e,h,b,j))+kf*judge(h<pu(e,h,b,j)) )
              call linear(s(e,h,b,j),gridb,il,ir,fai)
              if (fai>1d0) fai=1d0
              if (fai<0d0) fai=0d0  ! be careful
                 if (j<retire) then
                  do ep=1,ne
                     m(ep,pu(e,h,b,j),il,j+1)=m(ep,pu(e,h,b,j),il,j+1)+m(e,h,b,j)*fai*markov(e,ep)
                     m(ep,pu(e,h,b,j),ir,j+1)=m(ep,pu(e,h,b,j),ir,j+1)+m(e,h,b,j)*(1d0-fai)*markov(e,ep)
                  end do
                  else 
                  m(e,pu(e,h,b,j),il,j+1)=m(e,pu(e,h,b,j),il,j+1)+m(e,h,b,j)*fai
                  m(e,pu(e,h,b,j),ir,j+1)=m(e,pu(e,h,b,j),ir,j+1)+m(e,h,b,j)*(1d0-fai)
                  end if
           
           end if
          
            end do
            end do
            
            end do
           if(j<agemax)  then
           m(:,:,:,j+1)=m(:,:,:,j+1)*(1d0-death(j+1))/(1d0+popgrowth)
           
           end if
            end do
!!$omp end parallel do
! now we finished the distribution. each cohort there are population of measure 1.
! we want the aggregate house.
bequestpositive=bequestpositive/sum(m_y(agemax,:))
hold_a=hold_a/totalmeasure
sell_a=sell_a/totalmeasure
!a_a=a_a/totalmeasure

ownership=ownership/totalmeasure
saver=1d0-ownership 
!unconstrained=1d0-saver-constrained
bequest_a=bequest_a/totalmeasure
purchase_a=purchase_a/totalmeasure
end 

subroutine policy_function

real(8) time1,time2
integer j

v=small
c=small
s=small
pu=small_int

call opt_last

do j=agemax-1,agemin,(-1)
!print*,'age', j
call cpu_time(time1)

 
if (j>retire) then
call opt_after(j)

elseif (j==retire) then
call opt_retire

else
call opt_before(j)

end if

call cpu_time(time2)

!print*,'age ',j,'time cost is',(time2-time1)
end do

end


subroutine opt_retire
real(8) cons
integer ir,il,b,h,e,hp,ep,bp
real(8) fai,lasttmp,resource,tmp2,tmp3,tmp,gridbp(nbp)


!$omp parallel do private(h,e,bpmin,hp,bpmax,tmp,tmp2,tmp3,il,ir,fai,gridbp,lasttmp,bp,ep,resource,cons)
do b=1,max_index(retire)
   do h=1,max_index_h(retire)
    if (gridb(b)<-p*gridh(h)*lambdab*(1d0-delta_h)) cycle
       do e=1,ne
             do hp=1,nh
                lasttmp=small
               ! r=rb*judge(gridb(b).ge.0d0)+rm*judge(gridb(b)<0d0)
                resource=gridb(b)&
                +labor(retire,e)*(1d0-tau_ss)*w+p*gridh(h)-p*gridh(hp)-cost(gridh(h),gridh(hp))+initialwealth
                bpmax=resource*(judge(resource>0d0)*(1d0/qb)+judge(resource.le.0d0)*(1d0/qm))/(1d0+g)
                bpmax=bpmax-1d-10
                bpmin=-p*gridh(hp)*lambdab*(1d0-delta_h)
                if (bpmin.ge.bpmax) exit
                call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
                 do bp=1,nbp
                 cons=resource-gridbp(bp)*(1d0+g)*(qb*judge(gridbp(bp)>0d0)+qm*judge(gridbp(bp) .le. 0d0))
                 tmp3=0d0
                 call linear(gridbp(bp), gridb, il, ir, fai)
                 tmp3=fai*v(e,hp,il,retire+1)+(1d0-fai)*v(e,hp,ir,retire+1)  ! the vh is equal to vh in the latex  
                 tmp=u(cons,gridh(h))+beta*((1d0-death(retire+1))*tmp3+&
                 death(retire+1)*bequestutility(gridbp(bp)+(1d0-delta_h-kappa_h)*gridh(hp)*p))
                 if (tmp>v(e,h,b,retire)) then
                     v(e,h,b,retire)=tmp
                     s(e,h,b,retire)=gridbp(bp)
                     pu(e,h,b,retire)=hp
                     c(e,h,b,retire)=cons
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


subroutine opt_last
real(8) cons,income,gridbp(nbp)
integer ir,il
real(8) fai
integer b,h,e,bp,ep,hp

bpmin=0d0
!$omp parallel do private(h,e,bpmax,cons,temp,gridbp,bp,hp)
do b=1,nb
   do h=1,nh
    if (gridb(b)<-p*gridh(h)*lambdab*(1d0-delta_h)) cycle
        
           do e=1,ne
               do hp=1,nh
                  
             ! start optimization
                !r=rb*judge(gridb(b).ge.0d0)+rm*judge(gridb(b)<0d0)
                bpmax=(gridb(b)&
                +labor(60,e)*replace*w+p*gridh(h)-p*gridh(hp)-cost(gridh(h),gridh(hp))+initialwealth)/(qb*(1d0+g))
                if (bpmin.ge.bpmax) exit
                call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
                do bp=1,nbp
                  cons=gridb(b)&
                  +labor(60,e)*replace+p*gridh(h)-p*gridh(hp)-cost(gridh(h),gridh(hp))-gridbp(bp)*qb*(1d0+g)+initialwealth
                  if (cons.le.0d0) exit
                     temp=u(cons,gridh(h))+beta*bequestutility(gridbp(bp)+(1d0-delta_h-kappa_h)*p*gridh(hp))
                   if (temp>v(e,h,b,agemax)) then
                        v(e,h,b,agemax)=temp
                        s(e,h,b,agemax)=gridbp(bp)
                        c(e,h,b,agemax)=cons
                        pu(e,h,b,agemax)=hp
                    end if
                    end do
                   
                    end do
             enddo
             enddo
    end do      
   !$omp  end  parallel do   
      
end



subroutine opt_after(j)
real(8) cons
integer ir,il,b,h,e,hp,j,ep,bp,times
real(8) fai,lasttmp,resource,tmp2,tmp3,tmp,gridbp(nbp)

!print*, 'at age',j
!!$omp parallel do private(h,e,bpmin,hp,bpmax,tmp,tmp2,tmp3,il,ir,fai,gridbp,lasttmp,bp,ep,resource,cons)
do b=1,max_index(j)
   do h=1,max_index_h(j)
    if (gridb(b)<-p*gridh(h)*lambdab*(1d0-delta_h)) cycle
         
           do e=1,ne
                  
             ! start optimization
            
             do hp=1,nh
               ! print*, e,h,b,hp
                lasttmp=small
                times=0
               ! r=rb*judge(gridb(b).ge.0d0)+rm*judge(gridb(b)<0d0)
                resource=gridb(b)&
                +labor(retire,e)*replace*w+p*gridh(h)-p*gridh(hp)-cost(gridh(h),gridh(hp))+initialwealth
                bpmax=resource*(judge(resource>0d0)*(1d0/qb)+judge(resource.le.0d0)*(1d0/qm))/(1d0+g)
                bpmax=bpmax-1d-10
                bpmin=-p*gridh(hp)*lambdab*(1d0-delta_h)
                if (bpmin.ge.bpmax) exit
                call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
                 do bp=1,nbp
                 cons=resource-gridbp(bp)*(1d0+g)*(qb*judge(gridbp(bp)>0d0)+qm*judge(gridbp(bp) .le. 0d0))
                 
                 call linear(gridbp(bp), gridb, il, ir, fai)
                 tmp3=fai*v(e,hp,il,j+1)+(1d0-fai)*v(e,hp,ir,j+1)  ! the vh is equal to vh in the latex  
                 tmp=u(cons,gridh(h))+beta*((1d0-death(j+1))*tmp3+&
                 death(j+1)*bequestutility(gridbp(bp)+(1d0-delta_h-kappa_h)*gridh(hp)*p))
               !  print*,bp, tmp
                 if (tmp>v(e,h,b,j)) then
                     v(e,h,b,j)=tmp
                     s(e,h,b,j)=gridbp(bp)
                     pu(e,h,b,j)=hp
                     c(e,h,b,j)=cons
                 end if
                 if (tmp<lasttmp) then
               !  if (times<30)print*,gride(e),gridh(h),gridb(b),gridh(hp),gridbp(bp),times
                 exit
                 end if
                 lasttmp=tmp
                ! times=times+1
            end do
            
             enddo
             enddo
             enddo
      end do
  ! !$omp  end  parallel do   
             
end 


subroutine opt_before(j)
real(8) cons
integer ir,il,b,h,e,hp,j,ep,bp
real(8) fai,lasttmp,resource,tmp2,tmp3,tmp,gridbp(nbp)

!$omp parallel do private(h,e,bpmin,hp,bpmax,tmp,tmp2,tmp3,il,ir,fai,gridbp,lasttmp,bp,ep,resource,cons)
do b=1,max_index(j)
   do h=1,max_index_h(j)
    if (gridb(b)<-p*gridh(h)*lambdab*(1d0-delta_h)) cycle
       do e=1,ne
         
             ! start optimization
             do hp=1,nh
                lasttmp=small
               ! r=rb*judge(gridb(b).ge.0d0)+rm*judge(gridb(b)<0d0)
               resource=gridb(b)&
                +labor(j,e)*(1d0-tau_ss)*w+p*gridh(h)-p*gridh(hp)-cost(gridh(h),gridh(hp))+initialwealth
                
                bpmax=resource*(judge(resource>0d0)*(1d0/qb)+judge(resource.le.0d0)*(1d0/qm))/(1d0+g)
                bpmax=bpmax-1d-10
                bpmin=-p*gridh(hp)*lambdab*(1d0-delta_h)
                if (bpmin.ge.bpmax) exit
                call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
                 do bp=1,nbp
                 cons=resource-gridbp(bp)*(1d0+g)*(qb*judge(gridbp(bp)>0d0)+qm*judge(gridbp(bp) .le. 0d0))
                 tmp3=0d0
                 call linear(gridbp(bp), gridb, il, ir, fai)
                     do ep=1,ne
                        tmp2=fai*v(ep,hp,il,j+1)+(1d0-fai)*v(ep,hp,ir,j+1)  ! the vh is equal to vh in the latex
                        tmp3=tmp3+markov(e,ep)* tmp2
                     end do
                 tmp=u(cons,gridh(h))+beta*((1d0-death(j+1))*tmp3+&
                 death(j+1)*bequestutility(gridbp(bp)+(1d0-delta_h-kappa_h)*gridh(hp)*p))
                 if (tmp>v(e,h,b,j)) then
                     v(e,h,b,j)=tmp
                     s(e,h,b,j)=gridbp(bp)
                     pu(e,h,b,j)=hp
                     c(e,h,b,j)=cons
                 end if
                 if (tmp<lasttmp) exit
                 lasttmp=tmp
            end do
            
             enddo
             end do
             enddo
         
             enddo
      
 !$omp  end  parallel do        
end 




function cumsum(array,n)
integer n,i
real(8) cumsum(n),array(n),s
s=0d0
do i=1,n
s=s+array(i)
cumsum(i)=s
end do

end







function cost(h1,h2)
real(8) h1,h2,cost

!cost=(0.01d0*abs(h1-h2)+h1*delta_h)*p*judge(h1.ne.h2)+h1*delta_h*p*h1*judge(h1==h2) 
! if you sell h1 and buy h2, you have to pay the kf, tax
cost=p*delta_h*h1
cost=cost+kappa_h*abs(h1-h2)*p ! due to detrend, you need to add (1+g) here
end 




subroutine summarize
!this routine goes over the simulation and calculate some life cycle and output.
integer j,e,b,h
bequestpositive=0d0

cons_a=0d0
ownership=0d0
life_cons=0d0
life_house=0d0
life_asset=0d0
life_income=0d0
life_ownership=0d0
life_measure=0d0
maxsave=0d0
constrained=0d0
networth=0d0
!wealth50=0d0
!wealth70=0d0

do j=agemin,agemax
  do b=1,nb
     do h=1,nh
       if (gridb(b)<-p*lambdab*gridh(h)*(1d0-delta_H)) cycle 
        do e=1,ne
          if (m(e,h,b,j)==0d0) cycle
          life_measure(j)=life_measure(j)+m(e,h,b,j)
          if (j.le.retire) then
          life_income(j)=life_income(j)+m(e,h,b,j)*(labor(j,e)*(1d0-tau_ss)*w+initialwealth)
          totalincome=totalincome+m(e,h,b,j)*(labor(j,e)*(1d0-tau_ss)*w+initialwealth)
          totalincome=totalincome+m(e,h,b,j)*s(e,h,b,j)&
          *(judge(s(e,h,b,j)>0d0)*(1d0-qb)+judge(s(e,h,b,j)<=0d0)*(1d0-qm))
          else
          life_income(j)=life_income(j)+m(e,h,b,j)*(labor(retire,e)*(1d0-tau_ss)*w+initialwealth)
          totalincome=totalincome+m(e,h,b,j)*(labor(retire,e)*replace*w+initialwealth)
           totalincome=totalincome+m(e,h,b,j)*s(e,h,b,j)&
          *(judge(s(e,h,b,j)>0d0)*(1d0-qb)+judge(s(e,h,b,j)<=0d0)*(1d0-qm))
          end if
          bequestpositive=bequestpositive+m(e,h,b,j)*death(j+1)*judge(s(e,h,b,j)>0d0)
          
          networth=networth+m(e,h,b,j)*(gridb(b)*(qb*judge(gridb(b)>0d0)&
          +qm*judge(gridb(b)<0d0))+p*gridh(h)*(1d0-delta_h))
          life_cons(j)=life_cons(j)+m(e,h,b,j)*c(e,h,b,j)
          cons_a=cons_a+m(e,h,b,j)*c(e,h,b,j)
          life_house(j)=life_house(j)+m(e,h,b,j)*gridh(h)
          life_asset(j)=life_asset(j)+m(e,h,b,j)*s(e,h,b,j)
          life_ownership(j)=life_ownership(j)+m(e,h,b,j)*judge(h>1)
          ownership=ownership+m(e,h,b,j)*judge(h>1)
       
          if (s(e,h,b,j)>maxsave(j)) then
          maxsave(j)=s(e,h,b,j)
         ! maxsavemeasure(j)=m(e,h,b,j)
          end if
      
          networth=networth+m(e,h,b,j)*(p*gridh(h)+gridb(b))
 
     end do
    end do
end do
    life_cons(j)=life_cons(j)/(sum(m(:,:,:,j)))
    life_asset(j)=life_asset(j)/(sum(m(:,:,:,j)))
    life_house(j)=life_house(j)/(sum(m(:,:,:,j)))
    life_ownership(j)=life_ownership(j)/(sum(m(:,:,:,j)))
    constrained(j)=constrained(j)/(sum(m(:,:,:,j)))
    life_income(j)=life_income(j)/sum(m(:,:,:,j))
   ! print*, life_cons(j),life_asset(j),maxsave(j),maxsavemeasure(j)
end do
ownership=ownership/(totalmeasure)

end 




subroutine maxhousing_gen
integer j,k
maxhousing(agemin)=0d0
max_index_h(agemin)=1
do j=agemin+1,retire
   maxhousing(j)=(maxsaving(j-1)+labor(j-1,5)*(1d0-tau_ss)*w+initialwealth)/p
   !print*,max_index_h(j-1)
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
   !print*,maxsaving(j),max_index_h(j)
end do

do j=retire+1,agemax
   maxhousing(j)=(maxsaving(j-1)+labor(retire,5)*w*replace)/p
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



subroutine maxsaving_gen
integer j,k
maxsaving(21)=0d0
max_index(21)=zeropoint
do j=agemin+1,retire
   maxsaving(j)=(maxsaving(j-1)+labor(j,5)*(1d0-tau_ss)*w+initialwealth)/qb
   if (maxsaving(j)<gridb(max_index(j-1))) then
      max_index(j)=max_index(j-1)
   else
      max_index(j)=nb ! initialization
      do k=max_index(j-1),nb
         if (gridb(k)>maxsaving(j) )  then
             max_index(j)=k
             exit
         end if
      end do
   end if
end do

do j=retire+1,agemax
   maxsaving(j)=(maxsaving(j-1)+labor(retire,5)*w*replace+initialwealth)/qb
   if (maxsaving(j)<gridb(max_index(j-1))) then
      max_index(j)=max_index(j-1)
   else
      max_index(j)=nb ! initialization
      do k=max_index(j-1),nb
         if (gridb(k)>maxsaving(j) )  then
             max_index(j)=k
             exit
         end if
      end do
   end if
end do
end

end



