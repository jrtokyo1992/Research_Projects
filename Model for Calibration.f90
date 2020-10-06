
! This code is for the Calculation of Benchmark Equilibrium.

include 'parameter.f90'


program main
use toolbox
use global
integer q
real(8) life_purchase(agemin:agemax),life_sell(agemin:agemax)

!beta=0.98d0


popgrowth=0.025d0
call CPU_TIME(timestart)

! generate the grid and transition matrix for income.
call tauchen(markov,gride)


! generate age vector. this is for drawing graph.
call age_gen

call survival_gen_initial

call gridh_gen


call gridb_gen
print*,gridb(zeropoint)

call labor_gen
!print*, death
construction_lasttime=2.5d0
wealth_lasttime=1.5d0
klratio_lasttime=6d0

print*,'start'

call calibration

contains

subroutine calibration
integer b,h,e,j
real(8) networth,maxsavemeasure(agemin:agemax),wealth50,wealth70,bequestpeople,negativesaving(agemin:agemax)
real(8) minsave(agemin:agemax)
replace=0.7d0


print*,'average labor',totallabor/sum(m_y(agemin:agemax,:))
call steadystate

v_old=v
gridb_old=gridb
pu_old=pu
s_old=s
c_old=c
bequestpositive=0d0

cons_a=0d0
ownership=0d0
life_cons=0d0
life_house=0d0
life_asset=0d0
life_ownership=0d0
maxsave=0d0
constrained=0d0
networth=0d0
wealth50=0d0
wealth70=0d0
bequestpeople=0d0
maxhouse=0d0
minsave=0d0
do j=agemin,agemax
  do b=1,nb
     do h=1,nh
       if (gridb(b)<-p*lambdab*gridh(h)*(1d0-delta_h)) cycle 
        do e=1,ne
          if (m(e,h,b,j)==0d0) cycle
          if (j.le.retire) then
          totalincome=totalincome+m(e,h,b,j)*(labor(j,e)*(1d0-tau_ss)*w+initialwealth)
          else
          totalincome=totalincome+m(e,h,b,j)*(labor(retire,e)*replace*w+initialwealth)
          end if
          if (gridb(b)<minsave(j)) minsave(j)=gridb(b)
          bequestpositive=bequestpositive+m(e,h,b,j)*death(j+1)*judge(s(e,h,b,j)>0d0)
          bequestpeople=bequestpeople+m(e,h,b,j)*death(j+1)
        if (h==nh) maxhouse(j)=maxhouse(j)+m(e,h,b,j)
        if (b==nb) maxsave(j)=maxsave(j)+m(e,h,b,j)
          networth=networth+m(e,h,b,j)*(gridb(b)*(qb*judge(gridb(b)>0d0)&
          +qm*judge(gridb(b)<0d0))+p*gridh(h)) ! notice there that grid(b) contains return! if you consider the wealth at the 'beginning' of each period, you should get rid of the return
          life_cons(j)=life_cons(j)+m(e,h,b,j)*c(e,h,b,j)
          cons_a=cons_a+m(e,h,b,j)*c(e,h,b,j)
          life_house(j)=life_house(j)+m(e,h,b,j)*gridh(h)
          life_asset(j)=life_asset(j)+m(e,h,b,j)*(s(e,h,b,j)*(judge(s(e,h,b,j)>0d0)*qb+judge(s(e,h,b,j)<=0d0)*qm)     )
          life_ownership(j)=life_ownership(j)+m(e,h,b,j)*judge(h>1)
          ownership=ownership+m(e,h,b,j)*judge(h>1)
        
          
         wealth50=wealth50+m(e,h,b,j)*(p*gridh(h)*(1d0-delta_h)+&
         gridb(b)*(qb*judge(gridb(b)>0d0)+qm*judge(gridb(b).le.0d0)))*judge(j.ge.40 .and. j.le. 60)
         !wealth60=wealth60+m(e,h,b,j)*(p*gridh(h)*(1d0-delta_h)+gridb(b)*qb)*judge(j==50)
         wealth70=wealth70+m(e,h,b,j)*(p*gridh(h)*(1d0-delta_h)+&
         gridb(b)*(qb*judge(gridb(b)>0d0)+qm*judge(gridb(b).le.0d0)))*judge(j>69)
    
     end do
    end do
end do
     negativesaving(j)=sum(m(:,:,1:zeropoint-1,j))/sum(m(:,:,:,j))
     maxsave(j)=sum(m(:,:,nb,j))/sum(m(:,:,:,j))
    life_cons(j)=life_cons(j)/(sum(m(:,:,:,j)))
    life_asset(j)=life_asset(j)/(sum(m(:,:,:,j)))
    life_house(j)=life_house(j)/(sum(m(:,:,:,j)))
    life_ownership(j)=life_ownership(j)/(sum(m(:,:,:,j)))
    constrained(j)=constrained(j)/(sum(m(:,:,:,j)))
    maxhouse(j)=maxhouse(j)/(sum(m(:,:,:,j)))
    
    print*, life_cons(j),negativesaving(j),maxhouse(j),maxsave(j),minsave(j)
    
end do
ownership=ownership/(totalmeasure)
wealth50=wealth50/(sum(m(:,:,:,40:60)))
wealth70=wealth70/(sum(m(:,:,:,70:agemax)))



! do some output
print*,'parameter 1: h_constant'
print*, 'ownership is',ownership,'data is',0.76d0  ! this is data from chips in 2002 and 1999.
print*,'parameter 2: phi'
print*, 'consumption/totalincome ratio', cons_a/(totalincome+a_a*r),&
 'data is approximately at around 0.6d0'
print*,'or, consumption/gdp ratio', cons_a/(a_a**eta*totallabor**(1d0-eta)),'data is approximately 0.53.'
print*,'the household consumption gdp ratio declines from 50% in 1983 to 38% in 2008'
print*,'parameter 3: b constant'
print*,'ratio of people(who die) leaving positive bequest', bequestpositive/bequestpeople, 'data is 0.6 to 0.8()7'

print*,'parameter 4: beta'
print*, 'wealth-income ratio ', networth/(totalincome+a_a*r),&
'data is about 3.5 in 2002, but my dataset says that it is at around 4.8'
print*,'or capital-output ratio',a_a/(a_a**eta*totallabor**(1d0-eta)),'data is about 3.5 (at 1996 to 2000)'

print*, 'parameter 5: psi'
print*,'wealth70/50', wealth70/wealth50,'data is about',1.

print*,'parameter (set) 60: gridh'
print*,'price-income ratio',p*gridh(floor(nh*0.5d0))*sum(m(:,:,:,agemin:retire))/(totallabor*w*(1d0-tau_ss)),'data is roughly 7'
print*,'alternative moment 1', p*sum(life_house(agemin:retire))/(totallabor*w*(1d0-tau_ss))
print*,'alternative moment 2', p*sum(life_house)/(totallabor*w)
! we also want to have a good fit of the wealth distriution(?)

print*,'some alternative check'
print*, 'the ratio of housing wealth'
print*, hold_a*p*totalmeasure/networth

do j=1,14
life_house5(j)=sum(life_house(agemin+(j-1)*5   :agemin-1+j*5))/5d0
life_cons5(j)=sum(life_cons(agemin+(j-1)*5   :agemin-1+j*5))/5d0
life_asset5(j)=sum(life_asset(agemin+(j-1)*5   :agemin-1+j*5))/5d0
end do

age5=((/(i,i=1,14,1)/)-1d0)*5d0+21d0

call plot(age5, life_cons5)
call execplot(ylabel='consumption',xlabel='age',title='life cycle consumption')
call plot(age5, life_house5)
call execplot(ylabel='housing',xlabel='age',title='life cycle housing')
call plot(age5, life_asset5)
call execplot(ylabel='liquid asset',xlabel='age',title='life cycle liquid asset')
call plot(age, life_ownership)
call execplot(ylabel='ownership',xlabel='age',title='life cycle ownership')

open(1, file='purchase_buy_initial.xls', status='replace') 
do j=agemin,agemax
  
 write(1,*) life_purchase(j),life_sell(j),sum(m(:,:,:,j))

end do
close(1) 


open(1, file='purchase_initial.xls', status='replace') 
do j=agemin,agemax
  do b=1,nb
    do h=1,nh
     do e=1,ne
      write(1,*) e,h,b,j,pu(e,h,b,j)
     end do
     end do
     end do

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

open(1, file='price1.xls', status='replace') 
write(1,*) p,r,w
close(1) 

open(1, file='maxgrid_initial.xls', status='replace') 
do j=agemin,agemax
write(1,*) maxsaving(j),max_index(j),maxhousing(j),max_index_h(j)
end do
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
m_y(agemin,5)=0.25d0  ! this is from chips data.
totallabor=0d0
base=0d0
do j=agemin,agemax-1
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
        m_y(j+1,e)=m_y(j,e)
        base=base+m_y(j,e)*exp(period(retire)+gride(e)) 
      
       end if
       !print*,labor(j,e)
    end do
    m_y(j+1,:)=m_y(j+1,:)*(1d0-death(j+1))/(1d0+popgrowth)  
end do

totalmeasure=sum(m_y) 
print*,'total measure is', totalmeasure
tau_ss=base*replace/totallabor

end


subroutine steadystate
 ! find the initial wealth and transfer 
 ! one lesson: since you have to update three values at the same time, it will be better to slow down the update speed.
 ! otherwise it may be hard to find an equilibrium.
real(8) adjust,klmin,klmax
integer iter,j
adjust=0.78d0

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
if (error<0.07d0 .or. iter>20) then
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
hold_a=0d0
life_purchase=0d0
life_sell=0d0
! please pre allocate the matrixs before you use this subroutine!

m(1,1,zeropoint,agemin)=0.22d0
m(2,1,zeropoint,agemin)=0.22d0
m(3,1,zeropoint,agemin)=0.14d0
m(4,1,zeropoint,agemin)=0.17d0
m(5,1,zeropoint,agemin)=0.25d0
! I calculate the initial labor distribution from the chips data.
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
           bequest_a=bequest_a+death(j+1)*m(e,h,b,j)*(s(e,h,b,j)+p*(1d0-kappa_h-delta_h)*gridh(pu(e,h,b,j)))/(1d0+popgrowth) ! we need a pop growth here..
           life_purchase(j)=life_purchase(j)+m(e,h,b,j)*gridh(pu(e,h,b,j))
           if (j<agemax)life_sell(j+1)=life_sell(j+1)+m(e,h,b,j)*gridh(pu(e,h,b,j))*(1d0-delta_h)/(1d0+popgrowth)
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
          ! print*, sum(m(:,:,:,j))
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
              !  times=0
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
                 if (tmp<lasttmp) exit
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





subroutine maxsaving_gen
integer j,k,il,ir
real(8) temp,fai
maxsaving(agemin)=0d0
max_index(agemin)=zeropoint
do j=agemin+1,retire
   temp=(maxsaving(j-1)+labor(j-1,5)*(1d0-tau_ss)*w+initialwealth)*(1d0+r)
    call linear(temp,gridb,il,ir,fai)
   maxsaving(j)=gridb(ir)
   max_index(j)=ir
end do

do j=retire+1,agemax
   temp=(maxsaving(j-1)+labor(retire,5)*replace*w+initialwealth)*(1d0+r)
   call linear(temp,gridb,il,ir,fai)
   maxsaving(j)=gridb(ir)
   max_index(j)=ir
end do
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
   maxhousing(j)=(maxsaving(j-1)+labor(retire,5)*w*replace+initialwealth)/p
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

end

