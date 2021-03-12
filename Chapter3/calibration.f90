
! This code compute the initial equilibrium, and compare model moment with the data moment. 
! We first guess the aggregate variables and prices
! Based on prices, we first compute the policy function for household by backward inducation
! We use grid search when solving optimization problem. time consuming, but robust. 
! With the policy function, we simulate the whole model economy and get aggregate varaibles.
! The equilibrium, a fixed point, is reached, if new aggregate variables are close enough to our initial guess.
! otherwise, we need to update our guess and reiterate. 

! first, incorporate all the parameters, global variables, and subroutines.
include 'parameter.f90'

program main

use global


popgrowth = 0.025d0
tau_ss  = 0.08d0
replace = 0.75d0
!gama = 0.1d0
gama = 0d0
omega = 1d0
beta = beta_original * (1d0+g)**( (1d0-phi+alpha*phi)*(1d0-sigma)  )* (1d0+popgrowth)**((alpha-1d0)*phi*(1d0-sigma))
h_trend  = (1d0 + g)**alpha /( (1d0 + popgrowth)**(1d0-alpha) )
p_trend = ((1d0+ g)*(1d0+popgrowth))**(1d0 - alpha)

print*,'the beta, h_trend and p_trend', beta,h_trend, p_trend
! generate the grid and transition matrix for income.
call tauchen(markov,gride)

! generate age vector. this is for drawing graph.
call age_gen
! generate survival probability series
call survival_gen_initial
! generate the grid set for housing
call gridh_gen
! generate the grid set for liquid asset
call gridb_gen
! generate the labor process , the total pension benefit, the total labor force
call labor_gen
print*, pop_after_retire, pop_pre_retire
call calibration

contains

subroutine calibration
integer b,h,e,j
real(8) networth,maxsavemeasure(agemin:agemax),wealth50,wealth70,bequestpeople,negativesaving(agemin:agemax)
real(8) minsave(agemin:agemax)

call steadystate
l_p = (1d0 - alpha) * alpha**(alpha/(1d0-alpha)) * (p*(1d0-gama))**(1d0/(1d0-alpha))* land
g_e =  l_p + totallabor * tau_ss *w + house_trans_revenue - base*replace*w
g_e_ratio = g_e/((a_a **eta) *(totallabor**(1d0-eta)) )
print*, 'land profit',l_p
print*, 'labor income tax ',totallabor* tau_ss*w
print*, 'house transaction tax revenue',house_trans_revenue
!print*, 'firm tax', gama * (a_a **eta) *(totallabor**(1d0-eta)), gama*p*construct
!print*, 'pension benefit',base*replace*w
print*, 'total pension expenditure is', total_pension
print*, 'resulting government expenditure', g_e
print*, 'governmen expenditure/ gdp ratio', g_e_ratio

gridb_old = gridb
pu_old = pu

bequestpositive = 0d0

cons_a = 0d0
ownership = 0d0
life_cons = 0d0
life_house = 0d0
life_asset = 0d0
life_ownership = 0d0
maxsave = 0d0
!constrained = 0d0
networth = 0d0
maxhouse = 0d0
minsave = 0d0
do j = agemin,agemax
    life_cons(j) = sum(m(:,:,:,j)*c(:,:,:,j))/sum(m(:,:,:,j))
    life_asset(j) = sum(m(:,:,:,j)*s(:,:,:,j))/sum(m(:,:,:,j))
    life_ownership(j) = sum(m(:,2:nh,:,j))/sum(m(:,:,:,j))
    maxsave(j) = sum(m(:,:,nb,j))/sum(m(:,:,:,j))
    maxhouse(j) = sum(m(:,nh,:,j))/(sum(m(:,:,:,j)))
    ! house, we need the following
    do b = 1,nb
        do h = 1,nh
            if ( gridb(b) < -p * lambdab*gridh(h)*(1d0-delta_h)) cycle 
            networth = networth+sum(m(:,h,b,j)*(gridb(b)*(qb*judge(gridb(b)>0d0)&
            +qm*judge(gridb(b)<0d0))+p*gridh(h)))
            do e = 1, ne
                if (pu(e,h,b,j)<0) cycle
                life_house(j) = life_house(j) + m(e,h,b,j)*gridh(pu(e,h,b,j))
            end do
           
        end do
    end do
    life_house(j) = life_house(j)/(sum(m(:,:,:,j)))
    print*, j,maxhouse(j), maxsave(j)
    
end do
cons_a = sum(m*c)
totalincome = (1d0-tau_ss)* w* totallabor + base * replace * w
ownership = sum(m(:,2:nh,:,:))/sum(m)

print*,'parameter 1: h_constant'
print*, 'ownership is',ownership,'data is',0.76d0  ! this is data from chips in 2002 and 1999.
print*,'parameter 2: phi'
print*, 'consumption/totalincome ratio', cons_a/(totalincome+a_a*r),&
'data is approximately at around 0.6d0'
print*,'or, consumption/gdp ratio', cons_a/(a_a**eta*totallabor**(1d0-eta)),'data is approximately 0.53.'
print*,'the household consumption gdp ratio declines from 50% in 1983 to 38% in 2008'
print*,'parameter 4: beta'
print*, 'wealth-income ratio ', networth/(totalincome+a_a*r),&
'data is about 3.5 in 2002, but my dataset says that it is at around 4.8'
print*,'or capital-output ratio',a_a/(a_a**eta*totallabor**(1d0-eta)),'data is about 3.5 (at 1996 to 2000)'
print*,'parameter (set) 60: gridh'
print*,'price-income ratio',p*gridh(floor(nh*0.5d0))*sum(m(:,:,:,agemin:retire))/(totallabor*w*(1d0-tau_ss)),'data is roughly 7'
print*,'alternative moment 1', p*sum(life_house(agemin:retire))/(totallabor*w*(1d0-tau_ss))
print*,'alternative moment 2', p*sum(life_house)/(totallabor*w)
print*,'some alternative check'
print*, 'the ratio of housing wealth'
print*, hold_a*p*totalmeasure/networth

do j = 1,14
    life_house5(j) = sum(life_house(agemin+(j-1)*5  :agemin-1+j*5))/5d0
    life_cons5(j) = sum(life_cons(agemin+(j-1)*5  :agemin-1+j*5))/5d0
    life_asset5(j) = sum(life_asset(agemin+(j-1)*5  :agemin-1+j*5))/5d0
end do

age5=((/(i,i=1,14,1)/)-1d0)*5d0+21d0

call plot (age5, life_cons5)
call execplot (ylabel='consumption',xlabel='age',title='life cycle consumption')
call plot (age, life_house)
call execplot (ylabel='housing',xlabel='age',title='life cycle housing')
call plot (age, life_asset)
call execplot (ylabel='liquid asset',xlabel='age',title='life cycle liquid asset')
call plot (age, life_ownership)
call execplot (ylabel='ownership',xlabel='age',title='life cycle ownership')

open(1, file='C:\Users\shufe\Dropbox\pension\regression\EMPI\life_house.txt', status='replace') 
do j = agemin, agemax
    write(1,*) life_house(j)
end do
close(1) 

open(1, file='C:\Users\shufe\Dropbox\pension\regression\EMPI\life_cons.txt', status='replace') 
do j = agemin, agemax
    write(1,*) life_cons(j)
end do
close(1) 

open(1, file='C:\Users\shufe\Dropbox\pension\regression\EMPI\life_liquid.txt', status='replace') 
do j = agemin, agemax
    write(1,*) life_asset(j)
end do
close(1) 

open(1, file='purchase_sell_initial.xls', status='replace') 
do j = agemin,agemax
    write(1,*) life_purchase(j),life_sell(j),sum(m(:,:,:,j))
end do
close(1) 

open(1, file='purchase_initial.xls', status='replace') 
do j = agemin,agemax
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


open(1, file='aggregate_initial.xls', status='replace')
write(1,*) p,r,w,ownership,&
hold_a, construction,klratio, l_p, trans,totalmeasure,tau_ss, replace,omega, g_e_ratio
close(1)

open(1, file='life_cycle_initial.xls', status='replace') 
do j=agemin,agemax
    write(1,*) life_cons(j),life_asset(j),life_house(j),&
    life_resource(j),life_ownership(j),life_cost(j)
end do
close(1) 

open(1, file='gridh.xls', status='replace') 
  do h=1,nh
 write(1,*) gridh(h)
end do
close(1) 

end 

subroutine steadystate
 ! find the initial wealth and transfer 
 ! one lesson: since you have to update three values at the same time, it will be better to slow down the update speed.
 ! otherwise it may be hard to find an equilibrium.
real(8) adjust,klmin,klmax
integer iter,j
! set the speed for updating
adjust =0.78d0  
! initializing the variables that we need to find a fixed point.
construction = 11d0
trans = 0.5d0
klratio = 4d0
! initialize the current iteration times
iter=1
! prevent the k-l ratio from being too large or small.
klmin = 0d0
klmax = 100d0
! start the iteration
do
! given the initial guess of contruction, wealth, and klratio, find the prices
    r=(1d0-gama)*eta*klratio**(eta-1d0)-delta
    qb=1d0/(1d0+r)
    qm=1d0/(1d0+r+premium)
    w=(1d0-gama)*(1d0-eta)*klratio**eta
    p=(construction/land)**((1d0-alpha)/alpha)/(alpha*(1d0-gama) )  ! use final goods and land to construct house.
   
    print*,'xxxxxxxxxxxxxxxx'
    print*, 'inner loop',iter
    print*,' construction ,klratio, trans'
    print*,construction,klratio, trans
    print*,'housing price, wage and interest rate is'
    print*,p,w,r
    !print*, 'subsidy for pension and tau_p is:', subsidy, tau_p
    call maxsaving_gen
    call maxhousing_gen
    ! compute the policy function by backward induction.
    call policy_function
    ! given the policy function, simulate the household behavior  
    call simulation
    ! get aggregate variables
    trans_new=bequest_a
    ! need to detrend the aggregate house evoluation.
    !construction_new=purchase_a * (1d0+ g)**alpha * (1d0 + popgrowth)**alpha -(1d0-delta_h)*hold_a
    construction_new = ((1d0+ g)**alpha * (1d0 + popgrowth)**alpha -(1d0-delta_h))*hold_a
    klratio_new=max(a_a/(totallabor),0d0)
    print*,'new construction , klratio, and trans is'
    print*,construction_new,klratio_new, trans_new
    print*,'total housing demand and (detrended) demand is', purchase_a,&
    purchase_a * (1d0+ g)**alpha * (1d0 + popgrowth)**alpha
    print*, 'house selling is ', (1d0-delta_h)*hold_a
    error = abs(construction-construction_new)/construction&
    +abs(klratio-klratio_new)/klratio + abs(trans-trans_new)/trans
    print*, 'error is',error
    ! if the distance between initial guess and new value is close enough, then stop.
    if (error<0.05d0 .or. iter>20) exit
    ! update
    construction = adjust*construction+(1d0-adjust)*construction_new
    klratio = adjust*klratio+(1d0-adjust)*klratio_new
    trans= adjust*trans + (1d0-adjust)*trans_new
    iter = iter+1

end do
end 

end

