
! This code compute the initial equilibrium, 
!and compare model moment with the data moment. 
! We first guess the aggregate variables and prices
! Based on prices,  compute the policy function for household by backward inducation
! use grid search when solving optimization problem. 
! With the policy function, simulate the whole model economy and get aggregate varaibles.
! if new aggregate variables are close enough to our initial guess, stop
! otherwise,  update guess and re-iterate. 

! first, incorporate all the parameters and variables.
include 'parameter.f90'

program main
use toolbox
use global

integer j

! generate the grid and transition matrix for income.
call tauchen(markov,gride)

! generate age vector. this is for drawing graph.
call age_gen

! generate the grid set for housing stock
call gridh_gen
! generate the grid set for liquid asset
call gridb_gen

call sscompare

contains


subroutine sscompare
integer j,b,h,e,policy, sc, po

character(len = 100), dimension(3):: &
scenario_vct = [character(len= 100) ::'mortality_only','popgrowth_only','mortality_popgrowth']
character(len = 100), dimension(3):: &
policy_vct = [character(len = 100) ::'adjust_replacement','adjust_tax','pension_reform']

age5=((/(i,i=1,14,1)/)-1d0)*5d0+21d0

open(1, file='aggregate_initial.xls', status='old')
        read(1,*) p_init,r_init,w_init,ownership_init,&
        hold_a_init, construction_init,klratio_init, l_p_init,trans_init,&
        totalmeasure_init,tau_ss_init, replace_init,omega_init, g_e_ratio_init
    close(1)

! now calculate the new one: the tax rate is fixed, but replace ratio changes.
! generate the demographic structure in initial equilibrium 
! get death_old and survial_old
popgrowth_old = 0.025d0
call survival_gen_initial
survival_old = survival 
death_old = death

! generate the demographic structure in new equilibrium 
! get death_new and survival_New
popgrowth_new=-0.0049d0 ! new population growth
call survival_gen_new ! generate a new one, get the new survival and death

do sc = 1,size(scenario_vct)
    do po = 1, size(policy_vct)
        call steadystate_new( scenario_vct(sc), policy_vct (po))
        call summarize
        ! output the aggregate
        
        open(1, file=trim(scenario_vct(sc))//'_'// trim(policy_vct (po))//'_aggregate.txt',&
         status='replace') ! trim function is to remove extra space
            write(1,*) p,r,w,ownership,hold_a,construction,klratio, l_p
        close(1)
        ! output the life cycle
        open(1, file = trim(scenario_vct(sc))//'_'// trim(policy_vct (po))//'_life.txt',&
         status = 'replace')
            do j=agemin,agemax
                write(1,*) life_cons(j),life_asset(j),&
                life_house(j),life_ownership(j)
            end do
        close(1)
        ! output the distribution 
        open(1, file = trim(scenario_vct(sc))//'_'// trim(policy_vct (po))//'_measure.txt',&
         status = 'replace')
            do j=agemin,agemax
                do b=1,nb
                    do h=1,nh
                        do e=1,ne
                            write(1,*) e,h,b,j,m(e,h,b,j)
                        end do
                    end do
                end do
            end do
        close(1)
    end do
end do
end 


subroutine steadystate_new(scenario, policy)
 ! find the initial wealth and transfer 
 ! otherwise it may be hard to find an equilibrium.
    character(*)  policy, scenario
    real(8) adjust,klmin,klmax,replace_new,gama_new, tau_ss_new
    integer iter,j

    !-------- determine popgrowtha and death value according to scenario
    select case (scenario)
        case ('mortality_only')
            popgrowth = popgrowth_old
            death = death_new
        case ('popgrowth_only')
            popgrowth = popgrowth_new
            death = death_old
        case ('mortality_popgrowth')
            popgrowth = popgrowth_new
            death = death_new
    end select
    
    select case (policy)
        case('adjust_replacement', 'adjust_tax')
            tau_ss = tau_ss_init
            replace = replace_init
            omega = omega_init
        case('pension_reform')
            tau_ss = tau_ss_init
            replace = 0.58d0
            omega = 0.6d0
    end select

    ! get the total labor, total measure, the population before and after retire
    call labor_gen ! 
    
    !-------- scale the whole economy to the initial one!
    scaling = totalmeasure_init/totalmeasure
    print*,'the scaling is', scaling
    base = base * scaling
    totallabor = totallabor * scaling
    print*,'total labor is', totallabor
    m_y = m_y  * scaling
    pop_pre_retire = pop_pre_retire * scaling
    pop_after_retire = pop_after_retire * scaling
    print*,'pre and after retire population:', pop_pre_retire, pop_after_retire
    !-------- generate the beta, h_trend, and p_trend
    beta = beta_original * (1d0+g)**( (1d0-phi+alpha*phi)*(1d0-sigma)  )* &
    (1d0+popgrowth)**((alpha-1d0)*phi*(1d0-sigma))
    print*, 'beta is', beta
    h_trend  = (1d0 + g)**alpha /( (1d0 + popgrowth)**(1d0-alpha) )
    p_trend = ((1d0+ g)*(1d0+popgrowth))**(1d0 - alpha)
    
    print*, 'tau_ss, replace and omega', tau_ss, replace,omega
    !set the speed for updating
    adjust =0.775d0  
    ! initializing the variables that we need to find a fixed point.
    construction = construction_init
    trans = trans_init
    klratio = klratio_init
  
    ! initialize the current iteration times
    iter=1
    ! prevent the k-l ratio from being too large or small.
    klmin = 0d0
    klmax = 100d0
    ! start the iteration
    do
!       given the initial guess of contruction, wealth, and klratio, find the prices
        r = (1d0-gama)*eta*klratio**(eta-1d0)-delta
        qb = 1d0/(1d0+r)
        qm = 1d0/(1d0+r+premium)
        w = (1d0-gama)*(1d0-eta)*klratio**eta
        p=(construction/land)**((1d0-alpha)/alpha)/(alpha*(1d0-gama) )  
        ! use final goods and land to construct house.

        print*,'xxxxxxxxxxxxxxxx'
        print*, 'inner loop',iter
        select case (policy)
        case ('adjust_replacement')
            print*,' construction ,klratio, trans, repalce'
            print*,construction, klratio, trans, replace
        case ('adjust_tax','pension_reform')
            print*,' construction ,klratio, trans, tau_ss'
            print*,construction, klratio, trans, tau_ss
        end select
    
        !print*,'housing price, wage and interest rate is'
        !print*,p,w,r

        call maxsaving_gen
        call maxhousing_gen
        ! compute the policy function by backward induction.
        call policy_function
        ! given the policy function, simulate the household behavior  
        call simulation
        ! get aggregate variables
        ! need to detrend the aggregate house evoluation.
         construction_new = ((1d0+ g)**alpha * (1d0 + popgrowth)**alpha&
         -(1d0-delta_h))*hold_a
        print*, hold_a
        klratio_new = max(a_a/(totallabor),0d0)
        trans_new = bequest_a 
        l_p = (1d0 - alpha) * alpha**(alpha/(1d0-alpha)) &
        * (p*(1d0-gama))**(1d0/(1d0-alpha))* land
        g_e = (a_a **eta) *(totallabor**(1d0-eta))*g_e_ratio
        tau_ss_new  = 0d0
        gama_new = 0d0
        replace_new = 0d0
        print*,'new construction ,klratio'
        print*,construction_new,klratio_new
        select case (policy)
            case ('adjust_replacement')
                replace_new = ( l_p + totallabor * tau_ss *w +&
                house_trans_revenue - g_e  )/(base*w)
                print*,'new construction  ,klratio, trans, and replacement is'
                print*,construction_new,klratio_new, trans_new,replace_new
            case ('adjust_tax','pension_reform')
                tau_ss_new = (  base * replace * w + g_e -&
                house_trans_revenue  -l_p )/ (totallabor * w )
                print*,'new construction  ,klratio, trans,and tau_ss is'
                print*,construction_new,klratio_new, trans_new,tau_ss_new
        end select
    
        error = abs(construction-construction_new)/construction+&
        abs(klratio-klratio_new)/klratio + abs(trans-trans_new)/trans
        select case (policy)
            case ('adjust_replacement')
                error = error + abs( replace_new- replace )/replace
            case ('adjust_tax','pension_reform')
                error = error + abs( tau_ss_new- tau_ss )/tau_ss
        end select
    
        print*, 'error is',error
        ! if the initial guess and new value are close enough, then stop.
        if (error<0.05d0 .or. iter>25) then
            exit
        end if
    ! update
        construction = adjust*construction+(1d0-adjust)*construction_new
        klratio = adjust*klratio+(1d0-adjust)*klratio_new
        trans = adjust*trans+(1d0-adjust)*trans_new
        select case (policy)
            case ('adjust_replacement')
                replace = replace *adjust + (1d0 - adjust) * replace_new
            case ('adjust_tax','pension_reform')
            tau_ss = tau_ss *adjust + (1d0 - adjust) * tau_ss_new
        end select

        iter = iter+1

end do
end 

subroutine summarize
!this routine goes over the simulation and calculate some life cycle and output.
integer j,e,b,h
!bequestpositive=0d0
cons_a = 0d0
ownership = 0d0
life_cons = 0d0
life_house = 0d0
life_asset = 0d0
life_ownership = 0d0
maxsave = 0d0
!constrained = 0d0
networth = 0d0
!maxhouse = 0d0

do j = agemin,agemax
    life_cons(j) = sum(m(:,:,:,j)*c(:,:,:,j))/sum(m(:,:,:,j))
    life_asset(j) = sum(m(:,:,:,j)*s(:,:,:,j))/sum(m(:,:,:,j))
    life_ownership(j) = sum(m(:,2:nh,:,j))/sum(m(:,:,:,j))
    !maxsave(j) = sum(m(:,:,nb,j))/sum(m(:,:,:,j))
    !maxhouse(j) = sum(m(:,nh,:,j))/(sum(m(:,:,:,j)))
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
    
end do
cons_a = sum(m*c)
totalincome = (1d0-tau_ss)* w* totallabor + base * replace * w
ownership = sum(m(:,2:nh,:,:))/sum(m)

end 


end

