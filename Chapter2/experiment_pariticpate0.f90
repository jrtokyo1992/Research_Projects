! the case without hpf.

include 'parameter0712.f90'
module variable

use global
implicit none
! define apome state grid


! define policy function

! define apome cohort-wise variable and aggregate variables.

real(8) current_p(ne,nh,nb,agemin:agemax),current_r(ne,nb,agemin:agemax)
real(8) current_n(ne,nb,agemin:agemax),current_s(ne,nh,nb,agemin:agemax),current_o(ne,nb,agemin:agemax)
real(8) next_p(ne,nh,nb,agemin:agemax),next_r(ne,nb,agemin:agemax)
real(8) next_n(ne,nb,agemin:agemax),next_s(ne,nh,nb,agemin:agemax),next_o(ne,nb,agemin:agemax)

real(8) cn(ne,nb,agemin:agemax),pu_n(ne,nb,agemin:agemax),re_n(ne,nb,agemin:agemax),h_b(ne,nb,agemin:agemax)
real(8) change20, change40,change60,change80,change100



end module

program main
use toolbox
use global
use variable


!real(8) gridb_neg(nb*0.6d0)
integer init

print*,'standard'

! generate the grid and transition matrix for income.
call tauchen(markov,gride)



call housegrid_gen  ! generate the house grid

call gridb_gen
!print*, gridb
! generate the labor
call totallabor_gen

! generate the income matrix
call shouru_gen

! given the nbf, we now allocate the matrixs.

! calculate the equilibrium. Do the calibration.
call calibration

Contains


subroutine totallabor_gen
! calculating the total labor.
integer j,e,ep
m_y=0d0
m_y(agemin,3)=1d0
do j=agemin,retire-1
    do e=1,ne
    do ep=1,ne
    m_y(j+1,ep)=m_y(j+1,ep)+m_y(j,e)*markov(e,ep)
    end do
   end do
end do
totallabor=0d0
do j=agemin,retire
do e=1,ne
totallabor=totallabor+m_y(j,e)*exp(period(j)+ gride(e) )
end do
end do
end

subroutine calibration
integer b,h,e,k,j,i
real(8) networth,totalexp,houseexp,wealth50,wealth75,wealth60
real(8) bequestratio,networthratio,bequestmotive,bequestpositive,n_h,totalincome,ownership35
!print*,'start'
real(8) age5(11),life_cons5(11),life_asset5(11),life_house5(11),life_ownership5(11)
allocate(m(ne,nh,nb,agemin:agemax))
age(agemin)=21d0
do k=agemin+1,agemax
age(k)=age(k-1)+1d0
end do

call steadystate_outer





open(1, file='measure_nohpf.xls', status='replace')  
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

open(1, file='price_nohpf.xls', status='replace')  
     write(1,*) rb,rm,p,d
close(1)
!call house_wealth

open(1, file='experiment_nohpf_house.xls', status='replace')
  do j=agemin,agemax
    
      do b=1,nb
        do h=1,nh
          do e=1,ne
            write(1,*) app(e,h,b,j),keep(e,h,b,j)
           end do
           end do
           end do
           end do
        
close(1)


open(1, file='experiment_nohpf_nohouse.xls', status='replace')
  do j=agemin,agemax
   
      do b=1,nb
          do e=1,ne
          if (j<agemax) write(1,*) apr(e,b,j),apo(e,b,j),pu(e,b,j),buy(e,b,j)
           if (j==agemax) write(1,*) apr(e,b,j)
           end do
           end do
           end do
     
close(1)


print*,'now deallocate m'

deallocate(m)


end





subroutine steadystate_outer
integer iter
real(8) rbmin,rbmax,rb_new

trans_relay=0.1d0
construction_relay=0.09d0
iter=1
rbmin=0.015d0
rbmax=0.045d0 !?
rb=(rbmax+rbmin)*0.5d0
iter=1
do
print*,'-----------------------------------'
print*,'outer loop ',iter

rm=rb+premium
print*,'r is',rb

call steadystate_inner
print*,'asset demand',asset_a*75  ! asset_a is an average concept. 
!print*,'bound people',sum(m(:,:,nb,:))/(agemax-agemin+1)

if (asset_a*75>0.6d0) then
   rbmax=rb
else if (asset_a*75<-0.6d0) then
   rbmin=rb
else
exit
endif

rb_new=(rbmax+rbmin)*0.5d0

rb=rb*0.5d0+rb_new*0.5d0

!rb=(rbmax+rbmin)*0.5d0

if (iter>20) exit
iter=iter+1
end do

end

subroutine steadystate_inner

use toolbox
integer iter,i,j,b

real(8) newconstruction_new,trans_new,error,adjust,temp1,temp2,newconstruction
real(8) holder,seller,ownership_35,house_expenditure,aggregate_expenditure
real(8) networth,annual_income,n_h
real(8) ownership,networthratio
real(8) aggregate_labor
real(8) rentersize,ownersize,renterincome,ownerincome
real(8) housevalue(nh+1),life_gini(agemin:agemax),bmin,bmax

trans=trans_relay
newconstruction=construction_relay
iter=1
adjust=0.5d0

do
print*,'****************************************************'
print*, 'inner loop ',iter
p=(newconstruction/land)**((1d0-alpha)/alpha)/alpha
d=fi+p-((1d0-delta_h)/(1d0+rb))*p
print*,'the initial initial wealth and constuction'
print*, trans,newconstruction
print*, 'the price and rent'
print*,p,d




!print*,'bmax and bmin:', bmax,bmin

call maxsaving_gen

call policy_function

call simulation
!newconstruction_new=renthouse_a*delta_h+hold_a*delta_h+purchase_a-sell_a*(1d0-delta_h)
!print*,'house renting and house holding is',renthouse_a,hold_a
newconstruction_new=renthouse_a*delta_h+hold_a*delta_h+purchase_a-sell_a
trans_new=bequest_a

print*, 'the new initial wealth and construction are :'
print*,trans_new,newconstruction_new
error=abs(newconstruction-newconstruction_new)/newconstruction+abs(trans_new-trans)/trans
print*,'error for the',iter,'iteration:',error

if (error<0.03d0 .or. iter>20) then
! we have found the equilibrium. we next calculate moments
! 1. average homeownership to determine omega and phi
construction_relay=newconstruction
trans_relay=trans
exit
end if

newconstruction=adjust*newconstruction_new+(1d0-adjust)*newconstruction
trans=adjust*trans_new+(1d0-adjust)*trans
iter=iter+1

end do
end

subroutine simulation
integer b,h,e,j,ep
integer ir,il
real(8) fai,b_new
!call linear(0d0, gridb, nb, il, ir, fai)

m=0d0
m(3,1,zero,agemin)=1d0
life_house=0d0
life_asset=0d0
life_cons=0d0
life_ownership=0d0
life_sell=0d0
bequest_a=0d0
renthouse_a=0d0
hold_a=0d0
life_seller=0d0
life_buyer=0d0
life_renter=0d0
purchase_a=0d0
sell_a=0d0
asset_a=0d0
do j=agemin,agemax
   do b=1,nb
      do h=nh,1,(-1)
         if (gridb(b)*(1d0+rm)<-lambdab*p*gridh(h)) THEN
          exit
         END IF
            do e=1,ne
             if (m(e,h,b,j)==0d0) cycle ! ???
              life_house(j)=life_house(j)+m(e,h,b,j)*gridh(h)
              hold_a=hold_a+m(e,h,b,j)*gridh(h)
              life_ownership(j)=life_ownership(j)+m(e,h,b,j)*judge(h>1)
            if (j==agemax) then
               if (h==1) then
                  asset_a=asset_a+m(e,h,b,j)*apr(e,b,j)
                  bequest_a=bequest_a+m(e,h,b,j)*apr(e,b,j)*(1d0+rb)
                  life_cons(j)=life_cons(j)+m(e,h,b,j)*cr(e,b,j)
                  renthouse_a=renthouse_a+m(e,h,b,j)*re(e,b,j)
               else ! even at the start of age 75, you can sell the house.
                 sell_a=sell_a+m(e,h,b,j)*gridh(h)
                  if (keep(e,h,b,j)==1) then
                  hold_a=hold_a+m(e,h,b,j)*gridh(h)
                  life_cons(j)=life_cons(j)+m(e,h,b,j)*cp(e,h,b,j)
                  bequest_a=bequest_a+m(e,h,b,j)*(app(e,h,b,j)*(1d0+rb)+(1d0-delta_h-kappa_h)*gridh(h)*p)
                  asset_a=asset_a+m(e,h,b,j)*app(e,h,b,j)
                  else
                     life_sell(j)=life_sell(j)+m(e,h,b,j)*gridh(h)
                     b_new=gridb(b)*(1d0+rm*judge(gridb(b)<0d0)+rb*judge(gridb(b).ge.0d0))+p*gridh(h)*(1d0-delta_h-kappa_h)
                     call linear(b_new, gridb, il, ir, fai)
                     if (fai>1d0) fai=1d0
                     if (fai<0d0) fai=0d0
                     m(e,1,il,j)=m(e,1,il,j)+m(e,h,b,j)*fai
                     m(e,1,ir,j)=m(e,1,ir,j)+m(e,h,b,j)*(1d0-fai)
                     m(e,h,b,j)=0d0 ! this is important!
                  end if
               end if
            end if
            if (j<agemax) then ! you have to calculate the next period
              if (h>1) then ! you have house
                if (keep(e,h,b,j)==1 ) then  ! you keep!
                   life_cons(j)=life_cons(j)+m(e,h,b,j)*cp(e,h,b,j)
                   life_asset(j+1)=life_asset(j+1)+m(e,h,b,j)*app(e,h,b,j)
                   ! determine where you go to in the next period.
                   call linear(app(e,h,b,j), gridb, il, ir, fai)
                   if (fai>1d0) fai=1d0
                   if (fai<0d0) fai=0d0
                    if (j<retire) then
                       do ep=1,ne
                          m(ep,h,il,j+1)=m(ep,h,il,j+1)+m(e,h,b,j)*markov(e,ep)*fai
                          m(ep,h,ir,j+1)=m(ep,h,ir,j+1)+m(e,h,b,j)*markov(e,ep)*(1d0-fai)
                      end do
                    else
                       m(e,h,il,j+1)=m(e,h,il,j+1)+m(e,h,b,j)*fai
                       m(e,h,ir,j+1)=m(e,h,ir,j+1)+m(e,h,b,j)*(1d0-fai)
                    end if
                else ! you sell the house...... you have to know the asset after you sell the house and pay back the debt.
                    sell_a=sell_a+m(e,h,b,j)*gridh(h)
                    b_new=gridb(b)*(1d0+rm*judge(gridb(b)<0d0+rb*judge(gridb(b)>0d0)))+p*gridh(h)*(1d0-delta_h-kappa_h)
                    call linear(b_new, gridb, il, ir, fai)
                    if (fai>1d0) fai=1d0
                    if (fai<0d0) fai=0d0
                    m(e,1,il,j)=m(e,1,il,j)+m(e,h,b,j)*fai
                    m(e,1,ir,j)=m(e,1,ir,j)+m(e,h,b,j)*(1d0-fai)
                    m(e,h,b,j)=0d0
                    ! it is very important to let h starting from nh.!!!
                end if
              else ! you dont have house!
                if (buy(e,b,j)==1 ) then  ! you buy!
                    life_cons(j)=life_cons(j)+m(e,1,b,j)*co(e,b,j)
                    life_asset(j+1)=life_asset(j+1)+m(e,1,b,j)*apo(e,b,j)
                    purchase_a=purchase_a+m(e,1,b,j)*gridh(pu(e,b,j))
                    call linear(apo(e,b,j), gridb, il, ir, fai)
                    if (fai>1d0) fai=1d0
                    if (fai<0d0) fai=0d0
                    if (j<retire) then
                        do ep=1,ne
                           m(ep,pu(e,b,j),il,j+1)=m(ep,pu(e,b,j),il,j+1)+m(e,h,b,j)*markov(e,ep)*fai
                           m(ep,pu(e,b,j),ir,j+1)=m(ep,pu(e,b,j),ir,j+1)+m(e,h,b,j)*markov(e,ep)*(1d0-fai)
                         end do
                    else
                       m(e,pu(e,b,j),il,j+1)=m(e,pu(e,b,j),il,j+1)+m(e,h,b,j)*fai
                       m(e,pu(e,b,j),ir,j+1)=m(e,pu(e,b,j),ir,j+1)+m(e,h,b,j)*(1d0-fai)
                   end if
                else ! you still dont buy any
                    life_cons(j)=life_cons(j)+m(e,1,b,j)*cr(e,b,j)
                    life_asset(j+1)=life_asset(j+1)+m(e,h,b,j)*apr(e,b,j)
                    renthouse_a=renthouse_a+m(e,1,b,j)*re(e,b,j)
                    call linear(apr(e,b,j), gridb, il, ir, fai)
                    if (fai>1d0) fai=1d0
                    if (fai<0d0) fai=0d0
                    if (j<retire) then
                        do ep=1,ne
                           m(ep,1,il,j+1)=m(ep,1,il,j+1)+m(e,h,b,j)*markov(e,ep)*fai
                           m(ep,1,ir,j+1)=m(ep,1,ir,j+1)+m(e,h,b,j)*markov(e,ep)*(1d0-fai)
                        end do
                    else
                       m(e,1,il,j+1)=m(e,1,il,j+1)+m(e,h,b,j)*fai
                       m(e,1,ir,j+1)=m(e,1,ir,j+1)+m(e,h,b,j)*(1d0-fai)
                    end if
                end if
              end if
            end if
         end do
        end do
     end do
 end do
 
hold_a=hold_a/( (agemax-agemin+1) )
bf_a=sum(life_bf)/(agemax-agemin+1)
asset_a=asset_a/(agemax-agemin+1)+(sum(life_asset))/(agemax-agemin+1)
purchase_a=purchase_a/( (agemax-agemin+1)           )
sell_a=sell_a/ (agemax-agemin+1)
renthouse_a=renthouse_a/(agemax-agemin+1)
bequest_a=bequest_a/((agemax-agemin+1) )
ownership=sum(life_ownership)/(agemax-agemin+1)

end


subroutine policy_function
! apolving for household problem using backward induction

real(8) time1,time2
integer j
vo=small
apo=small
pu=small_int ! index
co=small
cr=small
cp=small
apr=small
app=small
re=small
vh=small
vp=small
vn=small
vr=small
keep=small_int
buy=small_int

print*,'?'
!print*, 'calculating the policy function....'
call keeper_last ! get vp at j==75

 call renter_last ! get vr at j==75

 vn(:,:,agemax)=vr(:,:,agemax)

 !vh(:,:,:,agemax)=vp(:,:,:,agemax)
print*,'?'
 call seller(agemax) ! get vs at j==75
 
 call owner(agemax) ! get vh at j==75


do j=agemax-1,retire+1,(-1)
!print*,j
call renter_afterretire(j) ! apolve get vr

call purchaser_afterretire(j) ! get vo
!print*,'xxxx'
call nonowner(j)
call keeper_afterretire(j)  ! get vp
call seller(j) ! get vs
call owner(j) ! get vh
end do


do j=retire,agemin,(-1)

call cpu_time(time1)
 call renter_beforeretire(j)
 call purchaser_beforeretire(j)
 call nonowner(j)  ! vn
 call keeper_beforeretire(j)
 call seller(j) ! get vs
 call owner(j) ! get vh
 call cpu_time(time2)
!print*, 'it takes',(time2-time1)*0.25d0,'to apolve',j
end do

end

subroutine keeper_last
! owner for age==75
real(8) bpmax,bpmin,cons,temp,reapource
integer b,h,e,bp
real(8) gridbp(nbp)

bpmin=0d0

do b=1,nb
      do h=2,nh
         if (gridb(b)*(1+rm)<-lambdab*gridh(h)*p) cycle
             do e=1,ne
                reapource=gridb(b)*(1d0+rb*judge(b .ge. zero)+rm*judge(b<zero))+&
                replace*shouru(agemax,e)+trans-gridh(h)*delta_h*p
                bpmax=(reapource)-1d-10
                if (bpmin.ge.bpmax) cycle
                call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
                do bp=1,nbp
                   cons=reapource-gridbp(bp)
                   !print*, cons
                   temp=u(cons,gridh(h),omega_bar)+beta*bequestutility(gridbp(bp)*(1d0+rb)+(1d0-delta_h-kappa_h)*gridh(h)*p)
                   if (temp>vp(e,h,b,agemax)) then
                     vp(e,h,b,agemax)=temp
                     app(e,h,b,agemax)=gridbp(bp)
                     cp(e,h,b,agemax)=cons
                   end if
                end do
end do
end do
end do

end

subroutine renter_last
! non-owner for age 75
real(8) bpmax,cons,bpmin,reapource
integer b,e,bp,j,rent
real(8) temp,gridbp(nbp)

bpmin=0d0
do b=zero,nb
  do e=1,ne
     do rent=1,nrent
        reapource=gridb(b)*(1d0+rb)+replace*shouru(agemax,e)+trans-gridrent(rent)*d
        bpmax=reapource-1d-10
        if (bpmin.ge.bpmax) exit ! in the last age, I simply use a grid search??
        call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
        do bp=1,nbp
            cons=reapource-gridbp(bp)
            temp=u(cons,gridrent(rent),1d0)+beta*bequestutility(gridbp(bp)*(1d0+rb))
            if (temp>vr(e,b,agemax)) then
               vr(e,b,agemax)=temp
               apr(e,b,agemax)=gridbp(bp)
               cr(e,b,agemax)=cons
               re(e,b,agemax)=gridrent(rent)

            end if
    end do
end do
end do
end do

end


subroutine renter_afterretire(j)
! renter for age >60
real(8) cons
integer b,e,j
integer ir,il,bp,rent
real(8) fai,reapource, temp3,temp,bpmin,bpmax,gridbp(nbp)

bpmin=0d0


 do b=zero,nb
    do e=1,ne
        do rent=1,nrent
         reapource=gridb(b)*(1d0+rb)+replace*shouru(j,e)+trans-gridrent(rent)*d
         bpmax=reapource-1d-10
         if (bpmin.ge.bpmax) cycle
            call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
            do bp=1,nbp
               cons=reapource-gridbp(bp)
               temp3=0d0
               call linear(gridbp(bp), gridb, il, ir, fai)
               temp3=fai*vn(e,il,j+1)+(1d0-fai)*vn(e,ir,j+1)
               temp=u(cons,gridrent(rent),1d0)+beta*temp3
            ! finally you get the temp.now compare with vr(e,b)
               if (temp>vr(e,b,j)) then
                   vr(e,b,j)=temp
                   apr(e,b,j)=gridbp(bp)
                   re(e,b,j)=gridrent(rent)
                   cr(e,b,j)=cons
                  current_r(e,b,j)=u(cons,gridrent(rent),1d0)
                  next_r(e,b,j)=temp3
               end if
               end do

end do
end do
end do
end

subroutine renter_beforeretire(j)
! the problem for renter age no larger than 60
real(8) bpmax,bpmin
real(8) temp,temp2,temp3,cons,reapource
integer b,h,e,hp,bp,ep,j,rent
integer ir,il
real(8) fai,gridbp(nbp)

bpmin=0d0

!!$omp parallel do private(bp,rent,e,bpmin,bpmax,reapource,ep,gridbp,temp,temp2,temp3,cons,fai,il,ir)
 do b=zero,max_index(j)
  do e=1,ne
     do rent=1,nrent
                reapource=gridb(b)*(1d0+rb)+(1d0-tau_ss)*shouru(j,e)+trans-gridrent(rent)*d
                bpmax=reapource-1d-10
                if (bpmin.ge.bpmax) exit
                call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
                do bp=1,nbp
                    cons=reapource-gridbp(bp)
                    temp3=0d0
                    call linear(gridbp(bp), gridb, il, ir, fai)
                    if (j<60) then
                        do ep=1,ne
                            temp2=fai*vn(ep,il,j+1)+(1d0-fai)*vn(ep,ir,j+1)
                            temp3=temp3+markov(e,ep)* temp2
                        end do
                    else ! you are at 60
                        temp3=fai*vn(e,il,j+1)+(1d0-fai)*vn(e,ir,j+1)
                    end if
                    temp=u(cons,gridrent(rent),1d0)+beta*temp3
                    if (temp>vr(e,b,j)) then
                        vr(e,b,j)=temp
                        apr(e,b,j)=gridbp(bp)
                        re(e,b,j)=gridrent(rent)
                        cr(e,b,j)=cons
                    end if
            end do
        end do
  end do
  end do
 ! !$omp  end  parallel do
end

subroutine purchaser_afterretire(j)
! purchaser for age >60
real(8) cons
integer b,h,e,hp,j,bp
integer ir,il
real(8) fai,reapource,temp,temp3,bpmin,bpmax,gridbp(nbp)


!$omp parallel do private(hp,e,bpmin,bpmax,bp,reapource,gridbp,temp,temp3,cons,fai,il,ir)

   do b=zero,nb
          do e=1,ne
            do hp=2,nh ! gridh(1)=0d0
              bpmin=-(lambdab)*p*gridh(hp)/(1d0+rm)
              reapource=gridb(b)*(1d0+rb)+replace*shouru(j,e)-p*gridh(hp)*(1d0+kf)+trans
              bpmax=reapource-1d-10
              if (bpmin.ge.bpmax) exit ! it is safe to use exit here
              call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
              do bp=1,nbp
                 cons=reapource-gridbp(bp)
                 temp3=0d0
                call linear(gridbp(bp), gridb, il, ir, fai)
                 temp3=fai*vh(e,hp,il,j+1)+(1d0-fai)*vh(e,hp,ir,j+1)
                 temp=u(cons,gridh(hp),omega_bar)+beta*temp3
                 if (temp>vo(e,b,j)) then
                     vo(e,b,j)=temp
                     apo(e,b,j)=gridbp(bp)
                     pu(e,b,j)=hp
                     co(e,b,j)=cons
                     current_o(e,b,j)=u(cons,gridh(hp),omega_bar)
                     next_o(e,b,j)=temp3
                 end if
             end do
     end do
  ! if (vo(e,b,bf)<0d0) print*, 'for',shouru(j,e)*replace,gridb(b),gridbf(bf)/(agemax+1-j),j,'value',vo(e,b,bf),'pu=',pu(e,b,bf,j)
end do
end do

!$omp  end  parallel do
end

subroutine purchaser_beforeretire(j)
! purchaser for age <=60
real(8) cons,tmp,tmp2,tmp3,bpmin,bpmax,reapource,gridbp(nbp)
integer b,h,e,bp,hp,ep
integer ir,il,j
real(8) fai


!$omp parallel do private(bp,hp,e,bpmin,bpmax,reapource,ep,gridbp,tmp,tmp2,tmp3,cons,fai,il,ir)
  do b=zero,max_index(j)
     do e=1,ne
        do hp=2,nh ! gridh(1)=0d0. you choose the amount h', the amount of house you want to have next period.
           bpmin=-(lambdab)*p*gridh(hp)/(1d0+rm)
           reapource=gridb(b)*(1d0+rb)+(1d0-tau_ss)*shouru(j,e)-p*gridh(hp)*(1d0+kf)+trans
           bpmax=reapource-1d-10
              if (bpmin.ge.bpmax) exit
              call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
              do bp=1,nbp
                 cons=reapource-gridbp(bp)
                 tmp3=0d0
                 call linear(gridbp(bp), gridb, il, ir, fai)
                 if (j<retire) then
                     do ep=1,ne
                        tmp2=fai*vh(ep,hp,il,j+1)+(1d0-fai)*vh(ep,hp,ir,j+1)  ! the vh is equal to vh in the latex
                        tmp3=tmp3+markov(e,ep)* tmp2
                     end do
                 else
                     tmp3=fai*vh(e,hp,il,j+1)+(1d0-fai)*vh(e,hp,ir,j+1)
                 end if
                 tmp=u(cons,gridh(hp),omega_bar)+beta*tmp3
                 if (tmp>vo(e,b,j)) then
                     vo(e,b,j)=tmp
                     apo(e,b,j)=gridbp(bp)
                     pu(e,b,j)=hp
                     co(e,b,j)=cons
                 end if

  end do
 ! if (vo(e,b,bf)<0d0) print*, 'for',e,b,bf,j,'value',vo(e,b,bf),'pu=',pu(e,b,bf,j)
end do
end do
end do
 !$omp  end  parallel do
end

subroutine keeper_afterretire(j)
! payer for age >60
real(8) cons
integer b,h,e,bp,j
integer ir,il
real(8) fai,reapource,temp,temp3,bpmin,bpmax,gridbp(nbp)


!$omp parallel do private(h,e,bpmin,bpmax,reapource,gridbp,temp,temp3,cons,bp,fai,il,ir)

 do b=1,nb
  do h=nh,2,(-1) ! gridh(1)=0d0f
     if (gridb(b)*(1d0+rm)<-lambdab*gridh(h)*p) exit
     do e=1,ne
        bpmin=-lambdab*p*gridh(h)
        reapource=gridb(b)*(1d0+rb*judge(b .ge. zero)+rm*judge(b<zero))+replace*shouru(j,e)-delta_h*p*gridh(h)+trans
        bpmax=reapource-1d-10
        if (bpmin.ge.bpmax) cycle
        call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
        do bp=1,nbp
            cons=reapource-gridbp(bp)
            temp3=0d0
            call linear(gridbp(bp), gridb, il, ir, fai)
            temp3=fai*vh(e,h,il,j+1)+(1d0-fai)*vh(e,h,ir,j+1)
            temp=u(cons,gridh(h),omega_bar)+beta*temp3
            if (temp>vp(e,h,b,j)) then
            vp(e,h,b,j)=temp
            app(e,h,b,j)=gridbp(bp)
            cp(e,h,b,j)=cons
            current_p(e,h,b,j)=u(cons,gridh(h),omega_bar)
            next_p(e,h,b,j)=temp3
            end if
       end do
    end do
  end do
end do

!$omp  end  parallel do
end

subroutine keeper_beforeretire(j)
! payer for age <=60
real(8) bpmax,bpmin,temp,temp2,temp3,cons,reapource,gridbp(nbp)
integer b,h,e,m,bp,ep,j
integer ir,il
real(8) fai

!$omp parallel do private(bp,h,e,bpmin,bpmax,reapource,ep,gridbp,temp,temp2,temp3,cons,fai,il,ir)
   ! first case, you have no debt.
          do b=1,nb
            do h=nh,2,(-1)
              if (gridb(b)*(1d0+rm)<-lambdab*gridh(h)*p) exit
                do e=ne,1,(-1)
                 reapource=gridb(b)*(1d0+rb*judge(b .ge. zero)+rm*judge(b<zero))+(1d0-tau_ss)*shouru(j,e)-p*delta_h*gridh(h)+trans
                 bpmin=-(lambdab)*p*gridh(h)
                bpmax=reapource-1d-10
                      if (bpmin.ge.bpmax) exit
                      call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
                      do bp=1,nbp
                            cons=reapource-gridbp(bp)
                         temp3=0d0
                         call linear(gridbp(bp), gridb, il, ir, fai)
                         if (j<60) then
                            do ep=1,ne
                               temp2=fai*vh(ep,h,il,j+1)+(1d0-fai)*vh(ep,h,ir,j+1)
                               temp3=temp3+markov(e,ep)* temp2
                            end do
                         else
                         temp3=fai*vh(e,h,il,j+1)+(1d0-fai)*vh(e,h,ir,j+1)
                         end if
                         temp=u(cons,gridh(h),omega_bar)+beta*temp3
                         if (temp>vp(e,h,b,j)) then
                            vp(e,h,b,j)=temp
                            app(e,h,b,j)=gridbp(bp)
                            cp(e,h,b,j)=cons
                         end if

            end do
         end do
    end do

end do
!$omp  end  parallel do
end

subroutine nonowner(j)
real(8) x
integer b,h,e,j


  do b=zero,nb
   do e=1,ne
      if (vo(e,b,j)>vr(e,b,j)) then
        vn(e,b,j)=vo(e,b,j)
        if (j>retire) then
        current_n(e,b,j)=current_o(e,b,j)
        next_n(e,b,j)=next_o(e,b,j)
        cn(e,b,j)=co(e,b,j)
        pu_n(e,b,j)=gridh(pu(e,b,j))
        re_n(e,b,j)=0d0
        end if
        buy(e,b,j)=1
      else if (vo(e,b,j)<vr(e,b,j)) then
        vn(e,b,j)=vr(e,b,j)
        if (j>retire) then
         current_n(e,b,j)=current_r(e,b,j)
         next_n(e,b,j)=next_r(e,b,j)
         cn(e,b,j)=cr(e,b,j)
         pu_n(e,b,j)=0d0
         re_n(e,b,j)=re(e,b,j)
        end if
        buy(e,b,j)=0
      else
        call simulate_uniform(x)
        vn(e,b,j)=vo(e,b,j)*judge(x<0.5d0)+vr(e,b,j)*judge(x.ge.0.5d0)
        buy(e,b,j)=1*judge(x<0.5d0)+0*judge(x .ge. 0.5d0)
      end if
     ! if (vo(e,b,bf,j)<0d0 .and. buy(e,b,bf,j)==1) print*, vo(e,b,bf),vr(e,b,bf)
   end do
end do


end

subroutine  owner(j)
real(8) x
integer b,h,e,j

  do b=1,nb
      do h=nh,2,(-1) ! gridh(1)=0d0
        if (gridb(b)*(1d0+rm)<-lambdab*p*gridh(h)) exit
        do e=1,ne
          if (vp(e,h,b,j)>vs(e,h,b,j)) then
            vh(e,h,b,j)=vp(e,h,b,j)
            keep(e,h,b,j)=1
          else if (vp(e,h,b,j)<vs(e,h,b,j)) then
            vh(e,h,b,j)=vs(e,h,b,j)
            keep(e,h,b,j)=0
          else
            call simulate_uniform(x)
            vh(e,h,b,j)=vp(e,h,b,j)*judge(x<0.5d0)+vs(e,h,b,j)*judge(x.ge.0.5d0)
            keep(e,h,b,j)=1*judge(x<0.5d0)+0*judge(x .ge. 0.5d0)
          end if
        end do
    end do

end do
end

subroutine seller(j)
! the value for the house seller.
real(8) bnew
integer ir,il,b,h,j
real(8) fai

  do b=1,nb
      do h=nh,1,(-1) ! gridh(1)=0d0
         if ((1d0+rm)*gridb(b)<-lambdab*p*gridh(h)) exit
         bnew=gridb(b)*(1d0+rm*judge(gridb(b)<0d0+rb*judge(gridb(b)>0d0)))&
         +(1-delta_h-kappa_h)*p*gridh(h)
         call linear(bnew, gridb, il, ir, fai)
         vs(:,h,b,j)=vn(:,il,j)*fai+vn(:,ir,j)*(1d0-fai)
         if (j>retire) then
         current_s(:,h,b,j)=current_n(:,il,j)*fai+current_n(:,ir,j)*(1d0-fai)
         next_s(:,h,b,j)=next_n(:,il,j)*fai+next_n(:,ir,j)*(1d0-fai)
         end if
     end do
   end do

end



function u(c,h,omega)
real(8) c,h,rent,u,omega
    u=(1-phi)*c**(1-gama)+phi*(omega*h)**(1-gama)
    u=u**((1-sigma)/(1-gama))-1
    !u=(c**(1d0-phi))*((omega*h)**phi)
    !u=u**(1-sigma)-1
    u=u/(1-sigma)

    return
end

function bequestutility(b)
real(8) bequestutility,b
    bequestutility=(b+bconstant)**(1-sigma)-1
    bequestutility=bequestutility*psi/(1-sigma)
    return
end

subroutine maxsaving_gen
integer j,k
maxsaving(agemin)=0d0
max_index(agemin)=zero
do j=agemin+1,retire
   maxsaving(j)=maxsaving(j-1)*(1d0+rb)+shouru(j,5)*(1d0-tau_ss)+trans
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







function destiny(previous)
integer previous,destiny
real(8) saikoro,kakuritsu(ne)
call simulate_uniform(saikoro)
!print*, saikoro
kakuritsu=cumsum(markov(previous,:))
!print*, kakuritsu
if (saikoro.le.kakuritsu(1)) then
destiny=1
else if (kakuritsu(1)<saikoro .and. saikoro.le.kakuritsu(2)) then
destiny=2
else if (kakuritsu(2)<saikoro .and. saikoro.le.kakuritsu(3)) then
destiny=3
else if (kakuritsu(3)<saikoro .and. saikoro.le.kakuritsu(4)) then
destiny=4
else
destiny=5
end if
end


end program
