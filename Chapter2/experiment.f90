
!this is for the experiment of hpf.
!here, the mandatory saving fund accumulated till 60 is evenly distributed in the remaning ages


include 'parameter_experiment.f90'

program main
use global
use toolbox
implicit none
real(8) wsharetop1_hpf,wsharetop10_hpf,wsharetop20_hpf,wsharemid40_hpf,wsharebot50_hpf
real(8) wsharetop1,wsharetop10,wsharetop20,wsharemid40,wsharebot50
real(8) participate
real(8),allocatable:: wealth(:),wdist(:),wealth_order(:),wdist_order(:)
integer,allocatable:: ind(:)
integer k
growth=0.08d0

age(agemin)=21d0
do k=agemin+1,agemax
age(k)=age(k-1)+1d0
end do
! generate the grid and transition matrix for income.
call tauchen(markov,gride)

!print*, gride
call housegrid_gen
! make 0 on the grid of asset.

call totallabor_gen

call shouru_gen

call gridb_gen

call gridbf_gen

call experiment

Contains

subroutine totallabor_gen
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
totallabor=totallabor+m_y(j,e)*exp(period(j)+ gride(e))
  end do
end do
end

subroutine experiment
integer b,h,e,k,j,i,bf
real(8) bfshare, s,p1,p2


allocate(m_hpf(ne,nh,nb,nbf,agemin:agemax))
allocate(m(ne,nh,nb,agemin:agemax))
age(agemin)=21d0
do k=agemin+1,agemax
age(k)=age(k-1)+1d0
end do

participate=1d0 ! everyone is in hpf

call steadystate_outer

open(1, file='price_hpf.xls', status='replace')  
      write(1,*) rb,rm,p,d
close(1)

! you get m_hpf
open(1, file='measure_hpf.xls', status='replace')  
do j=agemin,agemax
 do bf=1,nbf
  do b=1,nb
    do h=1,nh
      do e=1,ne
      write(1,*) m_hpf(e,h,b,bf,j)
    end do
    end do
    end do
    end do
    end do
close(1)

participate=0d0

call steadystate_outer


open(1, file='price_nohpf.xls', status='replace')  
      write(1,*) rb,rm,p,d
close(1)

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

! you get m

end 



function bequestutility(b)
real(8) bequestutility,b
    bequestutility=(b+bconstant)**(1-sigma)-1
    bequestutility=bequestutility*psi/(1-sigma)
    return
end


subroutine steadystate_outer
integer iter
real(8) rbmin,rbmax,rb_new

trans_relay=0.9d0
construction_relay=0.1d0
if (participate>0d0) rh_relay=0.03d0

iter=1
rbmin=0.015d0
rbmax=0.045d0 !?
iter=1
rb=(rbmax+rbmin)*0.5d0
do
print*,'-----------------------------------'
print*,'outer loop ',iter

!rh=rb
rm=rb+premium
print*,'r is',rb

call steadystate_inner
print*,'asset demand',asset_a*75
!print*,'bound people',sum(m(:,:,nb,:))/(agemax-agemin+1)

if (asset_a*75>1d0) then
   rbmax=rb
else if (asset_a*75<-1d0) then
   rbmin=rb
else
exit
endif

rb_new=(rbmax+rbmin)*0.5d0

rb=rb*0.5d0+rb_new*0.5d0

if (iter>20) exit
iter=iter+1
end do

end


subroutine steadystate_inner
use toolbox
integer iter,i,j,b

real(8) newconstruction_new,trans_new,error,adjust,temp1,temp2,newconstruction,rh_new
real(8) holder,seller,ownership_35,house_expenditure,aggregate_expenditure
real(8) networth,annual_income,n_h
real(8) ownership,wealth_gini,wealthdist(people*(agemax-agemin+1)),networthratio
real(8) housedist(people*(agemax-agemin+1)),house_gini,aggregate_labor
real(8) rentersize,ownersize,renterincome,ownerincome,hnratio_dist(people*(agemax-agemin+1))
real(8) housevalue(nh+1),life_gini(agemin:agemax)
real(8) theta_new

trans=trans_relay
newconstruction=construction_relay
if (participate>0d0) rh=rh_relay
iter=1
adjust=0.4d0

do
print*,'****************************************************'
print*, 'inner loop ',iter
p=(newconstruction/land)**((1d0-alpha)/alpha)/alpha
d=fi+p-((1d0-delta_h)/(1d0+rb))*p
print*,'the initial initial wealth,constuction and hpf rate '
print*, trans,newconstruction,rh
print*, 'the price and rent'
print*,p,d

!bmax=sum((1d0-tau_ss-theta)*shouru(:,4))*0.8d0+trans*(agemax-agemin+1) ! calcualte the max.
!print*,'bmax is',bmax

!print*, gridb


call maxsaving_gen
!print*, max_index
if (participate>0d0) call maxhpf_gen

call policy_function
call simulation

!print*,renthouse_a,hold_a
!newconstruction_new=renthouse_a*delta_h+hold_a*delta_h+purchase_a-sell_a*(1d0-delta_h)
if (participate>0d0) rh_new=rb-subsidy/(bf_a*(agemax-agemin+1))  ! notice that bf_a is devided by total age, so we add it back
print*,'aggregate house selling and purchase',sell_a,purchase_a
newconstruction_new=renthouse_a*delta_h+hold_a*delta_h+purchase_a-sell_a
trans_new=bequest_a

print*, 'the new initial wealth,construction and hpf rate  are :'
print*,trans_new,newconstruction_new,rh_new
if (participate>0d0) error=abs(newconstruction-newconstruction_new)/newconstruction+abs(trans_new-trans)/trans+abs(rh_new-rh)/rh
if (participate==0d0) error=abs(newconstruction-newconstruction_new)/newconstruction+abs(trans_new-trans)/trans
print*,'error for the',iter,'iteration:',error

if (error<0.05d0 .or. iter>15) then
! we have found the equilibrium. we next calculate moments
! 1. average homeownership to determine omega and phi
trans_relay=trans
construction_relay=newconstruction
if (participate>0d0)  rh_relay=rh

exit
end if


newconstruction=adjust*newconstruction_new+(1d0-adjust)*newconstruction
trans=adjust*trans_new+(1d0-adjust)*trans
if (participate>0d0)  rh=adjust*rh_new+(1d0-adjust)*rh
iter=iter+1

! in the subroutine demand side, there is subroutine simulation, which allocates a lot of matrix. we need to deallocate them




end do
end




subroutine simulation_nohpf
integer b,h,e,j,ep
integer ir,il
real(8) fai,b_new,loss(agemin:agemax)
!call linear(0d0, gridb, nb, il, ir, fai)


do j=agemin,agemax
   do b=1,nb
      do h=nh,1,(-1)
         if (gridb(b)*(1d0+rm)<-lambdab*p*gridh(h)) THEN
          exit
         END IF
            do e=1,ne
             if (m(e,h,b,j)==0d0) cycle ! ???
               hdist(h)=hdist(h)+m(e,h,b,j)
               wealth50=wealth50+m(e,h,b,j)*(p*gridh(h)*(1d0-delta_h)+gridb(b))*judge(j.ge.40 .and. j.le. 60)
               wealth60=wealth60+m(e,h,b,j)*(p*gridh(h)*(1d0-delta_h)+gridb(b))*judge(j==50)
               wealth75=wealth75+m(e,h,b,j)*(p*gridh(h)*(1d0-delta_h)+gridb(b))*judge(j==75)
              networth=networth+m(e,h,b,j)*(p*gridh(h)*(1d0-delta_h)+gridb(b))
              totalincome=totalincome+((1d0-tau_ss-theta)*judge(j .le. retire)&
              +replace*judge(j>retire))*shouru(j,e)*m(e,h,b,j)+trans*m(e,h,b,j)
           
              life_house(j)=life_house(j)+m(e,h,b,j)*gridh(h)
              hold_a=hold_a+m(e,h,b,j)*gridh(h)
              life_ownership(j)=life_ownership(j)+m(e,h,b,j)*judge(h>1)
            if (b==nb) maxasset(j)=maxasset(j)+m(e,h,b,j)
            if (h==nh) maxhouse(j)=maxhouse(j)+m(e,h,b,j)
             ! life_asset(j)=life_asset(j)+m(e,h,b,j)*gridb(b)
            if (j==agemax) then
               if (h==1) then
               life_renter(j)=life_renter(j)+m(e,h,b,j)
                  life_rent(j)=life_rent(j)+m(e,h,b,j)*re(e,b,j)
                  rentpeople=rentpeople+m(e,h,b,j)
                  asset_a=asset_a+m(e,h,b,j)*apr(e,b,j)
                  bequest_a=bequest_a+m(e,h,b,j)*apr(e,b,j)*(1d0+rb)
                 ! life_asset(j+1)=life_asset(j+1)+m(e,h,b,j)*sr(e,b,j)
                  life_cons(j)=life_cons(j)+m(e,h,b,j)*cr(e,b,j)
                 ! asset_a=asset_a+m(e,h,b,j)*sr(e,b,j)
                  bequestpositive=bequestpositive+m(e,h,b,j)*judge(apr(e,b,j)>1d-10)
               else ! even at the start of age 75, you can sell the house.
                 ! print*, e,h,gridb(b),m(e,h,b,j),vp(e,h,b,j)-vs(e,h,b,j)
                 sell_a=sell_a+m(e,h,b,j)*gridh(h)
                  if (keep(e,h,b,j)==1) then
                  hold_a=hold_a+m(e,h,b,j)*gridh(h)
                     life_cons(j)=life_cons(j)+m(e,h,b,j)*cp(e,h,b,j)
                     bequest_a=bequest_a+m(e,h,b,j)*(app(e,h,b,j)*(1d0+rb)+(1d0-delta_h-kappa_h)*gridh(h)*p)
                     asset_a=asset_a+m(e,h,b,j)*app(e,h,b,j)
                     bequestpositive=bequestpositive+m(e,h,b,j)*judge(app(e,h,b,j)>1d-10)
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
              !  if (j>60 ) then
               !  print*, 'at',shouru(j,e),gridb(b),&
                !gridh(h),j,vp(e,h,b,j),vs(e,h,b,j)
                !end if

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
                    life_sell(j)=life_sell(j)+m(e,h,b,j)*gridh(h)
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
                    life_rent(j)=life_rent(j)+m(e,h,b,j)*re(e,b,j)
                    life_renter(j)=life_renter(j)+m(e,h,b,j)
                    renthouse_a=renthouse_a+m(e,1,b,j)*re(e,b,j)
                    renterincome=renterincome+shouru(j,e)*m(e,h,b,j)*(replace*judge(j>retire)+(1d0-tau_ss)*judge(j .le.retire))
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




end

subroutine simulation
integer ir,il,j
real(8) fai

life_house=0d0
life_asset=0d0
life_cons=0d0
life_house_hpf=0d0
life_asset_hpf=0d0
life_cons_hpf=0d0
bequest_a=0d0
renthouse_a=0d0
life_ownership=0d0
life_seller=0d0
life_buyer=0d0
life_renter=0d0
life_rent=0d0
life_ownership_hpf=0d0
life_seller_hpf=0d0
life_buyer_hpf=0d0
life_renter_hpf=0d0
life_rent_hpf=0d0
purchase_a=0d0
sell_a=0d0
life_bf=0d0
hold_a=0d0
!loss=0d0
!hpf_pay=0d0
subsidy=0d0
!life_sell=0d0
bequestpositive=0d0
asset_a=0d0
maxhouse=0d0
maxrent=0d0
maxasset=0d0
maxhouse_hpf=0d0
maxrent_hpf=0d0
maxasset_hpf=0d0

rentpeople=0d0
rentpeople_hpf=0d0
hpfloan=0d0
hpfbalance=0d0
totalloan=0d0
renterincome=0d0
hpfconstrained=0d0
hpfunconstrained=0d0

networth=0d0
wealth75=0d0
wealth50=0d0
wealth60=0d0
totalincome=0d0
hdist=0d0
bfdist=0d0




if (participate<1d0) then
m=0d0
m(3,1,zero,agemin)=(1d0-participate)

call simulation_nohpf

end if

if (participate>0d0) then

m_hpf=0d0
m_hpf(3,1,zero,1,agemin)=participate

call simulation_hpf

end if

hold_a=hold_a/( (agemax-agemin+1) )
if (participate>0d0) bf_a=sum(life_bf)/(agemax-agemin+1)
if (participate>0d0) asset_a=asset_a/(agemax-agemin+1)+(sum(life_asset)+sum(life_asset_hpf))/(agemax-agemin+1)+bf_a
if (participate==0d0) asset_a=asset_a/(agemax-agemin+1)+(sum(life_asset)+sum(life_asset_hpf))/(agemax-agemin+1)
purchase_a=purchase_a/( (agemax-agemin+1)           )
sell_a=sell_a/ (agemax-agemin+1)
renthouse_a=renthouse_a/(agemax-agemin+1)
bequest_a=bequest_a/((agemax-agemin+1) )
!ownership=(sum(life_ownership)+sum(life_ownership_hpf))/(agemax-agemin+1)



end 


subroutine simulation_hpf
integer b,bf,h,e,j,ep
integer ir,iu,id,il
real(8) failu,faild,fairu,faird,fai,b_new,loss(agemin:agemax)


do j=agemin,agemax
  do bf=1,nbf
   do b=1,nb
      do h=nh,1,(-1)
         if (gridb(b)*(1d0+rm)<-lambdab*p*gridh(h)) THEN
          exit
         END IF
            do e=1,ne
             if (m_hpf(e,h,b,bf,j)==0d0) cycle ! ???
            !  if (j>43) print*, e,h,b,bf,j
            hdist(h)=hdist(h)+m_hpf(e,h,b,bf,j)
               wealth50=wealth50+m_hpf(e,h,b,bf,j)*(p*gridh(h)*(1d0-delta_h)+gridb(b))*judge(j.ge.40 .and. j.le. 60)
               wealth60=wealth60+m_hpf(e,h,b,bf,j)*(p*gridh(h)*(1d0-delta_h)+gridb(b))*judge(j==50)
               wealth75=wealth75+m_hpf(e,h,b,bf,j)*(p*gridh(h)*(1d0-delta_h)+gridb(b))*judge(j==75)
              networth=networth+m_hpf(e,h,b,bf,j)*(p*gridh(h)*(1d0-delta_h)+gridb(b))
              totalincome=totalincome+((1d0-tau_ss-theta)*judge(j .le. retire)&
              +replace*judge(j>retire))*shouru(j,e)*m_hpf(e,h,b,bf,j)+trans*m_hpf(e,h,b,bf,j)
               if (j==retire+1) totalincome=totalincome+gridbf(bf)*m_hpf(e,h,b,bf,j)
              if (b==nb) maxasset_hpf(j)=maxasset_hpf(j)+m_hpf(e,h,b,bf,j)
              if (h==nh) maxhouse_hpf(j)=maxhouse_hpf(j)+m_hpf(e,h,b,bf,j)
              life_house_hpf(j)=life_house_hpf(j)+m_hpf(e,h,b,bf,j)*gridh(h)
              hold_a=hold_a+m_hpf(e,h,b,bf,j)*gridh(h)
              life_ownership_hpf(j)=life_ownership_hpf(j)+m_hpf(e,h,b,bf,j)*judge(h>1)
             !if  (gridb(b)<0d0 .and. j.le. retire) subsidy=subsidy+m(e,h,b,bf,j)*tfunc(gridbf(bf),gridb(b))
            if (j==agemax) then
               if (h==1) then
                  life_rent_hpf(j)=life_rent_hpf(j)+m_hpf(e,h,b,bf,j)*re_hpf(e,b,bf,j)
                  renterincome=renterincome+shouru(j,e)*replace*m_hpf(e,h,b,bf,j)
                  rentpeople_hpf(j)=rentpeople_hpf(j)+m_hpf(e,1,b,bf,j)
                  if (re_hpf(e,b,bf,j)==gridrent(nrent)) maxrent_hpf(j)=maxrent_hpf(j)+m_hpf(e,1,b,bf,j)
                  bequest_a=bequest_a+m_hpf(e,h,b,bf,j)*apr_hpf(e,b,bf,j)*(1d0+rb)
                  asset_a=asset_a+m_hpf(e,h,b,bf,j)*apr_hpf(e,b,bf,j)
                  life_cons_hpf(j)=life_cons_hpf(j)+m_hpf(e,h,b,bf,j)*cr_hpf(e,b,bf,j)
                  bequestpositive=bequestpositive+m_hpf(e,h,b,bf,j)*judge(apr_hpf(e,b,bf,j)>1d-10)
               else ! if you have house at the begining of 75, you will sell it anyway: if you keep, then you sell at the beginning of 76;if you do not keep, then you sell now
                  sell_a=sell_a+m_hpf(e,h,b,bf,j)*gridh(h)
                  
                  if (keep_hpf(e,h,b,bf,j)==1) then
                     hold_a=hold_a+m_hpf(e,h,b,bf,j)*gridh(h)
                     asset_a=asset_a+m_hpf(e,h,b,bf,j)*app_hpf(e,h,b,bf,j)
                     
                     bequest_a=bequest_a+m_hpf(e,h,b,bf,j)*(app_hpf(e,h,b,bf,j)*(1d0+rb)+(1d0-delta_h-kappa_h)*gridh(h)*p)
                     life_cons_hpf(j)=life_cons_hpf(j)+m_hpf(e,h,b,bf,j)*cp_hpf(e,h,b,bf,j)
                     bequestpositive=bequestpositive+m_hpf(e,h,b,bf,j)*judge(app_hpf(e,h,b,bf,j)>1d-10) !?
                  else
                     life_seller_hpf(j)=life_seller_hpf(j)+m_hpf(e,h,b,bf,j)
                     b_new=gridb(b)*(1d0+rb*judge(gridb(b)>0d0)+rm*judge(gridb(b).le.0d0))+p*gridh(h)*(1d0-delta_h-kappa_h)
                     
                     call linear(b_new, gridb, il, ir, fai)
                     if (fai>1d0) fai=1d0
                     if (fai<0d0) fai=0d0
                     m_hpf(e,1,il,bf,j)=m_hpf(e,1,il,bf,j)+m_hpf(e,h,b,bf,j)*fai
                     m_hpf(e,1,ir,bf,j)=m_hpf(e,1,ir,bf,j)+m_hpf(e,h,b,bf,j)*(1d0-fai)
                     m_hpf(e,h,b,bf,j)=0d0 ! this is important!
                  end if
               end if
            end if
            if (j<agemax) then ! you have to calculate the next period
              if (h>1) then ! you have house
                if (keep_hpf(e,h,b,bf,j)==1 ) then  ! you keep!
                   life_cons_hpf(j)=life_cons_hpf(j)+m_hpf(e,h,b,bf,j)*cp_hpf(e,h,b,bf,j)
                   life_asset_hpf(j+1)=life_asset_hpf(j+1)+m_hpf(e,h,b,bf,j)*app_hpf(e,h,b,bf,j)
                   !totalloan=totalloan+m(e,h,b,bf,j)*app(e,h,b,bf,j)*judge(app(e,h,b,bf,j)<0d0)*(-1d0)
                    life_bf(j+1)=life_bf(j+1)+m_hpf(e,h,b,bf,j)*bfnext_p(e,h,b,bf,j)
                   ! determine where you go to in the next period.
                   call bilinear(bfnext_p(e,h,b,bf,j),gridbf,nbf,app_hpf(e,h,b,bf,j),gridb,nb,&
                    il,ir,iu,id,failu,fairu,faild,faird)
                    if (j<retire) then
                       do ep=1,ne
                          m_hpf(ep,h,il,iu,j+1)=m_hpf(ep,h,il,iu,j+1)+m_hpf(e,h,b,bf,j)*markov(e,ep)*failu
                          m_hpf(ep,h,il,id,j+1)=m_hpf(ep,h,il,id,j+1)+m_hpf(e,h,b,bf,j)*markov(e,ep)*faild
                          m_hpf(ep,h,ir,iu,j+1)=m_hpf(ep,h,ir,iu,j+1)+m_hpf(e,h,b,bf,j)*markov(e,ep)*fairu
                          m_hpf(ep,h,ir,id,j+1)=m_hpf(ep,h,ir,id,j+1)+m_hpf(e,h,b,bf,j)*markov(e,ep)*faird
                       end do
                    else
                       m_hpf(e,h,il,iu,j+1)=m_hpf(e,h,il,iu,j+1)+m_hpf(e,h,b,bf,j)*failu
                       m_hpf(e,h,il,id,j+1)=m_hpf(e,h,il,id,j+1)+m_hpf(e,h,b,bf,j)*faild
                       m_hpf(e,h,ir,iu,j+1)=m_hpf(e,h,ir,iu,j+1)+m_hpf(e,h,b,bf,j)*fairu
                       m_hpf(e,h,ir,id,j+1)=m_hpf(e,h,ir,id,j+1)+m_hpf(e,h,b,bf,j)*faird
                    end if
                else ! you sell the house...... you have to know the asset after you sell the house and pay back the debt.
                   sell_a=sell_a+m_hpf(e,h,b,bf,j)*gridh(h)
                   life_seller_hpf(j)=life_seller_hpf(j)+m_hpf(e,h,b,bf,j)
                    b_new=gridb(b)*(1d0+rm*judge(gridb(b)<0d0)+rb*judge(gridb(b).ge.0d0))+p*gridh(h)*(1d0-delta_h-kappa_h)
                    call linear(b_new, gridb, il, ir, fai)
                    if (fai>1d0) fai=1d0
                    if (fai<0d0) fai=0d0
                    m_hpf(e,1,il,bf,j)=m_hpf(e,1,il,bf,j)+m_hpf(e,h,b,bf,j)*fai
                    m_hpf(e,1,ir,bf,j)=m_hpf(e,1,ir,bf,j)+m_hpf(e,h,b,bf,j)*(1d0-fai)
                    m_hpf(e,h,b,bf,j)=0d0 ! this is important!
                    ! it is very important to let h starting from nh.!!!
                end if
              else ! you dont have house!
          
              ! print*, j,gridb(b)*(1d0+rb),shouru(j,e)*(1d0-theta-tau_ss),trans,d*gridrent(1)
                if (buy_hpf(e,b,bf,j)==1 ) then  ! you buy!
                    bfdist(bf)=bfdist(bf)+m_hpf(e,h,b,bf,j)
                    hpfloan=hpfloan+m_hpf(e,h,b,bf,j)*tfunc(p,gridbf(bf),apo_hpf(e,b,bf,j))/rategap
                    totalloan=totalloan+m_hpf(e,h,b,bf,j)*apo_hpf(e,b,bf,j)*judge(apo_hpf(e,b,bf,j)<0d0)*(-1d0)
                    life_buyer_hpf(j)=life_buyer_hpf(j)+m_hpf(e,1,b,bf,j)
                    life_cons_hpf(j)=life_cons_hpf(j)+m_hpf(e,1,b,bf,j)*co_hpf(e,b,bf,j)
                    life_asset_hpf(j+1)=life_asset_hpf(j+1)+m_hpf(e,h,b,bf,j)*apo_hpf(e,b,bf,j)
                    life_bf(j+1)=life_bf(j+1)+m_hpf(e,h,b,bf,j)*bfnext_o(e,b,bf,j)
                    if(apo_hpf(e,b,bf,j)<0d0 .and. j.le. retire) &
                    subsidy=subsidy+m_hpf(e,h,b,bf,j)*1/(1+rb)*tfunc(p,gridbf(bf),apo_hpf(e,b,bf,j))
                    purchase_a=purchase_a+m_hpf(e,1,b,bf,j)*gridh(pu_hpf(e,b,bf,j))
                   ! if (e==5 .and. h==1 .and. b==25 .and. bf==25 .and. j==45) print*,'interpolation'
                    call bilinear(bfnext_o(e,b,bf,j),gridbf,nbf,apo_hpf(e,b,bf,j),gridb,nb,&
                    il,ir,iu,id,failu,fairu,faild,faird)
                   !  if (e==5 .and. h==1 .and. b==25 .and. bf==25 .and. j==45) print*,'?'
                    if (j<retire) then
                        do ep=1,ne
                           m_hpf(ep,pu_hpf(e,b,bf,j),il,iu,j+1)=m_hpf(ep,pu_hpf(e,b,bf,j),il,iu,j+1)&
                           +m_hpf(e,h,b,bf,j)*markov(e,ep)*failu
                           m_hpf(ep,pu_hpf(e,b,bf,j),il,id,j+1)=m_hpf(ep,pu_hpf(e,b,bf,j),il,id,j+1)+&
                           m_hpf(e,h,b,bf,j)*markov(e,ep)*faild
                           m_hpf(ep,pu_hpf(e,b,bf,j),ir,iu,j+1)=m_hpf(ep,pu_hpf(e,b,bf,j),ir,iu,j+1)&
                           +m_hpf(e,h,b,bf,j)*markov(e,ep)*fairu
                           m_hpf(ep,pu_hpf(e,b,bf,j),ir,id,j+1)=m_hpf(ep,pu_hpf(e,b,bf,j),ir,id,j+1)&
                           +m_hpf(e,h,b,bf,j)*markov(e,ep)*faird
                        end do
                    else
                       m_hpf(e,pu_hpf(e,b,bf,j),il,iu,j+1)=m_hpf(e,pu_hpf(e,b,bf,j),il,iu,j+1)+m_hpf(e,h,b,bf,j)*failu
                       m_hpf(e,pu_hpf(e,b,bf,j),il,id,j+1)=m_hpf(e,pu_hpf(e,b,bf,j),il,id,j+1)+m_hpf(e,h,b,bf,j)*faild
                       m_hpf(e,pu_hpf(e,b,bf,j),ir,iu,j+1)=m_hpf(e,pu_hpf(e,b,bf,j),ir,iu,j+1)+m_hpf(e,h,b,bf,j)*fairu
                       m_hpf(e,pu_hpf(e,b,bf,j),ir,id,j+1)=m_hpf(e,pu_hpf(e,b,bf,j),ir,id,j+1)+m_hpf(e,h,b,bf,j)*faird
                    end if  
                   ! if (e==5 .and. h==1 .and. b==25 .and. bf==25 .and. j==45) print*,'??'
                    
                else ! you still dont buy any
                    renterincome=renterincome+shouru(j,e)*m_hpf(e,h,b,bf,j)&
                    *(replace*judge(j>retire)+(1d0-tau_ss)*judge(j .le.retire))
                    life_renter_hpf(j)=life_renter_hpf(j)+m_hpf(e,1,b,bf,j)
                    life_rent_hpf(j)=life_rent_hpf(j)+m_hpf(e,1,b,bf,j)*re_hpf(e,b,bf,j)
                    rentpeople_hpf(j)=rentpeople_hpf(j)+m_hpf(e,1,b,bf,j)
                    if (re_hpf(e,b,bf,j)==gridrent(nrent)) maxrent(j)=maxrent(j)+m_hpf(e,1,b,bf,j)
                    life_cons_hpf(j)=life_cons_hpf(j)+m_hpf(e,1,b,bf,j)*cr_hpf(e,b,bf,j)
                    renthouse_a=renthouse_a+m_hpf(e,1,b,bf,j)*re_hpf(e,b,bf,j)
                    life_asset_hpf(j+1)=life_asset_hpf(j+1)+m_hpf(e,h,b,bf,j)*apr_hpf(e,b,bf,j)
                    life_bf(j+1)=life_bf(j+1)+m_hpf(e,h,b,bf,j)*bfnext_r(e,b,bf,j)
                    call bilinear(bfnext_r(e,b,bf,j),gridbf,nbf,apr_hpf(e,b,bf,j),gridb,nb,&
                    il,ir,iu,id,failu,fairu,faild,faird)
                    if (j<retire) then
                        do ep=1,ne
                           m_hpf(ep,1,il,iu,j+1)=m_hpf(ep,1,il,iu,j+1)+m_hpf(e,h,b,bf,j)*markov(e,ep)*failu
                           m_hpf(ep,1,il,id,j+1)=m_hpf(ep,1,il,id,j+1)+m_hpf(e,h,b,bf,j)*markov(e,ep)*faild
                           m_hpf(ep,1,ir,iu,j+1)=m_hpf(ep,1,ir,iu,j+1)+m_hpf(e,h,b,bf,j)*markov(e,ep)*fairu
                           m_hpf(ep,1,ir,id,j+1)=m_hpf(ep,1,ir,id,j+1)+m_hpf(e,h,b,bf,j)*markov(e,ep)*faird
                        end do
                    else
                       m_hpf(e,1,il,iu,j+1)=m_hpf(e,1,il,iu,j+1)+m_hpf(e,h,b,bf,j)*failu
                       m_hpf(e,1,il,id,j+1)=m_hpf(e,1,il,id,j+1)+m_hpf(e,h,b,bf,j)*faild
                       m_hpf(e,1,ir,iu,j+1)=m_hpf(e,1,ir,iu,j+1)+m_hpf(e,h,b,bf,j)*fairu
                       m_hpf(e,1,ir,id,j+1)=m_hpf(e,1,ir,id,j+1)+m_hpf(e,h,b,bf,j)*faird
                    end if
                end if
              end if
            end if

         end do
        end do
     end do
    end do
  !  print*, 'life house is ',life_house(j)
  !  print*,'the loss of household after this age is',loss(j)
 ! maxrent(j)=maxrent(j)/rentpeople(j)
 ! if (life_buyer(j)>0d0 .and. j<=retire)hpfconstrained(j)=hpfconstrained(j)/life_buyer(j)
 ! if (life_buyer(j)>0d0 .and. j<=retire) hpfunconstrained(j)=hpfunconstrained(j)/life_buyer(j)
 
 end do



end




subroutine policy_function
! solving for household problem using backward induction

real(8) t1,t2,timecost
integer j
!print*, 'calculating the policy function....'
bfnext_o=small
bfnext_r=small
bfnext_p=small
co=small
!cn=small
cp=small
cr=small
pu=small_int
apo=small
app=small
!aps=small
apr=small
!apn=small
re=small
keep=small_int
buy=small_int
vr=small
vo=small
vn=small
vh=small
vp=small
vs=small

co_hpf=small
cn_hpf=small
cp_hpf=small
cr_hpf=small
pu_hpf=small_int
apo_hpf=small
app_hpf=small
aps_hpf=small
apr_hpf=small
apn_hpf=small
re_hpf=small
keep_hpf=small_int
buy_hpf=small_int
vr_hpf=small
vo_hpf=small
vn_hpf=small
vh_hpf=small
vp_hpf=small
vs_hpf=small
!print*,'ownerlast'

if (participate<1d0) then

call keeper_last ! get vp at j==75
call renter_last
vn(:,:,agemax)=vr(:,:,agemax)
call seller(agemax) ! get vs at j==75
call owner(agemax) ! get vh at j==75

do j=agemax-1,retire+1,(-1)

call renter_afterretire(j) ! solve get vr
call purchaser_afterretire(j) ! get vo
call nonowner(j)
call keeper_afterretire(j)  ! get vp
call seller(j) ! get vs
call owner(j)

end do

do j=retire,agemin,(-1)


call renter_beforeretire(j) ! solve get vr
call purchaser_beforeretire(j) ! get vo
call nonowner(j)
call keeper_beforeretire(j)  ! get vp
call seller(j) ! get vs
call owner(j)

end do

end if



if (participate>0d0) then



call keeper_last_hpf
call renter_last_hpf
vn_hpf(:,:,:,agemax)=vr_hpf(:,:,:,agemax)
call seller_hpf(agemax)
call owner_hpf(agemax)


do j=agemax-1,retire+1,(-1)

call renter_afterretire_hpf(j) ! solve get vr
call purchaser_afterretire_hpf(j) ! get vo
call nonowner_hpf(j)
call keeper_afterretire_hpf(j)  ! get vp
call seller_hpf(j) ! get vs
call owner_hpf(j)

end do

do j=retire,agemin,(-1)



call renter_beforeretire_hpf(j) ! solve get vr
call purchaser_beforeretire_hpf(j) ! get vo
call nonowner_hpf(j)
call keeper_beforeretire_hpf(j)  ! get vp
call seller_hpf(j) ! get vs
call owner_hpf(j)

end do

end if

end


subroutine keeper_last_hpf
real(8) bpmax,bpmin,cons,temp,resource
integer b,bf,h,e,bp
integer ir,iu,id,il
real(8) failu,faild,fairu,faird,gridbp(nbp)

bpmin=0d0
do bf=1,1
do b=zero,nb
      do h=1,nh
         if (gridb(b)*(1d0+rm)<-lambdab*gridh(h)*p) cycle
             do e=1,ne
                !rh=rb
                resource=gridb(b)*(1d0+rb*judge(b .ge.zero)+rm*judge(b<zero))+&
                replace*shouru(agemax,e)+trans-gridh(h)*delta_h*p
                !replace*shouru(agemax,e)+annuity(gridbf(bf))+trans+(1-delta_h-kappa_h)*p*gridh(h)
                bpmax=(resource)-1d-10
                if (bpmin.ge.bpmax) cycle
                call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
                do bp=1,nbp
                   cons=resource-gridbp(bp)
                   !print*, cons
                   temp=u(cons,gridh(h),omega_bar)+beta*bequestutility(gridbp(bp)*(1d0+rb)+(1-delta_h-kappa_h)*p*gridh(h))
                   if (temp>vp_hpf(e,h,b,bf,agemax)) then
                     vp_hpf(e,h,b,bf,agemax)=temp
                     app_hpf(e,h,b,bf,agemax)=gridbp(bp)
                     cp_hpf(e,h,b,bf,agemax)=cons
                   end if
                end do
end do
end do
end do
end do



end

subroutine renter_last_hpf
real(8) bpmax,cons,bpmin,resource
integer b,bf,e,bp
integer ir,iu,id,il,rent
real(8) failu,faild,fairu,faird,temp,gridbp(nbp)

bpmin=0d0
do bf=1,1
do b=zero,nb
  do e=1,ne
    do rent=1,nrent
        !resource=gridb(b)*(1d0+rb)+replace*shouru(agemax,e)+annuity(gridbf(bf))+trans-gridrent(rent)*d
        resource=gridb(b)*(1d0+rb*judge(b .ge.zero)+rm*judge(b<zero))+replace*shouru(agemax,e)+trans-gridrent(rent)*d
        bpmax=resource-1d-10
        if (bpmin.ge.bpmax) exit ! in the last age, I simply use a grid search??
        call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
        do bp=1,nbp
            cons=resource-gridbp(bp)
            temp=u(cons,gridrent(rent),1d0)+beta*bequestutility(gridbp(bp)*(1d0+rb))
            if (temp>vr_hpf(e,b,bf,agemax)) then
               vr_hpf(e,b,bf,agemax)=temp
               apr_hpf(e,b,bf,agemax)=gridbp(bp)
               cr_hpf(e,b,bf,agemax)=cons
               re_hpf(e,b,bf,agemax)=gridrent(rent)
            end if
    end do
    end do
end do
end do
end do
end

subroutine renter_afterretire_hpf(j)
real(8) cons
integer b,bf,e,j
integer ir,il,bp,rent
real(8) fai,resource, temp3,temp,bpmin,bpmax,gridbp(nbp)

bpmin=0d0
do bf=1,nbf*judge(j==61)+1*judge(j>61)
 
 bfnext_r(:,:,bf,j)=0d0
 do b=zero,nb

    do e=1,ne

      do rent=1,nrent
         !resource=gridb(b)*(1d0+rb)+replace*shouru(j,e)+annuity(gridbf(bf))+trans-gridrent(rent)*d
         resource=gridb(b)*(1d0+rb)+replace*shouru(j,e)+gridbf(bf)*judge(j==61)+trans-gridrent(rent)*d
         bpmax=resource-1d-10

         if (bpmin.ge.bpmax) exit
            call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
            do bp=1,nbp
               cons=resource-gridbp(bp)
               temp3=0d0
               call linear(gridbp(bp),gridb,il,ir,fai)
               temp3=fai*vn_hpf(e,il,bf,j+1)+(1d0-fai)*vn_hpf(e,ir,bf,j+1)
               temp=u(cons,gridrent(rent),1d0)+beta*temp3
            ! finally you get the temp.now compare with vr(e,b)
               if (temp>vr_hpf(e,b,bf,j)) then
                   vr_hpf(e,b,bf,j)=temp
                   apr_hpf(e,b,bf,j)=gridbp(bp)
                   re_hpf(e,b,bf,j)=gridrent(rent)
                   cr_hpf(e,b,bf,j)=cons
               end if
         end do
         end do
        ! print*,'the rent for',e,b,bf,j,'is',re(e,b,bf,j)
end do
end do
end do
end

subroutine renter_beforeretire_hpf(j)
real(8) bpmax,bpmin
real(8) temp,temp2,temp3,cons,resource
integer b,bf,h,e,hp,bp,ep,j,rent
integer ir,iu,id,il
real(8) failu,faild,fairu,faird,gridbp(nbp)

bpmin=0d0
do bf=1,max_index_hpf(j)
 do b=zero,nb 
  do e=1,ne
     bfnext_r(e,b,bf,j)=(gridbf(bf)*(1+rh)+theta*shouru(j,e))
     do rent=1,nrent
    ! choose  rent and saving
                resource=gridb(b)*(1d0+rb)+(1d0-theta-tau_ss)*shouru(j,e)+trans-gridrent(rent)*d
                bpmax=resource-1d-10
                if (bpmin.ge.bpmax) exit
                call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
                do bp=1,nbp
                    cons=resource-gridbp(bp)
                    temp3=0d0
                    call bilinear(bfnext_r(e,b,bf,j),gridbf,nbf,gridbp(bp),gridb,nb,&
                    il,ir,iu,id,failu,fairu,faild,faird)
                    if (j<60) then
                        do ep=1,ne
                            temp2=failu*vn_hpf(ep,il,iu,j+1)+fairu*vn_hpf(ep,ir,iu,j+1)
                            temp2=temp2+faild*vn_hpf(ep,il,id,j+1)+faird*vn_hpf(ep,ir,id,j+1)
                            temp3=temp3+markov(e,ep)* temp2
                        end do
                    else ! you are at 60
                        temp3=failu*vn_hpf(e,il,iu,j+1)+fairu*vn_hpf(e,ir,iu,j+1)
                        temp3=temp3+faild*vn_hpf(e,il,id,j+1)+faird*vn_hpf(e,ir,id,j+1)
                    end if
                    temp=u(cons,gridrent(rent),1d0)+beta*temp3
                    if (temp>vr_hpf(e,b,bf,j)) then
                        vr_hpf(e,b,bf,j)=temp
                        apr_hpf(e,b,bf,j)=gridbp(bp)
                        re_hpf(e,b,bf,j)=gridrent(rent)
                        cr_hpf(e,b,bf,j)=cons
                    end if
            end do
            end do
          ! if (bf>20 .and. b>30) print*,'the rent for',e,b,bf,j,'is',re(e,b,bf,j)
        end do
    end do
  end do
end

subroutine purchaser_afterretire_hpf(j)
real(8) cons
integer b,bf,h,e,hp,j,bp
integer ir,iu,id,il
real(8) fai,resource,temp,temp3,bpmin,bpmax,gridbp(nbp),lasttmp


!$omp parallel do private(lasttmp,b,hp,e,bpmin,bpmax,bp,resource,gridbp,temp,temp3,cons,fai,il,ir)
do bf=1,nbf*judge(j==61)+1*judge(j>61)
  bfnext_o(:,:,bf,j)=0d0
   do b=zero,nb
          do e=1,ne

            do hp=2,nh ! gridh(1)=0d0
              lasttmp=small
              bpmin=-(lambdab)*p*gridh(hp)/(1d0+rm)
              !resource=gridb(b)*(1d0+rb)+replace*shouru(j,e)-p*gridh(hp)*(1d0+kf)+annuity(gridbf(bf))*judge(j==61)+trans
              resource=gridb(b)*(1d0+rb)+replace*shouru(j,e)-p*gridh(hp)*(1d0+kf)+gridbf(bf)*judge(j==61)+trans

              bpmax=resource-1d-10
              if (bpmin.ge.bpmax) exit
              call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
              do bp=1,nbp
                 cons=resource-gridbp(bp)
                 temp3=0d0
                 call linear(gridbp(bp),gridb,il,ir,fai)
                 temp3=fai*vh_hpf(e,hp,il,1,j+1)+(1d0-fai)*vh_hpf(e,hp,ir,1,j+1)
                 temp=u(cons,gridh(hp),omega_bar)+beta*temp3
                 if (temp>vo_hpf(e,b,bf,j)) then
                     vo_hpf(e,b,bf,j)=temp
                     apo_hpf(e,b,bf,j)=gridbp(bp)
                     pu_hpf(e,b,bf,j)=hp
                     co_hpf(e,b,bf,j)=cons
                 end if
                 if (temp<lasttmp) exit
                 lasttmp=temp
             end do
     end do
   !  print*, 'for',shouru(j,e),gridb(b),gridbf(bf),j,'saving is',sr(e,b,bf,j)
end do
end do
end do
!$omp  end  parallel do
end

subroutine purchaser_beforeretire_hpf(j)
real(8) tmp,tmp2,tmp3,bpmin,bpmax,resource,gridbp(nbp)
integer b,bf,h,e,m,bp,hp,ep
integer ir,iu,id,il,j,sign
real(8) failu,faild,fairu,faird,lasttmp,cons
!$omp parallel do private(bp,b,hp,e,bpmin,bpmax,resource,ep,gridbp,cons,tmp,tmp2,tmp3,failu,faild,fairu,faird,il,ir,iu,id)
do bf=1,max_index_hpf(j) ! when you are young, your hpf account is impossible to be some high values in the gridbf point. so we skip these points
  do b=zero,nb  ! use nb here. the reason is, when you buy a house, you get subsidy from outside. this makes the calculation of maxsaving difficult
     do e=1,ne
        bfnext_o(e,b,bf,j)=((1d0+rh)*gridbf(bf)+theta*shouru(j,e))
        do hp=2,nh ! gridh(1)=0d0
           bpmin=-(lambdab)*p*gridh(hp)/(1d0+rm)
           ! first you choose amount you want to withdraw from the account
             resource=gridb(b)*(1d0+rb)+(1d0-theta-tau_ss)*shouru(j,e)-p*gridh(hp)*(1d0+kf)+trans

             bpmax=resource-1d-10
              if (bpmin.ge.bpmax) cycle
              call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
              do bp=1,nbp
                 cons=resource-gridbp(bp)+1/(1+rb)*tfunc(p,gridbf(bf),gridbp(bp))
                 tmp3=0d0
                 call bilinear(bfnext_o(e,b,bf,j),gridbf,nbf,gridbp(bp),gridb,nb,&
                 il,ir,iu,id,failu,fairu,faild,faird)
                 if (j<retire) then
                     do ep=1,ne
                        tmp2=failu*vh_hpf(ep,hp,il,iu,j+1)+fairu*vh_hpf(ep,hp,ir,iu,j+1)
                        tmp2=tmp2+faild*vh_hpf(ep,hp,il,id,j+1)+faird*vh_hpf(ep,hp,ir,id,j+1)
                        tmp3=tmp3+markov(e,ep)* tmp2
                     end do
                 else
                     tmp3=failu*vh_hpf(e,hp,il,iu,j+1)+fairu*vh_hpf(e,hp,ir,iu,j+1)
                     tmp3=tmp3+faild*vh_hpf(e,hp,il,id,j+1)+faird*vh_hpf(e,hp,ir,id,j+1)
                 end if
                 tmp=u(cons,gridh(hp),omega_bar)+beta*tmp3
                ! if (e==2 .and. b==15 .and. bf==2 ) print*,hp,m,bp,tmp
                 if (tmp>vo_hpf(e,b,bf,j)) then
                     vo_hpf(e,b,bf,j)=tmp
                     apo_hpf(e,b,bf,j)=gridbp(bp)
                     pu_hpf(e,b,bf,j)=hp
                     co_hpf(e,b,bf,j)=cons
                 end if
             end do
    end do
  end do
end do
end do

 !$omp  end  parallel do
end

subroutine keeper_afterretire_hpf(j)
real(8) cons
integer b,bf,h,e,bp,j
integer ir,il
real(8) fai,resource,temp,temp3,bpmin,bpmax,gridbp(nbp)


!$omp parallel do private(b,h,e,bpmin,bpmax,resource,gridbp,temp,temp3,cons,bp,fai,il,ir)
do bf=1,nbf*judge(j==61)+1*judge(j>61)
  bfnext_p(:,:,:,bf,j)=0d0
 do b=1,nb
  do h=nh,2,(-1) ! gridh(1)=0d0
     if (gridb(b)*(1d0+rm)<-lambdab*gridh(h)*p) exit

     do e=1,ne

        bpmin=-lambdab*p*gridh(h)/(1d0+rm)
        !resource=gridb(b)*(1d0+rb*judge(b .ge.zero)+rm*judge(b<zero))+&
        !replace*shouru(j,e)-delta_h*p*gridh(h)+annuity(gridbf(bf))+trans
        resource=gridb(b)*(1d0+rb*judge(b .ge.zero)+rm*judge(b<zero))+&
        replace*shouru(j,e)-delta_h*p*gridh(h)+gridbf(bf)*judge(j==61)+trans
        bpmax=resource-1d-10
        if (bpmin.ge.bpmax) cycle
        call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
        do bp=1,nbp
            cons=resource-gridbp(bp)
            temp3=0d0
            call linear(gridbp(bp),gridb,il,ir,fai)
            temp3=fai*vh_hpf(e,h,il,1,j+1)+(1d0-fai)*vh_hpf(e,h,ir,1,j+1)
            temp=u(cons,gridh(h),omega_bar)+beta*temp3
            if (temp>vp_hpf(e,h,b,bf,j)) then
            vp_hpf(e,h,b,bf,j)=temp
            app_hpf(e,h,b,bf,j)=gridbp(bp)
            cp_hpf(e,h,b,bf,j)=cons
            end if
       end do
    end do
  end do
end do
end do
!$omp  end  parallel do
end



subroutine keeper_beforeretire_hpf(j)
real(8) bpmax,bpmin,temp,temp2,temp3,cons,resource,gridbp(nbp)
integer b,bf,h,e,m,bp,ep,j
integer ir,iu,id,il
real(8) failu,faild,fairu,faird,gridm(nm)


!$omp parallel do private(bp,b,h,e,bpmin,bpmax,resource,ep,gridbp,temp,temp2,temp3,cons,failu,faild,fairu,faird,il,ir,iu,id,gridm,m)
do bf=1,max_index_hpf(j)
    do e=ne,1,(-1)
   ! first case, you have no debt.
          do b=1,nb

            do h=nh,2,(-1)
              if (gridb(b)*(1d0+rm)<-lambdab*gridh(h)*p) exit
                bpmin=-(lambdab)*p*gridh(h)/(1d0+rm)
                 ! OPtimization. choose the amount of pay back using hpf
                 if (gridb(b) .ge. 0d0) then

                 resource=gridb(b)*(1d0+rb*judge(b .ge.zero)+rm*judge(b<zero))&
                 +(1d0-theta-tau_ss)*shouru(j,e)-delta_h*p*gridh(h)+trans
                 bfnext_p(e,h,b,bf,j)=(gridbf(bf)*(1d0+rh)+theta*shouru(j,e))
                bpmax=resource-1d-10
                   if (bpmin.ge.bpmax) cycle
                   call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
                   do bp=1,nbp
                      cons=resource-gridbp(bp)
                      temp3=0d0
                      call bilinear(bfnext_p(e,h,b,bf,j),gridbf,nbf,gridbp(bp),gridb,nb,&
                      il,ir,iu,id,failu,fairu,faild,faird)
                      if (j<60) then
                         do ep=1,ne
                            temp2=failu*vh_hpf(ep,h,il,iu,j+1)+fairu*vh_hpf(ep,h,ir,iu,j+1)
                            temp2=temp2+faild*vh_hpf(ep,h,il,id,j+1)+faird*vh_hpf(ep,h,ir,id,j+1)
                            temp3=temp3+markov(e,ep)* temp2
                         end do
                      else
                         temp3=failu*vh_hpf(e,h,il,iu,j+1)+fairu*vh_hpf(e,h,ir,iu,j+1)
                         temp3=temp3+faild*vh_hpf(e,h,il,id,j+1)+faird*vh_hpf(e,h,ir,id,j+1)
                      end if
                      temp=u(cons,gridh(h),omega_bar)+beta*temp3
                      if (temp>vp_hpf(e,h,b,bf,j)) then
                         vp_hpf(e,h,b,bf,j)=temp
                         app_hpf(e,h,b,bf,j)=gridbp(bp)
                         cp_hpf(e,h,b,bf,j)=cons
                      end if
             end do
                 else ! your gridb(b) is negative. you can use hpf to pay back the debt
                 call grid_Cons_Grow(gridm, 0d0, min(-gridb(b),gridbf(bf)*(1d0+rh)+theta*shouru(j,e)), 0.01d0) ! generate a grid. using this grid, we
                 do m=1,nm
                 resource=gridb(b)*(1d0+rb*judge(b .ge.zero)+rm*judge(b<zero))&
                 +(1d0-theta-tau_ss)*shouru(j,e)-delta_h*p*gridh(h)+trans+gridm(m)
                 bfnext_p(e,h,b,bf,j)=gridbf(bf)*(1d0+rh)+theta*shouru(j,e)-gridm(m)
                   bpmax=resource-1d-10
                   if (bpmin.ge.bpmax) cycle
                   call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
                   do bp=1,nbp
                      cons=resource-gridbp(bp)
                      temp3=0d0
                      call bilinear(bfnext_p(e,h,b,bf,j),gridbf,nbf,gridbp(bp),gridb,nb,&
                      il,ir,iu,id,failu,fairu,faild,faird)
                      if (j<60) then
                         do ep=1,ne
                            temp2=failu*vh_hpf(ep,h,il,iu,j+1)+fairu*vh_hpf(ep,h,ir,iu,j+1)
                            temp2=temp2+faild*vh_hpf(ep,h,il,id,j+1)+faird*vh_hpf(ep,h,ir,id,j+1)
                            temp3=temp3+markov(e,ep)* temp2
                         end do
                      else
                         temp3=failu*vh_hpf(e,h,il,iu,j+1)+fairu*vh_hpf(e,h,ir,iu,j+1)
                         temp3=temp3+faild*vh_hpf(e,h,il,id,j+1)+faird*vh_hpf(e,h,ir,id,j+1)
                      end if
                      temp=u(cons,gridh(h),omega_bar)+beta*temp3
                      if (temp>vp_hpf(e,h,b,bf,j)) then
                         vp_hpf(e,h,b,bf,j)=temp
                         app_hpf(e,h,b,bf,j)=gridbp(bp)
                         cp_hpf(e,h,b,bf,j)=cons
                         hpfuse(e,h,b,bf,j)=gridm(m)
                      end if
             end do
             end do

             end if
             ! optimization finished
            end do
         end do
    end do

end do
!$omp  end  parallel do
end


subroutine nonowner_hpf(j)
real(8) x
integer b,bf,h,e,j


do bf=1,nbf
  do b=zero,nb

   do e=1,ne
      if (vo_hpf(e,b,bf,j)>vr_hpf(e,b,bf,j)) then
        vn_hpf(e,b,bf,j)=vo_hpf(e,b,bf,j)
       ! cn_hpf(e,b,bf,j)=co_hpf(e,b,bf,j)
       ! apn_hpf(e,b,bf,j)=apo(e,b,bf,j)
       ! hn(e,b,bf,j)=gridh(pu(e,b,bf,j))
        buy_hpf(e,b,bf,j)=1
      else if (vo_hpf(e,b,bf,j)<vr_hpf(e,b,bf,j)) then
        vn_hpf(e,b,bf,j)=vr_hpf(e,b,bf,j)
       ! cn_hpf(e,b,bf,j)=cr(e,b,bf,j)
        !apn(e,b,bf,j)=apr(e,b,bf,j)
      !  hn(e,b,bf,j)=0d0
        buy_hpf(e,b,bf,j)=0
      else
        call simulate_uniform(x)
        vn_hpf(e,b,bf,j)=vo_hpf(e,b,bf,j)*judge(x<0.5d0)+vr_hpf(e,b,bf,j)*judge(x.ge.0.5d0)
        buy_hpf(e,b,bf,j)=1*judge(x<0.5d0)
      end if
   end do
end do
end do

end

subroutine  owner_hpf(j)
real(8) x
integer b,bf,h,e,j



do bf=1,nbf
  do b=1,nb
      do h=2,nh ! gridh(1)=0d0
        if (gridb(b)*(1d0+rm)<-lambdab*p*gridh(h)) cycle
        do e=1,ne
          if (vp_hpf(e,h,b,bf,j)>vs_hpf(e,h,b,bf,j)) then
            vh_hpf(e,h,b,bf,j)=vp_hpf(e,h,b,bf,j)
            
            keep_hpf(e,h,b,bf,j)=1
          else if (vp_hpf(e,h,b,bf,j)<vs_hpf(e,h,b,bf,j)) then
            vh_hpf(e,h,b,bf,j)=vs_hpf(e,h,b,bf,j)
            keep_hpf(e,h,b,bf,j)=0
          else
            call simulate_uniform(x)
            vh_hpf(e,h,b,bf,j)=vp_hpf(e,h,b,bf,j)*judge(x<0.5d0)+vs_hpf(e,h,b,bf,j)*judge(x.ge.0.5d0)
            keep_hpf(e,h,b,bf,j)=1*judge(x<0.5d0)
          end if
        end do
    end do
  end do
end do
end

subroutine seller_hpf(j)
real(8) bnew
integer ir,il,b,h,j
real(8) fai

do b=1,nb
      do h=2,nh ! gridh(1)=0d0
         if (gridb(b)*(1d0+rm)<-lambdab*p*gridh(h)) cycle
         bnew=gridb(b)*(1d0+rm*judge(gridb(b)<0d0)+rb*judge(gridb(b).ge.0d0))+(1-delta_h-kappa_h)*p*gridh(h)
         ! print*, b,gridb(b),h,gridh(h),bnew
         call linear(bnew, gridb, il, ir, fai)
         vs_hpf(:,h,b,:,j)=vn_hpf(:,il,:,j)*fai+vn_hpf(:,ir,:,j)*(1d0-fai)
        ! cs_hpf(:,h,b,:,j)=cn_hpf(:,il,:,j)*fai+cn_hpf(:,ir,:,j)*(1d0-fai)
         aps_hpf(:,h,b,:,j)=apn_hpf(:,il,:,j)*fai+apn_hpf(:,ir,:,j)*(1d0-fai)
        ! hs_hpf(:,h,b,:,j)=hn_hpf(:,il,:,j)*fai+hn_hpf(:,ir,:,j)*(1d0-fai)
   end do
 end do
end



subroutine keeper_last
! owner for age==75
real(8) bpmax,bpmin,cons,temp,resource
integer b,h,e,bp
real(8) gridbp(nbp)

bpmin=0d0

do b=1,nb
      do h=2,nh
         if (gridb(b)*(1+rm)<-lambdab*gridh(h)*p) cycle
             do e=1,ne
                resource=gridb(b)*(1d0+rb*judge(b .ge. zero)+rm*judge(b<zero))+&
                replace*shouru(agemax,e)+trans-gridh(h)*delta_h*p
                bpmax=(resource)-1d-10
                if (bpmin.ge.bpmax) cycle
                call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
                do bp=1,nbp
                   cons=resource-gridbp(bp)
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
real(8) bpmax,cons,bpmin,resource
integer b,e,bp,j,rent
real(8) temp,gridbp(nbp)

bpmin=0d0
do b=zero,nb
  do e=1,ne
     do rent=1,nrent
        resource=gridb(b)*(1d0+rb)+replace*shouru(agemax,e)+trans-gridrent(rent)*d
        bpmax=resource-1d-10
        if (bpmin.ge.bpmax) exit ! in the last age, I simply use a grid search??
        call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
        do bp=1,nbp
            cons=resource-gridbp(bp)
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
real(8) fai,resource, temp3,temp,bpmin,bpmax,gridbp(nbp)

bpmin=0d0


 do b=zero,nb
    do e=1,ne
        do rent=1,nrent
         resource=gridb(b)*(1d0+rb)+replace*shouru(j,e)+trans-gridrent(rent)*d
         bpmax=resource-1d-10
         if (bpmin.ge.bpmax) cycle
            call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
            do bp=1,nbp
               cons=resource-gridbp(bp)
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
                
               end if
               end do

end do
end do
end do
end

subroutine renter_beforeretire(j)
! the problem for renter age no larger than 60
real(8) bpmax,bpmin
real(8) temp,temp2,temp3,cons,resource
integer b,h,e,hp,bp,ep,j,rent
integer ir,il
real(8) fai,gridbp(nbp)

bpmin=0d0

!!$omp parallel do private(bp,rent,e,bpmin,bpmax,resource,ep,gridbp,temp,temp2,temp3,cons,fai,il,ir)
 do b=zero,max_index(j)
  do e=1,ne
     do rent=1,nrent
                resource=gridb(b)*(1d0+rb)+(1d0-tau_ss)*shouru(j,e)+trans-gridrent(rent)*d
                bpmax=resource-1d-10
                if (bpmin.ge.bpmax) exit
                call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
                do bp=1,nbp
                    cons=resource-gridbp(bp)
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
real(8) fai,resource,temp,temp3,bpmin,bpmax,gridbp(nbp)


!$omp parallel do private(hp,e,bpmin,bpmax,bp,resource,gridbp,temp,temp3,cons,fai,il,ir)

   do b=zero,nb
          do e=1,ne
            do hp=2,nh ! gridh(1)=0d0
              bpmin=-(lambdab)*p*gridh(hp)/(1d0+rm)
              resource=gridb(b)*(1d0+rb)+replace*shouru(j,e)-p*gridh(hp)*(1d0+kf)+trans
              bpmax=resource-1d-10
              if (bpmin.ge.bpmax) exit ! it is safe to use exit here
              call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
              do bp=1,nbp
                 cons=resource-gridbp(bp)
                 temp3=0d0
                call linear(gridbp(bp), gridb, il, ir, fai)
                 temp3=fai*vh(e,hp,il,j+1)+(1d0-fai)*vh(e,hp,ir,j+1)
                 temp=u(cons,gridh(hp),omega_bar)+beta*temp3
                 if (temp>vo(e,b,j)) then
                     vo(e,b,j)=temp
                     apo(e,b,j)=gridbp(bp)
                     pu(e,b,j)=hp
                     co(e,b,j)=cons
                  !   current_o(e,b,j)=u(cons,gridh(hp),omega_bar)
                  !   next_o(e,b,j)=temp3
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
real(8) cons,tmp,tmp2,tmp3,bpmin,bpmax,resource,gridbp(nbp)
integer b,h,e,bp,hp,ep
integer ir,il,j
real(8) fai


!$omp parallel do private(bp,hp,e,bpmin,bpmax,resource,ep,gridbp,tmp,tmp2,tmp3,cons,fai,il,ir)
  do b=zero,max_index(j)
     do e=1,ne
        do hp=2,nh ! gridh(1)=0d0. you choose the amount h', the amount of house you want to have next period.
           bpmin=-(lambdab)*p*gridh(hp)/(1d0+rm)
           resource=gridb(b)*(1d0+rb)+(1d0-tau_ss)*shouru(j,e)-p*gridh(hp)*(1d0+kf)+trans
           bpmax=resource-1d-10
              if (bpmin.ge.bpmax) exit
              call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
              do bp=1,nbp
                 cons=resource-gridbp(bp)
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
real(8) fai,resource,temp,temp3,bpmin,bpmax,gridbp(nbp)


!$omp parallel do private(h,e,bpmin,bpmax,resource,gridbp,temp,temp3,cons,bp,fai,il,ir)

 do b=1,nb
  do h=nh,2,(-1) ! gridh(1)=0d0f
     if (gridb(b)*(1d0+rm)<-lambdab*gridh(h)*p) exit
     do e=1,ne
        bpmin=-lambdab*p*gridh(h)
        resource=gridb(b)*(1d0+rb*judge(b .ge. zero)+rm*judge(b<zero))+replace*shouru(j,e)-delta_h*p*gridh(h)+trans
        bpmax=resource-1d-10
        if (bpmin.ge.bpmax) cycle
        call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
        do bp=1,nbp
            cons=resource-gridbp(bp)
            temp3=0d0
            call linear(gridbp(bp), gridb, il, ir, fai)
            temp3=fai*vh(e,h,il,j+1)+(1d0-fai)*vh(e,h,ir,j+1)
            temp=u(cons,gridh(h),omega_bar)+beta*temp3
            if (temp>vp(e,h,b,j)) then
            vp(e,h,b,j)=temp
            app(e,h,b,j)=gridbp(bp)
            cp(e,h,b,j)=cons
           ! current_p(e,h,b,j)=u(cons,gridh(h),omega_bar)
            !next_p(e,h,b,j)=temp3
            end if
       end do
    end do
  end do
end do

!$omp  end  parallel do
end

subroutine keeper_beforeretire(j)
! payer for age <=60
real(8) bpmax,bpmin,temp,temp2,temp3,cons,resource,gridbp(nbp)
integer b,h,e,m,bp,ep,j
integer ir,il
real(8) fai

!$omp parallel do private(bp,h,e,bpmin,bpmax,resource,ep,gridbp,temp,temp2,temp3,cons,fai,il,ir)
   ! first case, you have no debt.
          do b=1,nb
            do h=nh,2,(-1)
              if (gridb(b)*(1d0+rm)<-lambdab*gridh(h)*p) exit
                do e=ne,1,(-1)
                 resource=gridb(b)*(1d0+rb*judge(b .ge. zero)+rm*judge(b<zero))+(1d0-tau_ss)*shouru(j,e)-p*delta_h*gridh(h)+trans
                 bpmin=-(lambdab)*p*gridh(h)
                bpmax=resource-1d-10
                      if (bpmin.ge.bpmax) exit
                      call grid_Cons_Grow(gridbp, bpmin, bpmax, growth)
                      do bp=1,nbp
                            cons=resource-gridbp(bp)
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
        buy(e,b,j)=1
      else if (vo(e,b,j)<vr(e,b,j)) then
        vn(e,b,j)=vr(e,b,j)
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
        
     end do
   end do

end



subroutine maxhpf_gen
integer j,k,il,ir
real(8) temp,fai
maxhpf(agemin)=0d0
max_index_hpf(agemin)=1
do j=agemin+1,retire
   temp=maxhpf(j-1)*(1d0+rh)+shouru(j,5)*theta
   call linear(temp,gridbf,il,ir,fai)
   maxhpf(j)=gridbf(ir)
   max_index_hpf(j)=ir
end do
end

subroutine maxsaving_gen
integer j,k,il,ir
real(8) temp,fai
maxsaving(agemin)=0d0
max_index(agemin)=zero
do j=agemin+1,retire
   temp=maxsaving(j-1)*(1d0+rb)+shouru(j,5)*(1d0-tau_ss)+trans
    call linear(temp,gridb,il,ir,fai)
   maxsaving(j)=gridb(ir)
   max_index(j)=ir
end do

do j=retire+1,agemax
   maxsaving(j)=maxsaving(j-1)*(1d0+rb)+shouru(j,5)*replace+trans
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




function u(c,h,omega)
real(8) c,h,rent,u,omega
    u=(1-phi)*c**(1-gama)+phi*(omega*h)**(1-gama)
    u=u**((1-sigma)/(1-gama))-1
    !u=(c**(1d0-phi))*((omega*h)**phi)
    !u=u**(1-sigma)-1
    u=u/(1-sigma)

   
end

function tfunc(p,x,b)
real(8) tfunc,x,b,bmax,p
! to be completed
bmax = p*gridh(4)
if (b .ge. 0d0) then
tfunc=0d0
else
!tfunc=minval((/(mult*x)**eta,-b,bmax /))
tfunc=minval((/(mult*x)**eta,-b/))

tfunc=tfunc*rategap

end if
end


end program
