include 'parameter.f90'
use toolbox
use global

integer j,b,h,e,ep
real(8),allocatable:: wg(:,:,:,:) ! this is important. it prevents the the dump stack.!!
real(8) hdistyoung1(nh),hdistmiddle1(nh), hdistold1(nh)
real(8) hdistyoung2(nh),hdistmiddle2(nh), hdistold2(nh)
real(8) hdistyoung3(nh),hdistmiddle3(nh), hdistold3(nh)
real(8) wsharetop1_1(7),wsharetop10_1(7),wsharetop20_1(7)
real(8) wsharebot1_1(7),wsharebot10_1(7),wsharebot20_1(7)
real(8) wsharetop1_2(7),wsharetop10_2(7),wsharetop20_2(7)
real(8) wsharebot1_2(7),wsharebot10_2(7),wsharebot20_2(7)
real(8) wsharetop1_3(7),wsharetop10_3(7),wsharetop20_3(7)
real(8) wsharebot1_3(7),wsharebot10_3(7),wsharebot20_3(7)
real(8), allocatable:: wealth(:), wdist(:),wealth_order(:), wdist_order(:)
integer,allocatable:: ind(:)

print*, '??'

call Grid_Cons_Equi(age5,1d0,14d0)
age5=(age5-1d0)*5d0+21d0

open(1, file='gridh.xls', status='old')  
   do h=1,nh
      read(1,*) gridh(h)
    end do
close(1)


! part 3: calculate the welfare redistribution

open(1, file='death_new.xls', status='old')
   do j=agemin,agemax+1
      read(1,*) death(j)
    end do
close(1)

! to exclude the impact of death rate on welfare, we use the new death rate when calculating the cev.



open(1, file='gridb.xls', status='old')  
   do b=1,nb
      read(1,*) gridb(b)
    end do
close(1)

!print*, gridb
open(1, file='labor.xls', status='old')  
   do j=agemin,retire
     do e=1,ne
      read(1,*) labor(j,e)
    end do
    end do
close(1)

!print*, labor

open(1, file='markov.xls', status='old')  
   do e=1,ne
     do ep=1,ne
      read(1,*) markov(e,ep)
    end do
    end do
close(1)

open(1, file='price.xls', status='old') 
read(1,*) p1,r1,w1
read(1,*) p2,r2,w2
read(1,*) p3,r3,w3
close(1)

open(1, file='policy1.xls', status='old')  
do j=agemin,agemax
  do b=1,nb
    do h=1,nh
      do e=1,ne
      read(1,*) c_1(e,h,b,j),s_1(e,h,b,j),pu_1(e,h,b,j)
    end do
    end do
    end do
    end do
close(1)


open(1, file='policy2.xls', status='old')  
do j=agemin,agemax
  do b=1,nb
    do h=1,nh
      do e=1,ne
      read(1,*) c_2(e,h,b,j),s_2(e,h,b,j),pu_2(e,h,b,j)
    end do
    end do
    end do
    end do
close(1)

open(1, file='policy3.xls', status='old')  
do j=agemin,agemax
  do b=1,nb
    do h=1,nh
      do e=1,ne
      read(1,*) c_3(e,h,b,j),s_3(e,h,b,j),pu_3(e,h,b,j)
    end do
    end do
    end do
    end do
close(1)


open(1, file='measure1.xls', status='old')  
do j=agemin,agemax
  do b=1,nb
    do h=1,nh
      do e=1,ne
      read(1,*) m_initial(e,h,b,j)
    end do
    end do
    end do
    end do
close(1)




!print*,'housing price is',p
print*,'we next calculate the welfare gain'
allocate(wg(ne,nh,nb,agemin:agemax))

open(1, file='v1.xls', status='old')  
   do j=agemin,agemax
     do b=1,nb
       do h=1,nh
         do e=1,ne
      read(1,*) v_old(e,h,b,j)
    end do
    end do
    end do
    end do
close(1)



open(1, file='v2.xls', status='old')  
   do j=agemin,agemax
     do b=1,nb
       do h=1,nh
         do e=1,ne
      read(1,*) v_new(e,h,b,j)
    end do
    end do
    end do
    end do
close(1)

call hypothetical_value ! get v_old adjusted for the death rate
do j=agemin,agemax
     do b=1,nb
       do h=1,nh
         do e=1,ne
       if(v_adjust(e,h,b,j)==small .or. v_new(e,h,b,j)==small) cycle
     
    end do
    end do
    end do
    end do

!call welfare_gain_cal ! we get the the welfare comparison between v1 and v2


open(1, file='welfaregain_12.xls', status='replace')  
   do j=agemin,agemax
     do b=1,nb
       do h=1,nh
         if (gridb(b)<-lambdab*p1*gridh(h)) cycle
          
          do e=1,ne
           if (pu_1(e,h,b,j)<0) cycle
           if (v_new(e,h,b,j)==small) cycle

            if (j .le. retire) write(1,*) labor(j,e),gridh(h),gridb(b),j,welfare_gain(e,h,b,j)
            if (j > retire) write(1,*) labor(retire,e),gridh(h),gridb(b),j,welfare_gain(e,h,b,j)
          end do
          end do
          end do
          end do
close(1) 





open(1, file='v3.xls', status='old')  
   do j=agemin,agemax
     do b=1,nb
       do h=1,nh
         do e=1,ne
      read(1,*) v_new(e,h,b,j)
    end do
    end do
    end do
    end do
close(1)


call welfare_gain_cal ! we get the welfare comparison between 

open(1, file='welfaregain_13.xls', status='replace')  
   do j=agemin,agemax
     do b=1,nb
       do h=1,nh
         if (gridb(b)<-lambdab*p1*gridh(h)) cycle
          
          do e=1,ne
           if (pu_1(e,h,b,j)<0) cycle
           if (v_new(e,h,b,j)==small) cycle
            if (j .le. retire) write(1,*) labor(j,e),gridh(h),gridb(b),j,welfare_gain(e,h,b,j)
            if (j > retire) write(1,*) labor(retire,e),gridh(h),gridb(b),j,welfare_gain(e,h,b,j)
          end do
          end do
          end do
          end do
close(1) 


contains
subroutine welfare_cev(e_start,h_start,b_start,j_start,cev,welfare_with_cev)
! we use wf(e,h,b,startage) to measure the cev of an agent with liquid asset b and housing asset h 
! we need to compute the life time utility of a person starting from e,h,b,j under the original equibrium
!wf_ini(e,h,b,startage)=0d0
! basically, this subroutine measures the value of a (e_start,h_start,b_start,j_start) person if he increase his composite consumption by cev
real(8) welfare_with_cev,temp,cev,temp2,fai
integer e,h,b,j,e_start,h_start,b_start,j_start,hleft,hright,bleft,bright,eleft,eright,ep,ir,il
wg=0d0
!print*,'now we are calculating the welfare at',e_start,h_start,j_start,'if the cev is',cev
do j=agemax,j_start,(-1)
   ! calculate all the welfare_cev....
   bleft=1*judge(j>j_start)+b_start*judge(j==j_start)
   bright=nb*judge(j>j_start)+b_start*judge(j==j_start)
   do b=bleft,bright
      hleft=1*judge(j>j_start)+h_start*judge(j==j_start)
      hright=nh*judge(j>j_start)+h_start*judge(j==j_start)
      do h=hleft,hright
       if (gridb(b)<-lambdab*p1*gridh(h)*(1d0-delta_h)) cycle ! we need this !
        eleft=1*judge(j>j_start)+e_start*judge(j==j_start)
        eright=ne*judge(j>j_start)+e_start*judge(j==j_start)
        do e=eleft,eright
       ! if(j<agemax) print*, 'currently,',e,h,b,j
           !wg(e,h,b,j)=0d0
           if (pu_1(e,h,b,j)<0) cycle ! even when I impose the condition gridb1(b)*qb<-lambdab*p*gridh(h)
           if (j<agemax) then
               call linear(s_1(e,h,b,j), gridb, il, ir, fai)
               temp=0d0
              ! print*,'xixixi'
               if (j.le.retire) then
                  do ep=1,ne
                     temp2=wg(ep,pu_1(e,h,b,j),il,j+1)*fai+wg(ep,pu_1(e,h,b,j),ir,j+1)*(1d0-fai)
                     temp=temp+markov(e,ep)*temp2
                  end do
               else
               temp=wg(e,pu_1(e,h,b,j),il,j+1)*fai+wg(e,pu_1(e,h,b,j),ir,j+1)*(1d0-fai)
               end if
               wg(e,h,b,j)=(1d0-phi)*c_1(e,h,b,j)**(1d0-gama)+phi*(gridh(h)+h_constant)**(1d0-gama)
               wg(e,h,b,j)=wg(e,h,b,j)**(1d0/(1d0-gama))
               wg(e,h,b,j)=wg(e,h,b,j)*(1d0+cev)
               wg(e,h,b,j)=(wg(e,h,b,j)**(1d0-sigma)-1d0)/(1d0-sigma)
              ! if (j<agemax) print*, 'for',e,h,b,j,wg(e,h,b,j),temp
               
               wg(e,h,b,j)=wg(e,h,b,j)+beta*temp*(1d0-death(j+1))
              ! if(j<agemax) print*,wg(e,h,b,j)
               wg(e,h,b,j)=wg(e,h,b,j)+beta*bequestutility(s_1(e,h,b,j)+(1d0-delta_h-kappa_h)*p1*gridh(pu_1(e,h,b,j)))*death(j+1)
              ! if(j<agemax) print*,wg(e,h,b,j)
              
            else
               wg(e,h,b,j)=(1d0-phi)*c_1(e,h,b,j)**(1d0-gama)+phi*(gridh(h)+h_constant)**(1d0-gama)
               wg(e,h,b,j)=wg(e,h,b,j)**(1d0/(1d0-gama))
               wg(e,h,b,j)=wg(e,h,b,j)*(1d0+cev)
               wg(e,h,b,j)=(wg(e,h,b,j)**(1d0-sigma)-1d0)/(1d0-sigma)
               wg(e,h,b,j)=wg(e,h,b,j)+beta*bequestutility(s_1(e,h,b,j)+(1d0-delta_h-kappa_h)*p1*gridh(pu_1(e,h,b,j)))*death(j+1)
               end if
             ! if (j==agemax-1) then
             ! print*, e,h,b,j
             ! print*, c_old(e,h,b,j),s_old(e,h,b,j),s_old(e,h,b,j)+(1d0-delta_h-kappa_h)*p*gridh(pu_old(e,h,b,j)),wg(e,h,b,j)
             ! read*
             ! end if
               
        end do
        end do
        end do
    ! print*, j,maxval(wg(:,:,:,j))
    ! read*
           
           
end do
! we get the welfare_cev(e_start,h_start,b_start,j_start) now.

welfare_with_cev=wg(e_start,h_start,b_start,j_start)

!print*,'calculation is over.',welfare_with_cev
end

subroutine cev_cal(e_start,h_start,b_start,j_start,cev)
real(8) cevmax,cevmin,wf_neweq,wf_withcev,failu,fairu,faild,faird,fai
real(8),intent(out):: cev
integer iter,e_start,h_start,b_start,j_start,il,ir,iu,id,outbound



wf_neweq=v_new(e_start,h_start,b_start,j_start)
cevmax=5d0
cevmin=-0.999999d0
!print*, 'cevmin is',cevmin
iter=1
do
cev=(cevmax+cevmin)*0.5d0
call welfare_cev(e_start,h_start,b_start,j_start,cev,wf_withcev)
!print*, cev,wf_withcev,wf_neweq
!read*
if (wf_withcev>wf_neweq+0.00005d0) then
cevmax=cev
elseif (wf_withcev<wf_neweq-0.00005d0) then
cevmin=cev
else
exit
end if

if (iter>15) exit
iter=iter+1
end do
end 



subroutine hypothetical_value
integer j,b,h,e,il,ir,ep
real(8) temp,fai,temp2
v_adjust=small
do j=agemax,agemin,(-1)
   do b=1,nb
      do h=1,nh
         if (gridb(b)<-lambdab*p1*gridh(h)*(1d0-delta_h)) cycle
        do e=1,ne
          if (pu_1(e,h,b,j)<0) cycle
            if (j<agemax) then
               call linear(s_1(e,h,b,j), gridb, il, ir, fai)
               temp=0d0
              ! print*,'xixixi'
               if (j.le.retire) then
                  do ep=1,ne
                     temp2=v_adjust(ep,pu_1(e,h,b,j),il,j+1)*fai+v_adjust(ep,pu_1(e,h,b,j),ir,j+1)*(1d0-fai)
                     temp=temp+markov(e,ep)*temp2
                  end do
               else
               temp=v_adjust(e,pu_1(e,h,b,j),il,j+1)*fai+v_adjust(e,pu_1(e,h,b,j),ir,j+1)*(1d0-fai)
               end if
               
               v_adjust(e,h,b,j)=u(c_1(e,h,b,j),gridh(h))+beta*temp*(1d0-death(j+1))
              ! if(j<agemax) print*,wg(e,h,b,j)
               v_adjust(e,h,b,j)=v_adjust(e,h,b,j)+beta*bequestutility(s_1(e,h,b,j)&
               +(1d0-delta_h-kappa_h)*p1*gridh(pu_1(e,h,b,j)))*death(j+1)
              ! if(j<agemax) print*,wg(e,h,b,j)
              
            else
               
               v_adjust(e,h,b,j)=u(c_1(e,h,b,j),gridh(h))&
               +beta*bequestutility(s_1(e,h,b,j)+(1d0-delta_h-kappa_h)*p1*gridh(pu_1(e,h,b,j)))*death(j+1)
               end if 
             !  print*, e,h,b,j,v_adjust(e,h,b,j)
        end do 
        end do
        end do
end do
end 


subroutine welfare_gain_cal
integer e,h,b,j
welfare_gain=0d0
!!$omp parallel do private(b,h,e)
do j=agemin,80
  if ((j .ne. 25) .and. (j .ne. 45) .and. (j .ne. 65))  cycle  
  print*, 'now it is age',j
  do b=1,nb
    do h=1,nh
      if (gridb(b)<-lambdab*p1*gridh(h)*(1d0-delta_h)) cycle
      do e=1,ne
       if (v_new(e,h,b,j)==small) cycle
       if (pu_1(e,h,b,j)<0) cycle
       if (m_initial(e,h,b,j)<=0d0) cycle ! There is no sense to study those points who are originally not there...
        call cev_cal(e,h,b,j,welfare_gain(e,h,b,j))
      !  print*, e,h,b,j,welfare_gain(e,h,b,j)
      end do
    end do
  end do

end do
!!$omp end parallel do


end


subroutine lifecycleplot
call plot(age5, life_cons1,legend='initial equilibrium')
call plot(age5, life_cons2,legend='new equilibrium: change replacement')
call plot(age5, life_cons3,legend='new equilibrium: change tax')
call execplot(ylabel='consumption',xlabel='age',title='life cycle consumption',filename='lifecons')
call plot(age5, life_asset1,legend='initial equilibrium')
call plot(age5, life_asset2,legend='new equilibrium: change replacement')
call plot(age5, life_asset3,legend='new equilibrium: change tax')
call execplot(ylabel='liquid asset',xlabel='age',title='life cycle liquid asset',filename='lifeasset')
call plot(age5, life_house1,legend='initial equilibrium')
call plot(age5,life_house2,legend='new equilibrium:change replacement')
call plot(age5,life_house3,legend='new equilibrium:change tax')
call execplot(ylabel='housing',xlabel='age',title='life cycle housing',filename='lifehouse')

end




!call plot(age, life_ownership)
end


