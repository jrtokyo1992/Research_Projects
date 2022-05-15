include 'parameter0712.f90'
program output
use toolbox
use global
integer j,i,ir,il,b,e,bf,h,ep
real(8) fai
real(8) h_e_hpf(ne,nb),h_e(ne,nb),h_e_hpf_inter(ne,nb)
real(8) h_bot1(agemin:agemax),h_bot1_hpf(agemin:agemax)
real(8) h_top1(agemin:agemax),h_top1_hpf(agemin:agemax)
real(8) h_med1(agemin:agemax),h_med1_hpf(agemin:agemax)
real(8) a_bot1(agemin:agemax),a_bot1_hpf(agemin:agemax)
real(8) a_top1(agemin:agemax),a_top1_hpf(agemin:agemax)
real(8) a_med1(agemin:agemax),a_med1_hpf(agemin:agemax)



real(8) h_bot1_5(1:11),h_bot1_hpf_5(1:11)
real(8) h_top1_5(1:11),h_top1_hpf_5(1:11)
real(8) h_med1_5(1:11),h_med1_hpf_5(1:11)
real(8) a_bot1_5(1:11),a_bot1_hpf_5(1:11)
real(8) a_top1_5(1:11),a_top1_hpf_5(1:11)
real(8) a_med1_5(1:11),a_med1_hpf_5(1:11)


real(8) life_ownership5_hpf(1:11),life_house5_hpf(1:11)
real(8) life_asset5_hpf(1:11),life_cons5_hpf(1:11)

zero=32

open(1,file='markov.xls',status='old')
do e=1,ne
 do ep=1,ne
   read(1,*) markov(e,ep)
 end do
 end do
close(1)


open(1, file='gridb.xls', status='old')
 
      do b=1,nb
      read(1,*) gridb(b)
           end do
close(1)

open(1, file='gridbf.xls', status='old')
 
      do bf=1,nbf
      read(1,*) gridbf(bf)
           end do
close(1)



open(1, file='shouru.xls', status='old')
 do j=agemin,agemax
      do e=1,ne
      read(1,*) shouru(j,e)
           end do
           end do
close(1)

open(1, file='policy_hpf_house.xls', status='old')
  do j=agemin,agemax
    do bf=1,nbf
      do b=1,nb
        do h=1,nh
          do e=1,ne
           read(1,*) app_hpf(e,h,b,bf,j),keep_hpf(e,h,b,bf,j),bfnext_p(e,h,b,bf,j)
           end do
           end do
           end do
           end do
           end do
close(1)


open(1, file='policy_hpf_nohouse.xls', status='old')
  do j=agemin,agemax
    do bf=1,nbf
      do b=1,nb
          do e=1,ne
           if (j<agemax) read(1,*) apr_hpf(e,b,bf,j),apo_hpf(e,b,bf,j),pu_hpf(e,b,bf,j),buy_hpf(e,b,bf,j),&
           bfnext_r(e,b,bf,j),bfnext_o(e,b,bf,j)
           if (j==agemax)   read(1,*) apr_hpf(e,b,bf,j)
           end do
           end do
           end do
           end do
close(1)




open(1, file='policy_nohpf_house.xls', status='old')
  do j=agemin,agemax
    
      do b=1,nb
        do h=1,nh
          do e=1,ne
           read(1,*) app(e,h,b,j),keep(e,h,b,j)
           end do
           end do
           end do
           end do
        
close(1)


open(1, file='policy_nohpf_nohouse.xls', status='old')
  do j=agemin,agemax
   
      do b=1,nb
          do e=1,ne
           if (j<agemax) read(1,*) apr(e,b,j),apo(e,b,j),pu(e,b,j),buy(e,b,j)
          if (j==agemax) read(1,*) apr(e,b,j)
           end do
           end do
           end do
     
close(1)


! we also need rb rm p 
open(1,file='price_original.xls',status='old')
read(1,*) rb,rm,p,D
close(1)

call housegrid_gen
!print*, gridh

print*, rb,rm,p
age=(/(I,I=21,75,1)/)*1d0
age5=((/(i,i=1,11,1)/)-1d0)*5d0+20d0
!****************we first compare the average life cycle 

call lifecycleplot



! second, output the the model generated data
call peoplesimulation
call hw_cal 

call lifehouse_compare

open(1, file='model_generated_data.xls', status='replace')
write(1,*) 'age ','humanwealth ','wageincome ','house ','hpf ','asset'
  do i=1,floor(people*participate)
    do j=agemin,agemax
write(1,*) j,hw(i,j),shouru(j,inc(i,j))*(replace*judge(j>retire)+(1d0-tau_ss-theta)*judge(j<=retire))&
,gridh(hou(i,j)),1,gridb(ass(i,j))
           end do
end do
do i=floor(people*participate)+1,people
    do j=agemin,agemax
      write(1,*)  j,hw(i,j),shouru(j,inc(i,j))*(replace*judge(j>retire)+(1d0-tau_ss)*judge(j<=retire))&
      ,gridh(hou(i,j)),0,gridb(ass(i,j))
  end do
end do
close(1)



contains


subroutine lifecycleplot
open(1, file='life_hpf.xls', status='old')  
   do j=agemin,agemax
      read(1,*) age(j),life_cons_hpf(j),life_house_hpf(j),life_asset_hpf(j),life_ownership_hpf(j)
    end do
close(1) 

open(1, file='life_nohpf.xls', status='old')  
   do j=agemin,agemax
      read(1,*) age(j),life_cons(j),life_house(j),life_asset(j),life_ownership(j)
    end do
close(1) 

do j=1,11
life_cons5(j)=sum(life_cons(agemin+(j-1)*5   :agemin-1+j*5))/5d0
life_house5(j)=sum(life_house(agemin+(j-1)*5   :agemin-1+j*5))/5d0
life_asset5(j)=sum(life_asset(agemin+(j-1)*5   :agemin-1+j*5))/5d0
life_ownership5(j)=sum(life_ownership(agemin+(j-1)*5   :agemin-1+j*5))/5d0
end do

do j=1,11
life_cons5_hpf(j)=sum(life_cons_hpf(agemin+(j-1)*5   :agemin-1+j*5))/5d0
life_house5_hpf(j)=sum(life_house_hpf(agemin+(j-1)*5   :agemin-1+j*5))/5d0
life_asset5_hpf(j)=sum(life_asset_hpf(agemin+(j-1)*5   :agemin-1+j*5))/5d0
life_ownership5_hpf(j)=sum(life_ownership_hpf(agemin+(j-1)*5   :agemin-1+j*5))/5d0
end do

!call plot(age5,life_cons5,legend='without hpf')
!call plot(age5,life_cons5_hpf,legend='hpf')
!call execplot(ylabel='consumption',xlabel='age',title='average life cycle: consumption')

call plot(age5,life_house5,legend='Non-Hpf',color='red')
call plot(age5,life_house5_hpf,legend='Hpf',color='blue')
call execplot(ylabel='Housing',xlabel='Age',title='Average Life Cyle: House')

call plot(age5,life_asset5,legend='Non-Hpf',color='red')
call plot(age5,life_asset5_hpf,legend='Hpf',color='blue')
call execplot(ylabel='Liquid asset',xlabel='Age',title='Average Life Cyle: Liquid Asset')

call plot(age5,life_ownership5,legend='Non-Hpf',color='red')
call plot(age5,life_ownership5_hpf,legend='Hpf',color='blue')
call execplot(ylabel='Ownership',xlabel='Age',title=' Average Life Cyle: Ownership')

end 




subroutine peoplesimulation
! this subroutine try to simulate 10000 people.
! we summarize those people with lower 1% total life time human wealth.
! and plot their average life house profile.

integer cordlr,cordlr_new,cordud
real(8) failu,faird,fairu,faild,x,x_new,fai,bf_next,bnew,h_dist(nh)
integer i,j,ir,il,iu,id
hw=0d0
h_dist=0d0
print*,'now we simulate 10000 people.'
zero=32
do i=1,floor(people*participate)    ! we first 
   ass(i,agemin)=zero ! ass is also an inde
   hou(i,agemin)=1 ! house is an index.

   inc(i,agemin)=3 ! you start with an average productivity.
   hpf(i,agemin)=1  ! also and an index
   do j=agemin,agemax
         ! print*, i,j,hou(i,agemin)
    !  print*, i,j,gridh(hou(i,j)),gridb(ass(i,j)),hpf(i,j),inc(i,j)
      h_dist(hou(i,j))=h_dist(hou(i,j))+1d0
      if (j<agemax) then
     ! print*,'after inerpolation', 'x=',x,il,ir,fai
     ! print*, 'falls into',(il)*judge(x<fai)+(ir)*judge(x.ge.fai)
      if (hou(i,j)==1)  then ! you dont have any house,you choose to either buy or not
       !  print*, i,j,buy_hpf(inc(i,j),cordlr,cordud,j)
         if (buy_hpf(inc(i,j),ass(i,j),hpf(i,j),j)==1) then
        ! print*, 'buy!'  ! you buy
            bf_next=bfnext_o(inc(i,j),ass(i,j),hpf(i,j),j)
            call bilinear(bf_next,gridbf,&
            nbf,apo_hpf(inc(i,j),ass(i,j),hpf(i,j),j),gridb,nb,&
                      il,ir,iu,id,failu,fairu,faild,faird)
            call simulate_uniform(x)
            call judge2(x,failu,fairu,faild,faird,il,ir,iu,id,cordlr,cordud)
            ass(i,j+1)=cordlr
            hpf(i,j+1)=cordud
            hou(i,j+1)=pu_hpf(inc(i,j),ass(i,j),hpf(i,j),j)
            
         else ! you still rent
        ! print*,'dont buy'
            bf_next=bfnext_r(inc(i,j),ass(i,j),hpf(i,j),j)
            call bilinear(bf_next,gridbf,&
            nbf,apr_hpf(inc(i,j),ass(i,j),hpf(i,j),j),gridb,nb,&
                      il,ir,iu,id,failu,fairu,faild,faird)
            call simulate_uniform(x)
            call judge2(x,failu,fairu,faild,faird,il,ir,iu,id,cordlr,cordud)
            ass(i,j+1)=cordlr
            hpf(i,j+1)=cordud
            hou(i,j+1)=1
         end if
      else
          bf_next=bfnext_p(inc(i,j),hou(i,j),ass(i,j),hpf(i,j),j)
         if (keep_hpf(inc(i,j),hou(i,j),ass(i,j),hpf(i,j),j)==1) then ! you just keep the house and pay your debt
           ! print*, 'keep'
           
             call bilinear(bf_next,gridbf,&
            nbf,app_hpf(inc(i,j),hou(i,j),ass(i,j),hpf(i,j),j),gridb,nb,&
                      il,ir,iu,id,failu,fairu,faild,faird)
            call simulate_uniform(x)
            call judge2(x,failu,fairu,faild,faird,il,ir,iu,id,cordlr,cordud)
            ass(i,j+1)=cordlr
            hpf(i,j+1)=cordud
            hou(i,j+1)=hou(i,j)
          else ! you have to sell the house.
             bnew=gridb(ass(i,j))*(1d0+rb*judge(gridb(ass(i,j))>0d0)+rm*judge(gridb(ass(i,j)).le.0d0))&
             +(1-delta_h-kappa_h)*p*gridh(hou(i,j))
             call linear(bnew,gridb, il, ir, fai)
             call simulate_uniform(x_new)
             cordlr_new=il*judge(x_new<fai)+ir*judge(x_new .ge. fai)
           !  print*,'falls into',(il)*judge(x_new<fai)+(ir)*judge(x_new.ge.fai)
         ! you need to choose whether to purchase or be a renter

             if (buy_hpf(inc(i,j),cordlr_new,hpf(i,j),j)==1) then   ! you buy
           ! print*,'sell and rebuy','a_new=',a_new(i,j)
                   call bilinear(bf_next,gridbf,nbf,apo_hpf(inc(i,j),cordlr_new,hpf(i,j),j),gridb,nb,&
                      il,ir,iu,id,failu,fairu,faild,faird)
            call simulate_uniform(x)
            call judge2(x,failu,fairu,faild,faird,il,ir,iu,id,cordlr,cordud)
            ass(i,j+1)=cordlr
            hpf(i,j+1)=cordud
                hou(i,j+1)=pu_hpf(inc(i,j),cordlr_new,hpf(i,j),j)

             else ! you still rent
            ! print*, 'sell and not buy','a_new=',a_new(i,j)
                   call bilinear(bf_next,gridbf,&
            nbf,apr_hpf(inc(i,j),cordlr_new,hpf(i,j),j),gridb,nb,&
                      il,ir,iu,id,failu,fairu,faild,faird)
            call simulate_uniform(x)
            call judge2(x,failu,fairu,faild,faird,il,ir,iu,id,cordlr,cordud)
            ass(i,j+1)=cordlr
            hpf(i,j+1)=cordud
            hou(i,j+1)=1
             end if
         end if
    end if

   if (j<retire) then
       inc(i,j+1)=destiny(inc(i,j)) ! notice here that income is integer
       
    else
       inc(i,j+1)=inc(i,j)
      
    end if
end if
end do
end do

! now simulate non-participate
do i=floor(people*participate)+1 ,people
   ass(i,agemin)=zero
   hou(i,agemin)=1 ! house is an index.
   inc(i,agemin)=3 ! you start with an average productivity.

   do j=agemin,agemax
     ! print*, i,j,ass(i,j),hou(i,j),inc(i,j)
   !  if (inc(i,j)==0) print*, i,j

     ! print*,'after inerpolation', 'x=',x,il,ir,fai
     ! print*, 'falls into',(il)*judge(x<fai)+(ir)*judge(x.ge.fai)
     h_dist(hou(i,j))=h_dist(hou(i,j))+1d0
     if (j<agemax) then
      if (hou(i,j)==1)  then ! you dont have any house,you choose to either buy or not.
         if (buy(inc(i,j),ass(i,j),j)==1) then
        ! print*, 'buy!'  ! you buy
             call linear(apo(inc(i,j),ass(i,j),j),gridb,il,ir,fai)
            call simulate_uniform(x)
            cordlr=il*judge(x<fai)+ir*judge(x .ge. fai)
            ass(i,j+1)=cordlr
            hou(i,j+1)=pu(inc(i,j),ass(i,j),j)
         else ! you still rent
        ! print*,'dont buy'
             call linear(apr(inc(i,j),ass(i,j),j),gridb,il,ir,fai)
            call simulate_uniform(x)
            cordlr=il*judge(x<fai)+ir*judge(x .ge. fai)
            ass(i,j+1)=cordlr
            hou(i,j+1)=1
         end if
      else
         if (keep(inc(i,j),hou(i,j),ass(i,j),j)==1) then ! you just keep the house and pay your debt
           ! print*, 'keep'
              call linear(app(inc(i,j),hou(i,j),ass(i,j),j),gridb,il,ir,fai)
            call simulate_uniform(x)
            cordlr=il*judge(x<fai)+ir*judge(x .ge. fai)
            ass(i,j+1)=cordlr
             hou(i,j+1)=hou(i,j)
          else ! you have to sell the house.
             ass_new(i,j)=gridb(ass(i,j))*(1d0+rb*judge(gridb(ass(i,j))>0d0)+rm*judge(gridb(ass(i,j)).le.0d0))&
             +(1-delta_h-kappa_h)*p*gridh(hou(i,j))
             call linear(ass_new(i,j),gridb, il, ir, fai)
             call simulate_uniform(x_new)
             cordlr_new=il*judge(x_new<fai)+ir*judge(x_new .ge. fai)
           !  print*,'falls into',(il)*judge(x_new<fai)+(ir)*judge(x_new.ge.fai)
         ! you need to choose whether to purchase or be a renter

             if (buy(inc(i,j),cordlr_new,j)==1) then   ! you buy
           ! print*,'sell and rebuy','a_new=',a_new(i,j)
                 call linear(apo(inc(i,j),cordlr_new,j),gridb,il,ir,fai)
            call simulate_uniform(x)
            cordlr=il*judge(x<fai)+ir*judge(x .ge. fai)
            ass(i,j+1)=cordlr
                hou(i,j+1)=pu(inc(i,j),cordlr_new,j)

             else ! you still rent
            ! print*, 'sell and not buy','a_new=',a_new(i,j)
                 call linear(apr(inc(i,j),cordlr_new,j),gridb,il,ir,fai)
            call simulate_uniform(x)
            cordlr=il*judge(x<fai)+ir*judge(x .ge. fai)
            ass(i,j+1)=cordlr
                hou(i,j+1)=1
             end if
         end if
    end if

      if (j<retire) then
       inc(i,j+1)=destiny(inc(i,j)) ! notice here that income is integer
      else
       inc(i,j+1)=inc(i,j)
    end if
end if
end do
end do

! now you have the history of 10000 people.  

end


subroutine hw_cal
! calculate the human wealth of people at period j
! notice here the human wealth is without the deduction of hpf.
real(8) s
integer i,j
do i=1,people
  hw(i,agemax)=replace*shouru(agemax,inc(i,agemax))
  do j=agemax-1,retire+1,(-1)
     hw(i,j)=replace*shouru(j,inc(i,j))+hw(i,j+1)/(1d0+rb)
  end do
  do j=retire,agemin,(-1)
     hw(i,j)=(1d0-tau_ss)*shouru(j,inc(i,j))+hw(i,j+1)/(1d0+rb)
     end do
end do

end


subroutine lifehouse_compare
use toolbox
integer i,j,np,n
real(8) hper(agemin:agemax),aper(agemin:agemax),hper_hpf(agemin:agemax),aper_hpf(agemin:agemax)
real(8) hw21_hpf(floor(people*participate)),hw21(floor(people*participate)+1:people)
integer ind_hpf(floor(people*participate)),ind(floor(people*participate)+1:people)
real(8) oper(agemin:agemax),oper_hpf(agemin:agemax)

hper_hpf=0d0
hper=0d0
oper_hpf=0d0
oper=0d0
! we firt calculate the average house during these two groups.
do j=agemin,agemax
hper_hpf(j)=hper_hpf(j)+sum(gridh(hou(1:floor(people*participate),j)))
  do i=1,floor(people*participate)
    if (hou(i,j)>1) oper_hpf(j)=oper_hpf(j)+1d0
  end do
end do
hper_hpf=hper_hpf/(people*participate)
oper_hpf=oper_hpf/(people*participate)
do j=agemin,agemax
hper(j)=hper(j)+sum(gridh(hou(floor(people*participate)+1:people,j)))
 do i=floor(people*participate)+1,people
    if (hou(i,j)>1) oper(j)=oper(j)+1d0
  end do
end do
hper=hper/(people*(1d0-participate))
oper=oper/(people*(1d0-participate))

!call plot(age,oper_hpf,legend='hpf')
!call plot(age,oper,legend='no hpf')
!call execplot(ylabel='average house', xlabel='age',title='the life cycle ownership comparison' )


!call plot(age(21:74),hper_hpf(21:74),legend='hpf',color='blue')
!call plot(age(21:74),hper(21:74),legend='no hpf',color='red')
!call execplot(ylabel='average house', xlabel='age',title='the life cycle house comparison' )


! now we break down to different human wealth percentile.
hw21_hpf=hw(1:floor(people*participate),agemin)
!now we plot the average life cycle house of those who have the lower 5% wealth.
ind_hpf=(/(I,I=1,floor(people*participate),1)/)
call Qsortc(hw21_hpf,ind_hpf)  ! now the hw21_hpf and ind_hpf is order. hw21_hpf stores hw, ind_hpf stores index of people.
hw21=hw(floor(people*participate)+1:people,agemin)
ind=(/(I,I=floor(people*participate)+1,people,1)/)
call Qsortc(hw21,ind) 



!******************the bot 20****************
!^^^^^^^^^^^^^^^for hpf^^^^^^^^^^^^^^^^
i=1
hper_hpf=0d0
aper_hpf=0d0
oper_hpf=0d0
np=floor(people*participate)
n=0
do
  !print*, 'the ith poorest people in hpf is index in',ind_hpf(i)
  do j=agemin,agemax
  hper_hpf(j)=hper_hpf(j)+gridh(hou(ind_hpf(i),j)) ! ind(i) indicate the index of the person who are at position i in the sorted array.
  aper_hpf(j)=aper_hpf(j) + ass(ind_hpf(i),j)
  oper_hpf(j)=oper_hpf(j)+judge(hou(ind_hpf(i),j)>1)
  if (hou(ind_hpf(i),j)==nh) n=n+1
  end do
if (i>np*0.2d0) exit
i=i+1

end do

hper_hpf=hper_hpf/(np*0.2d0)
aper_hpf=aper_hpf/(np*0.2d0)
oper_hpf=oper_hpf/(np*0.2d0)
print*,'max buyers in hpf',n/(np*0.2d0*55)
!^^^^^^^^^^^^^^^^non hpf^^^^^^^^^^^^^^^^^^^^^^^
i=floor(people*participate)+1
hper=0d0
aper=0d0
oper=0d0
np=people-floor(people*participate)  ! how many people are there in non hpf group
n=0
do
  do j=agemin,agemax
  hper(j)=hper(j)+gridh(hou(ind(i),j)) ! ind(i) indicate the index of the person who are at position i in the sorted array.
  aper(j)=aper(j) + ass(ind(i),j)
  oper(j)=oper(j)+ judge(hou(ind(i),j)>1)
   if (hou(ind(i),j)==nh) n=n+1
  end do
if (i>floor(people*participate)+1+np*0.2d0) exit
i=i+1

end do

hper=hper/(np*0.2d0)
oper=oper/(np*0.2d0)
print*,'max buyers in nohpf',n/(np*0.2d0*55)

print*,'for bot'
print*,'average house in hpf',sum(hper_hpf)/(agemax-agemin+1)
print*,'average house in non-hpf',sum(hper)/(agemax-agemin+1)
print*,'average ownership in hpf',sum(oper_hpf)/(agemax-agemin+1)
print*,'average ownership in non-hpf',sum(oper)/(agemax-agemin+1)
aper=aper/(np*0.2d0)
!^^^^^^^^^^plot^^^^^^^^^^^^^^^^^^^^^

call plot(age(21:74),hper(21:74),legend='Non-Hpf',color='red')
call plot(age(21:74),hper_hpf(21:74),legend='Hpf',color='blue')
call execplot(ylabel='Average House', xlabel='Age',title='Life Cycle House:Bot 20% Human Wealth')

call plot(age(21:74),oper(21:74),legend='Non-Hpf',color='red')
call plot(age(21:74),oper_hpf(21:74),legend='Hpf',color='blue')
call execplot(ylabel='Average Ownership', xlabel='Age',title='Life Cycle Ownership:Bot 20% Human Wealth')


!*************medium 20 ***************
!^^^^^^^^^^^^for hpf^^^^^^^^^^^^^^^^^^^^
i=floor(floor(people*participate)*0.4d0)
hper_hpf=0d0
aper_hpf=0d0
oper_hpf=0d0
np=floor(people*participate)
n=0
do
  do j=agemin,agemax
  hper_hpf(j)=hper_hpf(j)+gridh(hou(ind_hpf(i),j)) ! ind(i) indicate the index of the person who are at position i in the sorted array.
  aper_hpf(j)=aper_hpf(j) + ass(ind_hpf(i),j)
  oper_hpf(j)=oper_hpf(j)+judge(hou(ind_hpf(i),j)>1)
  if (hou(ind_hpf(i),j)==nh) n=n+1
  end do
if (i>np*0.6d0) exit
i=i+1

end do

hper_hpf=hper_hpf/(np*0.2d0)
aper_hpf=aper_hpf/(np*0.2d0)
oper_hpf=oper_hpf/(np*0.2d0)
print*,'for hpf, maxhouse buyer ', n/(np*0.2d0*55)
!^^^^^^^^^^^for non hpf^^^^^^^^^^^^^^


hper=0d0
aper=0d0
oper=0d0
np=people-floor(people*participate)
i=floor(people*participate)+1+floor(np*0.4d0)
n=0
do
  do j=agemin,agemax
  hper(j)=hper(j)+gridh(hou(ind(i),j)) ! ind(i) indicate the index of the person who are at position i in the sorted array.
  if (hou(ind(i),j)==nh) n=n+1
  aper(j)=aper(j) + ass(ind(i),j)
  oper(j)=oper(j)+judge(hou(ind(i),j)>1)
  end do
if (i>floor(people*participate)+1+np*0.6d0) exit
i=i+1

!print*,n,i
end do

hper=hper/(np*0.2d0)
aper=aper/(np*0.2d0)
oper=oper/(np*0.2d0)
print*, 'for nonhpf, max house',n/(np*0.2d0*55)

print*,'for mid'
print*,'average house in hpf',sum(hper_hpf)/(agemax-agemin+1)
print*,'average house in non-hpf',sum(hper)/(agemax-agemin+1)
print*,'average ownership in hpf',sum(oper_hpf)/(agemax-agemin+1)
print*,'average ownership in non-hpf',sum(oper)/(agemax-agemin+1)

!^^^^^^^^^^^^^plot^^^^^^^^^^^^^^^^^^^^

call plot(age(21:74),hper(21:74),legend='Non-Hpf',color='red')
call plot(age(21:74),hper_hpf(21:74),legend='Hpf',color='blue')
call execplot(ylabel='Average House', xlabel='Age',title='Life Cycle House:Mid 20% Human Wealth')

call plot(age(21:74),oper(21:74),legend='Non-Hpf',color='red')
call plot(age(21:74),oper_hpf(21:74),legend='Hpf',color='blue')
call execplot(ylabel='Average Ownership', xlabel='Age',title='Life Cycle Ownership:Mid 20% Human Wealth')


!*************now is for top 20*********
!^^^^^^^^^^^^^^for hpf^^^^^^^^^^^^^^

hper_hpf=0d0 
aper_hpf=0d0
oper_hpf=0d0
np=floor(people*participate)
i=floor(np*0.8d0)
n=0
do
  do j=agemin,agemax
  hper_hpf(j)=hper_hpf(j)+gridh(hou(ind_hpf(i),j)) ! ind(i) indicate the index of the person who are at position i in the sorted array.
  if (hou(ind_hpf(i),j)==nh) n=n+1
  aper_hpf(j)=aper_hpf(j) + ass(ind_hpf(i),j)
  oper_hpf(j)=oper_hpf(j)+judge(hou(ind_hpf(i),j)>1)
  end do
if (i.ge.np) exit
i=i+1

end do

hper_hpf=hper_hpf/(np*0.2d0)
aper_hpf=aper_hpf/(np*0.2d0)
oper_hpf=oper_hpf/(np*0.2d0)
print*,'for hpf, maxhouse buyer is', n/(np*0.2d0*55)
!^^^^^^^^for non hpf^^^^^^^^^^^^^^^

hper=0d0 
aper=0d0
oper=0d0
np=people-floor(people*participate)
i=floor(people*participate)+1+floor(np*0.8d0)
n=0
do
  do j=agemin,agemax
  hper(j)=hper(j)+gridh(hou(ind(i),j)) ! ind(i) indicate the index of the person who are at position i in the sorted array.
  if (hou(ind(i),j)==nh) n=n+1
  aper(j)=aper(j) + ass(ind(i),j)
  oper(j)=oper(j)+judge(hou(ind(i),j)>1)
  end do
if (i.ge.people) exit
i=i+1

end do

hper=hper/(np*0.2d0)
oper=oper/(np*0.2d0)
print*,'for non hpf, maxhouse buyer is', n/(np*0.2d0*55)

print*,'for top'
print*,'average house in hpf',sum(hper_hpf)/(agemax-agemin+1)
print*,'average house in non-hpf',sum(hper)/(agemax-agemin+1)
print*,'average ownership in hpf',sum(oper_hpf)/(agemax-agemin+1)
print*,'average ownership in non-hpf',sum(oper)/(agemax-agemin+1)

aper=aper/(np*0.2d0)

!^^^^^^^^^^plot^^^^^^^^^^^^^^^^

call plot(age(21:74),hper(21:74),legend='Non-Hpf',color='red')
call plot(age(21:74),hper_hpf(21:74),legend='Hpf',color='blue')
call execplot(ylabel='Average House', xlabel='Age',title='Life Cycle House:Top 20% Human Wealth')

call plot(age(21:74),oper(21:74),legend='Non-Hpf',color='red')
call plot(age(21:74),oper_hpf(21:74),legend='Hpf',color='blue')
call execplot(ylabel='Average Ownership', xlabel='Age',title='Life Cycle Ownership:Top 20% Human Wealth')

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





!**************************************************

end
