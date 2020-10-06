include 'parameter.f90'
use toolbox
use global

integer j,b,h,e,ep
real(8),allocatable:: wg(:,:,:,:) ! this is important. it prevents the the dump stack.!!

real(8) life_measure1(agemin:agemax)
real(8) wsharetop1_1(agemin:agemax),wsharetop10_1(agemin:agemax),wsharetop20_1(agemin:agemax)
real(8) wsharebot50_1(agemin:agemax),wsharebot20_1(agemin:agemax)
real(8) wsharetop1_2(agemin:agemax),wsharetop10_2(agemin:agemax),wsharetop20_2(agemin:agemax)
real(8) wsharebot50_2(agemin:agemax),wsharebot20_2(agemin:agemax)
real(8) wsharetop1_3(agemin:agemax),wsharetop10_3(agemin:agemax),wsharetop20_3(agemin:agemax)
real(8) wsharebot50_3(agemin:agemax),wsharebot20_3(agemin:agemax)
real(8) wsharemid40_1(agemin:agemax),wsharemid40_2(agemin:agemax),wsharemid40_3(agemin:agemax)
real(8) totalwealth_1(agemin:agemax),totalwealth_2(agemin:agemax),totalwealth_3(agemin:agemax)

real(8) hsharetop1_1(agemin:agemax),hsharetop10_1(agemin:agemax),hsharetop20_1(agemin:agemax)
real(8) hsharebot50_1(agemin:agemax),hsharemid40_1(agemin:agemax),hbot20_1(agemin:agemax)

real(8) hsharetop1_2(agemin:agemax),hsharetop10_2(agemin:agemax),hsharetop20_2(agemin:agemax)
real(8) hsharebot50_2(agemin:agemax),hsharemid40_2(agemin:agemax),hbot20_2(agemin:agemax)

real(8) hsharetop1_3(agemin:agemax),hsharetop10_3(agemin:agemax),hsharetop20_3(agemin:agemax)
real(8) hsharebot50_3(agemin:agemax),hsharemid40_3(agemin:agemax),hbot20_3(agemin:agemax)

real(8) htop1_3(agemin:agemax),htop10_3(agemin:agemax),htop20_3(agemin:agemax), hbot50_3(agemin:agemax),hmid40_3(agemin:agemax)
real(8) htop1_2(agemin:agemax),htop10_2(agemin:agemax),htop20_2(agemin:agemax), hbot50_2(agemin:agemax),hmid40_2(agemin:agemax)
real(8) htop1_1(agemin:agemax),htop10_1(agemin:agemax),htop20_1(agemin:agemax), hbot50_1(agemin:agemax),hmid40_1(agemin:agemax)
real(8) ltop1_3(agemin:agemax),ltop10_3(agemin:agemax),ltop20_3(agemin:agemax), lbot50_3(agemin:agemax),lmid40_3(agemin:agemax)
real(8) ltop1_2(agemin:agemax),ltop10_2(agemin:agemax),ltop20_2(agemin:agemax), lbot50_2(agemin:agemax),lmid40_2(agemin:agemax)
real(8) ltop1_1(agemin:agemax),ltop10_1(agemin:agemax),ltop20_1(agemin:agemax), lbot50_1(agemin:agemax),lmid40_1(agemin:agemax)
real(8) lbot20_1(agemin:agemax),lbot20_2(agemin:agemax),lbot20_3(agemin:agemax)

real(8) htop1_3_smooth(14),htop10_3_smooth(14),htop20_3_smooth(14), hbot50_3_smooth(14),hmid40_3_smooth(14)
real(8) htop1_2_smooth(14),htop10_2_smooth(14),htop20_2_smooth(14), hbot50_2_smooth(14),hmid40_2_smooth(14)
real(8) htop1_1_smooth(14),htop10_1_smooth(14),htop20_1_smooth(14), hbot50_1_smooth(14),hmid40_1_smooth(14)
real(8) ltop1_3_smooth(14),ltop10_3_smooth(14),ltop20_3_smooth(14), lbot50_3_smooth(14),lmid40_3_smooth(14)
real(8) ltop1_2_smooth(14),ltop10_2_smooth(14),ltop20_2_smooth(14), lbot50_2_smooth(14),lmid40_2_smooth(14)
real(8) ltop1_1_smooth(14),ltop10_1_smooth(14),ltop20_1_smooth(14), lbot50_1_smooth(14),lmid40_1_smooth(14)
real(8) lbot20_1_smooth(14),lbot20_2_smooth(14),lbot20_3_smooth(14)
real(8) hbot20_1_smooth(14),hbot20_2_smooth(14),hbot20_3_smooth(14)




real(8) ctop1_3(agemin:agemax),ctop10_3(agemin:agemax),ctop20_3(agemin:agemax), cbot50_3(agemin:agemax),cmid40_3(agemin:agemax)
real(8) ctop1_2(agemin:agemax),ctop10_2(agemin:agemax),ctop20_2(agemin:agemax), cbot50_2(agemin:agemax),cmid40_2(agemin:agemax)
real(8) ctop1_1(agemin:agemax),ctop10_1(agemin:agemax),ctop20_1(agemin:agemax), cbot50_1(agemin:agemax),cmid40_1(agemin:agemax)
real(8) cbot20_1(agemin:agemax),cbot20_2(agemin:agemax),cbot20_3(agemin:agemax)

real(8) etop1_3(agemin:agemax),etop10_3(agemin:agemax),etop20_3(agemin:agemax), ebot50_3(agemin:agemax),emid40_3(agemin:agemax)
real(8) etop1_2(agemin:agemax),etop10_2(agemin:agemax),etop20_2(agemin:agemax), ebot50_2(agemin:agemax),emid40_2(agemin:agemax)
real(8) etop1_1(agemin:agemax),etop10_1(agemin:agemax),etop20_1(agemin:agemax), ebot50_1(agemin:agemax),emid40_1(agemin:agemax)
real(8) ebot20_1(agemin:agemax),ebot20_2(agemin:agemax),ebot20_3(agemin:agemax)

real(8) hbot10_1(agemin:agemax),hbot10_2(agemin:agemax)
real(8) lbot10_1(agemin:agemax),lbot10_2(agemin:agemax)
real(8) cbot10_1(agemin:agemax),cbot10_2(agemin:agemax)
real(8) ebot10_1(agemin:agemax),ebot10_2(agemin:agemax)

real(8) wetop10_1(agemin:agemax),wetop20_1(agemin:agemax),wemid40_1(agemin:agemax),webot50_1(agemin:agemax)
real(8) webot20_1(agemin:agemax),webot10_1(agemin:agemax),webot20_2(agemin:agemax),webot20_3(agemin:agemax)
real(8) wetop20_2(agemin:agemax),wetop20_3(agemin:agemax),wetop10_2(agemin:agemax),wetop10_3(agemin:agemax)
real(8) wemid40_2(agemin:agemax),wemid40_3(agemin:agemax),webot50_2(agemin:agemax),webot50_3(agemin:agemax)
real(8) aggregatewealth_1(agemin:agemax),aggregatewealth_2(agemin:agemax),aggregatewealth_3(agemin:agemax)


real(8), allocatable:: wealth(:), wdist(:),wealth_order(:), wdist_order(:),we(:),we_order(:)
real(8),allocatable:: hw(:),hw_order(:),lw(:),lw_order(:),cw(:),cw_order(:),ew(:),ew_order(:)
integer,allocatable:: ind(:)



print*, '??'

call Grid_Cons_Equi(age5,1d0,14d0)
age5=(age5-1d0)*5d0+21d0
age=(/(i,i=agemin,agemax)/)*1d0
open(1, file='lifecycle1.xls', status='old')  
   do j=1,14
      read(1,*) life_cons1(j),life_asset1(j),life_house1(j),life_income1(j)
    end do
close(1) 

open(1, file='lifecycle2.xls', status='old')  
   do j=1,14
      read(1,*) life_cons2(j),life_asset2(j),life_house2(j),life_income2(j)
    end do
close(1) 

open(1, file='lifecycle3.xls', status='old')  
   do j=1,14
      read(1,*) life_cons3(j),life_asset3(j),life_house3(j),life_income3(j)
    end do
close(1) 

call lifecycleplot

!READ*
! part 2 plot wealth distribution


open(1, file='gridh.xls', status='old')  
   do h=1,nh
     
      read(1,*) gridh(h)
    end do
close(1)


! part 3: calculate the welfare redistribution

open(1, file='death.xls', status='old')
   do j=agemin,agemax+1
      read(1,*) death(j)
    end do
close(1)



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
      read(1,*) m1(e,h,b,j)
    end do
    end do
    end do
    end do
close(1)

do j=agemin,agemax
life_measure1(j)=sum(m1(:,:,:,j))
end do


open(1, file='measure2.xls', status='old')  
do j=agemin,agemax
  do b=1,nb
    do h=1,nh
      do e=1,ne
      read(1,*) m2(e,h,b,j)
    end do
    end do
    end do
    end do
close(1)

open(1, file='measure3.xls', status='old')  
do j=agemin,agemax
  do b=1,nb
    do h=1,nh
      do e=1,ne
      read(1,*) m3(e,h,b,j)
    end do
    end do
    end do
    end do
close(1)


call wealth_distribution_plot



contains
subroutine smoothplot
integer j
real(8) linjie
linjie=agemin
do j=1,14
htop10_1_smooth(j)=dot_product(htop10_1(21+(j-1)*5 : 20+j*5),life_measure1(21+(j-1)*5 : 20+j*5))&
/sum(life_measure1(21+(j-1)*5 : 20+j*5))
htop20_1_smooth(j)=dot_product(htop20_1(21+(j-1)*5: 20+j*5),life_measure1(21+(j-1)*5 : 20+j*5))&
/sum(life_measure1(21+(j-1)*5 : 20+j*5))
hmid40_1_smooth(j)=dot_product(hmid40_1(21+(j-1)*5: 20+j*5),life_measure1(21+(j-1)*5 : 20+j*5))&
/sum(life_measure1(21+(j-1)*5 : 20+j*5))
hbot50_1_smooth(j)=dot_product(hbot50_1(21+(j-1)*5: 20+j*5),life_measure1(21+(j-1)*5 : 20+j*5))&
/sum(life_measure1(21+(j-1)*5 : 20+j*5))
hbot20_1_smooth(j)=dot_product(hbot20_1(21+(j-1)*5: 20+j*5),life_measure1(21+(j-1)*5 : 20+j*5))&
/sum(life_measure1(21+(j-1)*5 : 20+j*5))
ltop10_1_smooth(j)=dot_product(ltop10_1(21+(j-1)*5 : 20+j*5),life_measure1(21+(j-1)*5 : 20+j*5))&
/sum(life_measure1(21+(j-1)*5 : 20+j*5))
ltop20_1_smooth(j)=dot_product(ltop20_1(21+(j-1)*5: 20+j*5),life_measure1(21+(j-1)*5 : 20+j*5))&
/sum(life_measure1(21+(j-1)*5 : 20+j*5))
lmid40_1_smooth(j)=dot_product(lmid40_1(21+(j-1)*5: 20+j*5),life_measure1(21+(j-1)*5 : 20+j*5))&
/sum(life_measure1(21+(j-1)*5 : 20+j*5))
lbot50_1_smooth(j)=dot_product(lbot50_1(21+(j-1)*5: 20+j*5),life_measure1(21+(j-1)*5 : 20+j*5))&
/sum(life_measure1(21+(j-1)*5 : 20+j*5))
lbot20_1_smooth(j)=dot_product(lbot20_1(21+(j-1)*5: 20+j*5),life_measure1(21+(j-1)*5 : 20+j*5))&
/sum(life_measure1(21+(j-1)*5 : 20+j*5))

htop10_2_smooth(j)=dot_product(htop10_2(21+(j-1)*5 : 20+j*5),life_measure1(21+(j-1)*5 : 20+j*5))&
/sum(life_measure1(21+(j-1)*5 : 20+j*5))
htop20_2_smooth(j)=dot_product(htop20_2(21+(j-1)*5: 20+j*5),life_measure1(21+(j-1)*5 : 20+j*5))&
/sum(life_measure1(21+(j-1)*5 : 20+j*5))
hmid40_2_smooth(j)=dot_product(hmid40_2(21+(j-1)*5: 20+j*5),life_measure1(21+(j-1)*5 : 20+j*5))&
/sum(life_measure1(21+(j-1)*5 : 20+j*5))
hbot50_2_smooth(j)=dot_product(hbot50_2(21+(j-1)*5: 20+j*5),life_measure1(21+(j-1)*5 : 20+j*5))&
/sum(life_measure1(21+(j-1)*5 : 20+j*5))
hbot20_2_smooth(j)=dot_product(hbot20_2(21+(j-1)*5: 20+j*5),life_measure1(21+(j-1)*5 : 20+j*5))&
/sum(life_measure1(21+(j-1)*5 : 20+j*5))
ltop10_2_smooth(j)=dot_product(ltop10_2(21+(j-1)*5 : 20+j*5),life_measure1(21+(j-1)*5 : 20+j*5))&
/sum(life_measure1(21+(j-1)*5 : 20+j*5))
ltop20_2_smooth(j)=dot_product(ltop20_2(21+(j-1)*5: 20+j*5),life_measure1(21+(j-1)*5 : 20+j*5))&
/sum(life_measure1(21+(j-1)*5 : 20+j*5))
lmid40_2_smooth(j)=dot_product(lmid40_2(21+(j-1)*5: 20+j*5),life_measure1(21+(j-1)*5 : 20+j*5))&
/sum(life_measure1(21+(j-1)*5 : 20+j*5))
lbot50_2_smooth(j)=dot_product(lbot50_2(21+(j-1)*5: 20+j*5),life_measure1(21+(j-1)*5 : 20+j*5))&
/sum(life_measure1(21+(j-1)*5 : 20+j*5))
lbot20_2_smooth(j)=dot_product(lbot20_2(21+(j-1)*5: 20+j*5),life_measure1(21+(j-1)*5 : 20+j*5))&
/sum(life_measure1(21+(j-1)*5 : 20+j*5))

htop10_3_smooth(j)=dot_product(htop10_3(21+(j-1)*5 : 20+j*5),life_measure1(21+(j-1)*5 : 20+j*5))&
/sum(life_measure1(21+(j-1)*5 : 20+j*5))
htop20_3_smooth(j)=dot_product(htop20_3(21+(j-1)*5: 20+j*5),life_measure1(21+(j-1)*5 : 20+j*5))&
/sum(life_measure1(21+(j-1)*5 : 20+j*5))
hmid40_3_smooth(j)=dot_product(hmid40_3(21+(j-1)*5: 20+j*5),life_measure1(21+(j-1)*5 : 20+j*5))&
/sum(life_measure1(21+(j-1)*5 : 20+j*5))
hbot50_3_smooth(j)=dot_product(hbot50_3(21+(j-1)*5: 20+j*5),life_measure1(21+(j-1)*5 : 20+j*5))&
/sum(life_measure1(21+(j-1)*5 : 20+j*5))
hbot20_3_smooth(j)=dot_product(hbot20_3(21+(j-1)*5: 20+j*5),life_measure1(21+(j-1)*5 : 20+j*5))&
/sum(life_measure1(21+(j-1)*5 : 20+j*5))
ltop10_3_smooth(j)=dot_product(ltop10_3(21+(j-1)*5 : 20+j*5),life_measure1(21+(j-1)*5 : 20+j*5))&
/sum(life_measure1(21+(j-1)*5 : 20+j*5))
ltop20_3_smooth(j)=dot_product(ltop20_3(21+(j-1)*5: 20+j*5),life_measure1(21+(j-1)*5 : 20+j*5))&
/sum(life_measure1(21+(j-1)*5 : 20+j*5))
lmid40_3_smooth(j)=dot_product(lmid40_3(21+(j-1)*5: 20+j*5),life_measure1(21+(j-1)*5 : 20+j*5))&
/sum(life_measure1(21+(j-1)*5 : 20+j*5))
lbot50_3_smooth(j)=dot_product(lbot50_3(21+(j-1)*5: 20+j*5),life_measure1(21+(j-1)*5 : 20+j*5))&
/sum(life_measure1(21+(j-1)*5 : 20+j*5))
lbot20_3_smooth(j)=dot_product(lbot20_3(21+(j-1)*5: 20+j*5),life_measure1(21+(j-1)*5 : 20+j*5))&
/sum(life_measure1(21+(j-1)*5 : 20+j*5))

end do

call plot(age5, htop10_1_smooth, legend='se1',color='blue')
call plot(age5, htop10_2_smooth, legend='se2',color='red')
call execplot(xlabel='Age',title=' House Asset: Top 10%',xlim=(/linjie,agemax*1d0/))

call plot(age5, ltop10_1_smooth, legend='se1',color='blue')
call plot(age5, ltop10_2_smooth, legend='se2',color='red')
call execplot(xlabel='Age',title=' Non-Housing Asset: Top 10%',xlim=(/linjie,agemax*1d0/))


call plot(age5, htop10_1_smooth, legend='se1',color='blue')
call plot(age5, htop10_3_smooth, legend='se3',color='goldenrod')
call execplot(xlabel='Age',title=' House Asset: Top 10%',xlim=(/linjie,agemax*1d0/))

call plot(age5, ltop10_1_smooth, legend='se1',color='blue')
call plot(age5, ltop10_3_smooth, legend='se3',color='goldenrod')
call execplot(xlabel='Age',title=' Non-Housing Asset: Top 10%',xlim=(/linjie,agemax*1d0/))


call plot(age5, htop20_1_smooth, legend='se1',color='blue')
call plot(age5, htop20_2_smooth, legend='se2',color='red')
call execplot(xlabel='Age',title=' House Asset: Top 20%',xlim=(/linjie,agemax*1d0/))

call plot(age5, ltop20_1_smooth, legend='se1',color='blue')
call plot(age5, ltop20_2_smooth, legend='se2',color='red')
call execplot(xlabel='Age',title=' Non-Housing Asset: Top 20%',xlim=(/linjie,agemax*1d0/))


call plot(age5, htop20_1_smooth, legend='se1',color='blue')
call plot(age5, htop20_3_smooth, legend='se3',color='goldenrod')
call execplot(xlabel='Age',title=' House Asset: Top 20%',xlim=(/linjie,agemax*1d0/))

call plot(age5, ltop20_1_smooth, legend='se1',color='blue')
call plot(age5, ltop20_3_smooth, legend='se3',color='goldenrod')
call execplot(xlabel='Age',title=' Non-Housing Asset: Top 20%',xlim=(/linjie,agemax*1d0/))



call plot(age5, hmid40_1_smooth, legend='se1',color='blue')
call plot(age5, hmid40_2_smooth, legend='se2',color='red')
call execplot(xlabel='Age',title=' House Asset: Mid 40%',xlim=(/linjie,agemax*1d0/))

call plot(age5, lmid40_1_smooth, legend='se1',color='blue')
call plot(age5, lmid40_2_smooth, legend='se2',color='red')
call execplot(xlabel='Age',title=' Non-Housing Asset: Mid 40%',xlim=(/linjie,agemax*1d0/))

call plot(age5, hmid40_1_smooth, legend='se1',color='blue')
call plot(age5, hmid40_3_smooth, legend='se3',color='goldenrod')
call execplot(xlabel='Age',title=' House Asset: Mid 40%',xlim=(/linjie,agemax*1d0/))

call plot(age5, lmid40_1_smooth, legend='se1',color='blue')
call plot(age5, lmid40_3_smooth, legend='se3',color='goldenrod')
call execplot(xlabel='Age',title=' Non-Housing Asset: Mid 40%',xlim=(/linjie,agemax*1d0/))


call plot(age5, hbot50_1_smooth, legend='se1',color='blue')
call plot(age5, hbot50_2_smooth, legend='se2',color='red')
call execplot(xlabel='Age',title=' House Asset: Bot 50%',xlim=(/linjie,agemax*1d0/))

call plot(age5, lbot50_1_smooth, legend='se1',color='blue')
call plot(age5, lbot50_2_smooth, legend='se2',color='red')
call execplot(xlabel='Age',title=' Non-Housing Asset: Bot 50%',xlim=(/linjie,agemax*1d0/))


call plot(age5, hbot50_1_smooth, legend='se1',color='blue')
call plot(age5, hbot50_3_smooth, legend='se3',color='goldenrod')
call execplot(xlabel='Age',title=' House Asset: Bot 50%',xlim=(/linjie,agemax*1d0/))

call plot(age5, lbot50_1_smooth, legend='se1',color='blue')
call plot(age5, lbot50_3_smooth, legend='se3',color='goldenrod')
call execplot(xlabel='Age',title=' Non-Housing Asset: Bot 50%',xlim=(/linjie,agemax*1d0/))


call plot(age5, hbot20_1_smooth, legend='se1',color='blue')
call plot(age5, hbot20_2_smooth, legend='se2',color='red')
call execplot(xlabel='Age',title=' House Asset: Bot 20%',xlim=(/linjie,agemax*1d0/))

call plot(age5, lbot20_1_smooth, legend='se1',color='blue')
call plot(age5, lbot20_2_smooth, legend='se2',color='red')
call execplot(xlabel='Age',title=' Non-Housing Asset: Bot 20%',xlim=(/linjie,agemax*1d0/))


call plot(age5, hbot20_1_smooth, legend='se1',color='blue')
call plot(age5, hbot20_3_smooth, legend='se3',color='goldenrod')
call execplot(xlabel='Age',title=' House Asset: Bot 20%',xlim=(/linjie,agemax*1d0/))

call plot(age5, lbot20_1_smooth, legend='se1',color='blue')
call plot(age5, lbot20_3_smooth, legend='se3',color='goldenrod')
call execplot(xlabel='Age',title=' Non-Housing Asset: Bot 20%',xlim=(/linjie,agemax*1d0/))


end 

subroutine lifecycleplot

!call plot(age5, life_income1,legend='se1')
!call plot(age5, life_income2,legend='se2')
!call plot(age5, life_income3,legend='se3')
!call execplot(ylabel='income',xlabel='age',title='life cycle income',filename='lifeincome')

call plot(age5, life_cons1,legend='se1',color='blue')
call plot(age5, life_cons2,legend='se2',color='red')
call plot(age5, life_cons3,legend='se3',color='goldenrod')
call execplot(ylabel='Consumption',xlabel='Age',title='Life Cycle Consumption')
call plot(age5, life_asset1,legend='se1',color='blue')
call plot(age5, life_asset2,legend='se2',color='red')
call plot(age5, life_asset3,legend='se3',color='goldenrod')
call execplot(ylabel='Non-housing ',xlabel='Age',title='Life Cycle Non-housing Asset')
call plot(age5, life_house1,legend='se1',color='blue')
call plot(age5,life_house2,legend='se2',color='red')
call plot(age5,life_house3,legend='se3',color='goldenrod')
call execplot(ylabel='Housing',xlabel='Age',title='Life Cycle Housing Asset')

end

subroutine wealth_distribution_cal(flag)
integer j,e,b,h,stage,nw,w,flag
real(8) wage,totalwealth
wage=0d0
if (flag==1) then
m=m1
p=p1
pu=pu_1
s=s_1
c=c_1
r=r1
wage=w1
end if
if (flag==2)  then
m=m2
p=p2
r=r2
pu=pu_2
c=c_2
s=s_2
wage=w2
end if
if (flag==3) then
m=m3
p=p3
pu=pu_3
c=c_3
s=s_3
r=r3
wage=w3
end if
do stage=agemin,agemax
 nw=0
  do b=1,nb
    do h=1,nh
      do e=1,ne
         if (m(e,h,b,stage)>0d0) then
         nw=nw+1
         end if
      end do
      end do
  end do


allocate(wealth(nw))
allocate(wdist(nw))
allocate(wealth_order(nw))
allocate(wdist_order(nw))
allocate(ind(nw))
allocate(hw(nw))
allocate(lw(nw))
allocate(cw(nw))
allocate(ew(nw))
allocate(we(nw))
allocate(ew_order(nw))
allocate(hw_order(nw))
allocate(lw_order(nw))
allocate(cw_order(nw))
allocate(we_order(nw))
w=0
we=0d0
totalwealth=0d0
  do b=1,nb
    do h=1,nh
      do e=1,ne
         if (m(e,h,b,stage)>0d0) then
         w=w+1
         wdist(w)=m(e,h,b,stage) ! this record the density
         wealth(w)=gridh(h)*p*(1d0-delta_h)+gridb(b)
         totalwealth=totalwealth+wealth(w)*wdist(w)
         hw(w)=gridh(h)*(1d0-delta_h)
          !hw(w)=gridh(h)*(1d0-delta_h)
         !lw(w)=gridb(b)*( judge(gridb(b)>=0d0)*(1d0/(1d0+r))+ judge(gridb(b)<0d0)*(1d0/(1d0+r+premium))    )
         lw(w)=gridb(b)
         cw(w)=c(e,h,b,stage)
         if (stage<=retire) ew(w)=labor(stage,e)*wage
         if (stage>retire) ew(w)=labor(retire,e)*wage
         end if
      end do
      end do
      end do

  if (dot_product(wealth,wdist)==0d0) then
print*, 'jump',stage
deallocate(wealth)
deallocate(wdist)
deallocate(wealth_order)
deallocate(wdist_order)
deallocate(ind)
deallocate(hw)
deallocate(hw_order)
deallocate(lw)
deallocate(lw_order)
deallocate(cw)
deallocate(cw_order)
deallocate(ew)
deallocate(ew_order)
deallocate(we)
deallocate(we_order)

cycle
end if

ind=(/(I,I=1,nw)/)
call qsortc(wealth,ind) 
we_order=wealth
do  w=1,nw
wdist_order(w)=wdist(ind(w))
hw_order(w)=hw(ind(w))
lw_order(w)=lw(ind(w))
cw_order(w)=cw(ind(w))
ew_order(w)=ew(ind(w))

!print*, stage,w,we_order(w)
end do

wdist_order=wdist_order/(sum(wdist_order))



if (flag==1) then

aggregatewealth_1(stage)=dot_product(wdist_order,we_order ) 
wsharetop10_1(stage)=sum_top(wdist_order,we_order,0.1d0)/aggregatewealth_1(stage) !? use top5 or top1
wsharetop20_1(stage)=sum_top(wdist_order,we_order,0.2d0)/aggregatewealth_1(stage) !? use top5 or top1
wsharebot50_1(stage)=sum_bot(wdist_order,we_order,0.5d0)/aggregatewealth_1(stage) !? use top5 or top1
wsharebot20_1(stage)=sum_bot(wdist_order,we_order,0.2d0)/aggregatewealth_1(stage) !? use top5 or top1
wsharemid40_1(stage)=1d0-(sum_top(wdist_order,we_order,0.3d0)+sum_bot(wdist_order,we_order,0.3d0))&
/aggregatewealth_1(stage)

htop10_1(stage)=sum_top(wdist_order,hw_order,0.1d0)
ltop10_1(stage)=sum_top(wdist_order,lw_order,0.1d0)
!ctop10_1(stage)=sum(cw_order( floor(nw*0.9d0):nw   ))/(nw*0.1d0)
htop20_1(stage)=sum_top(wdist_order,hw_order,0.2d0)
ltop20_1(stage)=sum_top(wdist_order,lw_order,0.2d0)
hmid40_1(stage)=sum_bot(wdist_order,hw_order,0.7d0)-sum_bot(wdist_order,hw_order,0.3d0)
lmid40_1(stage)=sum_bot(wdist_order,lw_order,0.7d0)-sum_bot(wdist_order,lw_order,0.3d0)

hbot50_1(stage)=sum_bot(wdist_order,hw_order,0.5d0)
lbot50_1(stage)=sum_bot(wdist_order,lw_order,0.5d0)
hbot20_1(stage)=sum_bot(wdist_order,hw_order,0.2d0)
lbot20_1(stage)=sum_bot(wdist_order,lw_order,0.2d0)

end if
if (flag==2)  then

aggregatewealth_2(stage)=dot_product(wdist_order,we_order ) 
wsharetop10_2(stage)=sum_top(wdist_order,we_order,0.1d0)/aggregatewealth_2(stage) !? use top5 or top1
wsharetop20_2(stage)=sum_top(wdist_order,we_order,0.2d0)/aggregatewealth_2(stage) !? use top5 or top1
wsharebot50_2(stage)=sum_bot(wdist_order,we_order,0.5d0)/aggregatewealth_2(stage) !? use top5 or top1
wsharebot20_2(stage)=sum_bot(wdist_order,we_order,0.2d0)/aggregatewealth_2(stage) !? use top5 or top1
wsharemid40_2(stage)=1d0-(sum_top(wdist_order,we_order,0.3d0)+sum_bot(wdist_order,we_order,0.3d0))&
/aggregatewealth_2(stage)

htop10_2(stage)=sum_top(wdist_order,hw_order,0.1d0)
ltop10_2(stage)=sum_top(wdist_order,lw_order,0.1d0)
!ctop10_2(stage)=sum(cw_order( floor(nw*0.9d0):nw   ))/(nw*0.1d0)
htop20_2(stage)=sum_top(wdist_order,hw_order,0.2d0)
ltop20_2(stage)=sum_top(wdist_order,lw_order,0.2d0)
hmid40_2(stage)=sum_bot(wdist_order,hw_order,0.7d0)-sum_bot(wdist_order,hw_order,0.3d0)
lmid40_2(stage)=sum_bot(wdist_order,lw_order,0.7d0)-sum_bot(wdist_order,lw_order,0.3d0)

hbot50_2(stage)=sum_bot(wdist_order,hw_order,0.5d0)
lbot50_2(stage)=sum_bot(wdist_order,lw_order,0.5d0)
hbot20_2(stage)=sum_bot(wdist_order,hw_order,0.2d0)
lbot20_2(stage)=sum_bot(wdist_order,lw_order,0.2d0)
end if
if (flag==3) then


aggregatewealth_3(stage)=dot_product(wdist_order,we_order ) 
wsharetop10_3(stage)=sum_top(wdist_order,we_order,0.1d0)/aggregatewealth_3(stage) !? use top5 or top1
wsharetop20_3(stage)=sum_top(wdist_order,we_order,0.2d0)/aggregatewealth_3(stage) !? use top5 or top1
wsharebot50_3(stage)=sum_bot(wdist_order,we_order,0.5d0)/aggregatewealth_3(stage) !? use top5 or top1
wsharebot20_3(stage)=sum_bot(wdist_order,we_order,0.2d0)/aggregatewealth_3(stage) !? use top5 or top1
wsharemid40_3(stage)=1d0-(sum_top(wdist_order,we_order,0.3d0)+sum_bot(wdist_order,we_order,0.3d0))&
/aggregatewealth_3(stage)

htop10_3(stage)=sum_top(wdist_order,hw_order,0.1d0)
ltop10_3(stage)=sum_top(wdist_order,lw_order,0.1d0)
!ctop10_3(stage)=sum(cw_order( floor(nw*0.9d0):nw   ))/(nw*0.1d0)
htop20_3(stage)=sum_top(wdist_order,hw_order,0.2d0)
ltop20_3(stage)=sum_top(wdist_order,lw_order,0.2d0)
hmid40_3(stage)=sum_bot(wdist_order,hw_order,0.7d0)-sum_bot(wdist_order,hw_order,0.3d0)
lmid40_3(stage)=sum_bot(wdist_order,lw_order,0.7d0)-sum_bot(wdist_order,lw_order,0.3d0)
hbot50_3(stage)=sum_bot(wdist_order,hw_order,0.5d0)
lbot50_3(stage)=sum_bot(wdist_order,lw_order,0.5d0)
hbot20_3(stage)=sum_bot(wdist_order,hw_order,0.2d0)
lbot20_3(stage)=sum_bot(wdist_order,lw_order,0.2d0)
end if
! the wealth_order and wdist_order is the ordered arry. now we want to calculate share



deallocate(wealth)
deallocate(wdist)
deallocate(wealth_order)
deallocate(wdist_order)
deallocate(ind)
deallocate(hw)
deallocate(hw_order)
deallocate(lw)
deallocate(lw_order)
deallocate(cw)
deallocate(cw_order)
deallocate(ew)
deallocate(ew_order)
deallocate(we)
deallocate(we_order)


end do


end 





subroutine wealth_distribution_plot
use toolbox
real(8) linjie
real(8) age_spline(100),ltop10_1_spline(100),ltop10_2_spline(100)
linjie=24d0


call wealth_distribution_cal(1)
call wealth_distribution_cal(2)
call wealth_distribution_cal(3)


do j=24,agemax
wetop10_1(j)=wsharetop10_1(j)*aggregatewealth_1(j)
wetop20_1(j)=wsharetop20_1(j)*aggregatewealth_1(j)
wemid40_1(j)=wsharemid40_1(j)*aggregatewealth_1(j)
webot50_1(j)=wsharebot50_1(j)*aggregatewealth_1(j)
webot20_1(j)=wsharebot20_1(j)*aggregatewealth_1(j)
wetop10_2(j)=wsharetop10_2(j)*aggregatewealth_2(j)
wetop20_2(j)=wsharetop20_2(j)*aggregatewealth_2(j)
wemid40_2(j)=wsharemid40_2(j)*aggregatewealth_2(j)
webot50_2(j)=wsharebot50_2(j)*aggregatewealth_2(j)
webot20_2(j)=wsharebot20_2(j)*aggregatewealth_2(j)
wetop10_3(j)=wsharetop10_3(j)*aggregatewealth_3(j)
wetop20_3(j)=wsharetop20_3(j)*aggregatewealth_3(j)
wemid40_3(j)=wsharemid40_3(j)*aggregatewealth_3(j)
webot50_3(j)=wsharebot50_3(j)*aggregatewealth_3(j)
webot20_3(j)=wsharebot20_3(j)*aggregatewealth_3(j)

end do


do j=24,agemax
print*, j,(wetop10_2(j)-wetop10_1(j))/wetop10_1(j),&
(wetop20_2(j)-wetop20_1(j))/wetop20_1(j),&
(wemid40_2(j)-wemid40_1(j))/wemid40_1(j),&
(webot50_2(j)-webot50_1(j))/webot50_1(j),&
(webot20_2(j)-webot20_1(j))/webot20_1(j)
end do
read*
do j=24,agemax
print*, j,(wetop10_3(j)-wetop10_1(j))/wetop10_1(j),&
(wetop20_3(j)-wetop20_1(j))/wetop20_1(j),&
(wemid40_3(j)-wemid40_1(j))/wemid40_1(j),&
(webot50_3(j)-webot50_1(j))/webot50_1(j),&
(webot20_3(j)-webot20_1(j))/webot20_1(j)
end do


read*


call plot(age(agemin:agemax), wsharetop10_1, legend='se1',color='blue')
call plot(age(agemin:agemax), wsharetop10_2, legend='se2',color='red')
call plot(age(agemin:agemax), wsharetop10_3, legend='se3',color='goldenrod')
call execplot(ylabel='Share',xlabel='Age',title=' Wealth Share:Top 10%',xlim=(/linjie,agemax*1d0/))

call plot(age(agemin:agemax), wsharetop20_1, legend='se1',color='blue')
call plot(age(agemin:agemax), wsharetop20_2, legend='se2',color='red')
call plot(age(agemin:agemax), wsharetop20_3, legend='se3',color='goldenrod')
call execplot(ylabel='Share',xlabel='Age',title=' Wealth Share:Top 20%',xlim=(/linjie,agemax*1d0/))


call plot(age(agemin:agemax), wsharemid40_1, legend='se1',color='blue')
call plot(age(agemin:agemax), wsharemid40_2, legend='se2',color='red')
call plot(age(agemin:agemax), wsharemid40_3, legend='se3',color='goldenrod')
call execplot(ylabel='Share',xlabel='Age',title=' Wealth Share: Mid 40%',xlim=(/linjie,agemax*1d0/))


call plot(age(agemin:agemax), wsharebot50_1, legend='se1',color='blue')
call plot(age(agemin:agemax), wsharebot50_2, legend='se2',color='red')
call plot(age(agemin:agemax), wsharebot50_3, legend='se3',color='goldenrod')
call execplot(ylabel='Share',xlabel='Age',title=' Wealth Share: Bot 50%',xlim=(/linjie,agemax*1d0/))

call plot(age(agemin:agemax), wsharebot20_1, legend='se1',color='blue')
call plot(age(agemin:agemax), wsharebot20_2, legend='se2',color='red')
call plot(age(agemin:agemax), wsharebot20_3, legend='se3',color='goldenrod')
call execplot(ylabel='Share',xlabel='Age',title=' Wealth Share: Bot 20%',xlim=(/linjie,agemax*1d0/))

call smoothplot
!*******************************************

call plot(age(agemin:agemax), htop10_1, legend='se1',color='blue')
call plot(age(agemin:agemax), htop10_2, legend='se2',color='red')
call execplot(xlabel='stage',title=' house in top wealth share 10%',xlim=(/linjie,agemax*1d0/))

call plot(age(agemin:agemax), htop10_1, legend='se1',color='blue')
call plot(age(agemin:agemax), htop10_3, legend='se3',color='green')
call execplot(xlabel='stage',title=' house in top wealth share 10%',xlim=(/linjie,agemax*1d0/))

call plot(age(agemin:agemax), htop20_1, legend='se1',color='blue')
call plot(age(agemin:agemax), htop20_2, legend='se2',color='red')
call execplot(xlabel='stage',title=' house in top wealth share 20%',xlim=(/linjie,agemax*1d0/))
call plot(age(agemin:agemax), htop20_1, legend='se1',color='blue')
call plot(age(agemin:agemax), htop20_3, legend='se3',color='green')
call execplot(xlabel='stage',title=' house in top wealth share 20%',xlim=(/linjie,agemax*1d0/))


call plot( age(agemin:agemax), hmid40_1, legend='se1',color='blue')
call plot(age(agemin:agemax) , hmid40_2, legend='se2',color='red')
call execplot(xlabel='stage',title='house in mid wealth share 40%',xlim=(/linjie,agemax*1d0/))

call plot(age(agemin:agemax) , hmid40_1, legend='se1',color='blue')
call plot(age(agemin:agemax) , hmid40_3, legend='se3',color='green')
call execplot(xlabel='stage',title='house in mid wealth share 40%',xlim=(/linjie,agemax*1d0/))


call plot(age(agemin:agemax), hbot50_1, legend='se1',color='blue')
call plot(age(agemin:agemax), hbot50_2, legend='se2',color='red')
call execplot(xlabel='stage',title='house in bot wealth share 50%',xlim=(/linjie,agemax*1d0/))

call plot(age(agemin:agemax), hbot50_1, legend='se1',color='blue')
call plot(age(agemin:agemax), hbot50_3, legend='se3',color='green')
call execplot(xlabel='stage',title='house in bot wealth share 50%',xlim=(/linjie,agemax*1d0/))

call plot(age(agemin:agemax), hbot20_1, legend='se1',color='blue')
call plot(age(agemin:agemax), hbot20_2, legend='se2',color='red')
call execplot(xlabel='stage',title='house in bot wealth share 20%',xlim=(/linjie,agemax*1d0/))

call plot(age(agemin:agemax), hbot20_1, legend='se1',color='blue')
call plot(age(agemin:agemax), hbot20_3, legend='se3',color='green')
call execplot(xlabel='stage',title='house in bot wealth share 20%',xlim=(/linjie,agemax*1d0/))


!***********************************

call plot(age(agemin:agemax), ltop10_1, legend='se1',color='blue')
call plot(age(agemin:agemax), ltop10_2, legend='se2',color='red')
call execplot(xlabel='stage',title='liquid asset in top wealth share 10%',xlim=(/linjie,agemax*1d0/))

call plot(age(agemin:agemax), ltop10_1, legend='se1',color='blue')
call plot(age(agemin:agemax), ltop10_3, legend='se3',color='green')
call execplot(xlabel='stage',title='liquid asset in top wealth share 10%',xlim=(/linjie,agemax*1d0/))

call plot(age(agemin:agemax), ltop20_1, legend='se1',color='blue')
call plot(age(agemin:agemax), ltop20_2, legend='se2',color='red')
call execplot(xlabel='stage',title='liquid asset in top wealth share 20%',xlim=(/linjie,agemax*1d0/))

call plot(age(agemin:agemax), ltop20_1, legend='se1',color='blue')
call plot(age(agemin:agemax), ltop20_3, legend='se3',color='green')
call execplot(xlabel='stage',title='liquid asset in top wealth share 20%',xlim=(/linjie,agemax*1d0/))


call plot(age(agemin:agemax) , lmid40_1, legend='se1',color='blue')
call plot(age(agemin:agemax) , lmid40_2, legend='se2',color='red')
call execplot(xlabel='stage',title='liquid asset in mid wealth share 40%',xlim=(/linjie,agemax*1d0/))


call plot(age(agemin:agemax) , lmid40_1, legend='se1',color='blue')
call plot(age(agemin:agemax) , lmid40_3, legend='se3',color='green')
call execplot(xlabel='stage',title='liquid asset in mid wealth share 40%',xlim=(/linjie,agemax*1d0/))


call plot(age(agemin:agemax), lbot50_1, legend='se1',color='blue')
call plot(age(agemin:agemax), lbot50_2, legend='se2',color='red')
call execplot(xlabel='stage',title='liquid asset in bot wealth share 50%',xlim=(/linjie,agemax*1d0/))

call plot(age(agemin:agemax), lbot50_1, legend='se1',color='blue')
call plot(age(agemin:agemax), lbot50_3, legend='se3',color='green')
call execplot(xlabel='stage',title='liquid asset in bot wealth share 50%',xlim=(/linjie,agemax*1d0/))

call plot(age(agemin:agemax), lbot20_1, legend='se1',color='blue')
call plot(age(agemin:agemax), lbot20_2, legend='se2',color='red')
call execplot(ylabel='share',xlabel='stage',title='liquid asset in bot wealth share 20%',xlim=(/linjie,agemax*1d0/))

call plot(age(agemin:agemax), lbot20_1, legend='se1',color='blue')
call plot(age(agemin:agemax), lbot20_3, legend='se3',color='green')
call execplot(xlabel='stage',title='liquid asset in bot wealth share 20%',xlim=(/linjie,agemax*1d0/))

!***************************************

call plot(age(agemin:agemax), ctop1_1, legend='se1')
call plot(age(agemin:agemax), ctop1_2, legend='se2')
call plot(age(agemin:agemax), ctop1_3, legend='se3')
call execplot(xlabel='stage',title='consumption in top wealth share 1%',xlim=(/linjie,agemax*1d0/))

call plot(age(agemin:agemax), ctop10_1, legend='se1')
call plot(age(agemin:agemax), ctop10_2, legend='se2')
call plot(age(agemin:agemax), ctop10_3, legend='se3')
call execplot(xlabel='stage',title='consumption in top wealth share 10%',xlim=(/linjie,agemax*1d0/))

call plot(age(agemin:agemax), ctop20_1, legend='se1')
call plot(age(agemin:agemax), ctop20_2, legend='se2')
call plot(age(agemin:agemax), ctop20_3, legend='se3')
call execplot(xlabel='stage',title='consumption in top wealth share 20%',xlim=(/linjie,agemax*1d0/))


call plot(age(agemin:agemax) , cmid40_1, legend='se1')
call plot(age(agemin:agemax) , cmid40_2, legend='se2')
call plot(age(agemin:agemax), cmid40_3, legend='se3')
call execplot(xlabel='stage',title='consumption in mid wealth share 40%',xlim=(/linjie,agemax*1d0/))


call plot(age(agemin:agemax), cbot50_1, legend='se1')
call plot(age(agemin:agemax), cbot50_2, legend='se2')
call plot(age(agemin:agemax), cbot50_3, legend='se3')
call execplot(xlabel='stage',title='consumption in bot wealth share 50%',xlim=(/linjie,agemax*1d0/))

call plot(age(agemin:agemax), cbot20_1, legend='se1')
call plot(age(agemin:agemax), cbot20_2, legend='se2')
call plot(age(agemin:agemax), cbot20_3, legend='se3')
call execplot(xlabel='stage',title='consumption bot wealth share 20%',xlim=(/linjie,agemax*1d0/))


call plot(age(agemin:agemax), etop10_1, legend='se1')
call plot(age(agemin:agemax), etop10_2, legend='se2')
call plot(age(agemin:agemax), etop10_3, legend='se3')
call execplot(xlabel='stage',title='income in top wealth share 10%',xlim=(/linjie,agemax*1d0/))

call plot(age(agemin:agemax), etop20_1, legend='se1')
call plot(age(agemin:agemax), etop20_2, legend='se2')
call plot(age(agemin:agemax), etop20_3, legend='se3')
call execplot(xlabel='stage',title='income in top wealth share 20%',xlim=(/linjie,agemax*1d0/))


call plot(age(agemin:agemax) , emid40_1, legend='se1')
call plot(age(agemin:agemax) , emid40_2, legend='se2')
call plot(age(agemin:agemax), emid40_3, legend='se3')
call execplot(xlabel='stage',title='income in mid wealth share 40%',xlim=(/linjie,agemax*1d0/))


call plot(age(agemin:agemax), ebot50_1, legend='se1')
call plot(age(agemin:agemax), ebot50_2, legend='se2')
call plot(age(agemin:agemax), ebot50_3, legend='se3')
call execplot(xlabel='stage',title='income in bot wealth share 50%',xlim=(/linjie,agemax*1d0/))

call plot(age(agemin:agemax), ebot20_1, legend='se1')
call plot(age(agemin:agemax), ebot20_2, legend='se2')
call plot(age(agemin:agemax), ebot20_3, legend='se3')
call execplot(xlabel='stage',title='income bot wealth share 20%',xlim=(/linjie,agemax*1d0/))






end 



function sum_top(dist,values,percentile)
real(8),dimension(:):: dist,values
real(8) percentile,s,v
real(8) sum_top
integer n,w,nw
nw=size(dist)
s=0d0
v=0d0
sum_top=0d0
do w=nw,1,(-1)
 s=s+dist(w)
 v=v+dist(w)*values(w)
 if (s .ge. percentile) then
 sum_top=v-(s-percentile)*values(w)
 exit
 end if
end do
!sum_top=sum_top/dot_product(dist, values)
end 


function sum_bot(dist,values,percentile)
real(8),dimension(:):: dist,values
real(8) percentile,s,v
real(8) sum_bot
integer n,w,nw
nw=size(dist)
s=0d0
v=0d0
sum_bot=0d0
do w=1,nw
 s=s+dist(w)
 v=v+dist(w)*values(w)
 if (s .ge. percentile) then
 sum_bot=v-(s-percentile)*values(w)
 exit
 end if
end do
!sum_bot=sum_bot/dot_product(dist, values) !  this is for the relative measure

end 

subroutine house_distribution_plot
real(8) linjie

call house_distribution_cal(1)
call house_distribution_cal(2)
call house_distribution_cal(3)

call plot(age(agemin:agemax), hsharetop1_1, legend='se1')
call plot(age(agemin:agemax), hsharetop1_2, legend='se2')
call plot(age(agemin:agemax), hsharetop1_3, legend='se3')
call execplot(xlabel='stage',ylabel='share',title=' top house share 1%',xlim=(/linjie,agemax*1d0/))


call plot(age(agemin:agemax), hsharetop10_1, legend='se1')
call plot(age(agemin:agemax), hsharetop10_2, legend='se2')
call plot(age(agemin:agemax), hsharetop10_3, legend='se3')
call execplot(ylabel='share',xlabel='stage',title=' top house share 10%',xlim=(/linjie,agemax*1d0/))


!-------------------------------------------------------

call plot(age(agemin:agemax), hsharebot50_1, legend='se1')
call plot(age(agemin:agemax), hsharebot50_2, legend='se2')
call plot(age(agemin:agemax), hsharebot50_3, legend='se3')
call execplot(ylabel='share',xlabel='stage',title=' bot house share 20%',xlim=(/linjie,agemax*1d0/))

call plot(age(agemin:agemax), hsharemid40_1, legend='se1')
call plot(age(agemin:agemax), hsharemid40_2, legend='se2')
call plot(age(agemin:agemax), hsharemid40_3, legend='se3')
call execplot(ylabel='share',xlabel='stage',title=' bot house share 10%',xlim=(/linjie,agemax*1d0/))


!call plot(age(agemin:agemax), hsharebot20_1, legend='se1')
!call plot(age(agemin:agemax), hsharebot20_2, legend='se2')
!call plot(age(agemin:agemax), hsharebot20_3, legend='se3')
!call execplot(ylabel='share',xlabel='stage',title=' bot house share 20%')



end 


subroutine house_distribution_cal(flag)
use toolbox
real(8) h_dist(7,nh)
integer j,h,flag
! i need to input the matrix m. with the matrix m, I can get the distribution for house and liquid asset imeediately.
! for each age-period. 20s, 30s, 40s....
! we plot the top1%, top10%, top20%, bot1%, bot10%, bot20% share.

if (flag==1) then
m=m1
end if
if (flag==2)  then
m=m2
end if
if (flag==3) then
m=m3
end if

do j=1,7
   do h=1,nh
      h_dist(j,h)=sum(  m(:,h,:,21+(j-1)*10:20+j*10)    )    
   end do

    h_dist(j,:)=h_dist(j,:)/sum(h_dist(j,:))
   ! print*, 'for', flag,j
   ! print*,'the distribution for house', h_dist(j,:)
   if (flag==1) then
hsharetop1_1(j)=hshare_top(h_dist(j,:),0.01d0)
hsharetop10_1(j)=hshare_top(h_dist(j,:),0.1d0)
hsharetop20_1(j)=hshare_top(h_dist(j,:),0.2d0)
hsharebot50_1(j)=hshare_bot(h_dist(j,:),0.5d0)
hsharemid40_1(j)=1d0-(hshare_bot(h_dist(j,:),0.3d0)+ hshare_bot(h_dist(j,:),0.3d0) )
end if

if (flag==2) then
hsharetop1_2(j)=hshare_top(h_dist(j,:),0.01d0)
hsharetop10_2(j)=hshare_top(h_dist(j,:),0.1d0)
hsharetop20_2(j)=hshare_top(h_dist(j,:),0.2d0)
hsharebot50_2(j)=hshare_bot(h_dist(j,:),0.5d0)
hsharemid40_2(j)=1d0-(hshare_bot(h_dist(j,:),0.3d0)+ hshare_bot(h_dist(j,:),0.3d0) )
end if

if (flag==3) then
hsharetop1_3(j)=hshare_top(h_dist(j,:),0.01d0)
hsharetop10_3(j)=hshare_top(h_dist(j,:),0.1d0)
hsharetop20_3(j)=hshare_top(h_dist(j,:),0.2d0)
hsharebot50_3(j)=hshare_bot(h_dist(j,:),0.5d0)
hsharemid40_3(j)=1d0-(hshare_bot(h_dist(j,:),0.3d0)+ hshare_bot(h_dist(j,:),0.3d0) )

end if

end do

! the above is about the housing distribution. we next need to calcualte wealth distriution.

end 

function hshare_top(h_dist,percentile)
real(8) h_dist(nh),percentile,v,s
real(8) hshare_top
integer h
s=0d0
v=0d0
hshare_top=0d0
do h=nh,1,(-1)
 s=s+h_dist(h)
 v=v+h_dist(h)*gridh(h)
 if (s .ge. percentile) then
 hshare_top=v-(s-percentile)*gridh(h)
 exit
 end if
end do
hshare_top=hshare_top/dot_product(h_dist, gridh)
end 

function hshare_bot(h_dist,percentile)
real(8) h_dist(nh),percentile,v,s, hshare_bot
integer h
s=0d0
v=0d0
hshare_bot=0d0
do h=1,nh
 s=s+h_dist(h)
 v=v+h_dist(h)*gridh(h)
 if (s .ge. percentile) then
 hshare_bot=v-(s-percentile)*gridh(h)
 exit
 end if
end do
!hshare_bot=hshare_bot/dot_product(h_dist, gridh)
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


!call plot((/(i,i=agemin,agemax)/)*1d0, life_ownership)
end


