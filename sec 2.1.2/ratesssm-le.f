      implicit real*4 (a-h,o-z)
c..............................................................
c.................. neutrino flux, different sources ..........
c..............................................................
      dimension E_pp(84),pp(84)
      real*4 n13(502)
      dimension E_cno(502),o15(502),f17(502)
      dimension e_b8(155),b8(155)
      dimension e_b8_2005(800),B8_2005(800)
      dimension e_hep(1000),hep(1000)
      common /pp/ E_pp,pp
      common /CNO/ E_cno,n13,o15,f17
      common /B8/ E_B8,B8
      common /B8_2005/e_b8_2005,b8_2005
      common /hep/ E_hep,hep
      dimension flux(8)
      data flux/5.98,1.44e-2,7.98e-7,4.93e-1,
     %              5.46e-4,2.78e-2,2.05e-2,5.28d-4/
c      data flux/5.991,1.421E-02,7.930E-07,4.844E-01,
c     %      5.691E-04,3.066E-02,2.331E-02,5.836E-04/ 
      common /nuflux/flux
c..............................................................
      parameter (nenu=401)
      dimension enu(nenu)
c..............................................................
c.................. Gallex/GNO, Sage, Homestake ...............
c..............................................................
      dimension probgach(nenu,8)
      dimension e_gal(58),cross_gal(58)
      dimension e_chl(19),cross_chl(19)
      dimension rr(8,3),rrssm(8,3)
      data rrssm/
     %   71.2564, 2.9692,5.7006E-02,35.40,13.167,1.677,2.332,6.041E-02,
     %   71.2564, 2.9692,5.7006E-02,35.40,13.167,1.677,2.332,6.041E-02,
     %   0.,0.24069,3.5191E-02,1.183,6.2516,4.573E-02,0.1427,3.7121E-03/
      common /bahcall_1/ E_gal,cross_gal
      common /bahcall_2/ E_chl,cross_chl
c..............................................................
c..............................................................


c. gallex/GNO and sage ...................................     
      open(11,file='../datafiles/galliumcross.dat',status='old')
      do i=1,58
         read(11,*)E_gal(i),cross_gal(i)
      enddo
      close(11)
c. homestake .............................................     
      open(12,file='../datafiles/chlorinecross.dat',status='old')
      do i=1,19
         read(12,*)E_chl(i),cross_chl(i)
      enddo
      close(12)
      
c..............................................................
c................... solar neutrino inputs ....................
c..............................................................
c.. neutrino spectrum
      open(11,file='../datafiles/ppenergy.dat',status='old')
      do i=1,84
         read(11,*)E_pp(i),pp(i)
      enddo
      close(11)
      open(13,file='../datafiles/cno.dat',status='old')
      do i=1,502
         read(13,*)E_cno(i),n13(i),o15(i),f17(i)
      enddo
      open(12,file='../datafiles/b8spec-2006.dat',status='old')
      do i=1,155
         read(12,*)e_b8(i),b8(i)
         b8(i)=1.e-3*b8(i)
      enddo
      close(12)
      open(12,file='../datafiles/b8sp2000.dat',status='old')
      read(12,*)
      read(12,*)
      do i=1,800
         read(12,*)E_B8_2005(i),B8_2005(i)
      enddo
      close(12)
      open(14,file='../datafiles/hepspectrum.dat',status='old')
      do i=1,1000
         read(14,*)e_hep(i),hep(i)
      enddo
      close(14)      




c------------------- SSM predictions ----------------------      
      do j=1,nenu
         enu(j)=0.1+19.9*float(j)/float(nenu)
         probgach(j,1)=1.
         probgach(j,2)=1.
         probgach(j,3)=1.
         probgach(j,4)=1.
         probgach(j,5)=1.
         probgach(j,6)=1.
         probgach(j,7)=1.
         probgach(j,8)=1.
      enddo
      call rates(nenu,enu,probgach,rr)
      write(*,*)(rr(k,1),k=1,8)
      write(*,*)(rr(k,2),k=1,8)
      write(*,*)(rr(k,3),k=1,8)

      end


