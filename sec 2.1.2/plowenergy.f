      implicit real*4 (a-h,o-z)
c.....................................................................
c..................... Gallex/GNO, Sage, Homestake ...................
c..... SSM (Standard Solar Model) given by program ratesssm-le.f .....
c.....................................................................
      dimension rr(8,3),rrssm(8,3)
      data rrssm/
     %   71.2564, 2.9692,5.7006E-02,35.40,13.167,1.677,2.332,6.041E-02,
     %   71.2564, 2.9692,5.7006E-02,35.40,13.167,1.677,2.332,6.041E-02,
     %   0.,0.24069,3.5191E-02,1.183,6.2516,4.573E-02,0.1427,3.7121E-03/
c.....................................................................
c......... high-energy chi2 map given by independent program  ........
c.....................................................................
      dimension probb(0:300),chi2b8(0:300),fb8(0:300)
c.....................................................................
c.....................................................................

      open(30,file='prob-highE.dat',status='old')
      do iprob=0,300
         read(30,*)probb(iprob),chi2b8(iprob),fb8(iprob)
      enddo

      open(31,file='prob-lowE.dat',status='unknown')

      do ipp=-70,70        
         p_pp=0.57+.07*float(ipp)/70.
      
         sigbe=0.05
         sigpep=0.11
         chi2min=1.e5

         do ipep=-3,4           !previously stablished in a larger range 
            p_pep=0.43+sigpep*float(ipep)/10.
            
            do ibe=-6,7         !previously stablished in a larger range
               p_be=0.52+sigbe*float(ibe)/10.
               
               do i8b=-1,1      !previously stablished in a larger range
                  p_8b=0.324+0.014*float(i8b)/10.
                  ffmin=divdif(fb8,probb,300,p_8b,2)
                  chi2_b8=divdif(chi2b8,probb,300,p_8b,2)

                  do l=1,3
                     rr(1,l)=rrssm(1,l)*p_pp
                     rr(2,l)=rrssm(2,l)*p_pep
                     rr(3,l)=rrssm(3,l)*p_8b
                     rr(4,l)=rrssm(4,l)*p_be
                     rr(5,l)=rrssm(5,l)*p_8b
                     rr(6,l)=rrssm(6,l)*p_pep
                     rr(7,l)=rrssm(7,l)*p_pep
                     rr(8,l)=rrssm(8,l)*p_pep
                  enddo
                  rbe7=(p_be + (1.-p_be)*0.22)*74. !prevision in 0805.3843
                  
                  call chi2lowenu(rr,rbe7,chi2le
     %                                 ,chi2ga,chi2gach,chi2be7,ffmin)
                  chi2=chi2ga+chi2_b8+((p_be-0.52)/sigbe)**2.
     %                 +((p_pep-0.43)/sigpep)**2.
                  if(chi2.lt.chi2min)then
                     chi2min=chi2
                     i8bmin=i8b
                     ibemin=ibe
                     ipepmin=ipep
                  endif
      
               enddo
            enddo
         enddo
c      write(31,*)p_pp,chi2min,p8min,pbemin,pepmin
         write(31,*)p_pp,chi2min,i8bmin,ibemin,ipepmin
      enddo
      end


