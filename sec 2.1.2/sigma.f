      implicit real*4 (a-h,o-z)

      open(11,file='prob-lowE.dat',status='old')
      chi2minle=1.e5
      do i=1,10000
         read(11,*,end=12)a,chi2le
         if(chi2le.lt.chi2minle)then
            chi2minle=chi2le
            pminle=a
         endif
      enddo
 12   continue
      iend=i-1
      close(11)
c      write(*,*)chi2minle,pminle
c      stop      

c.. finding sigple by visual comparison      
      pminle=0.573
      sigple=0.06

      open(11,file='prob-lowE.dat',status='old')
      open(22,file='sig-LE.dat',status='unknown')
      do i=1,iend
         read(11,*)a,chi2le
         chi2tstle=((a-pminle)/sigple)**2.
         write(22,*)a,chi2le-chi2minle,chi2tstle
      enddo
      close(11)
      
      end
