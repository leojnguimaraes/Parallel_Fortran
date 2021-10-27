      PROGRAM PI_Monte_Carlo
      USE OMP_LIB

c     Programa para calcular o número PI via Método de Monte Carlo

c     OBS:a função intrínseca do fortran "random_number" não serve para usar com OpenMP

c     Fonte para geração paralela de número randômico:
c     https://stackoverflow.com/questions/49465301/fortran-openmp-code-much-slower-than-its-not-parallel-version

c     Fonte que explica a diretiva FIRSTPRIVATE:
c     https://stackoverflow.com/questions/15304760/how-are-firstprivate-and-lastprivate-different-than-private-clauses-in-openmp

      IMPLICIT NONE

      REAL*8 total_trial, total_inside_circle, x(2), pi, pi_approx
      REAL*8 wtime

      INTEGER :: i, j, ntotal

      INTEGER :: iv(32), iy, idum2, idum
      idum2 = 123456789  ; iv(:) = 0  ; iy = 0

      ntotal=30000

      total_trial  = dfloat(ntotal)*dfloat(ntotal)
      total_inside_circle     = 0.0d0

      wtime=omp_get_wtime()

      !$OMP PARALLEL PRIVATE(x,idum) FIRSTPRIVATE(idum2,iv,iy) 

      idum = - omp_get_thread_num()

      !$OMP DO REDUCTION(+:total_inside_circle) SCHEDULE(DYNAMIC)
              DO i=1,ntotal
                DO j=1,ntotal
                  x(1)=ran2(idum, iv, iy, idum2)
                  x(2)=ran2(idum, iv, iy, idum2)
                  if (x(1)*x(1)+x(2)*x(2).le.1.0) 
     .            total_inside_circle=total_inside_circle+1.0d0
                END DO
              END DO
      !$OMP END DO

      !$OMP END PARALLEL

      wtime=omp_get_wtime()-wtime

      pi=4.0d0*datan(1.0d0)

      pi_approx=4.0d0*total_inside_circle/total_trial

      PRINT *, 'Elapsed time:',wtime
      PRINT *, 'Monte Carlo PI approx:', pi_approx
      PRINT *, 'Error (%)', 100.0d0*dabs(pi_approx-pi)/pi

      CONTAINS

      FUNCTION ran2(idum, iv, iy, idum2)
      INTEGER idum,IM1,IM2,IMM1,IA1,IA2,IQ1,IQ2,IR1,IR2,NTAB,NDIV
      REAL*8 ran2,AM,EPS,RNMX
      PARAMETER (IM1=2147483563,IM2=2147483399,AM=1./IM1,IMM1=IM1-1,
     *IA1=40014,IA2=40692,IQ1=53668,IQ2=52774,IR1=12211,IR2=3791,
     *NTAB=32,NDIV=1+IMM1/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
      INTEGER idum2,j,k,iv(NTAB),iy
      if (idum.le.0) then
        idum=max(-idum,1)
        idum2=idum
        do 11 j=NTAB+8,1,-1
          k=idum/IQ1
          idum=IA1*(idum-k*IQ1)-k*IR1
          if (idum.lt.0) idum=idum+IM1
          if (j.le.NTAB) iv(j)=idum
11      continue
        iy=iv(1)
      endif
      k=idum/IQ1
      idum=IA1*(idum-k*IQ1)-k*IR1
      if (idum.lt.0) idum=idum+IM1
      k=idum2/IQ2
      idum2=IA2*(idum2-k*IQ2)-k*IR2
      if (idum2.lt.0) idum2=idum2+IM2
      j=1+iy/NDIV
      iy=iv(j)-idum2
      iv(j)=idum
      if(iy.lt.1)iy=iy+IMM1
      ran2=min(AM*iy,RNMX)
      return
      END

      END PROGRAM
