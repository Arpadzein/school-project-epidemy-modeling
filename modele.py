from numpy import *
from matplotlib.pyplot import *

def euler_SIRS(b,d,d1,c,d2,r,s,v,S0,I0,R0,n):
    P=[0]*n
    S=[0]*n
    I=[0]*n
    R=[0]*n
    P[0]=S0+I0+R0
    S[0]=S0
    I[0]=I0
    R[0]=R0
    dt=1
    for i in range (1,n):
        S[i]=S[i-1]+dt*(b*P[i-1]-(d+v)*S[i-1]-d1*P[i-1]*S[i-1]+s*R[i-1]-c*S[i-1]*I[i-1]/P[i-1])
        I[i]=I[i-1]+dt*(c*S[i-1]*I[i-1]/P[i-1]-d*I[i-1]-d2*I[i-1]-r*I[i-1]-d1*I[i-1]*P[i-1])
        R[i]=R[i-1]+dt*(r*I[i-1]+v*S[i-1]-(s+d)*R[i-1]-d1*I[i-1]*P[i-1])
        P[i]=S[i]+I[i]+R[i]
    return(P,S,I,R)
def euler_SIS(b,d,d1,c,d2,s,S0,I0,n):
    P=[0]*n
    S=[0]*n
    I=[0]*n
    P[0]=S0+I0
    S[0]=S0
    I[0]=I0
    dt=1
    for i in range (1,n):
        S[i]=S[i-1]+dt*(b*P[i-1]-d*S[i-1]-d1*P[i-1]*S[i-1]+s*I[i-1]-c*S[i-1]*I[i-1]/P[i-1])
        I[i]=I[i-1]+dt*(c*S[i-1]*I[i-1]/P[i-1]-d*I[i-1]-d2*I[i-1]-s*I[i-1]-d1*I[i-1]*P[i-1])
        P[i]=S[i]+I[i]
    return(P,S,I)

P,S,I,R = euler_SIRS(0.1,0.1,0,1,0,0.4,0,0,9999,1,0,200)
# P,S,I = euler_SIS(0,0,0,0.12,0,0.01,9999,1,200)
x=linspace(0,199,200)
plot(x,P,'k',label="Population totale",linewidth=10)
plot(x,R,'b',label="Immunisés",linewidth=10)
plot(x,S,'g',label="Sains",linewidth=10)
plot(x,I,'r',label="Infectés",linewidth=10)
legend()
xlabel("Temps")
ylabel("Population")
title("Résolution de l'équation")
show()