#1
x=c(17, 16, 20, 24 ,22, 15, 21 ,18) 
x[x>=20]
x[x>=20]="100"
y<-x
y


#2
x=matrix(c(3,-1,-1,-1,-1,-1,4,-1,-1,-1,-1,-1,5,-1,-1,-1,-1,-1,6,-1,-1,-1,-1,-1,7),5,5,byrow=T) 
x
y<-x[,-5]         
y
yinfo<-c(nrow(y),ncol(y))
yinfo
y[-1]="0"
y1<-y
y1


#3-1
rowdata <- read.csv("C:/tmp/rowdata.txt", sep="") 
rdata=data.frame(rowdata)
rdata

#3-2
r<-is.na.data.frame(rdata)

#3-3
a=0
n=1
for(i in 1:4){
  if(is.na(rdata$v2[i])==F & is.na(rdata$v3[i])==F){
    a[n]=i
    n=n+1
  }
}
a

#3-4
rdata1=rdata[a,]
rdata1




#4
temp<-list(c("TRUE","FALSE")) 
temp[[2]]<-diag(1,2,2)
temp[[3]]<-seq(0,1,length=100)
temp[[4]]<-1:4
temp[[5]]<-1:4
temp[[6]]<-1:4
temp[[7]]<-1:4

temp[[2]]<-NULL
temp

length(temp)





#1
a=1 
a[2]=3
for (i in 1:20)
{
  
  
  a[i+2]=0.9*a[i+1]-0.1*a[i]+1
}
print(a[20])
a[2]
a[]


#2
which(a>4)[1] 


#3
A <- matrix(runif(100),50,5) 
v=rep(0,nrow(A))
for(i in 1:nrow(A))
{
  v[i]=sum(A[i,])
  
}
v


#4
print(tmp) 
#[1]  7  8  9 10  0  0  0  0  0  0


#5
sid <- sample(1:10, 1000, replace=T)
sid
x = matrix(rbinom(5000,10, 1/2),1000,5)
head(x)


#6
v <- list()
for (j in 1:10){
  va <- rep(0,5)
  for (i in 1:1000){
    if (sid[i]==j){
      va = va + x[i,]
    }
  }
  v[[j]] <- va
}
v


for (i in 1:10){
  v[[i]] <- v[[i]]/sum(sid==i)
}

v


sum1 <- unlist(v)
m.mat <- matrix(sum1,10,5,byrow=TRUE)
m.mat








#6-2
d5 <- function(x,y){
  sxy = x%*%y
  sx = sqrt(x%*%x)
  sy = sqrt(y%*%y)
  return(sxy/(sx*sy))
}
d5(x[1,],m.mat[1,])
a <- matrix(rep(0,10000), 1000, 10)
for(i in 1:1000){
  for(j in 1:10)
    a[i,j] <- d5(x[i,],m.mat[j,])
}
idist <- a
head(idist)



#7
ivec <- rep(0, 1000)
for(i in 1:1000){
  ivec[i] <- min(idist[i])
}
ivec




#8-1
set.seed(1)
a = list()
for (i in 1:1000){
  x = rpois(1,4)+1
  x = min(x,10)
  a[[i]] = sample(1:10, x)
}
head(a)

f=rep(0,9)
for(j in 1:9){
  
  for(i in 1:1000){
    
  if(length(a[[i]]) == j+1){
      
    f[j] = f[j] + 1
    }
  }
}
f




#8-2

pr=rep(0,10)
for(i in 1:1000){
  if(length(a[[i]])>=2 & length(a[[i]])<=3){
    pr[a[[i]][1]] = pr[a[[i]][1]] + 1 
  }
  else if(length(a[[i]])>=4 & length(a[[i]])<=6){
    pr[a[[i]][1]] = pr[a[[i]][1]] + 2 
    pr[a[[i]][2]] = pr[a[[i]][2]] + 1 
  }
  else if(length(a[[i]])>=7 & length(a[[i]])<=10){
    pr[a[[i]][1]] = pr[a[[i]][1]] + 3 
    pr[a[[i]][2]] = pr[a[[i]][2]] + 2 
    pr[a[[i]][3]] = pr[a[[i]][3]] + 1 
  }
}
pr




#9-1
set.seed(1)
m1 = 10
m2 = 5
num = 0


for (i in 1:4){
  g<-rbinom(1,1,1/2)
  if (g==0){
    m1=m1-1
    m2=m2+1
  }
  else if (g==1){
    m1=m1+1
    m2=m2-1
  }
}

m1


#9-2
set.seed(1)
m1 = 10
m2 = 5
num = 0

while(m1>0&m2>0){
  g<-rbinom(1,1,1/2)
  if (g==0){
    m1=m1-1
    m2=m2+1
  }
  else if (g==1){
    m1=m1+1
    m2=m2-1
  }
  num = num +1
}

num
m1
m2



#9-3
nu <-c(0,0)
for(k in 1:200){
  set.seed(k)
  m1 = 10
  m2 = 5
  num = 0
  while(m1>0&m2>0){
    g<-rbinom(1,1,1/2)
    if (g==0){
      m1=m1-1
      m2=m2+1
    }
    else if (g==1){
      m1=m1+1
      m2=m2-1
    }
    if (m1==0){
      nu[2]<-nu[2]+1
    }
    else if (m2==0){
      nu[1]<-nu[1]+1
    }
  }
}

nu


#10.
nnu <- rep(0,2)
for(k in 1:200){
  set.seed(k)
  m1 = 10
  m2 = 15 # m2=10,15,20,25 대입
  num = 0
  while(m1>0&m2>0){
    g<-rbinom(1,1,1/2)
    if (g==0){
      m1=m1-1
      m2=m2+1
    }
    else if (g==1){
      m1=m1+1
      m2=m2-1
    }
    if (m1==0){
      nnu[2]<-nnu[2]+1
    }
    else if (m2==0){
      nnu[1]<-nnu[1]+1
      
    }
  }
}
p = nnu[1]/200
p





