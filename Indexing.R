# Index

# Generate 2x3 marix
X <− matrix(c (4 , 7 , 3 , 8 , 9 , 2) , nrow = 2)
X

X[2,2]
X[,2]
?rnorm
numbers_1<-rnorm (80 ,mean=0,sd=1)
mat_1<- matrix( numbers_1 ,nrow=20, ncol=4)
mat_1

# convert into data frame (with headers)
df_1<-data.frame (mat_1)
df_1
names(df_1)<-c("var1","var2","var3","var4")
df_1
head(df_1)

df<−data.frame(plot="location_name_1", measure1=runif(100)∗1000,measure2=round(runif(100)∗100),value=rnorm (100 ,2 ,1) ,ID=rep (LETTERS,100))

df[df$value>3.0,]

df [ df$value >3.2 | df$measure1>950 ,]
