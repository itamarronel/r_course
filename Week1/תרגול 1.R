c(1, 3, 8) -> x
x = x * 2
x = x + 3
print(x)

x = seq(from = 1, to = 100, by =1)
print(x)

y = c("adam", "sam", "jim")

x = runif(5, -10, 10)


x = c('jim', 'sam', 'drake', 'ben', 'fred')
y = sample(x, size = 3, replace = FALSE)
print(y)

x = c(1, 2, 3)
y = c('xx', 'xy', 'yy')
df = data.frame(x, y)
save(df, file = '<your path>/df.rdata')
load(file = '<your path>/df.rdata')

# R course for beginners
# Week 1
# assignment by itamar ronel, id 032702391
x = c(1, 2, 3, 4, 5, 6) #מספר נבדק
y = c('m', 'f', 'f', 'm', 'f', 'f') #מגדר
z = runif(6, 15, 40) #גיל
d = rbinom(6, 1, 0.176) #משתנה דיכאון עם הסתברות 0.176 לפי הספרות
df = data.frame(x, y, z, d)
write.csv(df, file = "data/df.csv")




