# [ 모집단과 표본 ]
# 모집단의 평균이 mu, 표준편차가 sigma인 경우
# 모집단으로부터 추출된 표본의 평균(xbar)의 평균(E(xbar))은 결국 모집단의 평균인 mu에 근사,
# 표본평균의 표준편차(sd(xbar))는 sigma/sqrt(n)에 근사
# 표본평균들의 분포를 확인하면 정규분포를 따름

# xbar ~ N(mu, (sigma/sqrt(n))^2)

# [ 테스트1 : 표준정규분포를 따르는 모집단으로부터 추출된 표본평균의 분포 확인 ]
rnorm(n=10, mean=0, sd=1)    # 평균이 0, 표준편차가 1인 정규분포를 따르는 난수 10개 생성
rnorm(n=50, mean=0, sd=1)    # 평균이 0, 표준편차가 1인 정규분포를 따르는 난수 50개 생성
rnorm(n=100, mean=0, sd=1)   # 평균이 0, 표준편차가 1인 정규분포를 따르는 난수 100개 생성

mean(rnorm(n=10, mean=0, sd=1))   # 한번의 추출로 표본평균이 모집단의 평균에
mean(rnorm(n=50, mean=0, sd=1))   # 근사해진다는 결론은 얻기 어려움
mean(rnorm(n=100, mean=0, sd=1))

# [ 참고 ]
v1 <- c()
v1 <- c(v1, f1)
# 은

v1 <- c()
v1[1] <- 1
v1[2] <- 2
# 와 동일한 표현

# 1000개의 표본을 추출할 경우 각 표본의 평균이 나타내는 분포와
# 표본평균의 평균이 모평균에 근사해짐을 확인
v_10 <- c() ; v_50 <- c() ; v_100 <- c()
for (i in 1:1000) {
  v_10[i] <- mean(rnorm(n=10, mean=0, sd=1))
  v_50[i] <- mean(rnorm(n=50, mean=0, sd=1))
  v_100[i] <- mean(rnorm(n=100, mean=0, sd=1))
}

mean(v_10)    # 0.008810354, 모평균인 0에 근사
mean(v_50)    # 0.00117597, 모평균인 0에 근사
mean(v_100)   # -0.00263084, 모평균인 0에 근사

par(mfrow=c(1,3))
hist(v_10)   # 정규분포에 근사
hist(v_50)
hist(v_100)

# 표본평균의 표준편차와 모집단의 표준편차와의 관계
sd(v_10)       # 0.3076527
1 / sqrt(10)   # 0.3162278

sd(v_50)       # 0.137132
1 / sqrt(50)   # 0.1414214

sd(v_100)       # 0.0968927
1 / sqrt(100)   # 0.1

# xbar ~ N(mu, (sigma/sqrt(n))^2)
# xbar의 실제 분포와 이론적 분포와의 관계 시각화
x_v10 <- seq(-1,1,0.01)
y_v10 <- dnorm(x_v10, mean = 0, sd = 1 / sqrt(10))
y_v50 <- dnorm(x_v10, mean = 0, sd = 1 / sqrt(50))
y_v100 <- dnorm(x_v10, mean = 0, sd = 1 / sqrt(100))

par(mfrow=c(1,3))
hist(v_10, prob = T, ylim=c(0,1.5))        # 실제분포
lines(x_v10, y_v10, type='l', col='red')   # 이론적분포

hist(v_50, prob = T, ylim=c(0,3))
lines(x_v10, y_v50, type='l', col='red')

hist(v_100, prob = T, ylim=c(0,5))
lines(x_v10, y_v100, type='l', col='red')

# [ 테스트2 : 이항분포를 따르는 모집단으로부터 추출된 표본평균의 분포 확인 ]
X ~ B(n,p)   # mu = np, var = np(1-p)
X ~ B(100,0.5)

# 1) 1000개의 샘플로부터 얻는 표본 평균의 분포 확인
mu <- 100 * 0.5
sigma <- sqrt(100*0.5*0.5)
n10 <- 10
n50 <- 50
n100 <- 100

xbar_10 <- c() ; xbar_50 <- c() ; xbar_100 <- c()

for (i in 1:1000) {
  xbar_10[i] <- mean(rbinom(n=10, size=100, prob=0.5))
  xbar_50[i] <- mean(rbinom(n=50, size=100, prob=0.5))
  xbar_100[i] <- mean(rbinom(n=100, size=100, prob=0.5))
}

# 1-1) sample size 10인 표본평균의 분포
# xbar_10 ~ N(50, 25/sqrt(10))   # mu = 50, sigma = 5
xb_10 <- seq(min(xbar_10), max(xbar_10), 0.01)
yb_10 <- dnorm(xb_10, mean = mu, sd = sigma / sqrt(n10))

par(mfrow=c(1,3))
hist(xbar_10, prob=T, ylim=c(0,0.3))       # 실제분포
lines(xb_10, yb_10, type='l', col='red')   # 이론적분포

# 1-2) sample size 50인 표본평균의 분포
# xbar_50 ~ N(50, 25/sqrt(50))   # mu = 50, sigma = 5
xb_50 <- seq(min(xbar_50), max(xbar_50), 0.01)
yb_50 <- dnorm(xb_50, mean = mu, sd = sigma / sqrt(n50))

hist(xbar_50, prob=T, ylim=c(0,0.6))       # 실제분포
lines(xb_50, yb_50, type='l', col='red')   # 이론적분포

# 1-3) sample size 100인 표본평균의 분포
# xbar_100 ~ N(50, 25/sqrt(100))   # mu = 50, sigma = 5
xb_100 <- seq(min(xbar_100), max(xbar_100), 0.01)
yb_100 <- dnorm(xb_100, mean = mu, sd = sigma / sqrt(n100))

hist(xbar_100, prob=T, ylim=c(0,0.9))       # 실제분포
lines(xb_100, yb_100, type='l', col='red')   # 이론적분포

# 2) 표본 평균의 평균이 모집단의 평균(mu=50)와 비슷한지 확인
mean(xbar_10)    # 50.0101
mean(xbar_50)    # 49.97442
mean(xbar_100)   # 49.99179

# 3) 표본 평균의 표준편차가 모집단의 표준편차(sigma) / sqrt(n)과 비슷한지 확인
sd(xbar_10)         # 1.604476
sigma / sqrt(10)    # 1.581139

sd(xbar_50)         # 0.6959669
sigma / sqrt(50)    # 0.7071068

sd(xbar_100)        # 0.4968236
sigma / sqrt(100)   # 0.5

xbar ~ N(mu, (sigma / sqrt(n))^2)

# xbar의 평균 : mu
# xbar의 표준편차 : sigma / sqrt(n)

# xbar로부터 모평균 추정
# xbar ~ N(mu, (sigma / sqrt(n))^2)인 사실을 이용하여
# xbar의 표준화된 확률변수 Z = (xbar - mu)/sigma/sqrt(n) ~ N(0,1)임을 알 수 있다.

# P(-1.96 <= (xbar - mu)/sigma/sqrt(n) <= 1.96) = 95%이므로

# P(xbar - 1.96 * sigma/sqrt(n) <= mu <= xbar + 1.96 * sigma/sqrt(n)) = 95%

# 따라서 한 번 샘플링한 표본평균이 정규분포를 따른다는 가정하에
# 모집단의 평균은 [ xbar - 1.96 * sigma/sqrt(n), xbar + 1.96 * sigma/sqrt(n) ]
# 구간안에 있게 될 확률이 95%이고 이를 95% 신뢰구간이라 표현한다.

# 99% 신뢰구간
X ~ N(0,1)
P(X <= x) = 0.01/2
qnorm(0.01/2,mean=0,sd=1)     # -2.575829. 약 -2.58
qnorm(1-0.01/2,mean=0,sd=1)   #  2.575829. 약  2.58

# [ xbar - 2.58 * sigma/sqrt(n), xbar + 2.58 * sigma/sqrt(n) ]


# 예제)
# 우리나라 2세 영아의 머리 둘레는 작년과 분산이 동일할 것으로 확인(500).
# 한 번 추출한 샘플의 평균이 250일때(n=10) 모평균의 95% 신뢰구간을 구하여라.

# X ~ N(250, 500)
# xbar ~ N(250, (sqrt(500)/sqrt(10))^2) = N(250, (sigma / sqrt(n1))^2)

alpha <- 0.05   # 95% 신뢰수준
n1 <- 10
sigma <- sqrt(500)
xbar <- 250

# 95% 신뢰구간
# 1) 표준정규분포 근사 공식에 대입
ld <- xbar - 1.96 * sigma / sqrt(n1)   # 236.1407
lu <- xbar + 1.96 * sigma / sqrt(n1)   # 263.8593

# 2) xbar의 분포로부터 확인
qnorm(alpha/2, mean=xbar, sd=sigma/sqrt(n1))     # 236.141
qnorm(1-alpha/2, mean=xbar, sd=sigma/sqrt(n1))   # 263.859

# 확률변수 xbar의 분포 시각화
# xbar ~ N(250, (sqrt(500)/sqrt(10))^2) = N(250, (sigma / sqrt(n1))^2)
v_x <- seq(230,270,0.01)
v_y <- dnorm(v_x, mean = xbar, sd = sigma / sqrt(n1))
plot(v_x, v_y, type='l')

f <- function(x) {
  dnorm(x, mean = xbar, sd = sigma / sqrt(n1))
}
abline(h=0)
arrows(ld,0,ld,f(ld),length=0)   # 하한 신뢰구간 표시
arrows(lu,0,lu,f(lu),length=0)   # 상한 신뢰구간 표시

polygon(c(ld, seq(ld,lu,0.01), lu),
        c(0, f(seq(ld,lu,0.01)), 0),
        col = 'red')


# 99% 신뢰구간
# 1) 표준정규분포 근사 공식에 대입
ld <- xbar - 2.58 * sigma / sqrt(n1)   # 231.7566
lu <- xbar + 2.58 * sigma / sqrt(n1)   # 268.2434

# 2) xbar의 분포로부터 확인
alpha <- 0.01   # 99% 신뢰수준
qnorm(alpha/2, mean=xbar, sd=sigma/sqrt(n1))     # 231.7861
qnorm(1-alpha/2, mean=xbar, sd=sigma/sqrt(n1))   # 268.2139


# A사 K모델 자동차의 연비는 평균 12.5(km/l), 표준편차 0.5(km/l)로
# 알려져 있는데, 새로 개발된 엔진을 장착한 40대의 자동차 연비를
# 측정한 결과 표본평균이 12.64(km/l)로 나왔다.
# H0 : mu = 12.5   # 영가설, 귀무가설
# H1 : mu > 12.5   # 대립가설, 대안가설

xbar <- 12.64
n <- 40
sigma <- 0.5

# 1-1) 새로 개발된 엔진의 연비는 95% 신뢰수준(0.05 유의수준) 하 신뢰구간을 구해보자.
ld <- xbar - 1.96 * sigma / sqrt(n)
lu <- xbar + 1.96 * sigma / sqrt(n)
c(ld, lu)   # 12.48505, 12.79495

# 1-2) 새로 개발된 엔진의 연비는 기존보다 개선되었다는 가설의 기각 여부 확인
# H1은 채택하기 어렵다
# 기존연비보다 개선되었다고 보기 어렵다


# 예제2)
# 랜덤하게 샘플링한 초콜릿 16개 무게의 표본평균이 199.5(g),
# 모분산이 25.0라고 알려져 있을 때,
# 해당 라인에서 생산된 초콜릿의 무게는 200이다 라는 가설에 대한 검정 수행

xbar <- 199.5
n <- 16
sigma <- sqrt(25.0)

ld <- xbar - 1.96 * sigma / sqrt(n)
lu <- xbar + 1.96 * sigma / sqrt(n)
c(ld, lu)   # 197.05, 201.95


# 선생님
# H0 : mu = 200
# H1 : mu != 200
n <- 16
xbar <- 199.5
sigma <- 5

# E(xbar) = mu
# X ~ N(199.5, 25)
# xbar ~ N(199.5, (sigma / sqrt(n))^2)

ld <- xbar - 1.96 * sigma / sqrt(n)
lu <- xbar + 1.96 * sigma / sqrt(n)

c(ld, lu)   # [197.05, 201.95]
