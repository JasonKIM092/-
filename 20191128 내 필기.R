# 정규 분포 : X ~ N(mu, sigma^2)
# - 종모양, 평균을 기준으로 좌우 대칭의 모양을 한 분포
# - 중심(평균)에 밀집, 좌우로 갈수록 얇아지는 특징
# - mu(평균), sigma(표준편차) 두 모수를 갖는 특징
# - 표본평균의 분포를 이해하는데 필요

# 표준 정규 분포 : X ~ N(0,1)
# - 정규분포를 따르는 X를 표준화 시킨 확률변수가 갖는 분포
# - (X - mu)/sigma ~ N(0,1)
# - 표준화 시킨 확률변수는 평균이 0, 분산이 1인 표준정규분포를 따른다

# 정규분포곡선 시각화
# 1) X ~ N(10,4)   # mu = 10, sigma = 2
x_vector <- seq(0,20,0.01)
y_vector <- dnorm(x=x_vector, mean = 10, sd = 2)   # mean은 분포와 평균, sd는 표준편차 의미

plot(x_vector, y_vector, type = 'l')

p(X<=10) = pnorm(10, mean = 10, sd = 2)

# 2) X ~ N(0,1)
x_vector <- seq(-3,3,0.01)
y_vector <- dnorm(x=x_vector, mean = 0, sd = 1)   # mean은 분포와 평균, sd는 표준편차 의미

plot(x_vector, y_vector, type = 'l')
qnorm(0.025, 0, 1)   # 약 1.96
abline(h=0)

polygon(c(-1.96, seq(-1.96,1.96,0.001),1.96), c(0,dnorm(seq(-1.96,1.96,0.001)),0), 
        col = 'red', border = TRUE)


# 선생님
x1 <- seq(-3,3,0.01)
y1 <- dnorm(x1,0,1)
plot(x1,y1,type = 'l')

v_down <- qnorm(0.025,mean=0,sd=1)   # -1.959964. 약 1.96
v_up <- qnorm(1-0.025,mean=0,sd=1)   #  1.959964. 약 1.96

f <- function(x) {
  dnorm(x,0,1)
}

abline(h=0)                             # y = 0 그래프
arrows(v_up,0,v_up,f(v_up), length=0)   # 상한구간 표시
arrows(v_down,0,v_down,f(v_down), length=0)

arrows(v_up,0.1,v_up,f(v_up), length=0.1)
arrows(v_down,0.1,v_down,f(v_down), length=0.1)

text(v_up, 0.12, paste('X=', round(v_up,2)))
text(v_down, 0.12, paste('X=', round(v_down,2)))

polygon(c(v_down, seq(v_down,v_up,0.001), v_up),   # seq에서 선생님은 갭을 0.01로 했는데 그럴경우
        c(0, f(seq(v_down,v_up,0.001)),0),         # 양수부분 끝자리가 어긋나서 나는 0.001로 설정
        col='red')

text(0, 0.2, '95%', cex=5)

