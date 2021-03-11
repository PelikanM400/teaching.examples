# RCS and segmented model 
set.seed(20210302)

library('rms')
library('segmented')

fit.crude.linear <- glm(CPR ~ FBG, 
                        data = df, 
                        family = binomial('logit'))
summary(fit.crude.linear)

fit.crude.nonlin <- glm(CPR ~ rcs(FBG, 3), 
                        data = df, 
                        family = binomial('logit'))
summary(fit.crude.nonlin)

fit.seg <- segmented(fit.crude.linear, seg.Z = ~ FBG)
summary(fit.seg)
confint.segmented(fit.seg)
intercept(fit.seg)
slope(fit.seg)
plot.segmented(fit.seg, 
               ylab = 'lnOR', 
               xlab = 'FBG')
slope(fit.seg)
intercept(fit.seg)

ggplot() + 
  geom_smooth(aes(x = df$FBG, y = df$CPR), method = "glm", formula = y ~ rcs(x, 3), se = FALSE)